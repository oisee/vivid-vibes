CLASS zcl_o4d_voxel_landscape DEFINITION
  PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.

  PRIVATE SECTION.
    CONSTANTS:
      c_cols    TYPE i VALUE 40,
      c_rows    TYPE i VALUE 25.

    METHODS:
      get_height
        IMPORTING iv_x         TYPE f
                  iv_z         TYPE f
        RETURNING VALUE(rv_h)  TYPE f,
      hsv_to_hex
        IMPORTING iv_h         TYPE f
                  iv_s         TYPE f
                  iv_v         TYPE f
        RETURNING VALUE(rv_hex) TYPE string.

ENDCLASS.



CLASS ZCL_O4D_VOXEL_LANDSCAPE IMPLEMENTATION.


  METHOD zif_o4d_effect~get_name.
    rv_name = 'voxel_landscape'.
  ENDMETHOD.


  METHOD zif_o4d_effect~get_params.
  ENDMETHOD.


  METHOD zif_o4d_effect~set_param.
  ENDMETHOD.


  METHOD zif_o4d_effect~render.
    DATA(ls_ctx) = VALUE zif_o4d_effect=>ty_render_ctx(
      t  = is_sync-time  gt = is_sync-time
      bi = VALUE #( time = is_sync-time bar = is_sync-bar beat = is_sync-beat
                    bar_phase = is_sync-bar_phase pulse = is_sync-intensity )
      gbi = VALUE #( time = is_sync-time bar = is_sync-bar beat = is_sync-beat
                     bar_phase = is_sync-bar_phase pulse = is_sync-intensity )
    ).
    rs_frame = zif_o4d_effect~render_frame( ls_ctx ).
  ENDMETHOD.


  METHOD zif_o4d_effect~render_frame.
    DATA: lv_row TYPE i, lv_col TYPE i, lv_z TYPE f, lv_depth TYPE f,
          lv_scale TYPE f, lv_y_base TYPE f, lv_col_w TYPE f,
          lv_x TYPE f, lv_height TYPE f, lv_screen_x TYPE f, lv_screen_y TYPE f,
          lv_hue TYPE f, lv_sat TYPE f, lv_val TYPE f, lv_color TYPE string,
          lv_rect_h TYPE f, lv_sky_y TYPE f, lv_sky_val TYPE f, lv_sky_col TYPE string.

    DATA(lv_t) = is_ctx-t.
    DATA(ls_bi) = is_ctx-bi.
    DATA(lv_w) = CONV f( zif_o4d_effect=>c_width ).
    DATA(lv_h) = CONV f( zif_o4d_effect=>c_height ).

    " Scroll offsets
    DATA(lv_scroll_x) = lv_t * 6.
    DATA(lv_scroll_z) = lv_t * 2.

    " Sky gradient first (background)
    DATA(lv_sky_h) = lv_h * CONV f( '0.3' ).
    DO 10 TIMES.
      lv_sky_y = ( sy-index - 1 ) * lv_sky_h / 10.
      lv_sky_val = CONV f( '0.08' ) + CONV f( sy-index ) / 15.
      lv_sky_col = hsv_to_hex( iv_h = CONV f( '0.58' ) iv_s = CONV f( '0.8' ) iv_v = lv_sky_val ).
      APPEND VALUE zif_o4d_effect=>ty_rect(
        x = 0 y = lv_sky_y w = lv_w h = lv_sky_h / 10 + 1 fill = lv_sky_col
      ) TO rs_frame-rects.
    ENDDO.

    " Draw terrain from back to front
    DO c_rows TIMES.
      lv_row = c_rows - sy-index.
      lv_z = CONV f( lv_row ) + lv_scroll_z.
      lv_depth = CONV f( lv_row + 1 ) / c_rows.

      lv_scale = 1 / ( lv_depth * 3 + CONV f( '0.3' ) ).
      lv_y_base = lv_h * CONV f( '0.75' ) - lv_depth * lv_h * CONV f( '0.35' ).
      lv_col_w = lv_w / c_cols * lv_scale.
      IF lv_col_w < 1. lv_col_w = 1. ENDIF.

      DO c_cols TIMES.
        lv_col = sy-index - 1.
        lv_x = CONV f( lv_col ) + lv_scroll_x.

        lv_height = get_height( iv_x = lv_x iv_z = lv_z ).

        lv_screen_x = ( CONV f( lv_col ) / c_cols - CONV f( '0.5' ) ) * lv_w * lv_scale + lv_w / 2.
        lv_screen_y = lv_y_base - lv_height * 100 * lv_scale.

        lv_hue = CONV f( '0.25' ) - lv_height * CONV f( '0.15' ).
        lv_sat = CONV f( '0.6' ) + lv_height * CONV f( '0.25' ).
        lv_val = CONV f( '0.25' ) + lv_depth * CONV f( '0.55' ) + ls_bi-pulse * CONV f( '0.1' ).
       "lv_val = CONV f( '0.25' ) + lv_depth * CONV f( '0.55' ) + '0.5' * CONV f( '0.1' ).

        lv_color = hsv_to_hex( iv_h = lv_hue iv_s = lv_sat iv_v = lv_val ).

        IF lv_screen_x > ( 0 - lv_col_w ) AND lv_screen_x < lv_w + lv_col_w.
          lv_rect_h = lv_y_base - lv_screen_y.
          IF lv_rect_h > 0.
            APPEND VALUE zif_o4d_effect=>ty_rect(
              x = lv_screen_x - lv_col_w / 2
              y = lv_screen_y
              w = lv_col_w + 1
              h = lv_rect_h
              fill = lv_color
            ) TO rs_frame-rects.
          ENDIF.
        ENDIF.
      ENDDO.
    ENDDO.

    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_w / 2  y = 20
      text = 'VOXEL LANDSCAPE'
      color = '#88FFFF'  size = 12  align = 'center'
    ) TO rs_frame-texts.

    " Debug vars - voxel landscape state
    rs_frame-debug-vars = |\{"cols":{ c_cols },"rows":{ c_rows },| &&
      |"scroll_x":{ lv_scroll_x },"scroll_z":{ lv_scroll_z },| &&
      |"speed_x":6,"speed_z":2,"pulse":{ ls_bi-pulse },| &&
      |"wave_freqs":[0.15,0.3,0.6,0.25]\}|.
  ENDMETHOD.


  METHOD get_height.
    DATA(lv_h1) = sin( iv_x * CONV f( '0.15' ) ) * CONV f( '0.5' ).
    DATA(lv_h2) = sin( iv_x * CONV f( '0.3' ) + iv_z * CONV f( '0.2' ) ) * CONV f( '0.25' ).
    DATA(lv_h3) = sin( iv_x * CONV f( '0.6' ) + iv_z * CONV f( '0.4' ) ) * CONV f( '0.125' ).
    DATA(lv_h4) = cos( iv_z * CONV f( '0.25' ) ) * CONV f( '0.3' ).
    rv_h = lv_h1 + lv_h2 + lv_h3 + lv_h4.
    rv_h = ( rv_h + 1 ) / 2.
  ENDMETHOD.


  METHOD hsv_to_hex.
    DATA: lv_r TYPE f, lv_g TYPE f, lv_b TYPE f.
    DATA(lv_hh) = iv_h * 6.
    DATA(lv_i) = floor( lv_hh ).
    DATA(lv_f) = lv_hh - lv_i.
    DATA(lv_p) = iv_v * ( 1 - iv_s ).
    DATA(lv_q) = iv_v * ( 1 - lv_f * iv_s ).
    DATA(lv_tt) = iv_v * ( 1 - ( 1 - lv_f ) * iv_s ).
    CASE lv_i MOD 6.
      WHEN 0. lv_r = iv_v. lv_g = lv_tt. lv_b = lv_p.
      WHEN 1. lv_r = lv_q. lv_g = iv_v. lv_b = lv_p.
      WHEN 2. lv_r = lv_p. lv_g = iv_v. lv_b = lv_tt.
      WHEN 3. lv_r = lv_p. lv_g = lv_q. lv_b = iv_v.
      WHEN 4. lv_r = lv_tt. lv_g = lv_p. lv_b = iv_v.
      WHEN 5. lv_r = iv_v. lv_g = lv_p. lv_b = lv_q.
    ENDCASE.
    DATA(lv_ri) = CONV i( nmin( val1 = 255 val2 = nmax( val1 = 0 val2 = lv_r * 255 ) ) ).
    DATA(lv_gi) = CONV i( nmin( val1 = 255 val2 = nmax( val1 = 0 val2 = lv_g * 255 ) ) ).
    DATA(lv_bi) = CONV i( nmin( val1 = 255 val2 = nmax( val1 = 0 val2 = lv_b * 255 ) ) ).
    rv_hex = |#{ lv_ri ALIGN = RIGHT WIDTH = 2 PAD = '0' }{ lv_gi ALIGN = RIGHT WIDTH = 2 PAD = '0' }{ lv_bi ALIGN = RIGHT WIDTH = 2 PAD = '0' }|.
  ENDMETHOD.
ENDCLASS.
