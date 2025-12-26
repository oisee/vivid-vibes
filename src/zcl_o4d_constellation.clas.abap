CLASS zcl_o4d_constellation DEFINITION
  PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_star,
        x     TYPE f,
        y     TYPE f,
        z     TYPE f,
        hue   TYPE f,
      END OF ty_star,
      tt_stars TYPE STANDARD TABLE OF ty_star WITH EMPTY KEY.

    CONSTANTS:
      c_num_stars TYPE i VALUE 80.

    DATA:
      mv_initialized TYPE abap_bool,
      mt_stars       TYPE tt_stars.

    METHODS:
      init_stars,
      hsv_to_hex
        IMPORTING iv_h         TYPE f
                  iv_s         TYPE f
                  iv_v         TYPE f
        RETURNING VALUE(rv_hex) TYPE string.

ENDCLASS.

CLASS zcl_o4d_constellation IMPLEMENTATION.

  METHOD zif_o4d_effect~get_name.
    rv_name = 'constellation'.
  ENDMETHOD.

  METHOD zif_o4d_effect~get_params.
  ENDMETHOD.

  METHOD zif_o4d_effect~set_param.
  ENDMETHOD.

  METHOD zif_o4d_effect~render.
    DATA(ls_ctx) = VALUE zif_o4d_effect=>ty_render_ctx(
      t  = is_sync-time
      gt = is_sync-time
      bi = VALUE #( time = is_sync-time bar = is_sync-bar beat = is_sync-beat
                    bar_phase = is_sync-bar_phase pulse = is_sync-intensity )
      gbi = VALUE #( time = is_sync-time bar = is_sync-bar beat = is_sync-beat
                     bar_phase = is_sync-bar_phase pulse = is_sync-intensity )
    ).
    rs_frame = zif_o4d_effect~render_frame( ls_ctx ).
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    IF mv_initialized = abap_false.
      init_stars( ).
      mv_initialized = abap_true.
    ENDIF.

    DATA(lv_t) = is_ctx-t.
    DATA(ls_bi) = is_ctx-bi.
    DATA(lv_phase) = lv_t / 6.
    DATA(lv_base_dist) = CONV f( '0.15' ).
    DATA(lv_pulse_dist) = lv_base_dist + ls_bi-pulse * CONV f( '0.1' ).
    DATA(lv_w) = CONV f( zif_o4d_effect=>c_width ).
    DATA(lv_h) = CONV f( zif_o4d_effect=>c_height ).

    LOOP AT mt_stars ASSIGNING FIELD-SYMBOL(<star>).
      DATA(lv_idx) = sy-tabix.
      DATA(lv_x) = <star>-x + sin( lv_t * CONV f( '0.5' ) + <star>-hue * 10 ) * CONV f( '0.02' ).
      DATA(lv_y) = <star>-y + cos( lv_t * CONV f( '0.3' ) + <star>-hue * 8 ) * CONV f( '0.02' ).
      DATA(lv_brightness) = <star>-z * ( CONV f( '0.7' ) + ls_bi-pulse * CONV f( '0.3' ) ).
      DATA(lv_color) = hsv_to_hex( iv_h = <star>-hue iv_s = CONV f( '0.6' ) iv_v = lv_brightness ).
      DATA(lv_size) = 2 + <star>-z * 2.
      DATA(lv_px) = lv_x * lv_w.
      DATA(lv_py) = lv_y * lv_h.

      APPEND VALUE zif_o4d_effect=>ty_line(
        x1 = lv_px - lv_size y1 = lv_py x2 = lv_px + lv_size y2 = lv_py
        color = lv_color width = 1
      ) TO rs_frame-lines.
      APPEND VALUE zif_o4d_effect=>ty_line(
        x1 = lv_px y1 = lv_py - lv_size x2 = lv_px y2 = lv_py + lv_size
        color = lv_color width = 1
      ) TO rs_frame-lines.

      IF lv_phase > CONV f( '0.3' ).
        DATA(lv_cs) = nmin( val1 = CONV f( 1 ) val2 = ( lv_phase - CONV f( '0.3' ) ) * 2 ).
        LOOP AT mt_stars ASSIGNING FIELD-SYMBOL(<other>) FROM lv_idx + 1.
          DATA(lv_ox) = <other>-x + sin( lv_t * CONV f( '0.5' ) + <other>-hue * 10 ) * CONV f( '0.02' ).
          DATA(lv_oy) = <other>-y + cos( lv_t * CONV f( '0.3' ) + <other>-hue * 8 ) * CONV f( '0.02' ).
          DATA(lv_dx) = lv_ox - lv_x.
          DATA(lv_dy) = lv_oy - lv_y.
          DATA(lv_dist) = sqrt( lv_dx * lv_dx + lv_dy * lv_dy ).
          IF lv_dist < lv_pulse_dist.
            DATA(lv_alpha) = ( 1 - lv_dist / lv_pulse_dist ) * lv_cs.
            DATA(lv_lc) = hsv_to_hex(
              iv_h = ( <star>-hue + <other>-hue ) / 2
              iv_s = CONV f( '0.5' )
              iv_v = lv_alpha * CONV f( '0.6' )
            ).
            APPEND VALUE zif_o4d_effect=>ty_line(
              x1 = lv_px y1 = lv_py x2 = lv_ox * lv_w y2 = lv_oy * lv_h
              color = lv_lc width = 1
            ) TO rs_frame-lines.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    " Debug vars
    rs_frame-debug-vars = |\{"num_stars":{ c_num_stars },"phase":{ lv_phase },| &&
      |"base_dist":{ lv_base_dist },"pulse_dist":{ lv_pulse_dist },| &&
      |"conn_strength":{ lv_cs },"lines":{ lines( rs_frame-lines ) },"t":{ lv_t }\}|.
  ENDMETHOD.

  METHOD init_stars.
    DATA: lv_seed TYPE f VALUE '0.7391'.
    DO c_num_stars TIMES.
      lv_seed = frac( sin( lv_seed * 12345 + sy-index ) * 43758 ).
      DATA(lv_x) = lv_seed.
      lv_seed = frac( sin( lv_seed * 12345 + sy-index ) * 43758 ).
      DATA(lv_y) = lv_seed.
      lv_seed = frac( sin( lv_seed * 12345 + sy-index ) * 43758 ).
      DATA(lv_z) = CONV f( '0.5' ) + lv_seed * CONV f( '0.5' ).
      lv_seed = frac( sin( lv_seed * 12345 + sy-index ) * 43758 ).
      DATA(lv_hue) = lv_seed.
      APPEND VALUE ty_star( x = lv_x y = lv_y z = lv_z hue = lv_hue ) TO mt_stars.
    ENDDO.
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
