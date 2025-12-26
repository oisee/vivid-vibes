CLASS zcl_o4d_mountains DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    METHODS constructor.

  PRIVATE SECTION.
    DATA mv_name TYPE string VALUE 'MOUNTAINS'.
    METHODS int_to_hex IMPORTING iv_int TYPE i RETURNING VALUE(rv_hex) TYPE string.
    METHODS noise IMPORTING iv_x TYPE f iv_y TYPE f RETURNING VALUE(rv_val) TYPE f.
    METHODS fbm IMPORTING iv_x TYPE f iv_y TYPE f RETURNING VALUE(rv_val) TYPE f.

ENDCLASS.

CLASS zcl_o4d_mountains IMPLEMENTATION.

  METHOD constructor.
  ENDMETHOD.

  METHOD int_to_hex.
    DATA lv_hex TYPE x LENGTH 1.
    DATA lv_val TYPE i.
    lv_val = iv_int.
    IF lv_val < 0. lv_val = 0. ENDIF.
    IF lv_val > 255. lv_val = 255. ENDIF.
    lv_hex = lv_val.
    rv_hex = |{ lv_hex }|.
  ENDMETHOD.

  METHOD noise.
    DATA: lv_ix TYPE i, lv_iy TYPE i, lv_hash TYPE f.
    lv_ix = floor( iv_x ).
    lv_iy = floor( iv_y ).
    lv_hash = sin( lv_ix * '127.1' + lv_iy * '311.7' ) * '43758.5453'.
    rv_val = lv_hash - floor( lv_hash ).
  ENDMETHOD.

  METHOD fbm.
    DATA: lv_f TYPE f VALUE 0, lv_amp TYPE f VALUE '0.5'.
    DATA: lv_x TYPE f, lv_y TYPE f.
    lv_x = iv_x. lv_y = iv_y.

    DO 5 TIMES.
      lv_f = lv_f + lv_amp * noise( iv_x = lv_x iv_y = lv_y ).
      lv_x = lv_x * 2.
      lv_y = lv_y * 2.
      lv_amp = lv_amp * '0.5'.
    ENDDO.

    rv_val = lv_f.
  ENDMETHOD.

  METHOD zif_o4d_effect~get_name.
    rv_name = mv_name.
  ENDMETHOD.

  METHOD zif_o4d_effect~get_params.
    CLEAR rt_params.
  ENDMETHOD.

  METHOD zif_o4d_effect~set_param.
  ENDMETHOD.

  METHOD zif_o4d_effect~render.
    DATA ls_ctx TYPE zif_o4d_effect=>ty_render_ctx.
    ls_ctx-t = is_sync-time.
    ls_ctx-gbi-time = is_sync-time.
    ls_ctx-gbi-bar = is_sync-bar.
    ls_ctx-gbi-beat = is_sync-beat.
    ls_ctx-gbi-bar_phase = is_sync-bar_phase.
    ls_ctx-gbi-pulse = is_sync-intensity.
    rs_frame = zif_o4d_effect~render_frame( ls_ctx ).
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    CONSTANTS: lc_w TYPE f VALUE 640,  " zif_o4d_effect=>c_width
               lc_h TYPE f VALUE 400.  " zif_o4d_effect=>c_height

    DATA: lv_t TYPE f, lv_layer TYPE i, lv_x TYPE i.
    DATA: lv_depth TYPE f, lv_morph_speed TYPE f, lv_mountain_x TYPE f.
    DATA: lv_h TYPE f, lv_mountain_y TYPE f, lv_shade TYPE f.
    DATA: lv_r TYPE i, lv_g TYPE i, lv_b TYPE i.
    DATA: lv_hex_r TYPE string, lv_hex_g TYPE string, lv_hex_b TYPE string.
    DATA: lv_sun_x TYPE f, lv_sun_y TYPE f, lv_sun_dist TYPE f.
    DATA: lv_y_pos TYPE f, lv_sky_r TYPE i, lv_sky_g TYPE i, lv_sky_b TYPE i.
    DATA ls_rect TYPE zif_o4d_effect=>ty_rect.
    DATA ls_circle TYPE zif_o4d_effect=>ty_circle.
    DATA ls_text TYPE zif_o4d_effect=>ty_text.

    lv_t = is_ctx-gbi-time * '0.3'.

    " Sky gradient (sunset colors) - 20 bands to fill 400px height
    DO 20 TIMES.
      lv_y_pos = ( sy-index - 1 ) / 20.

      " Top: dark purple, Horizon: orange/red
      lv_sky_r = 25 + ( 180 - 25 ) * ( 1 - lv_y_pos ).
      lv_sky_g = 0 + ( 80 - 0 ) * ( 1 - lv_y_pos ).
      lv_sky_b = 75 + ( 25 - 75 ) * ( 1 - lv_y_pos ).

      lv_hex_r = int_to_hex( lv_sky_r ).
      lv_hex_g = int_to_hex( lv_sky_g ).
      lv_hex_b = int_to_hex( lv_sky_b ).

      CLEAR ls_rect.
      ls_rect-x = 0. ls_rect-y = ( sy-index - 1 ) * 20.
      ls_rect-w = lc_w. ls_rect-h = 21.
      ls_rect-fill = |#{ lv_hex_r }{ lv_hex_g }{ lv_hex_b }|.
      APPEND ls_rect TO rs_frame-rects_back.  " Sky behind sun!
    ENDDO.

    " Sun (centered at 70% width)
    lv_sun_x = lc_w * '0.7'.
    lv_sun_y = lc_h * '0.2' + sin( lv_t * '0.5' ) * 40.

    CLEAR ls_circle.
    ls_circle-x = lv_sun_x.
    ls_circle-y = lv_sun_y.
    ls_circle-radius = 50.  " Bigger sun for 640x400
    ls_circle-fill = '#FF9933'.
    APPEND ls_circle TO rs_frame-circles.

    " Sun glow
    CLEAR ls_circle.
    ls_circle-x = lv_sun_x.
    ls_circle-y = lv_sun_y.
    ls_circle-radius = 70.
    ls_circle-fill = '#FF660033'.
    APPEND ls_circle TO rs_frame-circles.

    " Mountain layers (4 layers, back to front)
    DO 4 TIMES.
      lv_layer = sy-index - 1.
      lv_depth = '0.3' + lv_layer * '0.15'.
      lv_morph_speed = '0.5' - lv_layer * '0.1'.

      " Draw mountain silhouette - 64 columns for 640px width
      DO 64 TIMES.
        lv_x = ( sy-index - 1 ) * 10.
        lv_mountain_x = lv_x / lc_w * ( 2 + lv_layer ) + lv_t * ( '0.1' + lv_layer * '0.05' ).

        " Height from FBM
        lv_h = fbm( iv_x = lv_mountain_x iv_y = lv_layer + lv_t * lv_morph_speed ) * '0.4'.
        lv_h = lv_h + fbm( iv_x = lv_mountain_x * 2 iv_y = lv_layer * 2 + lv_t * lv_morph_speed * '0.7' ) * '0.2'.

        " Peak emphasis
        DATA lv_peak_x TYPE f.
        lv_peak_x = lv_mountain_x * '0.5'.
        lv_h = lv_h + nmax( val1 = 0 val2 = '0.3' - abs( ( lv_peak_x - floor( lv_peak_x ) ) - '0.5' ) * 2 ) * '0.3'.

        lv_mountain_y = lv_depth + lv_h * ( '0.4' - lv_layer * '0.05' ).
        lv_mountain_y = lv_mountain_y * lc_h.

        lv_shade = 1 - lv_layer * '0.2'.

        " Layer colors (back=purple, front=dark green)
        CASE lv_layer.
          WHEN 0.
            lv_r = 13 * lv_shade. lv_g = 5 * lv_shade. lv_b = 25 * lv_shade.
          WHEN 1.
            lv_r = 25 * lv_shade. lv_g = 13 * lv_shade. lv_b = 38 * lv_shade.
          WHEN 2.
            lv_r = 13 * lv_shade. lv_g = 38 * lv_shade. lv_b = 25 * lv_shade.
          WHEN OTHERS.
            lv_r = 5 * lv_shade. lv_g = 25 * lv_shade. lv_b = 13 * lv_shade.
        ENDCASE.

        " Snow on peaks (back layers only)
        IF lv_h > '0.35' AND lv_layer < 2.
          DATA lv_snow TYPE f.
          lv_snow = ( lv_h - '0.35' ) * 3.
          IF lv_snow > 1. lv_snow = 1. ENDIF.
          lv_r = lv_r + ( 230 - lv_r ) * lv_snow * '0.5'.
          lv_g = lv_g + ( 242 - lv_g ) * lv_snow * '0.5'.
          lv_b = lv_b + ( 255 - lv_b ) * lv_snow * '0.5'.
        ENDIF.

        " Beat pulse
        lv_r = lv_r * ( '0.9' + is_ctx-gbi-pulse * '0.2' ).
        lv_g = lv_g * ( '0.9' + is_ctx-gbi-pulse * '0.2' ).
        lv_b = lv_b * ( '0.9' + is_ctx-gbi-pulse * '0.2' ).

        lv_hex_r = int_to_hex( lv_r ).
        lv_hex_g = int_to_hex( lv_g ).
        lv_hex_b = int_to_hex( lv_b ).

        CLEAR ls_rect.
        ls_rect-x = lv_x. ls_rect-y = lv_mountain_y.
        ls_rect-w = 11. ls_rect-h = lc_h - lv_mountain_y.
        ls_rect-fill = |#{ lv_hex_r }{ lv_hex_g }{ lv_hex_b }|.
        APPEND ls_rect TO rs_frame-rects.
      ENDDO.
    ENDDO.

    " Water reflection at bottom
    CLEAR ls_rect.
    ls_rect-x = 0. ls_rect-y = lc_h * '0.85'.
    ls_rect-w = lc_w. ls_rect-h = lc_h * '0.15'.
    ls_rect-fill = '#001933'.
    APPEND ls_rect TO rs_frame-rects.

    " Water ripples
    DO 5 TIMES.
      DATA lv_ripple_y TYPE f.
      DATA lv_ripple_a TYPE f.
      lv_ripple_y = lc_h * '0.875' + sy-index * 10.
      lv_ripple_a = sin( lv_t * 3 + sy-index ) * '0.3' + '0.3'.

      lv_r = 0.
      lv_g = 50 * lv_ripple_a.
      lv_b = 100 * lv_ripple_a.

      lv_hex_r = int_to_hex( lv_r ).
      lv_hex_g = int_to_hex( lv_g ).
      lv_hex_b = int_to_hex( lv_b ).

      CLEAR ls_rect.
      ls_rect-x = 0. ls_rect-y = lv_ripple_y.
      ls_rect-w = lc_w. ls_rect-h = 3.
      ls_rect-fill = |#{ lv_hex_r }{ lv_hex_g }{ lv_hex_b }|.
      APPEND ls_rect TO rs_frame-rects.
    ENDDO.

    " Title
    CLEAR ls_text.
    ls_text-x = lc_w / 2. ls_text-y = 20.
    ls_text-text = 'THE PROTEUS'.
    ls_text-color = '#FF9966'. ls_text-size = 16. ls_text-align = 'center'.
    APPEND ls_text TO rs_frame-texts.

    " Flash on beat 2
    IF is_ctx-gbi-beat = 2 AND is_ctx-gbi-pulse > '0.7'.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = '0.25'.
      rs_frame-flash-r = 1. rs_frame-flash-g = '0.5'. rs_frame-flash-b = '0.2'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
