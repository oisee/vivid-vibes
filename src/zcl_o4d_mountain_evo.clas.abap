CLASS zcl_o4d_mountain_evo DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    METHODS constructor.

  PRIVATE SECTION.
    DATA mv_name TYPE string VALUE 'MOUNTAIN_EVO'.
    METHODS int_to_hex IMPORTING iv_int TYPE i RETURNING VALUE(rv_hex) TYPE string.
    METHODS noise IMPORTING iv_x TYPE f iv_y TYPE f RETURNING VALUE(rv_val) TYPE f.
    METHODS fbm IMPORTING iv_x TYPE f iv_y TYPE f RETURNING VALUE(rv_val) TYPE f.

ENDCLASS.

CLASS zcl_o4d_mountain_evo IMPLEMENTATION.

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
    " Simple pseudo-random noise
    DATA: lv_ix TYPE i, lv_iy TYPE i, lv_hash TYPE f.
    lv_ix = floor( iv_x ).
    lv_iy = floor( iv_y ).
    lv_hash = sin( lv_ix * '127.1' + lv_iy * '311.7' ) * '43758.5453'.
    rv_val = lv_hash - floor( lv_hash ).
  ENDMETHOD.

  METHOD fbm.
    " Fractional Brownian Motion
    DATA: lv_f TYPE f VALUE 0, lv_amp TYPE f VALUE '0.5'.
    DATA: lv_x TYPE f, lv_y TYPE f.
    lv_x = iv_x. lv_y = iv_y.

    DO 4 TIMES.
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
    DATA: lv_t TYPE f, lv_phase TYPE f.
    DATA: lv_phase1 TYPE f, lv_phase2 TYPE f, lv_phase3 TYPE f, lv_phase4 TYPE f.
    DATA: lv_sun_rise TYPE f, lv_sun_dist TYPE f, lv_sun_glow TYPE f.
    DATA: lv_layer TYPE i, lv_depth TYPE f, lv_mountain_x TYPE f, lv_h TYPE f.
    DATA: lv_mountain_y TYPE f, lv_shade TYPE f.
    DATA: lv_x TYPE i, lv_y_pos TYPE f, lv_vibrancy TYPE f.
    DATA: lv_r TYPE i, lv_g TYPE i, lv_b TYPE i.
    DATA: lv_hex_r TYPE string, lv_hex_g TYPE string, lv_hex_b TYPE string.
    DATA ls_rect TYPE zif_o4d_effect=>ty_rect.
    DATA ls_circle TYPE zif_o4d_effect=>ty_circle.
    DATA ls_text TYPE zif_o4d_effect=>ty_text.

    lv_t = is_ctx-gbi-time.

    " Phase timing (25 second cycle)
    lv_phase = lv_t - floor( lv_t / 25 ) * 25.

    " Smooth phase transitions
    lv_phase1 = COND #( WHEN lv_phase < 3 THEN lv_phase / 3 ELSE 1 ).
    lv_phase2 = COND #( WHEN lv_phase < 5 THEN 0
                        WHEN lv_phase < 10 THEN ( lv_phase - 5 ) / 5
                        ELSE 1 ).
    lv_phase3 = COND #( WHEN lv_phase < 12 THEN 0
                        WHEN lv_phase < 17 THEN ( lv_phase - 12 ) / 5
                        ELSE 1 ).
    lv_phase4 = COND #( WHEN lv_phase < 18 THEN 0
                        WHEN lv_phase < 22 THEN ( lv_phase - 18 ) / 4
                        ELSE 1 ).

    " Sky gradient (evolves with phase)
    DATA: lv_sky_r TYPE i, lv_sky_g TYPE i, lv_sky_b TYPE i.
    DO 20 TIMES.
      lv_y_pos = ( sy-index - 1 ) / 20.

      " Top color (dark → purple)
      lv_r = ( '0.1' * lv_phase2 ) * 255.
      lv_g = 0.
      lv_b = ( '0.1' + '0.2' * lv_phase2 ) * 255.

      " Horizon color (dark → orange)
      DATA: lv_hr TYPE i, lv_hg TYPE i, lv_hb TYPE i.
      lv_hr = ( '0.1' + '0.7' * lv_phase2 ) * 255.
      lv_hg = ( '0.05' + '0.25' * lv_phase2 ) * 255.
      lv_hb = ( '0.2' - '0.1' * lv_phase2 ) * 255.

      " Interpolate
      lv_sky_r = lv_hr + ( lv_r - lv_hr ) * lv_y_pos.
      lv_sky_g = lv_hg + ( lv_g - lv_hg ) * lv_y_pos.
      lv_sky_b = lv_hb + ( lv_b - lv_hb ) * lv_y_pos.

      lv_hex_r = int_to_hex( lv_sky_r ).
      lv_hex_g = int_to_hex( lv_sky_g ).
      lv_hex_b = int_to_hex( lv_sky_b ).

      CLEAR ls_rect.
      ls_rect-x = 0. ls_rect-y = ( sy-index - 1 ) * 10. ls_rect-w = 320. ls_rect-h = 10.
      ls_rect-fill = |#{ lv_hex_r }{ lv_hex_g }{ lv_hex_b }|.
      APPEND ls_rect TO rs_frame-rects.
    ENDDO.

    " First sun (rises in phase 2)
    IF lv_phase2 > 0.
      lv_sun_rise = lv_phase2 * '0.4'.
      DATA lv_sun1_x TYPE f VALUE 224.
      DATA lv_sun1_y TYPE f.
      lv_sun1_y = 135 - lv_sun_rise * 100.

      " Sun glow
      lv_r = 255.
      lv_g = 128.
      lv_b = 26.
      lv_hex_r = int_to_hex( lv_r ).
      lv_hex_g = int_to_hex( lv_g ).
      lv_hex_b = int_to_hex( lv_b ).

      CLEAR ls_circle.
      ls_circle-x = lv_sun1_x.
      ls_circle-y = lv_sun1_y.
      ls_circle-radius = 20 * lv_phase2.
      ls_circle-fill = |#{ lv_hex_r }{ lv_hex_g }{ lv_hex_b }|.
      APPEND ls_circle TO rs_frame-circles.
    ENDIF.

    " Second sun (cyan, phase 3)
    IF lv_phase3 > 0.
      DATA lv_sun2_x TYPE f VALUE 96.
      DATA lv_sun2_y TYPE f.
      lv_sun2_y = 140 - lv_phase3 * 60.

      lv_r = 51.
      lv_g = 204.
      lv_b = 255.
      lv_hex_r = int_to_hex( lv_r ).
      lv_hex_g = int_to_hex( lv_g ).
      lv_hex_b = int_to_hex( lv_b ).

      CLEAR ls_circle.
      ls_circle-x = lv_sun2_x.
      ls_circle-y = lv_sun2_y.
      ls_circle-radius = 16 * lv_phase3.
      ls_circle-fill = |#{ lv_hex_r }{ lv_hex_g }{ lv_hex_b }|.
      APPEND ls_circle TO rs_frame-circles.
    ENDIF.

    " Mountains (fade out in phase 4)
    DATA lv_mtn_fade TYPE f.
    lv_mtn_fade = 1 - lv_phase4.

    IF lv_mtn_fade > 0.
      lv_vibrancy = lv_phase2 * '0.5'.

      DO 4 TIMES.
        lv_layer = sy-index - 1.
        lv_depth = '0.25' + lv_layer * '0.12'.

        " Simplified mountain silhouette using rectangles
        DO 32 TIMES.
          lv_x = ( sy-index - 1 ) * 10.
          lv_mountain_x = lv_x / 320 * ( 2 + lv_layer ) + lv_t * '0.05'.

          lv_h = fbm( iv_x = lv_mountain_x iv_y = CONV f( lv_layer + lv_t * '0.3' ) ) * '0.35'.
          lv_h = lv_h + fbm( iv_x = lv_mountain_x * 2 iv_y = CONV f( lv_layer * 2 ) ) * '0.15'.

          lv_mountain_y = lv_depth + lv_h * ( '0.35' - lv_layer * '0.04' ).
          lv_mountain_y = lv_mountain_y * 200.

          lv_shade = 1 - lv_layer * '0.2'.

          " Mountain colors with vibrancy
          CASE lv_layer.
            WHEN 0.  " Far - purple
              lv_r = ( '0.05' + '0.15' * lv_vibrancy ) * 255 * lv_shade.
              lv_g = ( '0.02' + '0.03' * lv_vibrancy ) * 255 * lv_shade.
              lv_b = ( '0.1' + '0.2' * lv_vibrancy ) * 255 * lv_shade.
            WHEN 1.  " Mid-far
              lv_r = ( '0.1' ) * 255 * lv_shade.
              lv_g = ( '0.05' + '0.15' * lv_vibrancy ) * 255 * lv_shade.
              lv_b = ( '0.15' + '0.25' * lv_vibrancy ) * 255 * lv_shade.
            WHEN 2.  " Mid-near - green
              lv_r = ( '0.05' ) * 255 * lv_shade.
              lv_g = ( '0.12' + '0.28' * lv_vibrancy ) * 255 * lv_shade.
              lv_b = ( '0.08' + '0.22' * lv_vibrancy ) * 255 * lv_shade.
            WHEN OTHERS.  " Near - dark green
              lv_r = ( '0.02' ) * 255 * lv_shade.
              lv_g = ( '0.08' + '0.22' * lv_vibrancy ) * 255 * lv_shade.
              lv_b = ( '0.04' + '0.16' * lv_vibrancy ) * 255 * lv_shade.
          ENDCASE.

          " Apply fade and pulse
          lv_r = lv_r * lv_mtn_fade * ( '0.9' + is_ctx-gbi-pulse * '0.2' ).
          lv_g = lv_g * lv_mtn_fade * ( '0.9' + is_ctx-gbi-pulse * '0.2' ).
          lv_b = lv_b * lv_mtn_fade * ( '0.9' + is_ctx-gbi-pulse * '0.2' ).

          lv_hex_r = int_to_hex( lv_r ).
          lv_hex_g = int_to_hex( lv_g ).
          lv_hex_b = int_to_hex( lv_b ).

          CLEAR ls_rect.
          ls_rect-x = lv_x. ls_rect-y = lv_mountain_y.
          ls_rect-w = 11. ls_rect-h = 200 - lv_mountain_y.
          ls_rect-fill = |#{ lv_hex_r }{ lv_hex_g }{ lv_hex_b }|.
          APPEND ls_rect TO rs_frame-rects.
        ENDDO.
      ENDDO.
    ENDIF.

    " Phase label
    CLEAR ls_text.
    ls_text-x = 160. ls_text-y = 10.
    ls_text-size = 8. ls_text-align = 'center'.

    IF lv_phase < 5.
      ls_text-text = 'MOUNTAINS APPEAR'.
      ls_text-color = '#8888AA'.
    ELSEIF lv_phase < 12.
      ls_text-text = 'SUNRISE'.
      ls_text-color = '#FF8844'.
    ELSEIF lv_phase < 18.
      ls_text-text = 'DUAL SUNS'.
      ls_text-color = '#44CCFF'.
    ELSE.
      ls_text-text = 'EVOLUTION'.
      ls_text-color = '#FF44FF'.
    ENDIF.
    APPEND ls_text TO rs_frame-texts.

    " Flash on beat 2
    IF is_ctx-gbi-beat = 2 AND is_ctx-gbi-pulse > '0.7'.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = '0.3'.
      rs_frame-flash-r = 1. rs_frame-flash-g = '0.6'. rs_frame-flash-b = '0.2'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
