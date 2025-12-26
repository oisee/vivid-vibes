class ZCL_O4D_MOUNTAINS_OOPS_A definition
  public
  create public .

*======================================================================
* MOUNTAINS OOPS - Demoscene joke effect
* Phase 1: Rounded mountains + SQUARE sun (wrong!)
* Phase 2: Sharp mountains + ROUND sun (correct!)
*======================================================================
public section.

  interfaces ZIF_O4D_EFFECT .
  PRIVATE SECTION.
    CONSTANTS: lc_w TYPE f VALUE 640,
               lc_h TYPE f VALUE 400.

    METHODS int_to_hex IMPORTING iv_int TYPE i RETURNING VALUE(rv_hex) TYPE string.

ENDCLASS.



CLASS ZCL_O4D_MOUNTAINS_OOPS_A IMPLEMENTATION.


  METHOD INT_TO_HEX.
    DATA lv_hex TYPE x LENGTH 1.
    DATA lv_val TYPE i.
    lv_val = iv_int.
    IF lv_val < 0. lv_val = 0. ENDIF.
    IF lv_val > 255. lv_val = 255. ENDIF.
    lv_hex = lv_val.
    rv_hex = |{ lv_hex }|.
  ENDMETHOD.


  METHOD ZIF_O4D_EFFECT~GET_NAME. rv_name = 'mountains_oops'. ENDMETHOD.


  METHOD ZIF_O4D_EFFECT~GET_PARAMS. ENDMETHOD.


  METHOD ZIF_O4D_EFFECT~RENDER.
    rs_frame = zif_o4d_effect~render_frame( VALUE #(
      t = is_sync-time gt = is_sync-time
      gbi = VALUE #( time = is_sync-time bar = is_sync-bar
                     beat = is_sync-beat bar_phase = is_sync-bar_phase
                     pulse = is_sync-intensity ) ) ).
  ENDMETHOD.


  METHOD ZIF_O4D_EFFECT~RENDER_FRAME.
    DATA: lv_t TYPE f,
          lv_phase TYPE i,        " 0 = wrong, 1 = transition, 2 = correct
          lv_phase_t TYPE f,
          lv_sun_x TYPE f,
          lv_sun_y TYPE f,
          lv_sun_size TYPE f,
          lv_layer TYPE i,
          lv_x TYPE i,
          lv_mountain_y TYPE f,
          lv_h TYPE f,
          lv_r TYPE i, lv_g TYPE i, lv_b TYPE i,
          lv_hex_r TYPE string, lv_hex_g TYPE string, lv_hex_b TYPE string.

    lv_t = is_ctx-gbi-time.

    " Determine phase based on BARS (8 bar cycle)
    DATA(lv_bar) = is_ctx-gbi-bar MOD 8.
    DATA(lv_bar_phase) = is_ctx-gbi-bar_phase.

    IF lv_bar < 3.
      lv_phase = 0.      " Wrong: round mountains + square sun (bars 0-2)
      lv_phase_t = lv_bar + lv_bar_phase.
    ELSEIF lv_bar < 4.
      lv_phase = 1.      " OOPS! transition (bar 3)
      lv_phase_t = lv_bar_phase.
    ELSE.
      lv_phase = 2.      " Correct: sharp mountains + round sun (bars 4-7)
      lv_phase_t = ( lv_bar - 4 ) + lv_bar_phase.
    ENDIF.

    " === SKY GRADIENT ===
    DATA(lv_sky_hue) = COND #( WHEN lv_phase = 0 THEN 280  " Purple-ish (wrong mood)
                               WHEN lv_phase = 1 THEN 0    " Red (oops!)
                               ELSE 30 ).                   " Orange (correct sunset)

    DO 20 TIMES.
      DATA(lv_y_pos) = CONV f( ( sy-index - 1 ) / 20 ).
      DATA(lv_sky_l) = 15 + ( 50 - 15 ) * ( 1 - lv_y_pos ).

      APPEND VALUE zif_o4d_effect=>ty_rect(
        x = 0 y = ( sy-index - 1 ) * 20
        w = lc_w h = 21
        fill = |hsl({ lv_sky_hue }, 60%, { CONV i( lv_sky_l ) }%)|
      ) TO rs_frame-rects.
    ENDDO.

    " === SUN ===
    lv_sun_x = lc_w * '0.7'.
    lv_sun_y = lc_h * '0.25' - sin( lv_phase_t ) * 30.  " Rising sun
    lv_sun_size = 30.

    IF lv_phase = 0 OR lv_phase = 1.
      " SQUARE SUN (wrong!)
      APPEND VALUE zif_o4d_effect=>ty_rect(
        x = lv_sun_x - lv_sun_size / 2
        y = lv_sun_y - lv_sun_size / 2
        w = lv_sun_size h = lv_sun_size
        fill = '#FFDD00'
      ) TO rs_frame-rects.

      " Square glow
      APPEND VALUE zif_o4d_effect=>ty_rect(
        x = lv_sun_x - lv_sun_size * '0.7'
        y = lv_sun_y - lv_sun_size * '0.7'
        w = lv_sun_size * '1.4' h = lv_sun_size * '1.4'
        fill = '#FFAA0033'
      ) TO rs_frame-rects.
    ELSE.
      " ROUND SUN (correct!)
      APPEND VALUE zif_o4d_effect=>ty_circle(
        x = lv_sun_x y = lv_sun_y
        radius = lv_sun_size
        fill = '#FFDD00'
      ) TO rs_frame-circles.

      APPEND VALUE zif_o4d_effect=>ty_circle(
        x = lv_sun_x y = lv_sun_y
        radius = lv_sun_size * '1.4'
        fill = '#FFAA0033'
      ) TO rs_frame-circles.
    ENDIF.

    " === MOUNTAINS (4 layers) ===
    DO 4 TIMES.
      lv_layer = sy-index - 1.

      " Layer colors
      CASE lv_layer.
        WHEN 0. lv_r = 30. lv_g = 20. lv_b = 50.   " Far - purple
        WHEN 1. lv_r = 40. lv_g = 30. lv_b = 60.
        WHEN 2. lv_r = 20. lv_g = 50. lv_b = 40.   " Near - green
        WHEN 3. lv_r = 15. lv_g = 40. lv_b = 30.
      ENDCASE.

      lv_hex_r = int_to_hex( lv_r ).
      lv_hex_g = int_to_hex( lv_g ).
      lv_hex_b = int_to_hex( lv_b ).

      DATA(lv_base_y) = lc_h * ( '0.4' + lv_layer * '0.12' ).
      DATA(lv_scroll) = lv_t * ( 10 + lv_layer * 5 ).

      DO 65 TIMES.
        lv_x = ( sy-index - 1 ) * 10.
        DATA(lv_nx) = ( lv_x + lv_scroll ) / 80.

        IF lv_phase = 0 OR lv_phase = 1.
          " ROUNDED mountains (wrong! mountains should be sharp)
          " Smooth sine waves
          lv_h = sin( lv_nx ) * 40
               + sin( lv_nx * '2.3' ) * 25
               + sin( lv_nx * '0.7' ) * 50.
          lv_h = lv_h * ( '0.8' - lv_layer * '0.15' ).
        ELSE.
          " SHARP mountains (correct!)
          " Triangle waves using ABS
          DATA(lv_tri1) = abs( ( lv_nx MOD 2 ) - 1 ) * 2 - 1.
          DATA(lv_tri2) = abs( ( lv_nx * '2.3' MOD 2 ) - 1 ) * 2 - 1.
          DATA(lv_tri3) = abs( ( lv_nx * '0.7' MOD 2 ) - 1 ) * 2 - 1.
          lv_h = lv_tri1 * 50 + lv_tri2 * 30 + lv_tri3 * 60.
          lv_h = lv_h * ( '0.8' - lv_layer * '0.15' ).
        ENDIF.

        lv_mountain_y = lv_base_y - lv_h.

        APPEND VALUE zif_o4d_effect=>ty_rect(
          x = lv_x y = lv_mountain_y
          w = 11 h = lc_h - lv_mountain_y
          fill = |#{ lv_hex_r }{ lv_hex_g }{ lv_hex_b }|
        ) TO rs_frame-rects.
      ENDDO.
    ENDDO.

    " === TEXT ===
    IF lv_phase = 0.
      APPEND VALUE zif_o4d_effect=>ty_text(
        x = lc_w / 2 y = 30
        text = 'MOUNTAINS v1.0'
        color = '#FFFFFF' size = 16 align = 'center'
      ) TO rs_frame-texts.
    ELSEIF lv_phase = 1.
      " OOPS! flash
      APPEND VALUE zif_o4d_effect=>ty_rect(
        x = 0 y = 0 w = lc_w h = lc_h
        fill = '#FF000044'
      ) TO rs_frame-rects.

      APPEND VALUE zif_o4d_effect=>ty_text(
        x = lc_w / 2 y = lc_h / 2
        text = 'OOPS!'
        color = '#FF0000' size = 48 align = 'center'
      ) TO rs_frame-texts.

      APPEND VALUE zif_o4d_effect=>ty_text(
        x = lc_w / 2 y = lc_h / 2 + 40
        text = 'wrong shapes...'
        color = '#FF8888' size = 16 align = 'center'
      ) TO rs_frame-texts.
    ELSE.
      APPEND VALUE zif_o4d_effect=>ty_text(
        x = lc_w / 2 y = 30
        text = 'MOUNTAINS v1.1 (fixed)'
        color = '#00FF00' size = 16 align = 'center'
      ) TO rs_frame-texts.
    ENDIF.

    " Beat pulse
    IF is_ctx-gbi-pulse > '0.5'.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = '0.15'.
      rs_frame-flash-r = 1. rs_frame-flash-g = 1. rs_frame-flash-b = 1.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_O4D_EFFECT~SET_PARAM. ENDMETHOD.
ENDCLASS.
