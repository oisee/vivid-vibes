CLASS zcl_o4d_mountains_sharp DEFINITION PUBLIC CREATE PUBLIC.
*======================================================================
* MOUNTAINS SHARP - Angular/sharp mountains + ROUND sun
* (the "correct" version after OOPS)
*======================================================================
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.

  PRIVATE SECTION.
    CONSTANTS: lc_w TYPE i VALUE 640,
               lc_h TYPE i VALUE 400.

    METHODS get_sky_brightness
      IMPORTING iv_sun_y       TYPE f
      RETURNING VALUE(rv_mult) TYPE f.

ENDCLASS.

CLASS zcl_o4d_mountains_sharp IMPLEMENTATION.

  METHOD zif_o4d_effect~get_name. rv_name = 'mountains_sharp'. ENDMETHOD.
  METHOD zif_o4d_effect~get_params. ENDMETHOD.
  METHOD zif_o4d_effect~set_param. ENDMETHOD.

  METHOD zif_o4d_effect~render.
    rs_frame = zif_o4d_effect~render_frame( VALUE #(
      t = is_sync-time gt = is_sync-time
      gbi = VALUE #( time = is_sync-time bar = is_sync-bar
                     beat = is_sync-beat bar_phase = is_sync-bar_phase
                     pulse = is_sync-intensity ) ) ).
  ENDMETHOD.

  METHOD get_sky_brightness.
    DATA(lv_progress) = ( lc_h - iv_sun_y ) / lc_h.
    rv_mult = '0.4' + lv_progress * '0.6'.
    IF rv_mult > 1. rv_mult = 1. ENDIF.
    IF rv_mult < '0.4'. rv_mult = '0.4'. ENDIF.
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    DATA: lv_t         TYPE f,
          lv_sun_x     TYPE f,
          lv_sun_y     TYPE f,
          lv_sun_size  TYPE f,
          lv_sky_mult  TYPE f,
          lv_layer     TYPE i,
          lv_x         TYPE i,
          lv_h         TYPE f,
          lv_mountain_y TYPE f.

    lv_t = is_ctx-gbi-time.

    " Sun position - rises over time
    lv_sun_x = lc_w * '0.7'.
    lv_sun_y = lc_h * '0.75' - lv_t * 12.
    IF lv_sun_y < lc_h * '0.12'. lv_sun_y = lc_h * '0.12'. ENDIF.
    lv_sun_size = 45.

    " Sky brightness based on sun position
    lv_sky_mult = get_sky_brightness( lv_sun_y ).

    " === SKY GRADIENT (warm sunset colors) ===
    DO 20 TIMES.
      DATA(lv_y_pos) = CONV f( ( sy-index - 1 ) / 20 ).
      DATA(lv_base_h) = 30 - lv_y_pos * 20.  " Orange to red
      DATA(lv_base_s) = 70 + lv_sky_mult * 20.
      DATA(lv_base_l) = CONV i( ( 15 + 40 * ( 1 - lv_y_pos ) ) * lv_sky_mult ).

      APPEND VALUE zif_o4d_effect=>ty_rect(
        x = 0 y = ( sy-index - 1 ) * 20
        w = lc_w h = 21
        fill = |hsl({ CONV i( lv_base_h ) }, { CONV i( lv_base_s ) }%, { lv_base_l }%)|
      ) TO rs_frame-rects.
    ENDDO.

    " === ROUND SUN (correct!) - behind mountains ===
    " Outer glow
    APPEND VALUE zif_o4d_effect=>ty_circle(
      x = lv_sun_x y = lv_sun_y
      radius = lv_sun_size * '1.8'
      fill = |hsl(40, 100%, { CONV i( 40 * lv_sky_mult ) }%)|
    ) TO rs_frame-circles.

    " Inner glow
    APPEND VALUE zif_o4d_effect=>ty_circle(
      x = lv_sun_x y = lv_sun_y
      radius = lv_sun_size * '1.3'
      fill = |hsl(45, 100%, { CONV i( 55 * lv_sky_mult ) }%)|
    ) TO rs_frame-circles.

    " Sun body
    APPEND VALUE zif_o4d_effect=>ty_circle(
      x = lv_sun_x y = lv_sun_y
      radius = lv_sun_size
      fill = '#FFEE44'
    ) TO rs_frame-circles.

    " === SHARP MOUNTAINS (4 layers) ===
    DO 4 TIMES.
      lv_layer = 4 - sy-index.  " Back to front

      " Layer colors - warmer with sunrise
      DATA(lv_lr) = CONV i( ( 25 + lv_layer * 8 ) * lv_sky_mult ).
      DATA(lv_lg) = CONV i( ( 35 + lv_layer * 15 ) * lv_sky_mult ).
      DATA(lv_lb) = CONV i( ( 25 + lv_layer * 5 ) * lv_sky_mult ).

      DATA(lv_base_y) = lc_h * ( '0.38' + lv_layer * '0.12' ).
      DATA(lv_scroll) = lv_t * ( 6 + lv_layer * 3 ).

      DO 65 TIMES.
        lv_x = ( sy-index - 1 ) * 10.
        DATA(lv_nx) = CONV f( ( lv_x + lv_scroll ) / 80 ).

        " SHARP: triangle waves using modulo
        DATA(lv_tri1) = abs( nmin( val1 = ( lv_nx MOD 2 ) val2 = 2 - ( lv_nx MOD 2 ) ) - 1 ).
        DATA(lv_tri2) = abs( nmin( val1 = ( lv_nx * '1.7' MOD 2 ) val2 = 2 - ( lv_nx * '1.7' MOD 2 ) ) - 1 ).
        DATA(lv_tri3) = abs( nmin( val1 = ( lv_nx * '0.6' MOD 2 ) val2 = 2 - ( lv_nx * '0.6' MOD 2 ) ) - 1 ).

        lv_h = lv_tri1 * 80 + lv_tri2 * 45 + lv_tri3 * 100.
        lv_h = lv_h * ( '0.6' - lv_layer * '0.1' ).

        lv_mountain_y = lv_base_y - lv_h.
        IF lv_mountain_y < 0. lv_mountain_y = 0. ENDIF.

        APPEND VALUE zif_o4d_effect=>ty_rect(
          x = lv_x y = lv_mountain_y
          w = 11 h = lc_h - lv_mountain_y
          fill = |rgb({ lv_lr },{ lv_lg },{ lv_lb })|
        ) TO rs_frame-rects.
      ENDDO.
    ENDDO.

    " === TITLE ===
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lc_w / 2 y = 25
      text = 'MOUNTAINS v1.1 (fixed)'
      color = '#88FF88' size = 14 align = 'center'
    ) TO rs_frame-texts.

    " Beat pulse
    IF is_ctx-gbi-pulse > '0.5'.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = '0.12'.
      rs_frame-flash-r = 1. rs_frame-flash-g = '0.9'. rs_frame-flash-b = '0.7'.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
