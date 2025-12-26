CLASS zcl_o4d_mountains_round DEFINITION PUBLIC CREATE PUBLIC.
*======================================================================
* MOUNTAINS ROUND - Wavy/rounded mountains + SQUARE sun
* (intentionally "wrong" for the OOPS joke)
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

CLASS zcl_o4d_mountains_round IMPLEMENTATION.

  METHOD zif_o4d_effect~get_name. rv_name = 'mountains_round'. ENDMETHOD.
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
    " Sun rises from bottom (y=400) to top (y=80)
    " Returns 0.3 (dark) to 1.0 (bright) based on sun position
    DATA(lv_progress) = ( lc_h - iv_sun_y ) / lc_h.
    rv_mult = '0.3' + lv_progress * '0.7'.
    IF rv_mult > 1. rv_mult = 1. ENDIF.
    IF rv_mult < '0.3'. rv_mult = '0.3'. ENDIF.
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
    lv_sun_y = lc_h * '0.8' - lv_t * 15.  " Rising sun
    IF lv_sun_y < lc_h * '0.15'. lv_sun_y = lc_h * '0.15'. ENDIF.
    lv_sun_size = 50.

    " Sky brightness based on sun position
    lv_sky_mult = get_sky_brightness( lv_sun_y ).

    " === SKY GRADIENT (brightens with sunrise) ===
    DO 20 TIMES.
      DATA(lv_y_pos) = CONV f( ( sy-index - 1 ) / 20 ).
      " Purple to orange gradient, affected by sun
      DATA(lv_base_h) = 280 - lv_sky_mult * 50.  " Hue shifts orange
      DATA(lv_base_s) = 50 + lv_sky_mult * 20.
      DATA(lv_base_l) = CONV i( ( 10 + 30 * ( 1 - lv_y_pos ) ) * lv_sky_mult ).

      APPEND VALUE zif_o4d_effect=>ty_rect(
        x = 0 y = ( sy-index - 1 ) * 20
        w = lc_w h = 21
        fill = |hsl({ CONV i( lv_base_h ) }, { CONV i( lv_base_s ) }%, { lv_base_l }%)|
      ) TO rs_frame-rects.
    ENDDO.

    " === SQUARE SUN (wrong!) - behind mountains ===
    " Glow first (bigger, behind)
    APPEND VALUE zif_o4d_effect=>ty_rect(
      x = lv_sun_x - lv_sun_size
      y = lv_sun_y - lv_sun_size
      w = lv_sun_size * 2 h = lv_sun_size * 2
      fill = |hsl(45, 100%, { CONV i( 50 * lv_sky_mult ) }%)|
    ) TO rs_frame-rects.

    " Sun body
    APPEND VALUE zif_o4d_effect=>ty_rect(
      x = lv_sun_x - lv_sun_size / 2
      y = lv_sun_y - lv_sun_size / 2
      w = lv_sun_size h = lv_sun_size
      fill = '#FFDD00'
    ) TO rs_frame-rects.

    " === ROUNDED MOUNTAINS (4 layers, front to back for proper overlap) ===
    DO 4 TIMES.
      lv_layer = 4 - sy-index.  " Back to front

      " Layer colors - get brighter with sunrise
      DATA(lv_lr) = CONV i( ( 20 + lv_layer * 10 ) * lv_sky_mult ).
      DATA(lv_lg) = CONV i( ( 15 + lv_layer * 12 ) * lv_sky_mult ).
      DATA(lv_lb) = CONV i( ( 40 + lv_layer * 8 ) * lv_sky_mult ).

      DATA(lv_base_y) = lc_h * ( '0.35' + lv_layer * '0.13' ).
      DATA(lv_scroll) = lv_t * ( 8 + lv_layer * 4 ).

      DO 65 TIMES.
        lv_x = ( sy-index - 1 ) * 10.
        DATA(lv_nx) = CONV f( ( lv_x + lv_scroll ) / 100 ).

        " ROUNDED: smooth sine waves
        lv_h = sin( lv_nx ) * 50
             + sin( lv_nx * '2.1' ) * 30
             + sin( lv_nx * '0.5' + lv_layer ) * 70.
        lv_h = lv_h * ( '0.7' - lv_layer * '0.12' ).

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
      text = 'MOUNTAINS v1.0'
      color = '#FFFFFF' size = 14 align = 'center'
    ) TO rs_frame-texts.

    " Beat pulse
    IF is_ctx-gbi-pulse > '0.5'.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = '0.1'.
      rs_frame-flash-r = 1. rs_frame-flash-g = 1. rs_frame-flash-b = 1.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
