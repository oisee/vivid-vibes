CLASS zcl_o4d_fire_twirl DEFINITION PUBLIC FINAL CREATE PUBLIC.
*======================================================================
* FIRE TWIRL - Zoomed in winding spiral
*======================================================================
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    CLASS-METHODS new
      IMPORTING iv_resolution TYPE i DEFAULT 80
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_o4d_fire_twirl.

  PRIVATE SECTION.
    DATA mv_resolution TYPE i.
    CONSTANTS c_pi TYPE f VALUE '3.14159265358979'.

    METHODS atan2 IMPORTING iv_y TYPE f iv_x TYPE f RETURNING VALUE(rv_a) TYPE f.
ENDCLASS.

CLASS zcl_o4d_fire_twirl IMPLEMENTATION.

  METHOD new.
    ro_obj = NEW #( ).
    ro_obj->mv_resolution = iv_resolution.
  ENDMETHOD.

  METHOD atan2.
    IF iv_x > 0.
      rv_a = atan( iv_y / iv_x ).
    ELSEIF iv_x < 0 AND iv_y >= 0.
      rv_a = atan( iv_y / iv_x ) + c_pi.
    ELSEIF iv_x < 0 AND iv_y < 0.
      rv_a = atan( iv_y / iv_x ) - c_pi.
    ELSEIF iv_x = 0 AND iv_y > 0.
      rv_a = c_pi / 2.
    ELSEIF iv_x = 0 AND iv_y < 0.
      rv_a = - c_pi / 2.
    ELSE.
      rv_a = 0.
    ENDIF.
  ENDMETHOD.

  METHOD zif_o4d_effect~get_name. rv_name = 'fire_twirl'. ENDMETHOD.
  METHOD zif_o4d_effect~get_params. ENDMETHOD.
  METHOD zif_o4d_effect~set_param. ENDMETHOD.

  METHOD zif_o4d_effect~render.
    rs_frame = zif_o4d_effect~render_frame( VALUE #(
      t = is_sync-time gt = is_sync-time
      gbi = VALUE #( bar = is_sync-bar bar_phase = is_sync-bar_phase pulse = is_sync-intensity )
    ) ).
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    CONSTANTS: lc_w TYPE f VALUE 640, lc_h TYPE f VALUE 400.

    DATA(lv_t) = is_ctx-gt.
    DATA(lv_cx) = lc_w / 2.
    DATA(lv_cy) = lc_h / 2.

    " Full screen - higher resolution for less artifacts
    DATA(lv_aspect) = lc_h / lc_w.
    DATA(lv_res_y) = CONV i( mv_resolution * lv_aspect ).
    DATA(lv_pw) = lc_w / mv_resolution.
    DATA(lv_ph) = lc_h / lv_res_y.

    " ZOOM factor - smaller = more zoomed in
    DATA(lv_zoom) = CONV f( '0.4' ).

    DATA lv_py TYPE i.
    WHILE lv_py < lv_res_y.
      DATA lv_px TYPE i.
      lv_px = 0.

      WHILE lv_px < mv_resolution.
        " Screen coords centered and ZOOMED
        DATA(lv_sx) = ( lv_px * lv_pw - lv_cx ) * lv_zoom.
        DATA(lv_sy) = ( lv_py * lv_ph - lv_cy ) * lv_zoom.

        " Polar
        DATA(lv_r) = sqrt( lv_sx * lv_sx + lv_sy * lv_sy ).
        DATA(lv_theta) = atan2( iv_y = lv_sy iv_x = lv_sx ).

        " Normalized radius (now smaller range = zoomed)
        DATA(lv_r_norm) = lv_r / 100.

        " WINDING: twist increases with radius squared
        DATA(lv_twist) = lv_t * '1.5' * ( 1 + lv_r_norm * lv_r_norm * 4 ).

        " 2 spiral arms for cleaner look
        DATA(lv_spiral) = lv_theta * 2 + lv_twist + lv_r_norm * 4.

        " Smooth sine wave
        DATA(lv_band) = sin( lv_spiral ).
        DATA(lv_v) = ( lv_band + 1 ) / 2.

        " Edge fade - gentler
        DATA(lv_fade) = 1 - lv_r_norm * '0.3'.
        IF lv_fade < 0. lv_fade = 0. ENDIF.
        IF lv_fade > 1. lv_fade = 1. ENDIF.

        DATA(lv_heat) = lv_v * lv_fade.

        " Fire palette
        DATA(lv_hue) = lv_heat * 50.
        DATA(lv_sat) = 100.
        DATA(lv_light) = 8 + lv_heat * 52.

        " Bright hot center
        IF lv_r < 30.
          DATA(lv_core) = 1 - lv_r / 30.
          lv_light = lv_light + lv_core * 30.
          lv_hue = lv_hue + lv_core * 15.
        ENDIF.

        APPEND VALUE zif_o4d_effect=>ty_rect(
          x = lv_px * lv_pw  y = lv_py * lv_ph
          w = lv_pw + 1  h = lv_ph + 1
          fill = |hsl({ lv_hue DECIMALS = 0 }, { lv_sat DECIMALS = 0 }%, { lv_light DECIMALS = 0 }%)|
        ) TO rs_frame-rects.

        lv_px = lv_px + 1.
      ENDWHILE.
      lv_py = lv_py + 1.
    ENDWHILE.

    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lc_w / 2 y = 20 text = |FIRE TWIRL| color = '#FF6600' size = 14 align = 'center'
    ) TO rs_frame-texts.

    IF is_ctx-gbi-pulse > '0.5'.
      rs_frame-flash = VALUE #( active = abap_true intensity = '0.2' r = 1 g = '0.4' b = 0 ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

