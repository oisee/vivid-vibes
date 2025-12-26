CLASS zcl_o4d_julia_morph DEFINITION PUBLIC FINAL CREATE PUBLIC.
*======================================================================
* JULIA MORPHING - Animated Julia Set with breathing c parameter
*======================================================================
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    CLASS-METHODS new
      IMPORTING iv_resolution TYPE i DEFAULT 64
                iv_max_iter   TYPE i DEFAULT 32
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_o4d_julia_morph.

  PRIVATE SECTION.
    DATA mv_resolution TYPE i.
    DATA mv_max_iter TYPE i.
    DATA mv_pixel_w TYPE f.
    DATA mv_pixel_h TYPE f.

    METHODS julia_iter
      IMPORTING iv_zx TYPE f iv_zy TYPE f
                iv_cx TYPE f iv_cy TYPE f
      RETURNING VALUE(rv_i) TYPE i.
ENDCLASS.

CLASS zcl_o4d_julia_morph IMPLEMENTATION.

  METHOD new.
    ro_obj = NEW #( ).
    ro_obj->mv_resolution = iv_resolution.
    ro_obj->mv_max_iter = iv_max_iter.
    ro_obj->mv_pixel_w = 640 / iv_resolution.
    ro_obj->mv_pixel_h = 400 / iv_resolution.
  ENDMETHOD.


  METHOD julia_iter.
    DATA(lv_zx) = iv_zx.
    DATA(lv_zy) = iv_zy.

    DO mv_max_iter TIMES.
      DATA(lv_zx2) = lv_zx * lv_zx.
      DATA(lv_zy2) = lv_zy * lv_zy.

      IF lv_zx2 + lv_zy2 > 4.
        rv_i = sy-index.
        RETURN.
      ENDIF.

      DATA(lv_new_zx) = lv_zx2 - lv_zy2 + iv_cx.
      lv_zy = 2 * lv_zx * lv_zy + iv_cy.
      lv_zx = lv_new_zx.
    ENDDO.

    rv_i = mv_max_iter.
  ENDMETHOD.


  METHOD zif_o4d_effect~get_name. rv_name = 'julia_morph'. ENDMETHOD.
  METHOD zif_o4d_effect~get_params. ENDMETHOD.
  METHOD zif_o4d_effect~set_param. ENDMETHOD.


  METHOD zif_o4d_effect~render.
    rs_frame = zif_o4d_effect~render_frame( VALUE #(
      t = is_sync-time gt = is_sync-time
      gbi = VALUE #( bar = is_sync-bar bar_phase = is_sync-bar_phase pulse = is_sync-intensity )
    ) ).
  ENDMETHOD.


  METHOD zif_o4d_effect~render_frame.
    CONSTANTS: lc_w TYPE f VALUE 640,
               lc_h TYPE f VALUE 400.

    DATA(lv_t) = is_ctx-gt.

    " Animate c parameter
    DATA(lv_orbit) = lv_t * '0.1'.
    DATA(lv_cx) = sin( lv_orbit ) * '0.35' + cos( lv_orbit * '1.3' ) * '0.4'.
    DATA(lv_cy) = cos( lv_orbit ) * '0.35' + sin( lv_orbit * '0.7' ) * '0.35'.

    DATA(lv_zoom) = CONV f( '1.5' ).
    DATA(lv_aspect) = lc_h / lc_w.
    DATA(lv_step_x) = lv_zoom * 2 / mv_resolution.
    DATA(lv_res_y) = CONV i( mv_resolution * lv_aspect ).
    DATA(lv_step_y) = lv_zoom * 2 * lv_aspect / lv_res_y.

    " Recalculate pixel sizes for full screen coverage
    DATA(lv_pw) = lc_w / mv_resolution.
    DATA(lv_ph) = lc_h / lv_res_y.

    DATA lv_py TYPE i.
    DATA lv_fy TYPE f.
    lv_fy = - lv_zoom * lv_aspect.

    WHILE lv_py < lv_res_y.
      DATA lv_px TYPE i.
      DATA lv_fx TYPE f.
      lv_px = 0.
      lv_fx = - lv_zoom.

      WHILE lv_px < mv_resolution.
        DATA(lv_iter) = julia_iter(
          iv_zx = lv_fx iv_zy = lv_fy
          iv_cx = lv_cx iv_cy = lv_cy
        ).

        IF lv_iter < mv_max_iter.
          DATA(lv_norm) = CONV f( lv_iter ) / mv_max_iter.
          DATA(lv_hue) = ( lv_norm * 360 + lv_t * 50 ) MOD 360.
          DATA(lv_light) = 20 + lv_norm * 50.

          APPEND VALUE zif_o4d_effect=>ty_rect(
            x = lv_px * lv_pw
            y = lv_py * lv_ph
            w = lv_pw + 1
            h = lv_ph + 1
            fill = |hsl({ lv_hue DECIMALS = 0 }, 85%, { lv_light DECIMALS = 0 }%)|
          ) TO rs_frame-rects.
        ELSE.
          APPEND VALUE zif_o4d_effect=>ty_rect(
            x = lv_px * lv_pw
            y = lv_py * lv_ph
            w = lv_pw + 1
            h = lv_ph + 1
            fill = '#050510'
          ) TO rs_frame-rects.
        ENDIF.

        lv_px = lv_px + 1.
        lv_fx = lv_fx + lv_step_x.
      ENDWHILE.

      lv_py = lv_py + 1.
      lv_fy = lv_fy + lv_step_y.
    ENDWHILE.

    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lc_w / 2 y = 20
      text = |JULIA MORPHING|
      color = '#fff' size = 14 align = 'center'
    ) TO rs_frame-texts.

    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lc_w / 2 y = lc_h - 15
      text = |c = { lv_cx DECIMALS = 3 } + { lv_cy DECIMALS = 3 }i|
      color = '#888' size = 10 align = 'center'
    ) TO rs_frame-texts.

    IF is_ctx-gbi-pulse > '0.7'.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = '0.15'.
      rs_frame-flash-r = '0.5'. rs_frame-flash-g = 0. rs_frame-flash-b = 1.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

