CLASS zcl_o4d_burning_ship DEFINITION PUBLIC FINAL CREATE PUBLIC.
*======================================================================
* BURNING SHIP - Dramatic fractal with abs() in iteration
*======================================================================
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    CLASS-METHODS new
      IMPORTING iv_resolution TYPE i DEFAULT 80
                iv_max_iter   TYPE i DEFAULT 64
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_o4d_burning_ship.

  PRIVATE SECTION.
    DATA mv_resolution TYPE i.
    DATA mv_max_iter TYPE i.
    DATA mv_pixel_w TYPE f.
    DATA mv_pixel_h TYPE f.

    METHODS burning_iter
      IMPORTING iv_cx TYPE f iv_cy TYPE f
      RETURNING VALUE(rv_i) TYPE i.
ENDCLASS.

CLASS zcl_o4d_burning_ship IMPLEMENTATION.

  METHOD new.
    ro_obj = NEW #( ).
    ro_obj->mv_resolution = iv_resolution.
    ro_obj->mv_max_iter = iv_max_iter.
    ro_obj->mv_pixel_w = 640 / iv_resolution.
    ro_obj->mv_pixel_h = 400 / iv_resolution.
  ENDMETHOD.


  METHOD burning_iter.
    DATA lv_zx TYPE f VALUE 0.
    DATA lv_zy TYPE f VALUE 0.

    DO mv_max_iter TIMES.
      DATA(lv_zx2) = lv_zx * lv_zx.
      DATA(lv_zy2) = lv_zy * lv_zy.

      IF lv_zx2 + lv_zy2 > 4.
        rv_i = sy-index.
        RETURN.
      ENDIF.

      " BURNING SHIP: z = (|Re(z)| + i|Im(z)|)² + c
      DATA(lv_new_zx) = lv_zx2 - lv_zy2 + iv_cx.
      lv_zy = 2 * abs( lv_zx ) * abs( lv_zy ) + iv_cy.
      lv_zx = lv_new_zx.
    ENDDO.

    rv_i = mv_max_iter.
  ENDMETHOD.


  METHOD zif_o4d_effect~get_name. rv_name = 'burning_ship'. ENDMETHOD.
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

    " Zoom animation
    DATA(lv_zoom_factor) = '1.0' + lv_t * '0.05'.
    DATA(lv_zoom) = '2.0' / lv_zoom_factor.

    DATA(lv_center_x) = CONV f( '-1.755' ).
    DATA(lv_center_y) = CONV f( '-0.03' ).
    lv_center_x = lv_center_x + sin( lv_t * '0.1' ) * lv_zoom * '0.1'.
    lv_center_y = lv_center_y + cos( lv_t * '0.13' ) * lv_zoom * '0.05'.

    DATA(lv_aspect) = lc_h / lc_w.
    DATA(lv_step_x) = lv_zoom * 2 / mv_resolution.
    DATA(lv_step_y) = lv_zoom * 2 * lv_aspect / mv_resolution.
    DATA(lv_res_y) = mv_resolution * lv_aspect.
    DATA(lv_start_x) = lv_center_x - lv_zoom.
    DATA(lv_start_y) = lv_center_y - lv_zoom * lv_aspect.

    DATA lv_py TYPE i.
    DATA lv_fy TYPE f.
    lv_fy = lv_start_y.

    WHILE lv_py < lv_res_y.
      DATA lv_px TYPE i.
      DATA lv_fx TYPE f.
      lv_px = 0.
      lv_fx = lv_start_x.

      WHILE lv_px < mv_resolution.
        DATA(lv_iter) = burning_iter( iv_cx = lv_fx iv_cy = lv_fy ).

        IF lv_iter < mv_max_iter.
          DATA(lv_norm) = CONV f( lv_iter ) / mv_max_iter.

          " Fire palette via HSL (red-orange-yellow)
          DATA(lv_hue) = lv_norm * 60.  " 0=red to 60=yellow
          DATA(lv_sat) = 100 - lv_norm * 20.
          DATA(lv_light) = 10 + lv_norm * 50.

          APPEND VALUE zif_o4d_effect=>ty_rect(
            x = lv_px * mv_pixel_w
            y = lv_py * mv_pixel_h
            w = mv_pixel_w + 1
            h = mv_pixel_h + 1
            fill = |hsl({ lv_hue DECIMALS = 0 }, { lv_sat DECIMALS = 0 }%, { lv_light DECIMALS = 0 }%)|
          ) TO rs_frame-rects.
        ELSE.
          APPEND VALUE zif_o4d_effect=>ty_rect(
            x = lv_px * mv_pixel_w
            y = lv_py * mv_pixel_h
            w = mv_pixel_w + 1
            h = mv_pixel_h + 1
            fill = '#000000'
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
      text = |BURNING SHIP|
      color = '#FF6600' size = 16 align = 'center'
    ) TO rs_frame-texts.

    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lc_w / 2 y = lc_h - 15
      text = |zoom: { lv_zoom_factor DECIMALS = 1 }x|
      color = '#884400' size = 10 align = 'center'
    ) TO rs_frame-texts.

    IF is_ctx-gbi-pulse > '0.6'.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = '0.2'.
      rs_frame-flash-r = 1. rs_frame-flash-g = '0.3'. rs_frame-flash-b = 0.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

