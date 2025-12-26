CLASS zcl_o4d_fractal_fly DEFINITION PUBLIC FINAL CREATE PUBLIC.
*======================================================================
* FRACTAL FLY-THROUGH - Camera flying over Mandelbrot heightmap
*======================================================================
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    CLASS-METHODS new
      IMPORTING iv_columns    TYPE i DEFAULT 80
                iv_rows       TYPE i DEFAULT 40
                iv_max_iter   TYPE i DEFAULT 24
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_o4d_fractal_fly.

  PRIVATE SECTION.
    DATA mv_columns TYPE i.
    DATA mv_rows TYPE i.
    DATA mv_max_iter TYPE i.

    METHODS mandel_iter
      IMPORTING iv_cx TYPE f iv_cy TYPE f
      RETURNING VALUE(rv_i) TYPE i.
ENDCLASS.

CLASS zcl_o4d_fractal_fly IMPLEMENTATION.

  METHOD new.
    ro_obj = NEW #( ).
    ro_obj->mv_columns = iv_columns.
    ro_obj->mv_rows = iv_rows.
    ro_obj->mv_max_iter = iv_max_iter.
  ENDMETHOD.


  METHOD mandel_iter.
    DATA lv_zx TYPE f VALUE 0.
    DATA lv_zy TYPE f VALUE 0.

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


  METHOD zif_o4d_effect~get_name. rv_name = 'fractal_fly'. ENDMETHOD.
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

    " Camera flying along Mandelbrot edge
    DATA(lv_cam_x) = '-0.75' + cos( lv_t * '0.08' ) * '0.5'.
    DATA(lv_cam_y) = sin( lv_t * '0.08' ) * '0.5'.
    DATA(lv_zoom) = '0.8' + sin( lv_t * '0.15' ) * '0.3'.

    DATA(lv_horizon) = lc_h * '0.35'.
    DATA(lv_ground_h) = lc_h - lv_horizon.
    DATA(lv_fov) = lv_zoom * 2.

    " Sky gradient
    DO 14 TIMES.
      DATA(lv_sky_y) = ( sy-index - 1 ) * ( lv_horizon / 14 ).
      DATA(lv_sky_t) = CONV f( sy-index - 1 ) / 14.

      DATA(lv_sky_hue) = 240 + lv_sky_t * 20.  " Blue gradient
      DATA(lv_sky_light) = 5 + lv_sky_t * 20.

      APPEND VALUE zif_o4d_effect=>ty_rect(
        x = 0 y = lv_sky_y
        w = lc_w h = lv_horizon / 14 + 1
        fill = |hsl({ lv_sky_hue DECIMALS = 0 }, 60%, { lv_sky_light DECIMALS = 0 }%)|
      ) TO rs_frame-rects_back.
    ENDDO.

    " Render terrain (back to front)
    DATA lv_row TYPE i.
    lv_row = mv_rows.

    WHILE lv_row > 0.
      DATA(lv_depth) = CONV f( lv_row ) / mv_rows.
      DATA(lv_screen_y) = lv_horizon + lv_ground_h * ( 1 - lv_depth ).
      DATA(lv_row_h) = nmax( val1 = 2 val2 = lv_ground_h / mv_rows * ( 1 - lv_depth * '0.7' ) ).
      DATA(lv_frac_y) = lv_cam_y + lv_depth * lv_fov.

      DATA lv_col TYPE i.
      lv_col = 0.

      WHILE lv_col < mv_columns.
        DATA(lv_col_norm) = CONV f( lv_col ) / mv_columns.
        DATA(lv_frac_x) = lv_cam_x + ( lv_col_norm - '0.5' ) * lv_fov * ( 1 + lv_depth ).
        DATA(lv_iter) = mandel_iter( iv_cx = lv_frac_x iv_cy = lv_frac_y ).
        DATA(lv_height) = CONV f( lv_iter ) / mv_max_iter.
        DATA(lv_fog) = lv_depth * '0.6'.

        DATA lv_hue TYPE f.
        DATA lv_sat TYPE f.
        DATA lv_light TYPE f.

        IF lv_iter >= mv_max_iter.
          " Water - blue
          lv_hue = 220.
          lv_sat = 70.
          lv_light = 15 * ( 1 - lv_fog ).
        ELSE.
          IF lv_height < '0.3'.
            " Green valleys
            lv_hue = 120.
            lv_sat = 60.
            lv_light = 20 + lv_height * 80.
          ELSEIF lv_height < '0.6'.
            " Brown hills
            lv_hue = 30.
            lv_sat = 50.
            lv_light = 25 + lv_height * 40.
          ELSE.
            " Snowy peaks
            lv_hue = 200.
            lv_sat = 10.
            lv_light = 60 + lv_height * 35.
          ENDIF.

          " Apply fog
          lv_light = lv_light * ( 1 - lv_fog ) + 15 * lv_fog.
          lv_sat = lv_sat * ( 1 - lv_fog ).
        ENDIF.

        DATA(lv_col_w) = lc_w / mv_columns * ( 1 + ( 1 - lv_depth ) * '0.5' ).
        DATA(lv_persp_x) = lc_w / 2 + ( lv_col_norm - '0.5' ) * lc_w * ( 1 - lv_depth * '0.3' ).

        APPEND VALUE zif_o4d_effect=>ty_rect(
          x = lv_persp_x - lv_col_w / 2
          y = lv_screen_y - lv_height * 30 * ( 1 - lv_depth )
          w = lv_col_w + 1
          h = lv_row_h + lv_height * 15 * ( 1 - lv_depth )
          fill = |hsl({ lv_hue DECIMALS = 0 }, { lv_sat DECIMALS = 0 }%, { lv_light DECIMALS = 0 }%)|
        ) TO rs_frame-rects.

        lv_col = lv_col + 1.
      ENDWHILE.

      lv_row = lv_row - 1.
    ENDWHILE.

    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lc_w / 2 y = 20
      text = |FRACTAL FLYOVER|
      color = '#88AAFF' size = 14 align = 'center'
    ) TO rs_frame-texts.

    IF is_ctx-gbi-pulse > '0.6'.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = '0.12'.
      rs_frame-flash-r = '0.5'. rs_frame-flash-g = '0.5'. rs_frame-flash-b = 1.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

