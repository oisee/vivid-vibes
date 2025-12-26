CLASS zcl_o4d_mandelbulb DEFINITION PUBLIC FINAL CREATE PUBLIC.
*======================================================================
* MANDELBULB - 3D Mandelbrot Fractal
*======================================================================
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    CLASS-METHODS new
      IMPORTING iv_power      TYPE f DEFAULT 8
                iv_resolution TYPE i DEFAULT 28
                iv_max_iter   TYPE i DEFAULT 10
                iv_size       TYPE f DEFAULT 160
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_o4d_mandelbulb.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_v3, x TYPE f, y TYPE f, z TYPE f, END OF ty_v3.
    DATA mv_power TYPE f.
    DATA mv_resolution TYPE i.
    DATA mv_max_iter TYPE i.
    DATA mv_bailout TYPE f VALUE 2.
    DATA mv_size TYPE f.
    CONSTANTS c_pi TYPE f VALUE '3.14159265358979'.

    METHODS atan2 IMPORTING iv_y TYPE f iv_x TYPE f RETURNING VALUE(rv_a) TYPE f.
    METHODS mandelbulb_test IMPORTING is_c TYPE ty_v3 RETURNING VALUE(rv_i) TYPE i.
    METHODS rotate IMPORTING is_v TYPE ty_v3 iv_rx TYPE f iv_ry TYPE f iv_rz TYPE f RETURNING VALUE(rs_v) TYPE ty_v3.
ENDCLASS.

CLASS zcl_o4d_mandelbulb IMPLEMENTATION.
  METHOD new.
    ro_obj = NEW #( ).
    ro_obj->mv_power = iv_power.
    ro_obj->mv_resolution = iv_resolution.
    ro_obj->mv_max_iter = iv_max_iter.
    ro_obj->mv_size = iv_size.
  ENDMETHOD.

  METHOD atan2.
    IF iv_x > 0. rv_a = atan( iv_y / iv_x ).
    ELSEIF iv_x < 0 AND iv_y >= 0. rv_a = atan( iv_y / iv_x ) + c_pi.
    ELSEIF iv_x < 0 AND iv_y < 0. rv_a = atan( iv_y / iv_x ) - c_pi.
    ELSEIF iv_x = 0 AND iv_y > 0. rv_a = c_pi / 2.
    ELSEIF iv_x = 0 AND iv_y < 0. rv_a = - c_pi / 2.
    ELSE. rv_a = 0. ENDIF.
  ENDMETHOD.

  METHOD mandelbulb_test.
    DATA(lv_x) = is_c-x. DATA(lv_y) = is_c-y. DATA(lv_z) = is_c-z.
    DO mv_max_iter TIMES.
      DATA(lv_r) = sqrt( lv_x * lv_x + lv_y * lv_y + lv_z * lv_z ).
      IF lv_r > mv_bailout. rv_i = sy-index. RETURN. ENDIF.
      IF lv_r < '0.0001'. lv_r = '0.0001'. ENDIF.
      DATA(lv_theta) = atan2( iv_y = lv_y iv_x = lv_x ).
      DATA(lv_zr) = lv_z / lv_r.
      IF lv_zr > 1. lv_zr = 1. ENDIF. IF lv_zr < -1. lv_zr = -1. ENDIF.
      DATA(lv_phi) = acos( lv_zr ).
      DATA(lv_rn) = lv_r ** mv_power.
      DATA(lv_theta_n) = lv_theta * mv_power.
      DATA(lv_phi_n) = lv_phi * mv_power.
      lv_x = lv_rn * sin( lv_phi_n ) * cos( lv_theta_n ) + is_c-x.
      lv_y = lv_rn * sin( lv_phi_n ) * sin( lv_theta_n ) + is_c-y.
      lv_z = lv_rn * cos( lv_phi_n ) + is_c-z.
    ENDDO.
    rv_i = mv_max_iter.
  ENDMETHOD.

  METHOD rotate.
    DATA(lv_cx) = cos( iv_rx ). DATA(lv_sx) = sin( iv_rx ).
    DATA(lv_cy) = cos( iv_ry ). DATA(lv_sy) = sin( iv_ry ).
    DATA(lv_cz) = cos( iv_rz ). DATA(lv_sz) = sin( iv_rz ).
    DATA(lv_y1) = is_v-y * lv_cx - is_v-z * lv_sx.
    DATA(lv_z1) = is_v-y * lv_sx + is_v-z * lv_cx.
    DATA(lv_x2) = is_v-x * lv_cy + lv_z1 * lv_sy.
    DATA(lv_z2) = lv_z1 * lv_cy - is_v-x * lv_sy.
    rs_v-x = lv_x2 * lv_cz - lv_y1 * lv_sz.
    rs_v-y = lv_x2 * lv_sz + lv_y1 * lv_cz.
    rs_v-z = lv_z2.
  ENDMETHOD.

  METHOD zif_o4d_effect~get_name. rv_name = 'mandelbulb'. ENDMETHOD.
  METHOD zif_o4d_effect~get_params. ENDMETHOD.
  METHOD zif_o4d_effect~set_param. ENDMETHOD.

  METHOD zif_o4d_effect~render.
    rs_frame = zif_o4d_effect~render_frame( VALUE #( t = is_sync-time gt = is_sync-time
      gbi = VALUE #( bar = is_sync-bar bar_phase = is_sync-bar_phase pulse = is_sync-intensity ) ) ).
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    DATA(lv_w) = CONV f( 640 ). DATA(lv_h) = CONV f( 400 ).
    DATA(lv_cx) = lv_w / 2. DATA(lv_cy) = lv_h / 2.
    DATA(lv_t) = is_ctx-gt.
    DATA(lv_rx) = lv_t * '0.15'. DATA(lv_ry) = lv_t * '0.2'. DATA(lv_rz) = lv_t * '0.08'.
    DATA(lv_range) = CONV f( '1.3' ).
    DATA(lv_step) = lv_range * 2 / mv_resolution.
    DATA(lv_start) = - lv_range.
    DATA(lv_d) = CONV f( 4 ).
    DATA: lv_px TYPE f, lv_py TYPE f, lv_pz TYPE f, lv_iter TYPE i.
    DATA: ls_pos TYPE ty_v3, ls_rot TYPE ty_v3.

    lv_pz = lv_start.
    DO mv_resolution TIMES.
      lv_py = lv_start.
      DO mv_resolution TIMES.
        lv_px = lv_start.
        DO mv_resolution TIMES.
          ls_pos = VALUE ty_v3( x = lv_px y = lv_py z = lv_pz ).
          lv_iter = mandelbulb_test( ls_pos ).
          IF lv_iter >= 2 AND lv_iter <= mv_max_iter - 1.
            DATA(ls_scaled) = VALUE ty_v3( x = ls_pos-x * mv_size y = ls_pos-y * mv_size z = ls_pos-z * mv_size ).
            ls_rot = rotate( is_v = ls_scaled iv_rx = lv_rx iv_ry = lv_ry iv_rz = lv_rz ).
            DATA(lv_z_norm) = ls_rot-z / mv_size.
            DATA(lv_s) = lv_d / ( lv_d - lv_z_norm ).
            DATA(lv_sx) = lv_cx + ls_rot-x * lv_s.
            DATA(lv_sy) = lv_cy + ls_rot-y * lv_s.
            IF lv_sx > 0 AND lv_sx < lv_w AND lv_sy > 0 AND lv_sy < lv_h.
              DATA(lv_iter_norm) = CONV f( lv_iter ) / mv_max_iter.
              DATA(lv_hue) = CONV i( 30 - lv_iter_norm * 40 + lv_t * 10 ) MOD 360.
              IF lv_hue < 0. lv_hue = lv_hue + 360. ENDIF.
              DATA(lv_light) = 35 + CONV i( ( lv_z_norm + 1 ) * 30 ).
              DATA(lv_point_size) = 4 + CONV i( lv_iter_norm * 5 ).
              DATA(lv_alpha) = CONV i( 35 + ( lv_z_norm + 1 ) * 35 ).
              APPEND VALUE zif_o4d_effect=>ty_circle( x = lv_sx y = lv_sy radius = lv_point_size
                fill = |hsl({ lv_hue }, 90%, { lv_light }%, { lv_alpha }%)| ) TO rs_frame-circles.
            ENDIF.
          ENDIF.
          lv_px = lv_px + lv_step.
        ENDDO.
        lv_py = lv_py + lv_step.
      ENDDO.
      lv_pz = lv_pz + lv_step.
    ENDDO.

    APPEND VALUE zif_o4d_effect=>ty_text( x = lv_cx y = 20 text = |MANDELBULB (n={ mv_power DECIMALS = 0 })|
      color = '#ff8800' size = 14 align = 'center' ) TO rs_frame-texts.
    IF is_ctx-gbi-pulse > '0.5'.
      rs_frame-flash = VALUE #( active = abap_true intensity = '0.1' r = 1 g = '0.5' b = 0 ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
