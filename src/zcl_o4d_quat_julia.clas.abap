CLASS zcl_o4d_quat_julia DEFINITION PUBLIC FINAL CREATE PUBLIC.
*======================================================================
* QUATERNION JULIA - 4D Fractal Slice
*======================================================================
* 3D slice of 4D quaternion Julia set
* Iteration: q = q² + c (quaternion math)
* Visualized as point cloud with depth coloring
* The c parameter animates for morphing organic shapes
*======================================================================
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    CLASS-METHODS new
      IMPORTING iv_resolution TYPE i DEFAULT 24    " Grid resolution per axis
                iv_max_iter   TYPE i DEFAULT 8     " Iteration limit
                iv_threshold  TYPE f DEFAULT 2     " Escape threshold
                iv_size       TYPE f DEFAULT 150   " Display size
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_o4d_quat_julia.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_quat, w TYPE f, x TYPE f, y TYPE f, z TYPE f, END OF ty_quat.
    TYPES: BEGIN OF ty_v3, x TYPE f, y TYPE f, z TYPE f, END OF ty_v3.
    TYPES: BEGIN OF ty_point, pos TYPE ty_v3, iter TYPE i, END OF ty_point.

    DATA mv_resolution TYPE i.
    DATA mv_max_iter TYPE i.
    DATA mv_threshold TYPE f.
    DATA mv_size TYPE f.
    DATA mv_threshold_sq TYPE f.

    METHODS quat_mult
      IMPORTING is_a        TYPE ty_quat
                is_b        TYPE ty_quat
      RETURNING VALUE(rs_r) TYPE ty_quat.

    METHODS quat_square
      IMPORTING is_q        TYPE ty_quat
      RETURNING VALUE(rs_r) TYPE ty_quat.

    METHODS quat_mag_sq
      IMPORTING is_q        TYPE ty_quat
      RETURNING VALUE(rv_m) TYPE f.

    METHODS julia_test
      IMPORTING is_pos      TYPE ty_v3
                is_c        TYPE ty_quat
      RETURNING VALUE(rv_i) TYPE i.

    METHODS rotate
      IMPORTING is_v  TYPE ty_v3
                iv_rx TYPE f
                iv_ry TYPE f
                iv_rz TYPE f
      RETURNING VALUE(rs_v) TYPE ty_v3.
ENDCLASS.



CLASS zcl_o4d_quat_julia IMPLEMENTATION.

  METHOD new.
    ro_obj = NEW #( ).
    ro_obj->mv_resolution = iv_resolution.
    ro_obj->mv_max_iter = iv_max_iter.
    ro_obj->mv_threshold = iv_threshold.
    ro_obj->mv_threshold_sq = iv_threshold * iv_threshold.
    ro_obj->mv_size = iv_size.
  ENDMETHOD.


  METHOD quat_mult.
    " Quaternion multiplication: (a1,b1,c1,d1) * (a2,b2,c2,d2)
    rs_r-w = is_a-w * is_b-w - is_a-x * is_b-x - is_a-y * is_b-y - is_a-z * is_b-z.
    rs_r-x = is_a-w * is_b-x + is_a-x * is_b-w + is_a-y * is_b-z - is_a-z * is_b-y.
    rs_r-y = is_a-w * is_b-y - is_a-x * is_b-z + is_a-y * is_b-w + is_a-z * is_b-x.
    rs_r-z = is_a-w * is_b-z + is_a-x * is_b-y - is_a-y * is_b-x + is_a-z * is_b-w.
  ENDMETHOD.


  METHOD quat_square.
    " Optimized q² = (w²-x²-y²-z², 2wx, 2wy, 2wz)
    rs_r-w = is_q-w * is_q-w - is_q-x * is_q-x - is_q-y * is_q-y - is_q-z * is_q-z.
    rs_r-x = 2 * is_q-w * is_q-x.
    rs_r-y = 2 * is_q-w * is_q-y.
    rs_r-z = 2 * is_q-w * is_q-z.
  ENDMETHOD.


  METHOD quat_mag_sq.
    " |q|² = w² + x² + y² + z²
    rv_m = is_q-w * is_q-w + is_q-x * is_q-x + is_q-y * is_q-y + is_q-z * is_q-z.
  ENDMETHOD.


  METHOD julia_test.
    " Test if point is in Julia set
    " Returns iteration count (higher = closer to set boundary = more interesting)
    DATA(ls_q) = VALUE ty_quat( w = is_pos-x x = is_pos-y y = is_pos-z z = 0 ).

    DO mv_max_iter TIMES.
      " q = q² + c
      ls_q = quat_square( ls_q ).
      ls_q-w = ls_q-w + is_c-w.
      ls_q-x = ls_q-x + is_c-x.
      ls_q-y = ls_q-y + is_c-y.
      ls_q-z = ls_q-z + is_c-z.

      " Check escape
      IF quat_mag_sq( ls_q ) > mv_threshold_sq.
        rv_i = sy-index.
        RETURN.
      ENDIF.
    ENDDO.

    " Didn't escape - inside set
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


  METHOD zif_o4d_effect~get_name.
    rv_name = 'quat_julia'.
  ENDMETHOD.


  METHOD zif_o4d_effect~get_params.
  ENDMETHOD.


  METHOD zif_o4d_effect~set_param.
  ENDMETHOD.


  METHOD zif_o4d_effect~render.
    rs_frame = zif_o4d_effect~render_frame( VALUE #(
      t = is_sync-time
      gt = is_sync-time
      gbi = VALUE #( bar = is_sync-bar bar_phase = is_sync-bar_phase pulse = is_sync-intensity )
    ) ).
  ENDMETHOD.


  METHOD zif_o4d_effect~render_frame.
    DATA(lv_w) = CONV f( zif_o4d_effect=>c_width ).
    DATA(lv_h) = CONV f( zif_o4d_effect=>c_height ).
    DATA(lv_cx) = lv_w / 2.
    DATA(lv_cy) = lv_h / 2.
    DATA(lv_t) = is_ctx-gt.

    " Animate c parameter for morphing shapes
    " Classic Julia c values orbit in 4D space
    DATA(ls_c) = VALUE ty_quat(
      w = '-0.2' + sin( lv_t * '0.3' ) * '0.15'
      x = '0.6' + cos( lv_t * '0.4' ) * '0.1'
      y = sin( lv_t * '0.2' ) * '0.2'
      z = cos( lv_t * '0.25' ) * '0.1'
    ).

    " Rotation
    DATA(lv_rx) = lv_t * '0.2'.
    DATA(lv_ry) = lv_t * '0.3'.
    DATA(lv_rz) = lv_t * '0.1'.

    " Sample grid
    DATA(lv_step) = CONV f( 2 ) / mv_resolution.
    DATA(lv_start) = CONV f( -1 ).

    " Perspective
    DATA(lv_d) = CONV f( 4 ).

    DATA: lv_px TYPE f, lv_py TYPE f, lv_pz TYPE f.
    DATA: lv_iter TYPE i.
    DATA: ls_pos TYPE ty_v3, ls_rot TYPE ty_v3.

    " Sample 3D space and render points inside/near the set
    lv_pz = lv_start.
    DO mv_resolution TIMES.
      lv_py = lv_start.
      DO mv_resolution TIMES.
        lv_px = lv_start.
        DO mv_resolution TIMES.

          ls_pos = VALUE ty_v3( x = lv_px y = lv_py z = lv_pz ).
          lv_iter = julia_test( is_pos = ls_pos is_c = ls_c ).

          " Only render points near the boundary (most interesting)
          IF lv_iter >= 3 AND lv_iter <= mv_max_iter - 1.
            " Scale to display size
            ls_pos-x = ls_pos-x * mv_size.
            ls_pos-y = ls_pos-y * mv_size.
            ls_pos-z = ls_pos-z * mv_size.

            " Rotate
            ls_rot = rotate( is_v = ls_pos iv_rx = lv_rx iv_ry = lv_ry iv_rz = lv_rz ).

            " Perspective
            DATA(lv_z_norm) = ls_rot-z / mv_size.
            DATA(lv_s) = lv_d / ( lv_d - lv_z_norm ).

            DATA(lv_sx) = lv_cx + ls_rot-x * lv_s.
            DATA(lv_sy) = lv_cy + ls_rot-y * lv_s.

            " Only render if on screen
            IF lv_sx > 0 AND lv_sx < lv_w AND lv_sy > 0 AND lv_sy < lv_h.
              " Color based on iteration count and depth
              DATA(lv_iter_norm) = CONV f( lv_iter ) / mv_max_iter.
              DATA(lv_hue) = CONV i( 280 - lv_iter_norm * 120 + lv_t * 20 ) MOD 360.
              DATA(lv_light) = 40 + CONV i( ( lv_z_norm + 1 ) * 25 ).
              DATA(lv_point_size) = 4 + CONV i( lv_iter_norm * 6 ).  " Larger circles

              " Alpha based on depth (far = more transparent)
              DATA(lv_alpha) = CONV i( 40 + ( lv_z_norm + 1 ) * 30 ).  " 40-100 hex

              " Render as larger, semi-transparent circle
              APPEND VALUE zif_o4d_effect=>ty_circle(
                x = lv_sx
                y = lv_sy
                radius = lv_point_size
                fill = |hsl({ lv_hue }, 80%, { lv_light }%, { lv_alpha }%)|
              ) TO rs_frame-circles.
            ENDIF.
          ENDIF.

          lv_px = lv_px + lv_step.
        ENDDO.
        lv_py = lv_py + lv_step.
      ENDDO.
      lv_pz = lv_pz + lv_step.
    ENDDO.

    " Title
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_cx y = 20
      text = 'QUATERNION JULIA'
      color = '#f0f' size = 14 align = 'center'
    ) TO rs_frame-texts.

    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_cx y = lv_h - 15
      text = |c = ({ ls_c-w DECIMALS = 2 }, { ls_c-x DECIMALS = 2 }, { ls_c-y DECIMALS = 2 }, { ls_c-z DECIMALS = 2 })|
      color = '#888' size = 10 align = 'center'
    ) TO rs_frame-texts.

    " Beat pulse
    IF is_ctx-gbi-pulse > '0.5'.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = '0.1'.
      rs_frame-flash-r = 1. rs_frame-flash-g = 0. rs_frame-flash-b = 1.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
