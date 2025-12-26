CLASS zcl_o4d_torus_knot DEFINITION PUBLIC FINAL CREATE PUBLIC.
*======================================================================
* TORUS KNOT - Parametric 3D Curve
*======================================================================
* A knot that winds around a torus surface
* (p,q) = winding numbers: p times around hole, q through hole
* Common knots: (2,3)=trefoil, (3,2), (2,5)=cinquefoil, (3,5)
* Precalculated vertices, only rotation at render
*======================================================================
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    CLASS-METHODS new
      IMPORTING iv_p          TYPE i DEFAULT 2      " Winds around torus hole
                iv_q          TYPE i DEFAULT 3      " Winds through torus hole
                iv_segments   TYPE i DEFAULT 300    " Curve resolution
                iv_tube_segs  TYPE i DEFAULT 8      " Tube cross-section segments
                iv_radius     TYPE f DEFAULT 100    " Torus radius
                iv_tube_r     TYPE f DEFAULT 25     " Tube radius
                iv_line_width TYPE f DEFAULT 2
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_o4d_torus_knot.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_v3, x TYPE f, y TYPE f, z TYPE f, END OF ty_v3.
    TYPES: BEGIN OF ty_edge, v1 TYPE ty_v3, v2 TYPE ty_v3, END OF ty_edge.

    DATA mt_edges TYPE STANDARD TABLE OF ty_edge WITH EMPTY KEY.
    DATA mv_p TYPE i.
    DATA mv_q TYPE i.
    DATA mv_segments TYPE i.
    DATA mv_radius TYPE f.
    DATA mv_line_width TYPE f.

    CONSTANTS c_pi TYPE f VALUE '3.14159265358979'.
    CONSTANTS c_2pi TYPE f VALUE '6.28318530717959'.

    METHODS init_geometry.
    METHODS get_knot_point
      IMPORTING iv_t        TYPE f
      RETURNING VALUE(rs_v) TYPE ty_v3.
    METHODS rotate
      IMPORTING is_v  TYPE ty_v3
                iv_rx TYPE f
                iv_ry TYPE f
                iv_rz TYPE f
      RETURNING VALUE(rs_v) TYPE ty_v3.
ENDCLASS.



CLASS zcl_o4d_torus_knot IMPLEMENTATION.

  METHOD new.
    ro_obj = NEW #( ).
    ro_obj->mv_p = iv_p.
    ro_obj->mv_q = iv_q.
    ro_obj->mv_segments = iv_segments.
    ro_obj->mv_radius = iv_radius.
    ro_obj->mv_line_width = iv_line_width.
    ro_obj->init_geometry( ).
  ENDMETHOD.


  METHOD init_geometry.
    " Generate knot curve as connected line segments
    DATA(lv_dt) = c_2pi / mv_segments.
    DATA(lv_t) = CONV f( 0 ).

    DATA(ls_prev) = get_knot_point( 0 ).

    DO mv_segments TIMES.
      lv_t = lv_t + lv_dt.
      DATA(ls_curr) = get_knot_point( lv_t ).

      APPEND VALUE ty_edge( v1 = ls_prev v2 = ls_curr ) TO mt_edges.
      ls_prev = ls_curr.
    ENDDO.
  ENDMETHOD.


  METHOD get_knot_point.
    " Torus knot parametric equations
    " r = cos(q*t) + 2 (distance from torus center)
    " x = r * cos(p*t)
    " y = r * sin(p*t)
    " z = -sin(q*t)

    DATA(lv_r) = cos( mv_q * iv_t ) + 2.

    rs_v-x = lv_r * cos( mv_p * iv_t ) * mv_radius / 3.
    rs_v-y = lv_r * sin( mv_p * iv_t ) * mv_radius / 3.
    rs_v-z = - sin( mv_q * iv_t ) * mv_radius / 3.
  ENDMETHOD.


  METHOD rotate.
    DATA(lv_cx) = cos( iv_rx ). DATA(lv_sx) = sin( iv_rx ).
    DATA(lv_cy) = cos( iv_ry ). DATA(lv_sy) = sin( iv_ry ).
    DATA(lv_cz) = cos( iv_rz ). DATA(lv_sz) = sin( iv_rz ).

    " Rotate around X
    DATA(lv_y1) = is_v-y * lv_cx - is_v-z * lv_sx.
    DATA(lv_z1) = is_v-y * lv_sx + is_v-z * lv_cx.

    " Rotate around Y
    DATA(lv_x2) = is_v-x * lv_cy + lv_z1 * lv_sy.
    DATA(lv_z2) = lv_z1 * lv_cy - is_v-x * lv_sy.

    " Rotate around Z
    rs_v-x = lv_x2 * lv_cz - lv_y1 * lv_sz.
    rs_v-y = lv_x2 * lv_sz + lv_y1 * lv_cz.
    rs_v-z = lv_z2.
  ENDMETHOD.


  METHOD zif_o4d_effect~get_name.
    rv_name = 'torus_knot'.
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

    " Set frame state
    rs_frame-state-line_width = mv_line_width.
    rs_frame-state-line_cap = 'round'.
    rs_frame-state-line_join = 'round'.

    " Smooth rotation
    DATA(lv_rx) = lv_t * '0.3'.
    DATA(lv_ry) = lv_t * '0.5'.
    DATA(lv_rz) = lv_t * '0.2'.

    " Perspective
    DATA(lv_d) = CONV f( 4 ).
    DATA(lv_scale) = CONV f( '1.5' ).

    " Color cycling based on position along curve
    DATA(lv_idx) = 0.

    LOOP AT mt_edges INTO DATA(ls_e).
      lv_idx = lv_idx + 1.

      " Rotate vertices
      DATA(ls_v1) = rotate( is_v = ls_e-v1 iv_rx = lv_rx iv_ry = lv_ry iv_rz = lv_rz ).
      DATA(ls_v2) = rotate( is_v = ls_e-v2 iv_rx = lv_rx iv_ry = lv_ry iv_rz = lv_rz ).

      " Perspective projection
      DATA(lv_z1) = ls_v1-z / mv_radius.
      DATA(lv_z2) = ls_v2-z / mv_radius.
      DATA(lv_s1) = lv_d / ( lv_d - lv_z1 ).
      DATA(lv_s2) = lv_d / ( lv_d - lv_z2 ).

      DATA(lv_px1) = lv_cx + ls_v1-x * lv_scale * lv_s1.
      DATA(lv_py1) = lv_cy + ls_v1-y * lv_scale * lv_s1.
      DATA(lv_px2) = lv_cx + ls_v2-x * lv_scale * lv_s2.
      DATA(lv_py2) = lv_cy + ls_v2-y * lv_scale * lv_s2.

      " Rainbow color cycling along curve + time animation
      DATA(lv_curve_pos) = CONV f( lv_idx ) / lines( mt_edges ).
      DATA(lv_hue) = CONV i( ( lv_curve_pos * 360 + lv_t * 50 ) ) MOD 360.

      " Depth-based brightness
      DATA(lv_avg_z) = ( lv_z1 + lv_z2 ) / 2.
      DATA(lv_light) = 40 + CONV i( ( lv_avg_z + 1 ) * 25 ).

      " Beat pulse
      IF is_ctx-gbi-pulse > '0.3'.
        lv_light = lv_light + CONV i( is_ctx-gbi-pulse * 15 ).
      ENDIF.

      APPEND VALUE zif_o4d_effect=>ty_line(
        x1 = lv_px1 y1 = lv_py1
        x2 = lv_px2 y2 = lv_py2
        width = mv_line_width
        color = |hsl({ lv_hue }, 85%, { lv_light }%)|
      ) TO rs_frame-lines.
    ENDLOOP.

    " Title with knot type
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_cx y = 20
      text = |TORUS KNOT ({ mv_p },{ mv_q })|
      color = '#fff' size = 14 align = 'center'
    ) TO rs_frame-texts.

    DATA(lv_knot_name) = SWITCH string( mv_p && mv_q
      WHEN '23' THEN 'Trefoil'
      WHEN '32' THEN 'Trefoil'
      WHEN '25' THEN 'Cinquefoil'
      WHEN '52' THEN 'Cinquefoil'
      WHEN '35' THEN 'Three-Five'
      ELSE ''
    ).
    IF lv_knot_name IS NOT INITIAL.
      APPEND VALUE zif_o4d_effect=>ty_text(
        x = lv_cx y = lv_h - 15
        text = lv_knot_name
        color = '#888' size = 10 align = 'center'
      ) TO rs_frame-texts.
    ENDIF.

    " Flash on bar
    IF is_ctx-gbi-on_bar = abap_true.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = '0.1'.
      rs_frame-flash-r = 1. rs_frame-flash-g = 1. rs_frame-flash-b = 1.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
