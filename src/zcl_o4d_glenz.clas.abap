CLASS zcl_o4d_glenz DEFINITION PUBLIC FINAL CREATE PUBLIC.
*======================================================================
* GLENZ VECTORS - Classic 90s transparent rotating diamonds
*======================================================================
* Multiple rotating polyhedra with additive transparency
* Back-to-front sorted for correct blending
*======================================================================
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    CLASS-METHODS new
      IMPORTING iv_count TYPE i DEFAULT 3
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_o4d_glenz.
    METHODS constructor IMPORTING iv_count TYPE i.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_v3, x TYPE f, y TYPE f, z TYPE f, END OF ty_v3.
    TYPES: BEGIN OF ty_face, v1 TYPE i, v2 TYPE i, v3 TYPE i, v4 TYPE i, END OF ty_face.
    TYPES: BEGIN OF ty_tri_z, z TYPE f, tri TYPE zif_o4d_effect=>ty_triangle, END OF ty_tri_z.
    DATA mv_count TYPE i.
    DATA mt_verts TYPE STANDARD TABLE OF ty_v3 WITH EMPTY KEY.
    DATA mt_faces TYPE STANDARD TABLE OF ty_face WITH EMPTY KEY.
    METHODS init_diamond.
    METHODS rotate IMPORTING is_v TYPE ty_v3 iv_rx TYPE f iv_ry TYPE f iv_rz TYPE f RETURNING VALUE(rs_v) TYPE ty_v3.
ENDCLASS.

CLASS zcl_o4d_glenz IMPLEMENTATION.
  METHOD new. ro_obj = NEW #( iv_count ). ENDMETHOD.
  METHOD constructor. mv_count = iv_count. init_diamond( ). ENDMETHOD.

  METHOD init_diamond.
    " Octahedron (double pyramid) - 6 vertices, 8 triangular faces
    APPEND VALUE ty_v3( x = 0 y = -1 z = 0 ) TO mt_verts.  " Top
    APPEND VALUE ty_v3( x = 1 y = 0 z = 0 ) TO mt_verts.   " +X
    APPEND VALUE ty_v3( x = 0 y = 0 z = 1 ) TO mt_verts.   " +Z
    APPEND VALUE ty_v3( x = -1 y = 0 z = 0 ) TO mt_verts.  " -X
    APPEND VALUE ty_v3( x = 0 y = 0 z = -1 ) TO mt_verts.  " -Z
    APPEND VALUE ty_v3( x = 0 y = 1 z = 0 ) TO mt_verts.   " Bottom
    " Top 4 faces (v4=0 means triangle, not quad)
    APPEND VALUE ty_face( v1 = 1 v2 = 2 v3 = 3 v4 = 0 ) TO mt_faces.
    APPEND VALUE ty_face( v1 = 1 v2 = 3 v3 = 4 v4 = 0 ) TO mt_faces.
    APPEND VALUE ty_face( v1 = 1 v2 = 4 v3 = 5 v4 = 0 ) TO mt_faces.
    APPEND VALUE ty_face( v1 = 1 v2 = 5 v3 = 2 v4 = 0 ) TO mt_faces.
    " Bottom 4 faces
    APPEND VALUE ty_face( v1 = 6 v2 = 3 v3 = 2 v4 = 0 ) TO mt_faces.
    APPEND VALUE ty_face( v1 = 6 v2 = 4 v3 = 3 v4 = 0 ) TO mt_faces.
    APPEND VALUE ty_face( v1 = 6 v2 = 5 v3 = 4 v4 = 0 ) TO mt_faces.
    APPEND VALUE ty_face( v1 = 6 v2 = 2 v3 = 5 v4 = 0 ) TO mt_faces.
  ENDMETHOD.

  METHOD rotate.
    DATA(lv_cx) = cos( iv_rx ). DATA(lv_sx) = sin( iv_rx ).
    DATA(lv_cy) = cos( iv_ry ). DATA(lv_sy) = sin( iv_ry ).
    DATA(lv_cz) = cos( iv_rz ). DATA(lv_sz) = sin( iv_rz ).
    " Rotate X
    DATA(lv_y1) = is_v-y * lv_cx - is_v-z * lv_sx.
    DATA(lv_z1) = is_v-y * lv_sx + is_v-z * lv_cx.
    " Rotate Y
    DATA(lv_x2) = is_v-x * lv_cy + lv_z1 * lv_sy.
    DATA(lv_z2) = lv_z1 * lv_cy - is_v-x * lv_sy.
    " Rotate Z
    rs_v-x = lv_x2 * lv_cz - lv_y1 * lv_sz.
    rs_v-y = lv_x2 * lv_sz + lv_y1 * lv_cz.
    rs_v-z = lv_z2.
  ENDMETHOD.

  METHOD zif_o4d_effect~get_name. rv_name = 'glenz'. ENDMETHOD.
  METHOD zif_o4d_effect~get_params. ENDMETHOD.
  METHOD zif_o4d_effect~set_param. ENDMETHOD.
  METHOD zif_o4d_effect~render.
    rs_frame = zif_o4d_effect~render_frame( VALUE #( t = is_sync-time gt = is_sync-time
      gbi = VALUE #( bar = is_sync-bar bar_phase = is_sync-bar_phase pulse = is_sync-intensity ) ) ).
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    DATA lt_all TYPE STANDARD TABLE OF ty_tri_z WITH EMPTY KEY.
    DATA(lv_w) = CONV f( zif_o4d_effect=>c_width ).
    DATA(lv_h) = CONV f( zif_o4d_effect=>c_height ).
    DATA(lv_cx) = lv_w / 2. DATA(lv_cy) = lv_h / 2.
    DATA(lv_t) = is_ctx-gt.
    DATA(lv_size) = CONV f( 60 ).

    " Dark background
    APPEND VALUE zif_o4d_effect=>ty_rect( x = 0 y = 0 w = lv_w h = lv_h fill = '#111' ) TO rs_frame-rects.

    " Render multiple diamonds
    DATA: lv_d TYPE i, lv_angle TYPE f, lv_ox TYPE f, lv_oy TYPE f,
          lv_rx TYPE f, lv_ry TYPE f, lv_rz TYPE f, lv_hue TYPE i.
    lv_d = 0.
    WHILE lv_d < mv_count.
      lv_angle = CONV f( lv_d ) / mv_count * '6.283'.
      lv_ox = sin( lv_angle + lv_t * '0.3' ) * 100.
      lv_oy = cos( lv_angle + lv_t * '0.3' ) * 50.
      lv_rx = lv_t * ( '0.5' + CONV f( lv_d ) * '0.2' ).
      lv_ry = lv_t * ( '0.7' + CONV f( lv_d ) * '0.15' ).
      lv_rz = lv_t * ( '0.3' + CONV f( lv_d ) * '0.25' ).
      lv_hue = lv_d * 360 / mv_count.

      " Transform vertices
      DATA lt_proj TYPE STANDARD TABLE OF ty_v3 WITH EMPTY KEY.
      CLEAR lt_proj.
      LOOP AT mt_verts INTO DATA(ls_v).
        DATA(ls_r) = rotate( is_v = ls_v iv_rx = lv_rx iv_ry = lv_ry iv_rz = lv_rz ).
        DATA(lv_scale) = 3 / ( 3 - ls_r-z ).
        APPEND VALUE ty_v3(
          x = lv_cx + lv_ox + ls_r-x * lv_size * lv_scale
          y = lv_cy + lv_oy + ls_r-y * lv_size * lv_scale
          z = ls_r-z
        ) TO lt_proj.
      ENDLOOP.

      " Create triangles for each face
      LOOP AT mt_faces INTO DATA(ls_f).
        DATA(ls_p1) = lt_proj[ ls_f-v1 ]. DATA(ls_p2) = lt_proj[ ls_f-v2 ]. DATA(ls_p3) = lt_proj[ ls_f-v3 ].
        DATA(lv_z_avg) = ( ls_p1-z + ls_p2-z + ls_p3-z ) / 3.
        DATA(lv_light) = 40 + CONV i( ( lv_z_avg + 1 ) * 30 ).
        APPEND VALUE ty_tri_z( z = lv_z_avg tri = VALUE #(
          x1 = ls_p1-x y1 = ls_p1-y x2 = ls_p2-x y2 = ls_p2-y x3 = ls_p3-x y3 = ls_p3-y
          fill = |hsla({ lv_hue }, 80%, { lv_light }%, 0.4)|
        ) ) TO lt_all.
      ENDLOOP.
      lv_d = lv_d + 1.
    ENDWHILE.

    " Sort back-to-front and add to frame
    SORT lt_all BY z ASCENDING.
    LOOP AT lt_all INTO DATA(ls_t).
      APPEND ls_t-tri TO rs_frame-triangles.
    ENDLOOP.

    APPEND VALUE zif_o4d_effect=>ty_text( x = lv_cx y = 25 text = 'GLENZ VECTORS' color = '#fff' size = 16 align = 'center' ) TO rs_frame-texts.
    IF is_ctx-gbi-pulse > '0.5'. rs_frame-flash-active = abap_true. rs_frame-flash-intensity = '0.12'.
      rs_frame-flash-r = 1. rs_frame-flash-g = '0.5'. rs_frame-flash-b = 1. ENDIF.
  ENDMETHOD.
ENDCLASS.
