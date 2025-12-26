CLASS zcl_o4d_sierpinski_tet DEFINITION PUBLIC FINAL CREATE PUBLIC.
*======================================================================
* SIERPINSKI TETRAHEDRON (TETRIX) - Filled Faces + Plasma
*======================================================================
* Recursive tetrahedron fractal with filled triangular faces
* Plasma color projection onto 3D surfaces
* Depth 0: 1 tetrahedron (4 faces)
* Depth 1: 4 tetrahedra (16 faces)
* Depth 2: 16 tetrahedra (64 faces)
* Depth 3: 64 tetrahedra (256 faces)
*======================================================================
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    CLASS-METHODS new
      IMPORTING iv_depth      TYPE i DEFAULT 3
                iv_size       TYPE f DEFAULT 200
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_o4d_sierpinski_tet.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_v3, x TYPE f, y TYPE f, z TYPE f, END OF ty_v3.
    TYPES: tt_v3 TYPE STANDARD TABLE OF ty_v3 WITH EMPTY KEY.

    " Face = 3 vertices + center for plasma sampling
    TYPES: BEGIN OF ty_face,
             v1     TYPE ty_v3,
             v2     TYPE ty_v3,
             v3     TYPE ty_v3,
             center TYPE ty_v3,
           END OF ty_face.
    TYPES: tt_faces TYPE STANDARD TABLE OF ty_face WITH EMPTY KEY.

    DATA mt_faces TYPE tt_faces.
    DATA mv_depth TYPE i.
    DATA mv_size TYPE f.
    DATA mv_total_tets TYPE i.

    METHODS init_geometry.
    METHODS generate_sierpinski
      IMPORTING it_verts TYPE tt_v3
                iv_depth TYPE i.
    METHODS add_tetrahedron_faces
      IMPORTING it_verts TYPE tt_v3.
    METHODS midpoint
      IMPORTING is_a        TYPE ty_v3
                is_b        TYPE ty_v3
      RETURNING VALUE(rs_m) TYPE ty_v3.
    METHODS face_center
      IMPORTING is_a        TYPE ty_v3
                is_b        TYPE ty_v3
                is_c        TYPE ty_v3
      RETURNING VALUE(rs_c) TYPE ty_v3.
    METHODS rotate
      IMPORTING is_v      TYPE ty_v3
                iv_rx     TYPE f
                iv_ry     TYPE f
                iv_rz     TYPE f
      RETURNING VALUE(rs_v) TYPE ty_v3.
    METHODS plasma
      IMPORTING iv_x        TYPE f
                iv_y        TYPE f
                iv_z        TYPE f
                iv_t        TYPE f
      RETURNING VALUE(rv_h) TYPE i.
ENDCLASS.



CLASS zcl_o4d_sierpinski_tet IMPLEMENTATION.

  METHOD new.
    ro_obj = NEW #( ).
    ro_obj->mv_depth = iv_depth.
    ro_obj->mv_size = iv_size.
    ro_obj->init_geometry( ).
  ENDMETHOD.


  METHOD init_geometry.
    " Regular tetrahedron vertices (centered at origin)
    DATA lt_base TYPE tt_v3.

    " Top vertex
    APPEND VALUE ty_v3( x = 0
                        y = 1
                        z = 0 ) TO lt_base.
    " Bottom-front vertex
    APPEND VALUE ty_v3( x = 0
                        y = '-0.333333'
                        z = '0.942809' ) TO lt_base.
    " Bottom-back-left vertex
    APPEND VALUE ty_v3( x = '-0.816497'
                        y = '-0.333333'
                        z = '-0.471405' ) TO lt_base.
    " Bottom-back-right vertex
    APPEND VALUE ty_v3( x = '0.816497'
                        y = '-0.333333'
                        z = '-0.471405' ) TO lt_base.

    " Scale to desired size
    LOOP AT lt_base ASSIGNING FIELD-SYMBOL(<v>).
      <v>-x = <v>-x * mv_size.
      <v>-y = <v>-y * mv_size.
      <v>-z = <v>-z * mv_size.
    ENDLOOP.

    " Generate fractal recursively
    mv_total_tets = 0.
    generate_sierpinski( it_verts = lt_base iv_depth = mv_depth ).
  ENDMETHOD.


  METHOD generate_sierpinski.
    IF iv_depth = 0.
      " Base case: add this tetrahedron's faces
      add_tetrahedron_faces( it_verts ).
      mv_total_tets = mv_total_tets + 1.
      RETURN.
    ENDIF.

    " Get the 4 vertices
    DATA(ls_v0) = it_verts[ 1 ].
    DATA(ls_v1) = it_verts[ 2 ].
    DATA(ls_v2) = it_verts[ 3 ].
    DATA(ls_v3) = it_verts[ 4 ].

    " Calculate midpoints of all 6 edges
    DATA(ls_m01) = midpoint( is_a = ls_v0 is_b = ls_v1 ).
    DATA(ls_m02) = midpoint( is_a = ls_v0 is_b = ls_v2 ).
    DATA(ls_m03) = midpoint( is_a = ls_v0 is_b = ls_v3 ).
    DATA(ls_m12) = midpoint( is_a = ls_v1 is_b = ls_v2 ).
    DATA(ls_m13) = midpoint( is_a = ls_v1 is_b = ls_v3 ).
    DATA(ls_m23) = midpoint( is_a = ls_v2 is_b = ls_v3 ).

    " Create 4 sub-tetrahedra at corners
    DATA lt_sub TYPE tt_v3.

    " Sub-tet 0: corner at v0
    CLEAR lt_sub.
    APPEND ls_v0 TO lt_sub.
    APPEND ls_m01 TO lt_sub.
    APPEND ls_m02 TO lt_sub.
    APPEND ls_m03 TO lt_sub.
    generate_sierpinski( it_verts = lt_sub iv_depth = iv_depth - 1 ).

    " Sub-tet 1: corner at v1
    CLEAR lt_sub.
    APPEND ls_m01 TO lt_sub.
    APPEND ls_v1 TO lt_sub.
    APPEND ls_m12 TO lt_sub.
    APPEND ls_m13 TO lt_sub.
    generate_sierpinski( it_verts = lt_sub iv_depth = iv_depth - 1 ).

    " Sub-tet 2: corner at v2
    CLEAR lt_sub.
    APPEND ls_m02 TO lt_sub.
    APPEND ls_m12 TO lt_sub.
    APPEND ls_v2 TO lt_sub.
    APPEND ls_m23 TO lt_sub.
    generate_sierpinski( it_verts = lt_sub iv_depth = iv_depth - 1 ).

    " Sub-tet 3: corner at v3
    CLEAR lt_sub.
    APPEND ls_m03 TO lt_sub.
    APPEND ls_m13 TO lt_sub.
    APPEND ls_m23 TO lt_sub.
    APPEND ls_v3 TO lt_sub.
    generate_sierpinski( it_verts = lt_sub iv_depth = iv_depth - 1 ).
  ENDMETHOD.


  METHOD add_tetrahedron_faces.
    " A tetrahedron has 4 triangular faces
    DATA(ls_v0) = it_verts[ 1 ].
    DATA(ls_v1) = it_verts[ 2 ].
    DATA(ls_v2) = it_verts[ 3 ].
    DATA(ls_v3) = it_verts[ 4 ].

    " Face 0: v0-v1-v2
    APPEND VALUE ty_face(
      v1 = ls_v0 v2 = ls_v1 v3 = ls_v2
      center = face_center( is_a = ls_v0 is_b = ls_v1 is_c = ls_v2 )
    ) TO mt_faces.

    " Face 1: v0-v1-v3
    APPEND VALUE ty_face(
      v1 = ls_v0 v2 = ls_v1 v3 = ls_v3
      center = face_center( is_a = ls_v0 is_b = ls_v1 is_c = ls_v3 )
    ) TO mt_faces.

    " Face 2: v0-v2-v3
    APPEND VALUE ty_face(
      v1 = ls_v0 v2 = ls_v2 v3 = ls_v3
      center = face_center( is_a = ls_v0 is_b = ls_v2 is_c = ls_v3 )
    ) TO mt_faces.

    " Face 3: v1-v2-v3 (base)
    APPEND VALUE ty_face(
      v1 = ls_v1 v2 = ls_v2 v3 = ls_v3
      center = face_center( is_a = ls_v1 is_b = ls_v2 is_c = ls_v3 )
    ) TO mt_faces.
  ENDMETHOD.


  METHOD midpoint.
    rs_m-x = ( is_a-x + is_b-x ) / 2.
    rs_m-y = ( is_a-y + is_b-y ) / 2.
    rs_m-z = ( is_a-z + is_b-z ) / 2.
  ENDMETHOD.


  METHOD face_center.
    rs_c-x = ( is_a-x + is_b-x + is_c-x ) / 3.
    rs_c-y = ( is_a-y + is_b-y + is_c-y ) / 3.
    rs_c-z = ( is_a-z + is_b-z + is_c-z ) / 3.
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


  METHOD plasma.
    " 3D plasma function - returns hue 0-360
    DATA(lv_v1) = sin( iv_x * '0.02' + iv_t * 2 ).
    DATA(lv_v2) = sin( iv_y * '0.03' + iv_t * '1.5' ).
    DATA(lv_v3) = sin( iv_z * '0.025' + iv_t ).
    DATA(lv_v4) = sin( sqrt( iv_x * iv_x + iv_y * iv_y ) * '0.02' + iv_t * '1.2' ).

    DATA(lv_v) = ( lv_v1 + lv_v2 + lv_v3 + lv_v4 ) / 4.

    " Map -1..1 to 0..360
    rv_h = CONV i( ( lv_v + 1 ) * 180 ) MOD 360.
  ENDMETHOD.


  METHOD zif_o4d_effect~get_name.
    rv_name = 'sierpinski_tet'.
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

    " Rotation angles (elegant tumbling)
    DATA(lv_rx) = lv_t * '0.35'.
    DATA(lv_ry) = lv_t * '0.45'.
    DATA(lv_rz) = lv_t * '0.15'.

    " Perspective distance
    DATA(lv_d) = CONV f( 5 ).

    " Structure to hold projected faces with depth for sorting
    TYPES: BEGIN OF ty_proj_face,
             x1    TYPE f, y1 TYPE f,
             x2    TYPE f, y2 TYPE f,
             x3    TYPE f, y3 TYPE f,
             avg_z TYPE f,
             hue   TYPE i,
           END OF ty_proj_face.
    DATA lt_proj TYPE STANDARD TABLE OF ty_proj_face WITH EMPTY KEY.

    " Transform and project all faces
    LOOP AT mt_faces INTO DATA(ls_f).
      " Rotate all vertices
      DATA(ls_rv1) = rotate( is_v = ls_f-v1 iv_rx = lv_rx iv_ry = lv_ry iv_rz = lv_rz ).
      DATA(ls_rv2) = rotate( is_v = ls_f-v2 iv_rx = lv_rx iv_ry = lv_ry iv_rz = lv_rz ).
      DATA(ls_rv3) = rotate( is_v = ls_f-v3 iv_rx = lv_rx iv_ry = lv_ry iv_rz = lv_rz ).
      DATA(ls_rc) = rotate( is_v = ls_f-center iv_rx = lv_rx iv_ry = lv_ry iv_rz = lv_rz ).

      " Perspective projection
      DATA(lv_z1) = ls_rv1-z / mv_size.
      DATA(lv_z2) = ls_rv2-z / mv_size.
      DATA(lv_z3) = ls_rv3-z / mv_size.
      DATA(lv_s1) = lv_d / ( lv_d - lv_z1 ).
      DATA(lv_s2) = lv_d / ( lv_d - lv_z2 ).
      DATA(lv_s3) = lv_d / ( lv_d - lv_z3 ).

      DATA(lv_px1) = lv_cx + ls_rv1-x * lv_s1.
      DATA(lv_py1) = lv_cy + ls_rv1-y * lv_s1.
      DATA(lv_px2) = lv_cx + ls_rv2-x * lv_s2.
      DATA(lv_py2) = lv_cy + ls_rv2-y * lv_s2.
      DATA(lv_px3) = lv_cx + ls_rv3-x * lv_s3.
      DATA(lv_py3) = lv_cy + ls_rv3-y * lv_s3.

      " Average Z for depth sorting
      DATA(lv_avg_z) = ( lv_z1 + lv_z2 + lv_z3 ) / 3.

      " Plasma color based on rotated center position
      DATA(lv_hue) = plasma(
        iv_x = ls_rc-x
        iv_y = ls_rc-y
        iv_z = ls_rc-z
        iv_t = lv_t
      ).

      APPEND VALUE ty_proj_face(
        x1 = lv_px1 y1 = lv_py1
        x2 = lv_px2 y2 = lv_py2
        x3 = lv_px3 y3 = lv_py3
        avg_z = lv_avg_z
        hue = lv_hue
      ) TO lt_proj.
    ENDLOOP.

    " Sort by depth (back to front = ascending avg_z)
    SORT lt_proj BY avg_z ASCENDING.

    " Render sorted triangles
    LOOP AT lt_proj INTO DATA(ls_p).
      " Lightness based on depth
      DATA(lv_light) = 35 + CONV i( ( ls_p-avg_z + 1 ) * 25 ).

      APPEND VALUE zif_o4d_effect=>ty_triangle(
        x1 = ls_p-x1 y1 = ls_p-y1
        x2 = ls_p-x2 y2 = ls_p-y2
        x3 = ls_p-x3 y3 = ls_p-y3
        fill = |hsl({ ls_p-hue }, 85%, { lv_light }%)|
      ) TO rs_frame-triangles.
    ENDLOOP.

    " Title
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_cx y = 20
      text = |SIERPINSKI PLASMA|
      color = '#0ff' size = 14 align = 'center'
    ) TO rs_frame-texts.

    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_cx y = lv_h - 15
      text = |Depth { mv_depth }: { mv_total_tets } tetrahedra, { lines( mt_faces ) } faces|
      color = '#888' size = 10 align = 'center'
    ) TO rs_frame-texts.

    " Beat pulse - plasma flash
    IF is_ctx-gbi-pulse > '0.5'.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = '0.1'.
      rs_frame-flash-r = 1. rs_frame-flash-g = 0. rs_frame-flash-b = 1.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

