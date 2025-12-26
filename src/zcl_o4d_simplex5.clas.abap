CLASS zcl_o4d_simplex5 DEFINITION PUBLIC FINAL CREATE PUBLIC.
*======================================================================
* 5-SIMPLEX (HEXATERON) - Simplest 5D polytope
*======================================================================
* 6 vertices, 15 edges (complete graph K6)
* 5D analog of tetrahedron: every vertex connects to every other
* Projection chain: 5D → 4D → 3D → 2D
*======================================================================
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    CLASS-METHODS new IMPORTING iv_line_width TYPE f DEFAULT 2
                      RETURNING VALUE(ro_obj) TYPE REF TO zcl_o4d_simplex5.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_v5, x TYPE f, y TYPE f, z TYPE f, w TYPE f, v TYPE f, END OF ty_v5.
    TYPES: BEGIN OF ty_edge, a TYPE i, b TYPE i, END OF ty_edge.
    DATA mt_verts TYPE STANDARD TABLE OF ty_v5 WITH EMPTY KEY.
    DATA mt_edges TYPE STANDARD TABLE OF ty_edge WITH EMPTY KEY.
    DATA mv_init TYPE abap_bool.
    DATA mv_line_width TYPE f VALUE 2.
    METHODS init_geometry.
    METHODS rot5d IMPORTING is_p TYPE ty_v5 iv_a1 TYPE f iv_a2 TYPE f iv_a3 TYPE f RETURNING VALUE(rs_p) TYPE ty_v5.
ENDCLASS.

CLASS zcl_o4d_simplex5 IMPLEMENTATION.
  METHOD new. ro_obj = NEW #( ). ro_obj->mv_line_width = iv_line_width. ENDMETHOD.

  METHOD init_geometry.
    IF mv_init = abap_true. RETURN. ENDIF.
    mv_init = abap_true.
    " 6 vertices of regular 5-simplex centered at origin
    " Using orthogonal projection from standard simplex in R^6
    DATA(lv_s) = CONV f( '0.4472' ).  " 1/sqrt(5)
    DATA(lv_t) = CONV f( '0.6325' ).  " sqrt(2/5)
    DATA(lv_u) = CONV f( '0.7746' ).  " sqrt(3/5)
    DATA(lv_r) = CONV f( '0.8944' ).  " sqrt(4/5)
    " Vertex 1: apex in V direction
    APPEND VALUE ty_v5( x = 0 y = 0 z = 0 w = 0 v = 1 ) TO mt_verts.
    " Vertex 2: apex in W direction
    APPEND VALUE ty_v5( x = 0 y = 0 z = 0 w = lv_r v = 0 - lv_s ) TO mt_verts.
    " Vertex 3: apex in Z direction
    APPEND VALUE ty_v5( x = 0 y = 0 z = lv_u w = 0 - lv_t v = 0 - lv_s ) TO mt_verts.
    " Vertex 4: apex in Y direction
    APPEND VALUE ty_v5( x = 0 y = lv_t z = 0 - lv_s w = 0 - lv_t v = 0 - lv_s ) TO mt_verts.
    " Vertex 5: positive X
    APPEND VALUE ty_v5( x = lv_s y = 0 - lv_s z = 0 - lv_s w = 0 - lv_t v = 0 - lv_s ) TO mt_verts.
    " Vertex 6: negative X (completing the simplex)
    APPEND VALUE ty_v5( x = 0 - lv_s y = 0 - lv_s z = 0 - lv_s w = 0 - lv_t v = 0 - lv_s ) TO mt_verts.
    " 15 edges: complete graph - every vertex connects to every other
    DATA: lv_i TYPE i, lv_j TYPE i.
    lv_i = 1.
    WHILE lv_i <= 6.
      lv_j = lv_i + 1.
      WHILE lv_j <= 6.
        APPEND VALUE ty_edge( a = lv_i b = lv_j ) TO mt_edges.
        lv_j = lv_j + 1.
      ENDWHILE.
      lv_i = lv_i + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD rot5d.
    " Rotate in XY, ZW, and XV planes
    DATA(lv_c1) = cos( iv_a1 ). DATA(lv_s1) = sin( iv_a1 ).
    DATA(lv_c2) = cos( iv_a2 ). DATA(lv_s2) = sin( iv_a2 ).
    DATA(lv_c3) = cos( iv_a3 ). DATA(lv_s3) = sin( iv_a3 ).
    " XY rotation
    DATA(lv_nx) = is_p-x * lv_c1 - is_p-y * lv_s1.
    DATA(lv_ny) = is_p-x * lv_s1 + is_p-y * lv_c1.
    " ZW rotation
    DATA(lv_nz) = is_p-z * lv_c2 - is_p-w * lv_s2.
    DATA(lv_nw) = is_p-z * lv_s2 + is_p-w * lv_c2.
    " XV rotation
    DATA(lv_nx2) = lv_nx * lv_c3 - is_p-v * lv_s3.
    DATA(lv_nv) = lv_nx * lv_s3 + is_p-v * lv_c3.
    rs_p = VALUE #( x = lv_nx2 y = lv_ny z = lv_nz w = lv_nw v = lv_nv ).
  ENDMETHOD.

  METHOD zif_o4d_effect~get_name. rv_name = 'simplex5'. ENDMETHOD.
  METHOD zif_o4d_effect~get_params. ENDMETHOD.
  METHOD zif_o4d_effect~set_param. ENDMETHOD.
  METHOD zif_o4d_effect~render.
    rs_frame = zif_o4d_effect~render_frame( VALUE #( t = is_sync-time gt = is_sync-time
      gbi = VALUE #( bar = is_sync-bar bar_phase = is_sync-bar_phase pulse = is_sync-intensity ) ) ).
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    init_geometry( ).
    rs_frame-state-line_width = mv_line_width.
    rs_frame-state-line_cap = 'round'.
    DATA(lv_w) = CONV f( zif_o4d_effect=>c_width ). DATA(lv_h) = CONV f( zif_o4d_effect=>c_height ).
    DATA(lv_cx) = lv_w / 2. DATA(lv_cy) = lv_h / 2.
    DATA(lv_t) = is_ctx-gt. DATA(lv_size) = CONV f( 140 ).

    " 5D rotation angles - different speeds for interesting motion
    DATA(lv_a1) = lv_t * '0.6'. DATA(lv_a2) = lv_t * '0.4'. DATA(lv_a3) = lv_t * '0.5'.

    " Project all vertices: 5D → 4D → 3D → 2D
    TYPES: BEGIN OF ty_proj, x TYPE f, y TYPE f, z TYPE f, v TYPE f, END OF ty_proj.
    DATA lt_proj TYPE STANDARD TABLE OF ty_proj WITH EMPTY KEY.

    LOOP AT mt_verts INTO DATA(ls_v).
      DATA(ls_r) = rot5d( is_p = ls_v iv_a1 = lv_a1 iv_a2 = lv_a2 iv_a3 = lv_a3 ).
      " 5D → 4D stereographic
      DATA(lv_s5) = '2.5' / ( '2.5' - ls_r-v ).
      DATA(lv_x4) = ls_r-x * lv_s5. DATA(lv_y4) = ls_r-y * lv_s5.
      DATA(lv_z4) = ls_r-z * lv_s5. DATA(lv_w4) = ls_r-w * lv_s5.
      " 4D → 3D stereographic
      DATA(lv_s4) = '2.5' / ( '2.5' - lv_w4 ).
      DATA(lv_x3) = lv_x4 * lv_s4. DATA(lv_y3) = lv_y4 * lv_s4. DATA(lv_z3) = lv_z4 * lv_s4.
      " 3D → 2D perspective
      DATA(lv_s3) = '2.5' / ( '2.5' - lv_z3 ).
      APPEND VALUE ty_proj(
        x = lv_cx + lv_x3 * lv_size * lv_s3
        y = lv_cy + lv_y3 * lv_size * lv_s3
        z = lv_z3 v = ls_r-v
      ) TO lt_proj.
    ENDLOOP.

    " Draw edges with rainbow colors based on vertex index
    DATA(lv_idx) = 0.
    LOOP AT mt_edges INTO DATA(ls_e).
      DATA(ls_pa) = lt_proj[ ls_e-a ]. DATA(ls_pb) = lt_proj[ ls_e-b ].
      DATA(lv_hue) = ( lv_idx * 24 ) MOD 360.  " 15 edges, spread across hue
      DATA(lv_light) = 50 + CONV i( ( ls_pa-z + ls_pb-z + 2 ) * 10 ).
      APPEND VALUE zif_o4d_effect=>ty_line(
        x1 = ls_pa-x y1 = ls_pa-y x2 = ls_pb-x y2 = ls_pb-y width = mv_line_width
        color = |hsl({ lv_hue }, 100%, { lv_light }%)|
      ) TO rs_frame-lines.
      lv_idx = lv_idx + 1.
    ENDLOOP.

    " Draw vertices as small circles
    DATA(lv_vi) = 0.
    LOOP AT lt_proj INTO DATA(ls_p).
      DATA(lv_vhue) = lv_vi * 60.
      APPEND VALUE zif_o4d_effect=>ty_rect(
        x = ls_p-x - 4 y = ls_p-y - 4 w = 8 h = 8
        fill = |hsl({ lv_vhue }, 100%, 70%)|
      ) TO rs_frame-rects.
      lv_vi = lv_vi + 1.
    ENDLOOP.

    APPEND VALUE zif_o4d_effect=>ty_text( x = lv_cx y = 20 text = '5-SIMPLEX (HEXATERON)' color = '#ff0' size = 14 align = 'center' ) TO rs_frame-texts.
    APPEND VALUE zif_o4d_effect=>ty_text( x = lv_cx y = lv_h - 15 text = '6 vertices, 15 edges - Complete K6' color = '#888' size = 10 align = 'center' ) TO rs_frame-texts.

    IF is_ctx-gbi-pulse > '0.5'. rs_frame-flash-active = abap_true. rs_frame-flash-intensity = '0.1'.
      rs_frame-flash-r = 1. rs_frame-flash-g = '0.5'. rs_frame-flash-b = 0. ENDIF.
  ENDMETHOD.
ENDCLASS.
