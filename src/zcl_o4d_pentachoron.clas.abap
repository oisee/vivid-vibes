CLASS zcl_o4d_pentachoron DEFINITION PUBLIC FINAL CREATE PUBLIC.
*======================================================================
* PENTACHORON (5-cell) - Simplest 4D polytope
*======================================================================
* 5 vertices, 10 edges - the 4D analog of tetrahedron
* Every vertex connects to every other vertex
*======================================================================
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    CLASS-METHODS new IMPORTING iv_line_width TYPE f DEFAULT 2
                      RETURNING VALUE(ro_obj) TYPE REF TO zcl_o4d_pentachoron.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_v4, x TYPE f, y TYPE f, z TYPE f, w TYPE f, END OF ty_v4.
    TYPES: BEGIN OF ty_edge, a TYPE i, b TYPE i, END OF ty_edge.
    DATA mt_verts TYPE STANDARD TABLE OF ty_v4 WITH EMPTY KEY.
    DATA mt_edges TYPE STANDARD TABLE OF ty_edge WITH EMPTY KEY.
    DATA mv_init TYPE abap_bool.
    DATA mv_line_width TYPE f VALUE 2.
    METHODS init_geometry.
    METHODS rot4d IMPORTING is_v TYPE ty_v4 iv_xw TYPE f iv_yz TYPE f iv_xy TYPE f RETURNING VALUE(rs_v) TYPE ty_v4.
ENDCLASS.

CLASS zcl_o4d_pentachoron IMPLEMENTATION.
  METHOD new. ro_obj = NEW #( ). ro_obj->mv_line_width = iv_line_width. ENDMETHOD.

  METHOD init_geometry.
    IF mv_init = abap_true. RETURN. ENDIF.
    mv_init = abap_true.
    " 5 vertices of a regular 4-simplex (pentachoron)
    " Coordinates chosen so all edges have equal length
    DATA(lv_a) = CONV f( '0.6324555' ).  " 1/sqrt(2.5)
    DATA(lv_b) = CONV f( '0.5' ).
    DATA(lv_c) = CONV f( '0.2886751' ).  " 1/sqrt(12)
    DATA(lv_d) = CONV f( '0.2041241' ).  " 1/sqrt(24)
    " Vertex positions for regular 4-simplex
    APPEND VALUE ty_v4( x = 1 y = 0 z = 0 - lv_c w = 0 - lv_d ) TO mt_verts.
    APPEND VALUE ty_v4( x = -1 y = 0 z = 0 - lv_c w = 0 - lv_d ) TO mt_verts.
    APPEND VALUE ty_v4( x = 0 y = 1 z = lv_c * 2 w = 0 - lv_d ) TO mt_verts.
    APPEND VALUE ty_v4( x = 0 y = -1 z = lv_c * 2 w = 0 - lv_d ) TO mt_verts.
    APPEND VALUE ty_v4( x = 0 y = 0 z = 0 w = lv_d * 4 ) TO mt_verts.
    " 10 edges: every vertex connects to every other (complete graph K5)
    DATA: lv_i TYPE i, lv_j TYPE i.
    lv_i = 1.
    WHILE lv_i <= 5.
      lv_j = lv_i + 1.
      WHILE lv_j <= 5.
        APPEND VALUE ty_edge( a = lv_i b = lv_j ) TO mt_edges.
        lv_j = lv_j + 1.
      ENDWHILE.
      lv_i = lv_i + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD rot4d.
    DATA(lv_c1) = cos( iv_xw ). DATA(lv_s1) = sin( iv_xw ).
    DATA(lv_c2) = cos( iv_yz ). DATA(lv_s2) = sin( iv_yz ).
    DATA(lv_c3) = cos( iv_xy ). DATA(lv_s3) = sin( iv_xy ).
    " XW rotation
    DATA(lv_nx) = is_v-x * lv_c1 - is_v-w * lv_s1.
    DATA(lv_nw) = is_v-x * lv_s1 + is_v-w * lv_c1.
    " YZ rotation
    DATA(lv_ny) = is_v-y * lv_c2 - is_v-z * lv_s2.
    DATA(lv_nz) = is_v-y * lv_s2 + is_v-z * lv_c2.
    " XY rotation
    DATA(lv_nx2) = lv_nx * lv_c3 - lv_ny * lv_s3.
    DATA(lv_ny2) = lv_nx * lv_s3 + lv_ny * lv_c3.
    rs_v = VALUE #( x = lv_nx2 y = lv_ny2 z = lv_nz w = lv_nw ).
  ENDMETHOD.

  METHOD zif_o4d_effect~get_name. rv_name = 'pentachoron'. ENDMETHOD.
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
    DATA(lv_t) = is_ctx-gt. DATA(lv_size) = CONV f( 120 ).

    DATA(lv_xw) = lv_t * '0.5'. DATA(lv_yz) = lv_t * '0.7'. DATA(lv_xy) = lv_t * '0.3'.

    TYPES: BEGIN OF ty_proj, x TYPE f, y TYPE f, z TYPE f, w TYPE f, END OF ty_proj.
    DATA lt_proj TYPE STANDARD TABLE OF ty_proj WITH EMPTY KEY.

    LOOP AT mt_verts INTO DATA(ls_v).
      DATA(ls_r) = rot4d( is_v = ls_v iv_xw = lv_xw iv_yz = lv_yz iv_xy = lv_xy ).
      DATA(lv_s4) = 2 / ( 2 - ls_r-w ).
      DATA(lv_s3) = 2 / ( 2 - ls_r-z * lv_s4 ).
      DATA(lv_sx) = lv_cx + ls_r-x * lv_s4 * lv_s3 * lv_size.
      DATA(lv_sy) = lv_cy + ls_r-y * lv_s4 * lv_s3 * lv_size.
      APPEND VALUE ty_proj( x = lv_sx y = lv_sy z = ls_r-z w = ls_r-w ) TO lt_proj.
    ENDLOOP.

    " Draw edges - rainbow colors based on edge index
    DATA lv_idx TYPE i. lv_idx = 0.
    LOOP AT mt_edges INTO DATA(ls_e).
      DATA(ls_pa) = lt_proj[ ls_e-a ]. DATA(ls_pb) = lt_proj[ ls_e-b ].
      DATA(lv_hue) = lv_idx * 36.  " 10 edges, 360/10 = 36
      DATA(lv_avg_w) = ( ls_pa-w + ls_pb-w ) / 2.
      DATA(lv_light) = 50 + CONV i( lv_avg_w * 25 ).
      APPEND VALUE zif_o4d_effect=>ty_line(
        x1 = ls_pa-x y1 = ls_pa-y x2 = ls_pb-x y2 = ls_pb-y width = mv_line_width
        color = |hsl({ lv_hue }, 100%, { lv_light }%)|
      ) TO rs_frame-lines.
      lv_idx = lv_idx + 1.
    ENDLOOP.

    " Draw vertices as bright dots
    DATA lv_vi TYPE i. lv_vi = 0.
    LOOP AT lt_proj INTO DATA(ls_p).
      DATA(lv_vh) = lv_vi * 72.  " 5 vertices
      APPEND VALUE zif_o4d_effect=>ty_rect(
        x = ls_p-x - 5 y = ls_p-y - 5 w = 10 h = 10 fill = |hsl({ lv_vh }, 100%, 70%)|
      ) TO rs_frame-rects.
      lv_vi = lv_vi + 1.
    ENDLOOP.

    APPEND VALUE zif_o4d_effect=>ty_text( x = lv_cx y = 20 text = 'PENTACHORON (5-cell)' color = '#0ff' size = 16 align = 'center' ) TO rs_frame-texts.
    APPEND VALUE zif_o4d_effect=>ty_text( x = lv_cx y = lv_h - 15 text = '5 vertices, 10 edges - simplest 4D' color = '#888' size = 10 align = 'center' ) TO rs_frame-texts.

    IF is_ctx-gbi-pulse > '0.5'. rs_frame-flash-active = abap_true. rs_frame-flash-intensity = '0.15'.
      rs_frame-flash-r = 0. rs_frame-flash-g = 1. rs_frame-flash-b = 1. ENDIF.
  ENDMETHOD.
ENDCLASS.
