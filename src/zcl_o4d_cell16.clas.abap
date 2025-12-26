CLASS zcl_o4d_cell16 DEFINITION PUBLIC FINAL CREATE PUBLIC.
*======================================================================
* 16-CELL (Hexadecachoron) - 4D cross-polytope
*======================================================================
* 8 vertices, 24 edges - dual of tesseract
* The 4D analog of the octahedron
*======================================================================
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    CLASS-METHODS new IMPORTING iv_line_width TYPE f DEFAULT 2
                      RETURNING VALUE(ro_obj) TYPE REF TO zcl_o4d_cell16.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_v4, x TYPE f, y TYPE f, z TYPE f, w TYPE f, END OF ty_v4.
    TYPES: BEGIN OF ty_edge, a TYPE i, b TYPE i, END OF ty_edge.
    DATA mt_verts TYPE STANDARD TABLE OF ty_v4 WITH EMPTY KEY.
    DATA mt_edges TYPE STANDARD TABLE OF ty_edge WITH EMPTY KEY.
    DATA mv_init TYPE abap_bool.
    DATA mv_line_width TYPE f VALUE 2.
    METHODS init_geometry.
    METHODS rot4d IMPORTING is_v TYPE ty_v4 iv_xw TYPE f iv_yz TYPE f RETURNING VALUE(rs_v) TYPE ty_v4.
ENDCLASS.

CLASS zcl_o4d_cell16 IMPLEMENTATION.
  METHOD new. ro_obj = NEW #( ). ro_obj->mv_line_width = iv_line_width. ENDMETHOD.

  METHOD init_geometry.
    IF mv_init = abap_true. RETURN. ENDIF.
    mv_init = abap_true.
    " 8 vertices: ±1 on each axis, others 0
    APPEND VALUE ty_v4( x = 1 y = 0 z = 0 w = 0 ) TO mt_verts.
    APPEND VALUE ty_v4( x = -1 y = 0 z = 0 w = 0 ) TO mt_verts.
    APPEND VALUE ty_v4( x = 0 y = 1 z = 0 w = 0 ) TO mt_verts.
    APPEND VALUE ty_v4( x = 0 y = -1 z = 0 w = 0 ) TO mt_verts.
    APPEND VALUE ty_v4( x = 0 y = 0 z = 1 w = 0 ) TO mt_verts.
    APPEND VALUE ty_v4( x = 0 y = 0 z = -1 w = 0 ) TO mt_verts.
    APPEND VALUE ty_v4( x = 0 y = 0 z = 0 w = 1 ) TO mt_verts.
    APPEND VALUE ty_v4( x = 0 y = 0 z = 0 w = -1 ) TO mt_verts.
    " 24 edges: connect non-opposite vertices (distance sqrt(2))
    DATA: lv_i TYPE i, lv_j TYPE i.
    lv_i = 1.
    WHILE lv_i <= 8.
      lv_j = lv_i + 1.
      WHILE lv_j <= 8.
        DATA(ls_a) = mt_verts[ lv_i ]. DATA(ls_b) = mt_verts[ lv_j ].
        " Skip opposite vertices (sum = 0 for all coords means opposite)
        DATA(lv_sum) = ls_a-x + ls_b-x + ls_a-y + ls_b-y + ls_a-z + ls_b-z + ls_a-w + ls_b-w.
        IF lv_sum <> 0 OR ( ls_a-x = ls_b-x AND ls_a-y = ls_b-y AND ls_a-z = ls_b-z AND ls_a-w = ls_b-w ).
          " Not opposite, connect
          APPEND VALUE ty_edge( a = lv_i b = lv_j ) TO mt_edges.
        ENDIF.
        lv_j = lv_j + 1.
      ENDWHILE.
      lv_i = lv_i + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD rot4d.
    DATA(lv_c1) = cos( iv_xw ). DATA(lv_s1) = sin( iv_xw ).
    DATA(lv_c2) = cos( iv_yz ). DATA(lv_s2) = sin( iv_yz ).
    DATA(lv_nx) = is_v-x * lv_c1 - is_v-w * lv_s1.
    DATA(lv_nw) = is_v-x * lv_s1 + is_v-w * lv_c1.
    DATA(lv_ny) = is_v-y * lv_c2 - is_v-z * lv_s2.
    DATA(lv_nz) = is_v-y * lv_s2 + is_v-z * lv_c2.
    rs_v = VALUE #( x = lv_nx y = lv_ny z = lv_nz w = lv_nw ).
  ENDMETHOD.

  METHOD zif_o4d_effect~get_name. rv_name = 'cell16'. ENDMETHOD.
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
    DATA(lv_t) = is_ctx-gt. DATA(lv_size) = CONV f( 100 ).

    DATA(lv_xw) = lv_t * '0.6'. DATA(lv_yz) = lv_t * '0.4'.

    TYPES: BEGIN OF ty_proj, x TYPE f, y TYPE f, z TYPE f, w TYPE f, END OF ty_proj.
    DATA lt_proj TYPE STANDARD TABLE OF ty_proj WITH EMPTY KEY.

    LOOP AT mt_verts INTO DATA(ls_v).
      DATA(ls_r) = rot4d( is_v = ls_v iv_xw = lv_xw iv_yz = lv_yz ).
      DATA(lv_s4) = 2 / ( 2 - ls_r-w ).
      DATA(lv_s3) = 2 / ( 2 - ls_r-z * lv_s4 ).
      APPEND VALUE ty_proj(
        x = lv_cx + ls_r-x * lv_s4 * lv_s3 * lv_size
        y = lv_cy + ls_r-y * lv_s4 * lv_s3 * lv_size
        z = ls_r-z w = ls_r-w
      ) TO lt_proj.
    ENDLOOP.

    " Draw edges - color by W depth
    LOOP AT mt_edges INTO DATA(ls_e).
      DATA(ls_pa) = lt_proj[ ls_e-a ]. DATA(ls_pb) = lt_proj[ ls_e-b ].
      DATA(lv_avg_w) = ( ls_pa-w + ls_pb-w ) / 2.
      DATA(lv_hue) = CONV i( ( lv_avg_w + 1 ) * 180 ).
      DATA(lv_light) = 45 + CONV i( ( lv_avg_w + 1 ) * 25 ).
      APPEND VALUE zif_o4d_effect=>ty_line(
        x1 = ls_pa-x y1 = ls_pa-y x2 = ls_pb-x y2 = ls_pb-y width = mv_line_width
        color = |hsl({ lv_hue }, 100%, { lv_light }%)|
      ) TO rs_frame-lines.
    ENDLOOP.

    " Vertices
    LOOP AT lt_proj INTO DATA(ls_p).
      DATA(lv_br) = 180 + CONV i( ls_p-w * 60 ).
      APPEND VALUE zif_o4d_effect=>ty_rect(
        x = ls_p-x - 4 y = ls_p-y - 4 w = 8 h = 8
        fill = |rgb({ lv_br },{ lv_br },{ lv_br })|
      ) TO rs_frame-rects.
    ENDLOOP.

    APPEND VALUE zif_o4d_effect=>ty_text( x = lv_cx y = 20 text = '16-CELL (Hexadecachoron)' color = '#f0f' size = 14 align = 'center' ) TO rs_frame-texts.
    APPEND VALUE zif_o4d_effect=>ty_text( x = lv_cx y = lv_h - 15 text = '8 vertices, 24 edges - 4D octahedron' color = '#888' size = 10 align = 'center' ) TO rs_frame-texts.

    IF is_ctx-gbi-pulse > '0.5'. rs_frame-flash-active = abap_true. rs_frame-flash-intensity = '0.1'.
      rs_frame-flash-r = 1. rs_frame-flash-g = 0. rs_frame-flash-b = 1. ENDIF.
  ENDMETHOD.
ENDCLASS.
