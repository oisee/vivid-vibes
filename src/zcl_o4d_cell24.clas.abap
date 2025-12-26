CLASS zcl_o4d_cell24 DEFINITION PUBLIC FINAL CREATE PUBLIC.
*======================================================================
* 24-CELL - Unique to 4D, self-dual polytope
*======================================================================
* 24 vertices, 96 edges, 96 triangular faces, 24 octahedral cells
* No 3D analog! Only exists in 4 dimensions
*======================================================================
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    CLASS-METHODS new IMPORTING iv_line_width TYPE f DEFAULT 2
                      RETURNING VALUE(ro_obj) TYPE REF TO zcl_o4d_cell24.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_v4, x TYPE f, y TYPE f, z TYPE f, w TYPE f, END OF ty_v4.
    TYPES: BEGIN OF ty_edge, a TYPE i, b TYPE i, END OF ty_edge.
    DATA mt_verts TYPE STANDARD TABLE OF ty_v4 WITH EMPTY KEY.
    DATA mt_edges TYPE STANDARD TABLE OF ty_edge WITH EMPTY KEY.
    DATA mv_init TYPE abap_bool.
    DATA mv_line_width TYPE f VALUE 2.
    METHODS init_geometry.
    METHODS rot4d IMPORTING is_v TYPE ty_v4 iv_xw TYPE f iv_yz TYPE f iv_zw TYPE f RETURNING VALUE(rs_v) TYPE ty_v4.
ENDCLASS.

CLASS zcl_o4d_cell24 IMPLEMENTATION.
  METHOD new. ro_obj = NEW #( ). ro_obj->mv_line_width = iv_line_width. ENDMETHOD.

  METHOD init_geometry.
    IF mv_init = abap_true. RETURN. ENDIF.
    mv_init = abap_true.
    " 24 vertices of 24-cell:
    " 8 vertices from 16-cell (±1, 0, 0, 0) permutations
    " 16 vertices from tesseract (±1, ±1, 0, 0) permutations / sqrt(2)
    " Type 1: 8 vertices like 16-cell
    APPEND VALUE ty_v4( x = 1 y = 0 z = 0 w = 0 ) TO mt_verts.
    APPEND VALUE ty_v4( x = -1 y = 0 z = 0 w = 0 ) TO mt_verts.
    APPEND VALUE ty_v4( x = 0 y = 1 z = 0 w = 0 ) TO mt_verts.
    APPEND VALUE ty_v4( x = 0 y = -1 z = 0 w = 0 ) TO mt_verts.
    APPEND VALUE ty_v4( x = 0 y = 0 z = 1 w = 0 ) TO mt_verts.
    APPEND VALUE ty_v4( x = 0 y = 0 z = -1 w = 0 ) TO mt_verts.
    APPEND VALUE ty_v4( x = 0 y = 0 z = 0 w = 1 ) TO mt_verts.
    APPEND VALUE ty_v4( x = 0 y = 0 z = 0 w = -1 ) TO mt_verts.
    " Type 2: 16 vertices (±0.5, ±0.5, ±0.5, ±0.5) - scaled for edge length 1
    DATA: lv_a TYPE f, lv_b TYPE f, lv_c TYPE f, lv_d TYPE f.
    DATA(lv_h) = CONV f( '0.5' ).
    DO 2 TIMES. lv_a = COND f( WHEN sy-index = 1 THEN lv_h ELSE 0 - lv_h ).
      DO 2 TIMES. lv_b = COND f( WHEN sy-index = 1 THEN lv_h ELSE 0 - lv_h ).
        DO 2 TIMES. lv_c = COND f( WHEN sy-index = 1 THEN lv_h ELSE 0 - lv_h ).
          DO 2 TIMES. lv_d = COND f( WHEN sy-index = 1 THEN lv_h ELSE 0 - lv_h ).
            APPEND VALUE ty_v4( x = lv_a y = lv_b z = lv_c w = lv_d ) TO mt_verts.
          ENDDO.
        ENDDO.
      ENDDO.
    ENDDO.
    " 96 edges: connect vertices at distance 1 (or scaled equivalent)
    DATA(lv_n) = lines( mt_verts ).
    DATA: lv_i TYPE i, lv_j TYPE i, lv_dist TYPE f.
    lv_i = 1.
    WHILE lv_i <= lv_n.
      lv_j = lv_i + 1.
      WHILE lv_j <= lv_n.
        DATA(ls_a) = mt_verts[ lv_i ]. DATA(ls_b) = mt_verts[ lv_j ].
        lv_dist = sqrt( ( ls_a-x - ls_b-x ) ** 2 + ( ls_a-y - ls_b-y ) ** 2 +
                        ( ls_a-z - ls_b-z ) ** 2 + ( ls_a-w - ls_b-w ) ** 2 ).
        IF lv_dist > '0.99' AND lv_dist < '1.01'.
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
    DATA(lv_c3) = cos( iv_zw ). DATA(lv_s3) = sin( iv_zw ).
    DATA(lv_nx) = is_v-x * lv_c1 - is_v-w * lv_s1.
    DATA(lv_nw) = is_v-x * lv_s1 + is_v-w * lv_c1.
    DATA(lv_ny) = is_v-y * lv_c2 - is_v-z * lv_s2.
    DATA(lv_nz) = is_v-y * lv_s2 + is_v-z * lv_c2.
    DATA(lv_nz2) = lv_nz * lv_c3 - lv_nw * lv_s3.
    DATA(lv_nw2) = lv_nz * lv_s3 + lv_nw * lv_c3.
    rs_v = VALUE #( x = lv_nx y = lv_ny z = lv_nz2 w = lv_nw2 ).
  ENDMETHOD.

  METHOD zif_o4d_effect~get_name. rv_name = 'cell24'. ENDMETHOD.
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
    DATA(lv_t) = is_ctx-gt. DATA(lv_size) = CONV f( 110 ).

    DATA(lv_xw) = lv_t * '0.35'. DATA(lv_yz) = lv_t * '0.45'. DATA(lv_zw) = lv_t * '0.25'.

    TYPES: BEGIN OF ty_proj, x TYPE f, y TYPE f, z TYPE f, w TYPE f, END OF ty_proj.
    DATA lt_proj TYPE STANDARD TABLE OF ty_proj WITH EMPTY KEY.

    LOOP AT mt_verts INTO DATA(ls_v).
      DATA(ls_r) = rot4d( is_v = ls_v iv_xw = lv_xw iv_yz = lv_yz iv_zw = lv_zw ).
      DATA(lv_s4) = '1.8' / ( '1.8' - ls_r-w ).
      DATA(lv_s3) = '1.8' / ( '1.8' - ls_r-z * lv_s4 ).
      APPEND VALUE ty_proj(
        x = lv_cx + ls_r-x * lv_s4 * lv_s3 * lv_size
        y = lv_cy + ls_r-y * lv_s4 * lv_s3 * lv_size
        z = ls_r-z w = ls_r-w
      ) TO lt_proj.
    ENDLOOP.

    " Draw edges with W-based color gradient
    LOOP AT mt_edges INTO DATA(ls_e).
      DATA(ls_pa) = lt_proj[ ls_e-a ]. DATA(ls_pb) = lt_proj[ ls_e-b ].
      DATA(lv_avg_w) = ( ls_pa-w + ls_pb-w ) / 2.
      DATA(lv_hue) = CONV i( ( lv_avg_w + 1 ) * 120 + 180 ) MOD 360.  " Cyan to magenta
      DATA(lv_light) = 40 + CONV i( ( lv_avg_w + 1 ) * 30 ).
      APPEND VALUE zif_o4d_effect=>ty_line(
        x1 = ls_pa-x y1 = ls_pa-y x2 = ls_pb-x y2 = ls_pb-y width = mv_line_width
        color = |hsl({ lv_hue }, 80%, { lv_light }%)|
      ) TO rs_frame-lines.
    ENDLOOP.

    APPEND VALUE zif_o4d_effect=>ty_text( x = lv_cx y = 20 text = '24-CELL' color = '#0ff' size = 16 align = 'center' ) TO rs_frame-texts.
    APPEND VALUE zif_o4d_effect=>ty_text( x = lv_cx y = lv_h - 15 text = '24 verts, 96 edges - UNIQUE TO 4D!' color = '#f80' size = 10 align = 'center' ) TO rs_frame-texts.

    IF is_ctx-gbi-pulse > '0.5'. rs_frame-flash-active = abap_true. rs_frame-flash-intensity = '0.12'.
      rs_frame-flash-r = 0. rs_frame-flash-g = 1. rs_frame-flash-b = '0.5'. ENDIF.
  ENDMETHOD.
ENDCLASS.
