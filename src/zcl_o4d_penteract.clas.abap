CLASS zcl_o4d_penteract DEFINITION PUBLIC FINAL CREATE PUBLIC.
*======================================================================
* PENTERACT - 5D Hypercube
*======================================================================
* 32 vertices, 80 edges rotating in 5D space
* Projection chain: 5D → 4D → 3D → 2D
* 10 rotation planes: XY, XZ, XW, XV, YZ, YW, YV, ZW, ZV, WV
*======================================================================
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    CLASS-METHODS new IMPORTING iv_line_width TYPE f DEFAULT 2
                      RETURNING VALUE(ro_obj) TYPE REF TO zcl_o4d_penteract.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_v5, x TYPE f, y TYPE f, z TYPE f, w TYPE f, v TYPE f, END OF ty_v5.
    TYPES: BEGIN OF ty_edge, a TYPE i, b TYPE i, END OF ty_edge.
    DATA mt_verts TYPE STANDARD TABLE OF ty_v5 WITH EMPTY KEY.
    DATA mt_edges TYPE STANDARD TABLE OF ty_edge WITH EMPTY KEY.
    DATA mv_init TYPE abap_bool.
    DATA mv_line_width TYPE f VALUE 2.
    METHODS init_geometry.
    METHODS rot5d IMPORTING is_p TYPE ty_v5 iv_xv TYPE f iv_yw TYPE f iv_zw TYPE f RETURNING VALUE(rs_p) TYPE ty_v5.
    METHODS proj5to4 IMPORTING is_p TYPE ty_v5 iv_d TYPE f RETURNING VALUE(rs_p) TYPE ty_v5.
    METHODS proj4to3 IMPORTING is_p TYPE ty_v5 iv_d TYPE f RETURNING VALUE(rs_p) TYPE ty_v5.
ENDCLASS.

CLASS zcl_o4d_penteract IMPLEMENTATION.
  METHOD new. ro_obj = NEW #( ). ro_obj->mv_line_width = iv_line_width. ENDMETHOD.

  METHOD init_geometry.
    IF mv_init = abap_true. RETURN. ENDIF.
    mv_init = abap_true.
    " 32 vertices: all combinations of ±1 in 5D
    DATA: lv_x TYPE f, lv_y TYPE f, lv_z TYPE f, lv_w TYPE f, lv_v TYPE f.
    DO 2 TIMES. lv_x = COND f( WHEN sy-index = 1 THEN -1 ELSE 1 ).
      DO 2 TIMES. lv_y = COND f( WHEN sy-index = 1 THEN -1 ELSE 1 ).
        DO 2 TIMES. lv_z = COND f( WHEN sy-index = 1 THEN -1 ELSE 1 ).
          DO 2 TIMES. lv_w = COND f( WHEN sy-index = 1 THEN -1 ELSE 1 ).
            DO 2 TIMES. lv_v = COND f( WHEN sy-index = 1 THEN -1 ELSE 1 ).
              APPEND VALUE ty_v5( x = lv_x y = lv_y z = lv_z w = lv_w v = lv_v ) TO mt_verts.
            ENDDO.
          ENDDO.
        ENDDO.
      ENDDO.
    ENDDO.
    " 80 edges: connect vertices differing in exactly one coordinate
    DATA(lv_n) = lines( mt_verts ).
    DATA: lv_i TYPE i, lv_j TYPE i, lv_diff TYPE i.
    lv_i = 1.
    WHILE lv_i <= lv_n.
      lv_j = lv_i + 1.
      WHILE lv_j <= lv_n.
        DATA(ls_a) = mt_verts[ lv_i ]. DATA(ls_b) = mt_verts[ lv_j ].
        lv_diff = 0.
        IF ls_a-x <> ls_b-x. lv_diff = lv_diff + 1. ENDIF.
        IF ls_a-y <> ls_b-y. lv_diff = lv_diff + 1. ENDIF.
        IF ls_a-z <> ls_b-z. lv_diff = lv_diff + 1. ENDIF.
        IF ls_a-w <> ls_b-w. lv_diff = lv_diff + 1. ENDIF.
        IF ls_a-v <> ls_b-v. lv_diff = lv_diff + 1. ENDIF.
        IF lv_diff = 1. APPEND VALUE ty_edge( a = lv_i b = lv_j ) TO mt_edges. ENDIF.
        lv_j = lv_j + 1.
      ENDWHILE.
      lv_i = lv_i + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD rot5d.
    " Rotate in XV, YW, and ZW planes
    DATA(lv_c1) = cos( iv_xv ). DATA(lv_s1) = sin( iv_xv ).
    DATA(lv_c2) = cos( iv_yw ). DATA(lv_s2) = sin( iv_yw ).
    DATA(lv_c3) = cos( iv_zw ). DATA(lv_s3) = sin( iv_zw ).
    " XV rotation
    DATA(lv_nx) = is_p-x * lv_c1 - is_p-v * lv_s1.
    DATA(lv_nv) = is_p-x * lv_s1 + is_p-v * lv_c1.
    " YW rotation
    DATA(lv_ny) = is_p-y * lv_c2 - is_p-w * lv_s2.
    DATA(lv_nw) = is_p-y * lv_s2 + is_p-w * lv_c2.
    " ZW rotation
    DATA(lv_nz) = is_p-z * lv_c3 - lv_nw * lv_s3.
    DATA(lv_nw2) = is_p-z * lv_s3 + lv_nw * lv_c3.
    rs_p = VALUE #( x = lv_nx y = lv_ny z = lv_nz w = lv_nw2 v = lv_nv ).
  ENDMETHOD.

  METHOD proj5to4.
    " Stereographic: 5D → 4D
    DATA(lv_scale) = iv_d / ( iv_d - is_p-v ).
    rs_p-x = is_p-x * lv_scale. rs_p-y = is_p-y * lv_scale.
    rs_p-z = is_p-z * lv_scale. rs_p-w = is_p-w * lv_scale. rs_p-v = is_p-v.
  ENDMETHOD.

  METHOD proj4to3.
    " Stereographic: 4D → 3D
    DATA(lv_scale) = iv_d / ( iv_d - is_p-w ).
    rs_p-x = is_p-x * lv_scale. rs_p-y = is_p-y * lv_scale.
    rs_p-z = is_p-z * lv_scale. rs_p-w = is_p-w. rs_p-v = is_p-v.
  ENDMETHOD.

  METHOD zif_o4d_effect~get_name. rv_name = 'penteract'. ENDMETHOD.
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
    DATA(lv_t) = is_ctx-gt.
    DATA(lv_size) = CONV f( 80 ).

    " 5D rotation angles
    DATA(lv_xv) = lv_t * '0.4'. DATA(lv_yw) = lv_t * '0.3'. DATA(lv_zw) = lv_t * '0.5'.

    " Project all vertices: 5D → 4D → 3D → 2D
    TYPES: BEGIN OF ty_proj, x TYPE f, y TYPE f, z TYPE f, v TYPE f, END OF ty_proj.
    DATA lt_proj TYPE STANDARD TABLE OF ty_proj WITH EMPTY KEY.
    DATA: lv_sx TYPE f, lv_sy TYPE f.

    LOOP AT mt_verts INTO DATA(ls_v).
      DATA(ls_r) = rot5d( is_p = ls_v iv_xv = lv_xv iv_yw = lv_yw iv_zw = lv_zw ).
      DATA(ls_4d) = proj5to4( is_p = ls_r iv_d = 3 ).
      DATA(ls_3d) = proj4to3( is_p = ls_4d iv_d = 3 ).
      DATA(lv_scale) = 3 / ( 3 - ls_3d-z ).
      lv_sx = lv_cx + ls_3d-x * lv_size * lv_scale.
      lv_sy = lv_cy + ls_3d-y * lv_size * lv_scale.
      APPEND VALUE ty_proj( x = lv_sx y = lv_sy z = ls_3d-z v = ls_r-v ) TO lt_proj.
    ENDLOOP.

    " Draw edges with V-dimension coloring
    LOOP AT mt_edges INTO DATA(ls_e).
      DATA(ls_pa) = lt_proj[ ls_e-a ]. DATA(ls_pb) = lt_proj[ ls_e-b ].
      DATA(lv_avg_v) = ( ls_pa-v + ls_pb-v ) / 2.
      DATA(lv_hue) = CONV i( ( lv_avg_v + 1 ) * 150 ) MOD 360.
      DATA(lv_light) = 40 + CONV i( ( ls_pa-z + ls_pb-z + 2 ) * 15 ).
      APPEND VALUE zif_o4d_effect=>ty_line(
        x1 = ls_pa-x y1 = ls_pa-y x2 = ls_pb-x y2 = ls_pb-y width = mv_line_width
        color = |hsl({ lv_hue }, 90%, { lv_light }%)|
      ) TO rs_frame-lines.
    ENDLOOP.

    APPEND VALUE zif_o4d_effect=>ty_text( x = lv_cx y = 20 text = 'PENTERACT (5D)' color = '#ff0' size = 14 align = 'center' ) TO rs_frame-texts.
    APPEND VALUE zif_o4d_effect=>ty_text( x = lv_cx y = lv_h - 15 text = '32 vertices, 80 edges' color = '#888' size = 10 align = 'center' ) TO rs_frame-texts.

    IF is_ctx-gbi-pulse > '0.5'. rs_frame-flash-active = abap_true. rs_frame-flash-intensity = '0.08'.
      rs_frame-flash-r = 1. rs_frame-flash-g = 1. rs_frame-flash-b = 0. ENDIF.
  ENDMETHOD.
ENDCLASS.
