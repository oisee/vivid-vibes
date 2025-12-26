CLASS zcl_o4d_cell120 DEFINITION PUBLIC FINAL CREATE PUBLIC.
*======================================================================
* 120-CELL (HECATONICOSACHORON) - 4D Dodecahedron Analog
*======================================================================
* 600 vertices, 1200 edges, 720 pentagonal faces, 120 dodecahedral cells
* Uses golden ratio φ = (1+√5)/2 for vertex coordinates
* The most complex regular 4D polytope!
*======================================================================
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    CLASS-METHODS new IMPORTING iv_line_width TYPE f DEFAULT 2
                      RETURNING VALUE(ro_obj) TYPE REF TO zcl_o4d_cell120.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_v4, x TYPE f, y TYPE f, z TYPE f, w TYPE f, END OF ty_v4.
    TYPES: BEGIN OF ty_edge, a TYPE i, b TYPE i, END OF ty_edge.
    DATA mt_verts TYPE STANDARD TABLE OF ty_v4 WITH EMPTY KEY.
    DATA mt_edges TYPE STANDARD TABLE OF ty_edge WITH EMPTY KEY.
    DATA mv_init TYPE abap_bool.
    DATA mv_line_width TYPE f VALUE 2.
    DATA mv_phi TYPE f.    " Golden ratio
    DATA mv_phi2 TYPE f.   " φ²
    DATA mv_iphi TYPE f.   " 1/φ
    METHODS init_geometry.
    METHODS add_vert IMPORTING iv_x TYPE f iv_y TYPE f iv_z TYPE f iv_w TYPE f.
    METHODS add_perms IMPORTING iv_a TYPE f iv_b TYPE f iv_c TYPE f iv_d TYPE f iv_even TYPE abap_bool DEFAULT abap_false.
    METHODS rot4d IMPORTING is_v TYPE ty_v4 iv_xw TYPE f iv_yz TYPE f RETURNING VALUE(rs_v) TYPE ty_v4.
ENDCLASS.

CLASS zcl_o4d_cell120 IMPLEMENTATION.
  METHOD new. ro_obj = NEW #( ). ro_obj->mv_line_width = iv_line_width. ENDMETHOD.

  METHOD add_vert.
    APPEND VALUE ty_v4( x = iv_x y = iv_y z = iv_z w = iv_w ) TO mt_verts.
  ENDMETHOD.

  METHOD add_perms.
    " Add all permutations with sign changes
    DATA: lt_vals TYPE STANDARD TABLE OF f WITH EMPTY KEY,
          lv_a TYPE f, lv_b TYPE f, lv_c TYPE f, lv_d TYPE f.
    APPEND iv_a TO lt_vals. APPEND iv_b TO lt_vals. APPEND iv_c TO lt_vals. APPEND iv_d TO lt_vals.
    " For 120-cell we use all 8 sign combinations on distinct coordinates
    DATA: lv_s1 TYPE i, lv_s2 TYPE i, lv_s3 TYPE i, lv_s4 TYPE i.
    lv_s1 = 1.
    WHILE lv_s1 >= -1.
      lv_s2 = 1.
      WHILE lv_s2 >= -1.
        lv_s3 = 1.
        WHILE lv_s3 >= -1.
          lv_s4 = 1.
          WHILE lv_s4 >= -1.
            add_vert( iv_x = iv_a * lv_s1 iv_y = iv_b * lv_s2 iv_z = iv_c * lv_s3 iv_w = iv_d * lv_s4 ).
            lv_s4 = lv_s4 - 2.
          ENDWHILE.
          lv_s3 = lv_s3 - 2.
        ENDWHILE.
        lv_s2 = lv_s2 - 2.
      ENDWHILE.
      lv_s1 = lv_s1 - 2.
    ENDWHILE.
  ENDMETHOD.

  METHOD init_geometry.
    IF mv_init = abap_true. RETURN. ENDIF.
    mv_init = abap_true.
    " Golden ratio constants
    mv_phi = ( 1 + sqrt( CONV f( 5 ) ) ) / 2.  " φ ≈ 1.618
    mv_phi2 = mv_phi * mv_phi.                  " φ² ≈ 2.618
    mv_iphi = 1 / mv_phi.                       " 1/φ ≈ 0.618

    " 120-cell vertices (simplified subset for performance)
    " Full 120-cell has 600 vertices - we use representative subset
    DATA(lv_r2) = sqrt( CONV f( 2 ) ).
    DATA(lv_r5) = sqrt( CONV f( 5 ) ).

    " Type 1: 16 vertices (±1,±1,±1,±1) - like tesseract but scaled
    add_perms( iv_a = 1 iv_b = 1 iv_c = 1 iv_d = 1 ).

    " Type 2: 8 vertices (±2,0,0,0) permutations - like 16-cell scaled
    DATA: lv_i TYPE i.
    lv_i = 1.
    WHILE lv_i <= 4.
      DATA(lv_s) = 1.
      WHILE lv_s >= -1.
        CASE lv_i.
          WHEN 1. add_vert( iv_x = 2 * lv_s iv_y = 0 iv_z = 0 iv_w = 0 ).
          WHEN 2. add_vert( iv_x = 0 iv_y = 2 * lv_s iv_z = 0 iv_w = 0 ).
          WHEN 3. add_vert( iv_x = 0 iv_y = 0 iv_z = 2 * lv_s iv_w = 0 ).
          WHEN 4. add_vert( iv_x = 0 iv_y = 0 iv_z = 0 iv_w = 2 * lv_s ).
        ENDCASE.
        lv_s = lv_s - 2.
      ENDWHILE.
      lv_i = lv_i + 1.
    ENDWHILE.

    " Type 3: 96 vertices (±φ,±1,±1/φ,0) and permutations
    add_perms( iv_a = mv_phi iv_b = 1 iv_c = mv_iphi iv_d = 0 ).
    add_perms( iv_a = mv_phi iv_b = mv_iphi iv_c = 0 iv_d = 1 ).
    add_perms( iv_a = 1 iv_b = mv_phi iv_c = 0 iv_d = mv_iphi ).
    add_perms( iv_a = 1 iv_b = 0 iv_c = mv_phi iv_d = mv_iphi ).
    add_perms( iv_a = mv_iphi iv_b = mv_phi iv_c = 1 iv_d = 0 ).
    add_perms( iv_a = 0 iv_b = 1 iv_c = mv_iphi iv_d = mv_phi ).

    " Build edges: connect vertices at specific distance (edge length)
    DATA(lv_n) = lines( mt_verts ).
    DATA(lv_edge_len) = mv_iphi * 2.  " Approximate edge length
    DATA(lv_tol) = '0.15'.
    DATA: lv_j TYPE i, lv_dist TYPE f.
    lv_i = 1.
    WHILE lv_i <= lv_n AND lv_i <= 200.  " Limit for performance
      lv_j = lv_i + 1.
      WHILE lv_j <= lv_n AND lv_j <= 200.
        DATA(ls_a) = mt_verts[ lv_i ]. DATA(ls_b) = mt_verts[ lv_j ].
        lv_dist = sqrt( ( ls_a-x - ls_b-x ) ** 2 + ( ls_a-y - ls_b-y ) ** 2 +
                        ( ls_a-z - ls_b-z ) ** 2 + ( ls_a-w - ls_b-w ) ** 2 ).
        IF lv_dist > lv_edge_len - lv_tol AND lv_dist < lv_edge_len + lv_tol.
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

  METHOD zif_o4d_effect~get_name. rv_name = 'cell120'. ENDMETHOD.
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
    DATA(lv_t) = is_ctx-gt. DATA(lv_size) = CONV f( 60 ).

    DATA(lv_xw) = lv_t * '0.25'. DATA(lv_yz) = lv_t * '0.35'.

    TYPES: BEGIN OF ty_proj, x TYPE f, y TYPE f, z TYPE f, w TYPE f, END OF ty_proj.
    DATA lt_proj TYPE STANDARD TABLE OF ty_proj WITH EMPTY KEY.
    DATA(lv_max) = nmin( val1 = lines( mt_verts ) val2 = 200 ).

    DATA: lv_idx TYPE i.
    lv_idx = 1.
    WHILE lv_idx <= lv_max.
      DATA(ls_v) = mt_verts[ lv_idx ].
      DATA(ls_r) = rot4d( is_v = ls_v iv_xw = lv_xw iv_yz = lv_yz ).
      DATA(lv_s4) = '4' / ( '4' - ls_r-w ).
      DATA(lv_s3) = '4' / ( '4' - ls_r-z * lv_s4 ).
      APPEND VALUE ty_proj(
        x = lv_cx + ls_r-x * lv_s4 * lv_s3 * lv_size
        y = lv_cy + ls_r-y * lv_s4 * lv_s3 * lv_size
        z = ls_r-z w = ls_r-w
      ) TO lt_proj.
      lv_idx = lv_idx + 1.
    ENDWHILE.

    " Draw edges with golden hue gradient
    LOOP AT mt_edges INTO DATA(ls_e).
      IF ls_e-a > lv_max OR ls_e-b > lv_max. CONTINUE. ENDIF.
      DATA(ls_pa) = lt_proj[ ls_e-a ]. DATA(ls_pb) = lt_proj[ ls_e-b ].
      DATA(lv_avg_w) = ( ls_pa-w + ls_pb-w ) / 2.
      DATA(lv_hue) = CONV i( 30 + ( lv_avg_w + 2 ) * 15 ) MOD 360.  " Gold to orange
      DATA(lv_light) = 40 + CONV i( ( lv_avg_w + 2 ) * 15 ).
      APPEND VALUE zif_o4d_effect=>ty_line(
        x1 = ls_pa-x y1 = ls_pa-y x2 = ls_pb-x y2 = ls_pb-y width = mv_line_width
        color = |hsl({ lv_hue }, 85%, { lv_light }%)|
      ) TO rs_frame-lines.
    ENDLOOP.

    APPEND VALUE zif_o4d_effect=>ty_text( x = lv_cx y = 20 text = '120-CELL' color = '#ffd700' size = 16 align = 'center' ) TO rs_frame-texts.
    APPEND VALUE zif_o4d_effect=>ty_text( x = lv_cx y = lv_h - 15
      text = |{ lines( mt_verts ) } verts, { lines( mt_edges ) } edges - φ = { mv_phi DECIMALS = 3 }|
      color = '#c90' size = 10 align = 'center' ) TO rs_frame-texts.

    IF is_ctx-gbi-pulse > '0.5'. rs_frame-flash-active = abap_true. rs_frame-flash-intensity = '0.15'.
      rs_frame-flash-r = 1. rs_frame-flash-g = '0.85'. rs_frame-flash-b = 0. ENDIF.
  ENDMETHOD.
ENDCLASS.
