CLASS zcl_o4d_tesseract DEFINITION PUBLIC FINAL CREATE PUBLIC.
*======================================================================
* TESSERACT - 4D Hypercube Rotation
*======================================================================
* 16 vertices, 32 edges rotating through 4D space
* Uses stereographic projection: 4D → 3D → 2D
* Rotates in XW and YZ planes simultaneously (Clifford rotation)
*======================================================================
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    CLASS-METHODS new
      IMPORTING iv_size TYPE f DEFAULT 120 iv_line_width TYPE f DEFAULT 2
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_o4d_tesseract.
    METHODS constructor IMPORTING iv_size TYPE f iv_line_width TYPE f.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_v4, x TYPE f, y TYPE f, z TYPE f, w TYPE f, END OF ty_v4.
    TYPES: BEGIN OF ty_edge, a TYPE i, b TYPE i, END OF ty_edge.
    DATA mv_size TYPE f.
    DATA mv_line_width TYPE f.
    DATA mt_verts TYPE STANDARD TABLE OF ty_v4 WITH EMPTY KEY.
    DATA mt_edges TYPE STANDARD TABLE OF ty_edge WITH EMPTY KEY.
    METHODS init_geometry.
    METHODS rot4d IMPORTING is_v TYPE ty_v4 iv_xw TYPE f iv_yz TYPE f RETURNING VALUE(rs_v) TYPE ty_v4.
    METHODS proj4to3 IMPORTING is_v TYPE ty_v4 iv_d TYPE f RETURNING VALUE(rs_v) TYPE ty_v4.
    METHODS proj3to2 IMPORTING is_v TYPE ty_v4 iv_d TYPE f iv_cx TYPE f iv_cy TYPE f
                     EXPORTING ev_x TYPE f ev_y TYPE f ev_z TYPE f.
ENDCLASS.

CLASS zcl_o4d_tesseract IMPLEMENTATION.
  METHOD new. ro_obj = NEW #( iv_size = iv_size iv_line_width = iv_line_width ). ENDMETHOD.
  METHOD constructor. mv_size = iv_size. mv_line_width = iv_line_width. init_geometry( ). ENDMETHOD.

  METHOD init_geometry.
    " 16 vertices of unit hypercube: all combinations of ±1 in 4D
    DATA: lv_x TYPE f, lv_y TYPE f, lv_z TYPE f, lv_w TYPE f.
    DO 2 TIMES. lv_x = COND f( WHEN sy-index = 1 THEN -1 ELSE 1 ).
      DO 2 TIMES. lv_y = COND f( WHEN sy-index = 1 THEN -1 ELSE 1 ).
        DO 2 TIMES. lv_z = COND f( WHEN sy-index = 1 THEN -1 ELSE 1 ).
          DO 2 TIMES. lv_w = COND f( WHEN sy-index = 1 THEN -1 ELSE 1 ).
            APPEND VALUE ty_v4( x = lv_x y = lv_y z = lv_z w = lv_w ) TO mt_verts.
          ENDDO.
        ENDDO.
      ENDDO.
    ENDDO.
    " 32 edges: connect vertices that differ in exactly one coordinate
    DATA(lv_n) = lines( mt_verts ).
    DATA lv_i TYPE i. DATA lv_j TYPE i. DATA lv_diff TYPE i.
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
        IF lv_diff = 1. APPEND VALUE ty_edge( a = lv_i b = lv_j ) TO mt_edges. ENDIF.
        lv_j = lv_j + 1.
      ENDWHILE.
      lv_i = lv_i + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD rot4d.
    " Rotate in XW plane and YZ plane (Clifford/double rotation)
    DATA(lv_cos_xw) = cos( iv_xw ). DATA(lv_sin_xw) = sin( iv_xw ).
    DATA(lv_cos_yz) = cos( iv_yz ). DATA(lv_sin_yz) = sin( iv_yz ).
    " XW rotation
    DATA(lv_nx) = is_v-x * lv_cos_xw - is_v-w * lv_sin_xw.
    DATA(lv_nw) = is_v-x * lv_sin_xw + is_v-w * lv_cos_xw.
    " YZ rotation
    DATA(lv_ny) = is_v-y * lv_cos_yz - is_v-z * lv_sin_yz.
    DATA(lv_nz) = is_v-y * lv_sin_yz + is_v-z * lv_cos_yz.
    rs_v = VALUE #( x = lv_nx y = lv_ny z = lv_nz w = lv_nw ).
  ENDMETHOD.

  METHOD proj4to3.
    " Stereographic projection from 4D to 3D
    DATA(lv_scale) = iv_d / ( iv_d - is_v-w ).
    rs_v-x = is_v-x * lv_scale.
    rs_v-y = is_v-y * lv_scale.
    rs_v-z = is_v-z * lv_scale.
    rs_v-w = is_v-w.  " Keep W for depth info
  ENDMETHOD.

  METHOD proj3to2.
    " Perspective projection from 3D to 2D
    DATA(lv_scale) = iv_d / ( iv_d - is_v-z ).
    ev_x = iv_cx + is_v-x * lv_scale * mv_size.
    ev_y = iv_cy + is_v-y * lv_scale * mv_size.
    ev_z = is_v-z.  " Return Z for depth-based coloring
  ENDMETHOD.

  METHOD zif_o4d_effect~get_name. rv_name = 'tesseract'. ENDMETHOD.
  METHOD zif_o4d_effect~get_params. ENDMETHOD.
  METHOD zif_o4d_effect~set_param. ENDMETHOD.
  METHOD zif_o4d_effect~render.
    rs_frame = zif_o4d_effect~render_frame( VALUE #( t = is_sync-time gt = is_sync-time
      gbi = VALUE #( bar = is_sync-bar bar_phase = is_sync-bar_phase pulse = is_sync-intensity ) ) ).
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    " Set frame-level line width
    rs_frame-state-line_width = mv_line_width.
    rs_frame-state-line_cap = 'round'.

    DATA(lv_w) = CONV f( zif_o4d_effect=>c_width ).
    DATA(lv_h) = CONV f( zif_o4d_effect=>c_height ).
    DATA(lv_cx) = lv_w / 2. DATA(lv_cy) = lv_h / 2.
    DATA(lv_t) = is_ctx-gt.

    " Rotation angles - two independent rotations for 4D effect
    DATA(lv_xw) = lv_t * '0.7'.   " XW plane rotation
    DATA(lv_yz) = lv_t * '0.5'.   " YZ plane rotation

    " Project all vertices
    TYPES: BEGIN OF ty_proj, x TYPE f, y TYPE f, z TYPE f, w TYPE f, END OF ty_proj.
    DATA lt_proj TYPE STANDARD TABLE OF ty_proj WITH EMPTY KEY.
    DATA lv_x TYPE f. DATA lv_y TYPE f. DATA lv_z TYPE f.

    LOOP AT mt_verts INTO DATA(ls_v).
      " 4D rotation
      DATA(ls_rot) = rot4d( is_v = ls_v iv_xw = lv_xw iv_yz = lv_yz ).
      " 4D to 3D projection
      DATA(ls_3d) = proj4to3( is_v = ls_rot iv_d = 3 ).
      " 3D to 2D projection
      proj3to2( EXPORTING is_v = ls_3d iv_d = 4 iv_cx = lv_cx iv_cy = lv_cy
                IMPORTING ev_x = lv_x ev_y = lv_y ev_z = lv_z ).
      APPEND VALUE ty_proj( x = lv_x y = lv_y z = lv_z w = ls_rot-w ) TO lt_proj.
    ENDLOOP.

    " Draw edges with depth-based coloring
    LOOP AT mt_edges INTO DATA(ls_e).
      DATA(ls_pa) = lt_proj[ ls_e-a ]. DATA(ls_pb) = lt_proj[ ls_e-b ].
      " Color based on W coordinate (4th dimension depth)
      DATA(lv_avg_w) = ( ls_pa-w + ls_pb-w ) / 2.
      DATA(lv_bright) = CONV i( 128 + lv_avg_w * 60 ).
      lv_bright = nmax( val1 = 80 val2 = nmin( val1 = 255 val2 = lv_bright ) ).
      " Hue based on position in 4D
      DATA(lv_hue) = CONV i( ( lv_avg_w + 1 ) * 180 ) MOD 360.
      APPEND VALUE zif_o4d_effect=>ty_line(
        x1 = ls_pa-x y1 = ls_pa-y x2 = ls_pb-x y2 = ls_pb-y width = mv_line_width
        color = |hsl({ lv_hue }, 80%, { CONV i( lv_bright / 255 * 60 + 20 ) }%)|
      ) TO rs_frame-lines.
    ENDLOOP.

    " Draw vertices as small dots
    LOOP AT lt_proj INTO DATA(ls_p).
      DATA(lv_vbright) = CONV i( 150 + ls_p-w * 50 ).
      APPEND VALUE zif_o4d_effect=>ty_rect(
        x = ls_p-x - 2 y = ls_p-y - 2 w = 4 h = 4
        fill = |rgb({ lv_vbright },{ lv_vbright },{ lv_vbright })|
      ) TO rs_frame-rects.
    ENDLOOP.

    " Title
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_cx y = 25 text = 'TESSERACT' color = '#00ffff' size = 16 align = 'center'
    ) TO rs_frame-texts.

    " Beat pulse
    IF is_ctx-gbi-pulse > '0.5'.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = '0.1'.
      rs_frame-flash-r = 0. rs_frame-flash-g = 1. rs_frame-flash-b = 1.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
