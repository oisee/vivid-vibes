CLASS zcl_o4d_menger DEFINITION PUBLIC FINAL CREATE PUBLIC.
*======================================================================
* MENGER SPONGE - Precalculated 3D Fractal
*======================================================================
* Recursive cube fractal with holes
* Depth 2: 400 cubes, Depth 3: 8000 cubes
* All geometry precalculated at init - only rotation at render
*======================================================================
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    CLASS-METHODS new
      IMPORTING iv_depth      TYPE i DEFAULT 2
                iv_line_width TYPE f DEFAULT 2
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_o4d_menger.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_v3, x TYPE f, y TYPE f, z TYPE f, END OF ty_v3.
    TYPES: BEGIN OF ty_cube, cx TYPE f, cy TYPE f, cz TYPE f, size TYPE f, END OF ty_cube.
    TYPES: BEGIN OF ty_edge, x1 TYPE f, y1 TYPE f, z1 TYPE f, x2 TYPE f, y2 TYPE f, z2 TYPE f, END OF ty_edge.

    DATA mt_cubes TYPE STANDARD TABLE OF ty_cube WITH EMPTY KEY.
    DATA mt_edges TYPE STANDARD TABLE OF ty_edge WITH EMPTY KEY.
    DATA mv_depth TYPE i.
    DATA mv_line_width TYPE f.
    DATA mv_total_cubes TYPE i.

    METHODS init_geometry.
    METHODS generate_menger IMPORTING iv_cx TYPE f iv_cy TYPE f iv_cz TYPE f
                                      iv_size TYPE f iv_depth TYPE i.
    METHODS add_cube_edges IMPORTING iv_cx TYPE f iv_cy TYPE f iv_cz TYPE f iv_size TYPE f.
    METHODS rotate IMPORTING is_v TYPE ty_v3 iv_rx TYPE f iv_ry TYPE f iv_rz TYPE f
                   RETURNING VALUE(rs_v) TYPE ty_v3.
ENDCLASS.



CLASS ZCL_O4D_MENGER IMPLEMENTATION.


  METHOD new.
    ro_obj = NEW #( ).
    ro_obj->mv_depth = iv_depth.
    ro_obj->mv_line_width = iv_line_width.
    ro_obj->init_geometry( ).
  ENDMETHOD.


  METHOD init_geometry.
    " Generate Menger sponge recursively
    generate_menger( iv_cx = 0 iv_cy = 0 iv_cz = 0 iv_size = 2 iv_depth = mv_depth ).
    mv_total_cubes = lines( mt_cubes ).
  ENDMETHOD.


  METHOD generate_menger.
    IF iv_depth = 0.
      " Base case: add this cube's edges
      APPEND VALUE ty_cube( cx = iv_cx cy = iv_cy cz = iv_cz size = iv_size ) TO mt_cubes.
      add_cube_edges( iv_cx = iv_cx iv_cy = iv_cy iv_cz = iv_cz iv_size = iv_size ).
      RETURN.
    ENDIF.

    " Divide into 3x3x3 grid and recurse for non-removed cubes
    DATA(lv_sub_size) = iv_size / 3.
    DATA(lv_offset) = iv_size / 3.

    DATA: lv_ix TYPE i, lv_iy TYPE i, lv_iz TYPE i.
    DATA: lv_x TYPE f, lv_y TYPE f, lv_z TYPE f.
    DATA: lv_zeros TYPE i.

    lv_ix = -1.
    WHILE lv_ix <= 1.
      lv_iy = -1.
      WHILE lv_iy <= 1.
        lv_iz = -1.
        WHILE lv_iz <= 1.
          " Count how many coordinates are zero (center positions)
          lv_zeros = 0.
          IF lv_ix = 0. lv_zeros = lv_zeros + 1. ENDIF.
          IF lv_iy = 0. lv_zeros = lv_zeros + 1. ENDIF.
          IF lv_iz = 0. lv_zeros = lv_zeros + 1. ENDIF.

          " Remove if 2 or more coordinates are zero (center cross)
          IF lv_zeros < 2.
            lv_x = iv_cx + lv_ix * lv_offset.
            lv_y = iv_cy + lv_iy * lv_offset.
            lv_z = iv_cz + lv_iz * lv_offset.
            generate_menger( iv_cx = lv_x iv_cy = lv_y iv_cz = lv_z
                            iv_size = lv_sub_size iv_depth = iv_depth - 1 ).
          ENDIF.

          lv_iz = lv_iz + 1.
        ENDWHILE.
        lv_iy = lv_iy + 1.
      ENDWHILE.
      lv_ix = lv_ix + 1.
    ENDWHILE.
  ENDMETHOD.


  METHOD add_cube_edges.
    DATA(lv_h) = iv_size / 2.

    " 8 vertices of cube
    DATA(lv_x1) = iv_cx - lv_h. DATA(lv_x2) = iv_cx + lv_h.
    DATA(lv_y1) = iv_cy - lv_h. DATA(lv_y2) = iv_cy + lv_h.
    DATA(lv_z1) = iv_cz - lv_h. DATA(lv_z2) = iv_cz + lv_h.

    " 12 edges of cube
    " Bottom face (z1)
    APPEND VALUE ty_edge( x1 = lv_x1 y1 = lv_y1 z1 = lv_z1 x2 = lv_x2 y2 = lv_y1 z2 = lv_z1 ) TO mt_edges.
    APPEND VALUE ty_edge( x1 = lv_x2 y1 = lv_y1 z1 = lv_z1 x2 = lv_x2 y2 = lv_y2 z2 = lv_z1 ) TO mt_edges.
    APPEND VALUE ty_edge( x1 = lv_x2 y1 = lv_y2 z1 = lv_z1 x2 = lv_x1 y2 = lv_y2 z2 = lv_z1 ) TO mt_edges.
    APPEND VALUE ty_edge( x1 = lv_x1 y1 = lv_y2 z1 = lv_z1 x2 = lv_x1 y2 = lv_y1 z2 = lv_z1 ) TO mt_edges.
    " Top face (z2)
    APPEND VALUE ty_edge( x1 = lv_x1 y1 = lv_y1 z1 = lv_z2 x2 = lv_x2 y2 = lv_y1 z2 = lv_z2 ) TO mt_edges.
    APPEND VALUE ty_edge( x1 = lv_x2 y1 = lv_y1 z1 = lv_z2 x2 = lv_x2 y2 = lv_y2 z2 = lv_z2 ) TO mt_edges.
    APPEND VALUE ty_edge( x1 = lv_x2 y1 = lv_y2 z1 = lv_z2 x2 = lv_x1 y2 = lv_y2 z2 = lv_z2 ) TO mt_edges.
    APPEND VALUE ty_edge( x1 = lv_x1 y1 = lv_y2 z1 = lv_z2 x2 = lv_x1 y2 = lv_y1 z2 = lv_z2 ) TO mt_edges.
    " Vertical edges
    APPEND VALUE ty_edge( x1 = lv_x1 y1 = lv_y1 z1 = lv_z1 x2 = lv_x1 y2 = lv_y1 z2 = lv_z2 ) TO mt_edges.
    APPEND VALUE ty_edge( x1 = lv_x2 y1 = lv_y1 z1 = lv_z1 x2 = lv_x2 y2 = lv_y1 z2 = lv_z2 ) TO mt_edges.
    APPEND VALUE ty_edge( x1 = lv_x2 y1 = lv_y2 z1 = lv_z1 x2 = lv_x2 y2 = lv_y2 z2 = lv_z2 ) TO mt_edges.
    APPEND VALUE ty_edge( x1 = lv_x1 y1 = lv_y2 z1 = lv_z1 x2 = lv_x1 y2 = lv_y2 z2 = lv_z2 ) TO mt_edges.
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


  METHOD zif_o4d_effect~get_name. rv_name = 'menger'. ENDMETHOD.


  METHOD zif_o4d_effect~get_params. ENDMETHOD.


  METHOD zif_o4d_effect~set_param. ENDMETHOD.


  METHOD zif_o4d_effect~render.
    rs_frame = zif_o4d_effect~render_frame( VALUE #( t = is_sync-time gt = is_sync-time
      gbi = VALUE #( bar = is_sync-bar bar_phase = is_sync-bar_phase pulse = is_sync-intensity ) ) ).
  ENDMETHOD.


  METHOD zif_o4d_effect~render_frame.
    DATA(lv_w) = CONV f( zif_o4d_effect=>c_width ).
    DATA(lv_h) = CONV f( zif_o4d_effect=>c_height ).
    DATA(lv_cx) = lv_w / 2. DATA(lv_cy) = lv_h / 2.
    DATA(lv_t) = is_ctx-gt.
    DATA(lv_size) = CONV f( 100 ).

    " Set frame state for line rendering
    rs_frame-state-line_width = mv_line_width.
    rs_frame-state-line_cap = 'round'.

    " Rotation angles (slow, elegant rotation)
    DATA(lv_rx) = lv_t * '0.3'.
    DATA(lv_ry) = lv_t * '0.4'.
    DATA(lv_rz) = lv_t * '0.7'. "'0.2'.

    " Transform and draw all edges
    LOOP AT mt_edges INTO DATA(ls_e).
      " Rotate start point
      DATA(ls_v1) = rotate( is_v = VALUE ty_v3( x = ls_e-x1 y = ls_e-y1 z = ls_e-z1 )
                            iv_rx = lv_rx iv_ry = lv_ry iv_rz = lv_rz ).
      " Rotate end point
      DATA(ls_v2) = rotate( is_v = VALUE ty_v3( x = ls_e-x2 y = ls_e-y2 z = ls_e-z2 )
                            iv_rx = lv_rx iv_ry = lv_ry iv_rz = lv_rz ).

      " Perspective projection
      DATA(lv_d) = CONV f( 4 ).
      DATA(lv_s1) = lv_d / ( lv_d - ls_v1-z ).
      DATA(lv_s2) = lv_d / ( lv_d - ls_v2-z ).

      DATA(lv_px1) = lv_cx + ls_v1-x * lv_size * lv_s1.
      DATA(lv_py1) = lv_cy + ls_v1-y * lv_size * lv_s1.
      DATA(lv_px2) = lv_cx + ls_v2-x * lv_size * lv_s2.
      DATA(lv_py2) = lv_cy + ls_v2-y * lv_size * lv_s2.

      " Color based on average Z depth
      DATA(lv_avg_z) = ( ls_v1-z + ls_v2-z ) / 2.
      DATA(lv_hue) = CONV i( 200 + lv_avg_z * 30 ) MOD 360.
      DATA(lv_light) = 40 + CONV i( ( lv_avg_z + 2 ) * 15 ).

      APPEND VALUE zif_o4d_effect=>ty_line(
        x1 = lv_px1 y1 = lv_py1 x2 = lv_px2 y2 = lv_py2
        width = mv_line_width
        color = |hsl({ lv_hue }, 70%, { lv_light }%)|
      ) TO rs_frame-lines.
    ENDLOOP.

    " Title
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_cx y = 20 text = |MENGER SPONGE (Depth { mv_depth })|
      color = '#0ff' size = 14 align = 'center'
    ) TO rs_frame-texts.

    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_cx y = lv_h - 15
      text = |{ mv_total_cubes } cubes, { lines( mt_edges ) } edges|
      color = '#888' size = 10 align = 'center'
    ) TO rs_frame-texts.

    " Beat flash
    IF is_ctx-gbi-pulse > '0.5'.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = '0.1'.
      rs_frame-flash-r = 0. rs_frame-flash-g = 1. rs_frame-flash-b = 1.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
