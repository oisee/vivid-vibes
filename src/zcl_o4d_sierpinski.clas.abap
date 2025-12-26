"! @title SIERPINSKI (Menger Sponge) - 3D fractal with solid faces
"!
"! ╔══════════════════════════════════════════════════════════════════╗
"! ║                         TWEAK GUIDE                              ║
"! ╠══════════════════════════════════════════════════════════════════╣
"! ║  PARAMETER      │ LOCATION           │ EFFECT                   ║
"! ╠══════════════════════════════════════════════════════════════════╣
"! ║  depth >= 2     │ render_frame       │ Recursion depth (2=fast  ║
"! ║                 │                    │ 3=detailed but heavy)    ║
"! ║  lv_ax speed    │ lv_t * 0.5         │ X-axis rotation speed    ║
"! ║  lv_ay speed    │ lv_t * 0.6         │ Y-axis rotation speed    ║
"! ║  lv_zoom        │ sin(t*1.2)*0.5     │ Zoom oscillation         ║
"! ║  lv_base        │ 90 * zoom          │ Base cube size           ║
"! ║  lv_scale       │ 200                │ Projection scale         ║
"! ║  lv_dist        │ project/280        │ Camera distance (FOV)    ║
"! ║  hue offset     │ depth*80 + t*40    │ Color by depth + cycling ║
"! ║  saturation     │ 0.75               │ Color saturation         ║
"! ║  light base     │ 0.35               │ Ambient light            ║
"! ║  light factor   │ 0.65               │ Directional light        ║
"! ╠══════════════════════════════════════════════════════════════════╣
"! ║  BEAT SYNC: pulse adds to base size (+8 pixels on beat)         ║
"! ║  WARNING: depth=3 creates 8000+ triangles - may cause lag!      ║
"! ╚══════════════════════════════════════════════════════════════════╝
CLASS zcl_o4d_sierpinski DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_face,
             x1 TYPE f, y1 TYPE f, z1 TYPE f,
             x2 TYPE f, y2 TYPE f, z2 TYPE f,
             x3 TYPE f, y3 TYPE f, z3 TYPE f,
             x4 TYPE f, y4 TYPE f, z4 TYPE f,
             nx TYPE f, ny TYPE f, nz TYPE f,
             depth TYPE i,
           END OF ty_face.
    CLASS-METHODS hsv_to_hex IMPORTING iv_h TYPE f iv_s TYPE f iv_v TYPE f RETURNING VALUE(rv_hex) TYPE string.
    CLASS-METHODS int_to_hex IMPORTING iv_int TYPE i RETURNING VALUE(rv_hex) TYPE string.
    CLASS-METHODS project
      IMPORTING ix TYPE f iy TYPE f iz TYPE f iv_ax TYPE f iv_ay TYPE f iv_cx TYPE f iv_cy TYPE f iv_scale TYPE f
      EXPORTING ev_sx TYPE f ev_sy TYPE f ev_z TYPE f.
ENDCLASS.



CLASS ZCL_O4D_SIERPINSKI IMPLEMENTATION.


  METHOD zif_o4d_effect~get_name. rv_name = 'sierpinski'. ENDMETHOD.


  METHOD zif_o4d_effect~get_params. ENDMETHOD.


  METHOD zif_o4d_effect~set_param. ENDMETHOD.


  METHOD zif_o4d_effect~render.
    DATA(ls_ctx) = VALUE zif_o4d_effect=>ty_render_ctx(
      t = is_sync-time gt = is_sync-time bi = VALUE #( pulse = is_sync-intensity ) ).
    rs_frame = zif_o4d_effect~render_frame( ls_ctx ).
  ENDMETHOD.


  METHOD zif_o4d_effect~render_frame.
    TYPES: BEGIN OF ty_cube, cx TYPE f, cy TYPE f, cz TYPE f, size TYPE f, depth TYPE i, END OF ty_cube.
    TYPES: BEGIN OF ty_proj_face, z TYPE f, tri1 TYPE zif_o4d_effect=>ty_triangle, tri2 TYPE zif_o4d_effect=>ty_triangle, END OF ty_proj_face.

    DATA lt_cubes TYPE STANDARD TABLE OF ty_cube WITH EMPTY KEY.
    DATA lt_next TYPE STANDARD TABLE OF ty_cube WITH EMPTY KEY.
    DATA lt_faces TYPE STANDARD TABLE OF ty_face WITH EMPTY KEY.
    DATA lt_proj TYPE STANDARD TABLE OF ty_proj_face WITH EMPTY KEY.

    DATA(lv_t) = is_ctx-t.
    DATA(lv_cx) = CONV f( zif_o4d_effect=>c_width ) / 2.
    DATA(lv_cy) = CONV f( zif_o4d_effect=>c_height ) / 2.

    "--- TWEAK: Rotation speeds ---
    DATA(lv_ax) = lv_t * CONV f( '0.3' ) .  " X rotation (0.3-0.8 good range)
    DATA(lv_ay) = lv_t * CONV f( '0.5' ).  " Y rotation

    "--- TWEAK: Zoom animation ---
    DATA(lv_zoom) = '1.5' + cos( lv_t / 3 ) * CONV f( '0.5' ).  " 0.5-1.5 range
    "DATA(lv_base) = 90 * lv_zoom + is_ctx-bi-pulse * 8.  " Base size + beat pulse
    DATA(lv_base) = 130 * lv_zoom + 8.  " Base size + beat pulse
    DATA(lv_scale) = CONV f( 300 ).  " Projection scale

    APPEND VALUE ty_cube( cx = 0 cy = 0 cz = 0 size = lv_base depth = 0 ) TO lt_cubes.

    "--- TWEAK: Recursion - depth >= 2 renders faces, higher = more detail ---
    WHILE lines( lt_cubes ) > 0.
      CLEAR lt_next.
      LOOP AT lt_cubes INTO DATA(ls_c).
        IF ls_c-depth >= 2.  " TWEAK: Change to 3 for more detail (but slower!)
          DATA(lv_s) = ls_c-size / 2.
          DATA(lv_x) = ls_c-cx. DATA(lv_y) = ls_c-cy. DATA(lv_z) = ls_c-cz.
          DATA(lv_d) = ls_c-depth.
          " Add all 6 faces of the cube
          APPEND VALUE ty_face( x1 = lv_x - lv_s y1 = lv_y - lv_s z1 = lv_z + lv_s
                                x2 = lv_x + lv_s y2 = lv_y - lv_s z2 = lv_z + lv_s
                                x3 = lv_x + lv_s y3 = lv_y + lv_s z3 = lv_z + lv_s
                                x4 = lv_x - lv_s y4 = lv_y + lv_s z4 = lv_z + lv_s
                                nx = 0 ny = 0 nz = 1 depth = lv_d ) TO lt_faces.
          APPEND VALUE ty_face( x1 = lv_x + lv_s y1 = lv_y - lv_s z1 = lv_z - lv_s
                                x2 = lv_x - lv_s y2 = lv_y - lv_s z2 = lv_z - lv_s
                                x3 = lv_x - lv_s y3 = lv_y + lv_s z3 = lv_z - lv_s
                                x4 = lv_x + lv_s y4 = lv_y + lv_s z4 = lv_z - lv_s
                                nx = 0 ny = 0 nz = -1 depth = lv_d ) TO lt_faces.
          APPEND VALUE ty_face( x1 = lv_x - lv_s y1 = lv_y + lv_s z1 = lv_z - lv_s
                                x2 = lv_x + lv_s y2 = lv_y + lv_s z2 = lv_z - lv_s
                                x3 = lv_x + lv_s y3 = lv_y + lv_s z3 = lv_z + lv_s
                                x4 = lv_x - lv_s y4 = lv_y + lv_s z4 = lv_z + lv_s
                                nx = 0 ny = 1 nz = 0 depth = lv_d ) TO lt_faces.
          APPEND VALUE ty_face( x1 = lv_x - lv_s y1 = lv_y - lv_s z1 = lv_z + lv_s
                                x2 = lv_x + lv_s y2 = lv_y - lv_s z2 = lv_z + lv_s
                                x3 = lv_x + lv_s y3 = lv_y - lv_s z3 = lv_z - lv_s
                                x4 = lv_x - lv_s y4 = lv_y - lv_s z4 = lv_z - lv_s
                                nx = 0 ny = -1 nz = 0 depth = lv_d ) TO lt_faces.
          APPEND VALUE ty_face( x1 = lv_x + lv_s y1 = lv_y - lv_s z1 = lv_z + lv_s
                                x2 = lv_x + lv_s y2 = lv_y - lv_s z2 = lv_z - lv_s
                                x3 = lv_x + lv_s y3 = lv_y + lv_s z3 = lv_z - lv_s
                                x4 = lv_x + lv_s y4 = lv_y + lv_s z4 = lv_z + lv_s
                                nx = 1 ny = 0 nz = 0 depth = lv_d ) TO lt_faces.
          APPEND VALUE ty_face( x1 = lv_x - lv_s y1 = lv_y - lv_s z1 = lv_z - lv_s
                                x2 = lv_x - lv_s y2 = lv_y - lv_s z2 = lv_z + lv_s
                                x3 = lv_x - lv_s y3 = lv_y + lv_s z3 = lv_z + lv_s
                                x4 = lv_x - lv_s y4 = lv_y + lv_s z4 = lv_z - lv_s
                                nx = -1 ny = 0 nz = 0 depth = lv_d ) TO lt_faces.
        ELSE.
          " Menger subdivision: 3x3x3 grid, skip center crosses
          DATA(lv_ns) = ls_c-size / 3.
          DATA: lv_dx TYPE i, lv_dy TYPE i, lv_dz TYPE i.
          DO 3 TIMES.
            lv_dx = sy-index - 2.
            DO 3 TIMES.
              lv_dy = sy-index - 2.
              DO 3 TIMES.
                lv_dz = sy-index - 2.
                DATA(lv_zeros) = 0.
                IF lv_dx = 0. lv_zeros = lv_zeros + 1. ENDIF.
                IF lv_dy = 0. lv_zeros = lv_zeros + 1. ENDIF.
                IF lv_dz = 0. lv_zeros = lv_zeros + 1. ENDIF.
                IF lv_zeros < 2.  " Skip if 2+ axes are zero (center cross)
                  APPEND VALUE ty_cube( cx = ls_c-cx + lv_dx * lv_ns cy = ls_c-cy + lv_dy * lv_ns
                    cz = ls_c-cz + lv_dz * lv_ns size = lv_ns depth = ls_c-depth + 1 ) TO lt_next.
                ENDIF.
              ENDDO.
            ENDDO.
          ENDDO.
        ENDIF.
      ENDLOOP.
      lt_cubes = lt_next.
    ENDWHILE.

    DATA: lv_sx1 TYPE f, lv_sy1 TYPE f, lv_z1 TYPE f,
          lv_sx2 TYPE f, lv_sy2 TYPE f, lv_z2 TYPE f,
          lv_sx3 TYPE f, lv_sy3 TYPE f, lv_z3 TYPE f,
          lv_sx4 TYPE f, lv_sy4 TYPE f, lv_z4 TYPE f.

    LOOP AT lt_faces INTO DATA(ls_f).
      project( EXPORTING ix = ls_f-x1 iy = ls_f-y1 iz = ls_f-z1 iv_ax = lv_ax iv_ay = lv_ay
               iv_cx = lv_cx iv_cy = lv_cy iv_scale = lv_scale IMPORTING ev_sx = lv_sx1 ev_sy = lv_sy1 ev_z = lv_z1 ).
      project( EXPORTING ix = ls_f-x2 iy = ls_f-y2 iz = ls_f-z2 iv_ax = lv_ax iv_ay = lv_ay
               iv_cx = lv_cx iv_cy = lv_cy iv_scale = lv_scale IMPORTING ev_sx = lv_sx2 ev_sy = lv_sy2 ev_z = lv_z2 ).
      project( EXPORTING ix = ls_f-x3 iy = ls_f-y3 iz = ls_f-z3 iv_ax = lv_ax iv_ay = lv_ay
               iv_cx = lv_cx iv_cy = lv_cy iv_scale = lv_scale IMPORTING ev_sx = lv_sx3 ev_sy = lv_sy3 ev_z = lv_z3 ).
      project( EXPORTING ix = ls_f-x4 iy = ls_f-y4 iz = ls_f-z4 iv_ax = lv_ax iv_ay = lv_ay
               iv_cx = lv_cx iv_cy = lv_cy iv_scale = lv_scale IMPORTING ev_sx = lv_sx4 ev_sy = lv_sy4 ev_z = lv_z4 ).

      "--- TWEAK: Lighting calculation ---
      "DATA(lv_light) = CONV f( '0.35' ) + CONV f( '0.65' ) * nmax( val1 = 0 val2 = ls_f-nz * cos( lv_ax ) + ls_f-ny * sin( lv_ax ) ).
      DATA(lv_light) = CONV f( '0.15' ) + CONV f( '0.65' ) * nmax( val1 = 0 val2 = ls_f-nz * cos( lv_ax ) + ls_f-ny * sin( lv_ax ) ).

      "--- TWEAK: Color by depth + time cycling ---
      "DATA(lv_hue) = ls_f-depth * 60 + lv_t * 30.  " 80° per depth level, 40°/sec cycle
      DATA(lv_hue) = conv f( ls_f-depth * 80 + 130 ).  " 80° per depth level, 40°/sec cycle
      DATA(lv_col) = hsv_to_hex( iv_h = lv_hue iv_s = CONV f( '0.75' ) iv_v = lv_light ).
      DATA(lv_avgz) = ( lv_z1 + lv_z2 + lv_z3 + lv_z4 ) / 4.

      APPEND VALUE ty_proj_face(
        z = lv_avgz
        tri1 = VALUE #( x1 = lv_sx1 y1 = lv_sy1 x2 = lv_sx2 y2 = lv_sy2 x3 = lv_sx3 y3 = lv_sy3 fill = lv_col )
        tri2 = VALUE #( x1 = lv_sx1 y1 = lv_sy1 x2 = lv_sx3 y2 = lv_sy3 x3 = lv_sx4 y3 = lv_sy4 fill = lv_col )
      ) TO lt_proj.
    ENDLOOP.

    " Z-sort for correct depth rendering
    SORT lt_proj BY z DESCENDING . "ASCENDING.

    LOOP AT lt_proj INTO DATA(ls_p).
      APPEND ls_p-tri1 TO rs_frame-triangles.
      APPEND ls_p-tri2 TO rs_frame-triangles.
    ENDLOOP.

    APPEND VALUE zif_o4d_effect=>ty_text(
      x = 320 y = 25 text = 'MENGER SPONGE' color = '#88ffff' size = 12 align = 'center'
    ) TO rs_frame-texts.
  ENDMETHOD.


  METHOD project.
    " Rotate around X then Y, apply perspective
    DATA(lv_y1) = iy * cos( iv_ax ) - iz * sin( iv_ax ).
    DATA(lv_z1) = iy * sin( iv_ax ) + iz * cos( iv_ax ).
    DATA(lv_x2) = ix * cos( iv_ay ) + lv_z1 * sin( iv_ay ).
    DATA(lv_z2) = ( 0 - ix ) * sin( iv_ay ) + lv_z1 * cos( iv_ay ).
    DATA(lv_dist) = CONV f( 280 ).  "--- TWEAK: Camera distance (smaller = more perspective)
    DATA(lv_persp) = lv_dist / ( lv_dist + lv_z2 ).
    ev_sx = iv_cx + lv_x2 * lv_persp * iv_scale / lv_dist.
    ev_sy = iv_cy - lv_y1 * lv_persp * iv_scale / lv_dist.
    ev_z = lv_z2.
  ENDMETHOD.


  METHOD hsv_to_hex.
    DATA: lv_c TYPE f, lv_x TYPE f, lv_m TYPE f, lv_r TYPE f, lv_g TYPE f, lv_b TYPE f.
    DATA(lv_h) = CONV f( CONV i( iv_h ) MOD 360 ). lv_c = iv_v * iv_s.
    lv_x = lv_c * ( 1 - abs( frac( lv_h / 120 ) * 2 - 1 ) ). lv_m = iv_v - lv_c.
    IF lv_h < 60. lv_r = lv_c. lv_g = lv_x. lv_b = 0.
    ELSEIF lv_h < 120. lv_r = lv_x. lv_g = lv_c. lv_b = 0.
    ELSEIF lv_h < 180. lv_r = 0. lv_g = lv_c. lv_b = lv_x.
    ELSEIF lv_h < 240. lv_r = 0. lv_g = lv_x. lv_b = lv_c.
    ELSEIF lv_h < 300. lv_r = lv_x. lv_g = 0. lv_b = lv_c.
    ELSE. lv_r = lv_c. lv_g = 0. lv_b = lv_x. ENDIF.
    rv_hex = |#{ int_to_hex( CONV i( ( lv_r + lv_m ) * 255 ) ) }{ int_to_hex( CONV i( ( lv_g + lv_m ) * 255 ) ) }{ int_to_hex( CONV i( ( lv_b + lv_m ) * 255 ) ) }|.
  ENDMETHOD.


  METHOD int_to_hex.
    DATA lv_hex TYPE x LENGTH 1. DATA(lv_val) = nmax( val1 = 0 val2 = nmin( val1 = 255 val2 = iv_int ) ).
    lv_hex = lv_val. rv_hex = |{ lv_hex }|. TRANSLATE rv_hex TO LOWER CASE.
  ENDMETHOD.
ENDCLASS.
