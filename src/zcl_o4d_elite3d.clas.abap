"! @title ELITE3D - Classic Elite wireframe ships
"!
"! ╔══════════════════════════════════════════════════════════════════╗
"! ║                         TWEAK GUIDE                              ║
"! ╠══════════════════════════════════════════════════════════════════╣
"! ║  PARAMETER      │ LOCATION           │ EFFECT                   ║
"! ╠══════════════════════════════════════════════════════════════════╣
"! ║  lv_phase_dur   │ render_frame/4     │ Seconds per ship (4s)    ║
"! ║  lv_swipe       │ 0.5s transition    │ Ship slide-in duration   ║
"! ║  lv_fov         │ render_ship/320    │ Field of view (larger =  ║
"! ║                 │                    │ more perspective)        ║
"! ║  iv_pos_z       │ render calls       │ Ship distance (closer =  ║
"! ║                 │                    │ bigger, 150-250 good)    ║
"! ║  iv_rot_y       │ lv_t * 0.7         │ Y rotation speed         ║
"! ║  iv_rot_x       │ sin(t) * 0.2       │ X wobble amplitude       ║
"! ║  line width     │ 2 + pulse          │ Line thickness + beat    ║
"! ╠══════════════════════════════════════════════════════════════════╣
"! ║  SHIPS: mt_cobra (green), mt_coriolis (cyan), mt_thargoid (yellow)║
"! ║  Each ship has vertices[] and edges[] - modify for custom ships ║
"! ╠══════════════════════════════════════════════════════════════════╣
"! ║  BEAT SYNC: pulse adds to line width (thicker lines on beat)    ║
"! ║  COMBAT: Thargoid phase has laser fire when pulse > 0.5         ║
"! ╚══════════════════════════════════════════════════════════════════╝
CLASS zcl_o4d_elite3d DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    METHODS constructor.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_vertex,
             x TYPE f, y TYPE f, z TYPE f,
           END OF ty_vertex,
           tt_vertices TYPE STANDARD TABLE OF ty_vertex WITH EMPTY KEY,
           BEGIN OF ty_edge,
             v1 TYPE i, v2 TYPE i,
           END OF ty_edge,
           tt_edges TYPE STANDARD TABLE OF ty_edge WITH EMPTY KEY,
           BEGIN OF ty_ship,
             name     TYPE string,
             vertices TYPE tt_vertices,
             edges    TYPE tt_edges,
             color    TYPE string,
           END OF ty_ship.

    CONSTANTS: c_pi TYPE f VALUE '3.14159265'.

    DATA: mt_cobra    TYPE ty_ship,
          mt_thargoid TYPE ty_ship,
          mt_coriolis TYPE ty_ship.

    METHODS:
      init_ships,
      rotate_vertex
        IMPORTING is_v TYPE ty_vertex iv_rx TYPE f iv_ry TYPE f iv_rz TYPE f
        RETURNING VALUE(rs_v) TYPE ty_vertex,
      render_ship
        IMPORTING is_ship   TYPE ty_ship
                  iv_t      TYPE f
                  iv_pulse  TYPE f
                  iv_pos_x  TYPE f
                  iv_pos_y  TYPE f
                  iv_pos_z  TYPE f
                  iv_rot_x  TYPE f
                  iv_rot_y  TYPE f
                  iv_scale  TYPE f DEFAULT 1
        CHANGING  cs_frame  TYPE zif_o4d_effect=>ty_frame.
ENDCLASS.

CLASS zcl_o4d_elite3d IMPLEMENTATION.

  METHOD constructor.
    init_ships( ).
  ENDMETHOD.

  METHOD init_ships.
    DATA: lv_r TYPE f, lv_h TYPE f, lv_i TYPE i, lv_angle TYPE f, lv_size TYPE f,
          lv_neg_h TYPE f, lv_neg_30 TYPE f, lv_s6 TYPE f, lv_neg_s6 TYPE f.

    "=== COBRA MK III - iconic trading ship ===
    "--- TWEAK: Vertex coordinates define ship shape ---
    mt_cobra-name = 'COBRA MK III'.
    mt_cobra-color = '#00FF00'.  "--- TWEAK: Ship wireframe color
    mt_cobra-vertices = VALUE #(
      ( x = 0   y = 0   z = 60 )    " nose (front point)
      ( x = -12 y = -5  z = 30 )    " cockpit left
      ( x = 12  y = -5  z = 30 )    " cockpit right
      ( x = 0   y = -9  z = 38 )    " cockpit bottom
      ( x = -52 y = 0   z = -30 )   " left wing tip
      ( x = 52  y = 0   z = -30 )   " right wing tip
      ( x = -22 y = 0   z = -45 )   " left engine
      ( x = 22  y = 0   z = -45 )   " right engine
      ( x = 0   y = 12  z = -38 )   " top fin
      ( x = -15 y = 5   z = -45 )   " left tail
      ( x = 15  y = 5   z = -45 )   " right tail
    ).
    "--- TWEAK: Edges connect vertices (0-indexed) ---
    mt_cobra-edges = VALUE #(
      ( v1 = 0 v2 = 1 ) ( v1 = 0 v2 = 2 ) ( v1 = 0 v2 = 3 )
      ( v1 = 1 v2 = 2 ) ( v1 = 1 v2 = 3 ) ( v1 = 2 v2 = 3 )
      ( v1 = 1 v2 = 4 ) ( v1 = 2 v2 = 5 )
      ( v1 = 4 v2 = 6 ) ( v1 = 5 v2 = 7 )
      ( v1 = 6 v2 = 7 ) ( v1 = 6 v2 = 8 ) ( v1 = 7 v2 = 8 )
      ( v1 = 4 v2 = 8 ) ( v1 = 5 v2 = 8 )
      ( v1 = 6 v2 = 9 ) ( v1 = 7 v2 = 10 )
      ( v1 = 9 v2 = 10 ) ( v1 = 9 v2 = 8 ) ( v1 = 10 v2 = 8 )
    ).

    "=== THARGOID - octagonal alien ship ===
    mt_thargoid-name = 'THARGOID'.
    mt_thargoid-color = '#FFFF00'.
    lv_r = 55. lv_h = 22.  "--- TWEAK: Radius and height
    lv_neg_h = 0 - lv_h.
    lv_neg_30 = -40.

    " Generate 8-sided shape programmatically
    DO 8 TIMES.
      lv_i = sy-index - 1.
      lv_angle = CONV f( lv_i ) * 2 * c_pi / 8.
      APPEND VALUE ty_vertex( x = lv_r * cos( lv_angle ) y = lv_neg_h z = lv_r * sin( lv_angle ) ) TO mt_thargoid-vertices.
      APPEND VALUE ty_vertex( x = lv_r * cos( lv_angle ) y = lv_h z = lv_r * sin( lv_angle ) ) TO mt_thargoid-vertices.
    ENDDO.
    APPEND VALUE ty_vertex( x = 0 y = lv_neg_30 z = 0 ) TO mt_thargoid-vertices.  " bottom center
    APPEND VALUE ty_vertex( x = 0 y = 40 z = 0 ) TO mt_thargoid-vertices.         " top center

    DO 8 TIMES.
      lv_i = sy-index - 1.
      DATA(lv_next) = ( lv_i + 1 ) MOD 8.
      APPEND VALUE ty_edge( v1 = lv_i * 2 v2 = lv_next * 2 ) TO mt_thargoid-edges.
      APPEND VALUE ty_edge( v1 = lv_i * 2 + 1 v2 = lv_next * 2 + 1 ) TO mt_thargoid-edges.
      APPEND VALUE ty_edge( v1 = lv_i * 2 v2 = lv_i * 2 + 1 ) TO mt_thargoid-edges.
      APPEND VALUE ty_edge( v1 = lv_i * 2 v2 = 16 ) TO mt_thargoid-edges.
      APPEND VALUE ty_edge( v1 = lv_i * 2 + 1 v2 = 17 ) TO mt_thargoid-edges.
    ENDDO.

    "=== CORIOLIS STATION - rotating space station ===
    mt_coriolis-name = 'CORIOLIS'.
    mt_coriolis-color = '#00FFFF'.
    lv_size = 50.  "--- TWEAK: Station size
    lv_s6 = lv_size * CONV f( '0.6' ).
    lv_neg_s6 = 0 - lv_s6.

    mt_coriolis-vertices = VALUE #(
      ( x = lv_size  y = 0 z = 0 )          " +X axis
      ( x = 0 - lv_size y = 0 z = 0 )       " -X axis
      ( x = 0 y = lv_size  z = 0 )          " +Y axis
      ( x = 0 y = 0 - lv_size z = 0 )       " -Y axis
      ( x = 0 y = 0 z = lv_size )           " +Z axis
      ( x = 0 y = 0 z = 0 - lv_size )       " -Z axis
      ( x = lv_s6 y = lv_s6 z = lv_s6 )     " corners
      ( x = lv_neg_s6 y = lv_s6 z = lv_s6 )
      ( x = lv_s6 y = lv_neg_s6 z = lv_s6 )
      ( x = lv_neg_s6 y = lv_neg_s6 z = lv_s6 )
      ( x = lv_s6 y = lv_s6 z = lv_neg_s6 )
      ( x = lv_neg_s6 y = lv_s6 z = lv_neg_s6 )
    ).
    mt_coriolis-edges = VALUE #(
      ( v1 = 0 v2 = 6 ) ( v1 = 0 v2 = 8 ) ( v1 = 0 v2 = 10 )
      ( v1 = 1 v2 = 7 ) ( v1 = 1 v2 = 9 ) ( v1 = 1 v2 = 11 )
      ( v1 = 2 v2 = 6 ) ( v1 = 2 v2 = 7 ) ( v1 = 2 v2 = 10 ) ( v1 = 2 v2 = 11 )
      ( v1 = 3 v2 = 8 ) ( v1 = 3 v2 = 9 )
      ( v1 = 4 v2 = 6 ) ( v1 = 4 v2 = 7 ) ( v1 = 4 v2 = 8 ) ( v1 = 4 v2 = 9 )
      ( v1 = 5 v2 = 10 ) ( v1 = 5 v2 = 11 )
      ( v1 = 6 v2 = 7 ) ( v1 = 6 v2 = 8 ) ( v1 = 7 v2 = 9 ) ( v1 = 8 v2 = 9 )
      ( v1 = 10 v2 = 11 )
    ).
  ENDMETHOD.

  METHOD zif_o4d_effect~get_name.
    rv_name = 'elite3d'.
  ENDMETHOD.

  METHOD zif_o4d_effect~get_params.
  ENDMETHOD.

  METHOD zif_o4d_effect~set_param.
  ENDMETHOD.

  METHOD zif_o4d_effect~render.
    DATA(ls_ctx) = VALUE zif_o4d_effect=>ty_render_ctx(
      t = is_sync-time gt = is_sync-time
      bi = VALUE #( pulse = is_sync-intensity bar_phase = is_sync-bar_phase )
    ).
    rs_frame = zif_o4d_effect~render_frame( ls_ctx ).
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    DATA(lv_t) = is_ctx-t.
    DATA(lv_pulse) = is_ctx-bi-pulse.

    "--- TWEAK: Phase timing ---
    DATA(lv_phase_dur) = CONV f( 4 ).  " 4 seconds per ship
    DATA(lv_phase) = floor( lv_t / lv_phase_dur ) MOD 3.  " 3 ships cycle
    DATA(lv_phase_t) = frac( lv_t / lv_phase_dur ) * lv_phase_dur.

    "--- TWEAK: Slide-in transition ---
    DATA(lv_swipe) = nmin( val1 = CONV f( 1 ) val2 = lv_phase_t / CONV f( '0.5' ) ).  " 0.5s swipe
    DATA(lv_offset_x) = ( 1 - lv_swipe ) * 800.  " Slide from right

    CASE lv_phase.
      WHEN 0.  " Cobra Mk III
        render_ship(
          EXPORTING is_ship = mt_cobra iv_t = lv_t iv_pulse = lv_pulse
                    iv_pos_x = lv_offset_x iv_pos_y = 0 iv_pos_z = 180  "--- TWEAK: Distance
                    iv_rot_x = sin( lv_t ) * CONV f( '0.2' )  "--- TWEAK: Wobble
                    iv_rot_y = lv_t * CONV f( '0.7' )         "--- TWEAK: Spin speed
          CHANGING  cs_frame = rs_frame
        ).
      WHEN 1.  " Coriolis Station
        render_ship(
          EXPORTING is_ship = mt_coriolis iv_t = lv_t iv_pulse = lv_pulse
                    iv_pos_x = lv_offset_x iv_pos_y = 0 iv_pos_z = 220
                    iv_rot_x = lv_t * CONV f( '0.3' )  " Slower tumble
                    iv_rot_y = lv_t * CONV f( '0.2' )
          CHANGING  cs_frame = rs_frame
        ).
      WHEN 2.  " Thargoid - COMBAT!
        render_ship(
          EXPORTING is_ship = mt_thargoid iv_t = lv_t iv_pulse = lv_pulse
                    iv_pos_x = lv_offset_x + sin( lv_t ) * 40  " Evasive movement
                    iv_pos_y = cos( lv_t * CONV f( '1.3' ) ) * 25
                    iv_pos_z = 200
                    iv_rot_x = sin( lv_t ) * CONV f( '0.3' )
                    iv_rot_y = lv_t * 2  " Fast spin
          CHANGING  cs_frame = rs_frame
        ).
        "--- TWEAK: Laser fire on beat ---
        IF lv_pulse > CONV f( '0.5' ).
          DATA(lv_lx) = 320 + sin( lv_t * 5 ) * 80.
          APPEND VALUE zif_o4d_effect=>ty_line(
            x1 = 320 y1 = 380 x2 = lv_lx y2 = 200 - lv_pulse * 60
            color = '#FF0000' width = 3
          ) TO rs_frame-lines.
        ENDIF.
    ENDCASE.

    " Debug vars - ship state and derived values
    DATA(lv_ship_name) = COND string( WHEN lv_phase = 0 THEN 'cobra'
                                      WHEN lv_phase = 1 THEN 'coriolis' ELSE 'thargoid' ).
    DATA(lv_rot_y) = COND f( WHEN lv_phase = 0 THEN lv_t * CONV f( '0.7' )
                             WHEN lv_phase = 1 THEN lv_t * CONV f( '0.2' ) ELSE lv_t * 2 ).
    rs_frame-debug-vars = |\{"ship":"{ lv_ship_name }","phase":{ lv_phase },| &&
      |"phase_t":{ lv_phase_t },"swipe":{ lv_swipe },"offset_x":{ lv_offset_x },| &&
      |"rot_y":{ lv_rot_y },"phase_dur":{ lv_phase_dur },"pulse":{ lv_pulse },| &&
      |"fov":320,"pos_z":[180,220,200]\}|.
  ENDMETHOD.

  METHOD rotate_vertex.
    " Standard 3D rotation: X then Y axis
    DATA: lv_y TYPE f, lv_z TYPE f, lv_x TYPE f.
    lv_y = is_v-y * cos( iv_rx ) - is_v-z * sin( iv_rx ).
    lv_z = is_v-y * sin( iv_rx ) + is_v-z * cos( iv_rx ).
    rs_v-x = is_v-x. rs_v-y = lv_y. rs_v-z = lv_z.
    lv_x = rs_v-x * cos( iv_ry ) + rs_v-z * sin( iv_ry ).
    lv_z = rs_v-z * cos( iv_ry ) - rs_v-x * sin( iv_ry ).
    rs_v-x = lv_x. rs_v-z = lv_z.
  ENDMETHOD.

  METHOD render_ship.
    "--- TWEAK: Projection parameters ---
    DATA: lv_fov TYPE f VALUE 320,  " Field of view (bigger = more perspective)
          lv_cx TYPE f VALUE 320,   " Screen center X
          lv_cy TYPE f VALUE 200.   " Screen center Y

    LOOP AT is_ship-edges INTO DATA(ls_edge).
      READ TABLE is_ship-vertices INDEX ls_edge-v1 + 1 INTO DATA(ls_v1).
      READ TABLE is_ship-vertices INDEX ls_edge-v2 + 1 INTO DATA(ls_v2).

      " Rotate vertices
      DATA(ls_v1r) = rotate_vertex( is_v = ls_v1 iv_rx = iv_rot_x iv_ry = iv_rot_y iv_rz = 0 ).
      DATA(ls_v2r) = rotate_vertex( is_v = ls_v2 iv_rx = iv_rot_x iv_ry = iv_rot_y iv_rz = 0 ).

      " Translate to world position
      ls_v1r-x = ls_v1r-x + iv_pos_x. ls_v1r-y = ls_v1r-y + iv_pos_y. ls_v1r-z = ls_v1r-z + iv_pos_z.
      ls_v2r-x = ls_v2r-x + iv_pos_x. ls_v2r-y = ls_v2r-y + iv_pos_y. ls_v2r-z = ls_v2r-z + iv_pos_z.

      " Project to 2D (only if in front of camera)
      IF ls_v1r-z > 50 AND ls_v2r-z > 50.
        DATA(lv_x1) = lv_cx + ls_v1r-x * lv_fov / ls_v1r-z.
        DATA(lv_y1) = lv_cy - ls_v1r-y * lv_fov / ls_v1r-z.
        DATA(lv_x2) = lv_cx + ls_v2r-x * lv_fov / ls_v2r-z.
        DATA(lv_y2) = lv_cy - ls_v2r-y * lv_fov / ls_v2r-z.

        "--- TWEAK: Line width = base + beat pulse ---
        APPEND VALUE zif_o4d_effect=>ty_line(
          x1 = lv_x1 y1 = lv_y1 x2 = lv_x2 y2 = lv_y2
          color = is_ship-color width = 2 + iv_pulse
        ) TO cs_frame-lines.
      ENDIF.
    ENDLOOP.

    " Ship name label
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = 320 y = 380 text = is_ship-name
      color = is_ship-color size = 16 align = 'center'
    ) TO cs_frame-texts.
  ENDMETHOD.

ENDCLASS.
