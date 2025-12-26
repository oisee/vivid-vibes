CLASS zcl_o4d_amigaball2 DEFINITION PUBLIC FINAL CREATE PUBLIC.
*======================================================================
* AMIGA BALL V2 - Spin up, squash, and EXPLODE!
*======================================================================
* BAR-BASED TIMING (default 8 bar cycle):
*   Bars 0-50%: Phase 0 - Spin up (stop → fast spin)
*   Bars 50-75%: Phase 1 - Super spin + squash
*   Bars 75-87%: Phase 2 - EXPLODE!
*   Bars 87-100%: Phase 3 - Particles fade
*
* Usage:
*   DATA(lo_ball) = zcl_o4d_amigaball2=>new( 8 ).  " 8 bar cycle
*   DATA(lo_fast) = zcl_o4d_amigaball2=>new( 4 ).  " 4 bar cycle
*======================================================================
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.

    CLASS-METHODS new
      IMPORTING iv_cycle_bars TYPE i DEFAULT 8
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_o4d_amigaball2.

    METHODS constructor
      IMPORTING iv_cycle_bars TYPE i.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_tri_z, z TYPE f, tri TYPE zif_o4d_effect=>ty_triangle, END OF ty_tri_z.

    DATA mv_cycle_bars TYPE i VALUE 8.

    CLASS-METHODS i2h IMPORTING iv_i TYPE i RETURNING VALUE(rv) TYPE string.
ENDCLASS.



CLASS ZCL_O4D_AMIGABALL2 IMPLEMENTATION.


  METHOD new.
    ro_obj = NEW #( iv_cycle_bars ).
  ENDMETHOD.


  METHOD constructor.
    mv_cycle_bars = iv_cycle_bars.
    IF mv_cycle_bars < 2. mv_cycle_bars = 2. ENDIF.
  ENDMETHOD.


  METHOD zif_o4d_effect~get_name. rv_name = 'amiga_ball_2'. ENDMETHOD.


  METHOD zif_o4d_effect~get_params. ENDMETHOD.


  METHOD zif_o4d_effect~set_param. ENDMETHOD.


  METHOD zif_o4d_effect~render.
    rs_frame = zif_o4d_effect~render_frame( VALUE #(
      t = is_sync-time gt = is_sync-time
      gbi = VALUE #( bar = is_sync-bar bar_phase = is_sync-bar_phase
                     pulse = is_sync-intensity ) ) ).
  ENDMETHOD.


  METHOD zif_o4d_effect~render_frame.
    TYPES tt_triz TYPE STANDARD TABLE OF ty_tri_z WITH EMPTY KEY.
    DATA lt_tris TYPE tt_triz.

    DATA(lv_t) = is_ctx-gt.
    DATA(lv_w) = CONV f( zif_o4d_effect=>c_width ).
    DATA(lv_h) = CONV f( zif_o4d_effect=>c_height ).
    DATA(lv_pi) = zif_o4d_effect=>c_pi.

    " === BAR-BASED CYCLE ===
    DATA(lv_bar) = is_ctx-gbi-bar.
    DATA(lv_bar_phase) = is_ctx-gbi-bar_phase.

    " Position within cycle (0.0 to 1.0)
    DATA(lv_cycle_bar) = lv_bar MOD mv_cycle_bars.
    DATA(lv_cycle_pos) = ( CONV f( lv_cycle_bar ) + lv_bar_phase ) / mv_cycle_bars.

    " Phase thresholds (proportional to cycle)
    " 0-50%: spin up, 50-75%: squash, 75-87%: explode, 87-100%: fade
    DATA(lv_phase) = COND i(
      WHEN lv_cycle_pos < '0.50' THEN 0     " Spin up
      WHEN lv_cycle_pos < '0.75' THEN 1     " Super spin + squash
      WHEN lv_cycle_pos < '0.875' THEN 2    " Explode
      ELSE 3 ).                              " Particles fade

    " Phase progress (0.0 to 1.0 within each phase)
    DATA(lv_phase_prog) = COND f(
      WHEN lv_phase = 0 THEN lv_cycle_pos / '0.50'
      WHEN lv_phase = 1 THEN ( lv_cycle_pos - '0.50' ) / '0.25'
      WHEN lv_phase = 2 THEN ( lv_cycle_pos - '0.75' ) / '0.125'
      ELSE ( lv_cycle_pos - '0.875' ) / '0.125' ).

    " Sky gradient
    DATA lv_y TYPE i.
    lv_y = 0.
    WHILE lv_y < lv_h.
      DATA(lv_sky_b) = 80 + CONV i( CONV f( lv_y ) / lv_h * 120 ).
      APPEND VALUE zif_o4d_effect=>ty_line(
        x1 = 0 y1 = lv_y x2 = lv_w y2 = lv_y width = 1
        color = |#0050{ i2h( lv_sky_b ) }|
      ) TO rs_frame-lines.
      lv_y = lv_y + 4.
    ENDWHILE.

    " Ball parameters
    DATA(lv_r) = CONV f( 70 ).
    DATA(lv_cx) = 301."lv_w / 2.
    DATA(lv_cy) = 270."lv_h / 2 + 20.

    DATA lv_rot TYPE f.
    DATA lv_sq_y TYPE f.
    DATA lv_sq_x TYPE f.
    DATA lv_explode TYPE f.

    CASE lv_phase.
      WHEN 0.  " Spin up: 0 → max speed
        " Ease-in curve for acceleration
        DATA(lv_spin_factor) = lv_phase_prog * lv_phase_prog.  " Quadratic ease-in
        lv_rot = lv_t * lv_spin_factor * 6.  " Max rotation speed
        lv_sq_y = 1. lv_sq_x = 1.
        lv_explode = 0.

      WHEN 1.  " Super spin + squash
        lv_rot = lv_t * 6 + lv_phase_prog * 15.  " Even faster!
        lv_sq_y = 1 - lv_phase_prog * '0.5'.     " Squash vertically
        lv_sq_x = 1 + lv_phase_prog * '0.6'.     " Stretch horizontally
        lv_explode = 0.

      WHEN 2.  " EXPLODE!
        lv_explode = lv_phase_prog.
        lv_rot = lv_t * 8.
        lv_sq_y = '0.5' * ( 1 - lv_explode ).
        lv_sq_x = '1.6' * ( 1 - lv_explode ).

      WHEN OTHERS.
        lv_explode = 1.
    ENDCASE.

    IF lv_phase < 3.
      DATA(lv_cos_rot) = cos( lv_rot ).
      DATA(lv_sin_rot) = sin( lv_rot ).

      DATA(lv_lat_n) = 8.
      DATA(lv_lon_n) = 16.

      DATA lv_lat TYPE i.
      DATA lv_lon TYPE i.
      lv_lat = 0.
      WHILE lv_lat < lv_lat_n.
        lv_lon = 0.
        WHILE lv_lon < lv_lon_n.
          DATA(lv_phi1) = ( CONV f( lv_lat ) / lv_lat_n - '0.5' ) * lv_pi.
          DATA(lv_phi2) = ( CONV f( lv_lat + 1 ) / lv_lat_n - '0.5' ) * lv_pi.
          DATA(lv_th1) = CONV f( lv_lon ) / lv_lon_n * lv_pi * 2.
          DATA(lv_th2) = CONV f( lv_lon + 1 ) / lv_lon_n * lv_pi * 2.

          DATA(lv_x00) = cos( lv_phi1 ) * cos( lv_th1 ). DATA(lv_y00) = sin( lv_phi1 ). DATA(lv_z00) = cos( lv_phi1 ) * sin( lv_th1 ).
          DATA(lv_x10) = cos( lv_phi1 ) * cos( lv_th2 ). DATA(lv_y10) = sin( lv_phi1 ). DATA(lv_z10) = cos( lv_phi1 ) * sin( lv_th2 ).
          DATA(lv_x11) = cos( lv_phi2 ) * cos( lv_th2 ). DATA(lv_y11) = sin( lv_phi2 ). DATA(lv_z11) = cos( lv_phi2 ) * sin( lv_th2 ).
          DATA(lv_x01) = cos( lv_phi2 ) * cos( lv_th1 ). DATA(lv_y01) = sin( lv_phi2 ). DATA(lv_z01) = cos( lv_phi2 ) * sin( lv_th1 ).

          DATA(lv_rx00) = lv_x00 * lv_cos_rot - lv_z00 * lv_sin_rot. DATA(lv_rz00) = lv_x00 * lv_sin_rot + lv_z00 * lv_cos_rot.
          DATA(lv_rx10) = lv_x10 * lv_cos_rot - lv_z10 * lv_sin_rot. DATA(lv_rz10) = lv_x10 * lv_sin_rot + lv_z10 * lv_cos_rot.
          DATA(lv_rx11) = lv_x11 * lv_cos_rot - lv_z11 * lv_sin_rot. DATA(lv_rz11) = lv_x11 * lv_sin_rot + lv_z11 * lv_cos_rot.
          DATA(lv_rx01) = lv_x01 * lv_cos_rot - lv_z01 * lv_sin_rot. DATA(lv_rz01) = lv_x01 * lv_sin_rot + lv_z01 * lv_cos_rot.

          DATA(lv_z_avg) = ( lv_rz00 + lv_rz10 + lv_rz11 + lv_rz01 ) / 4.

          IF lv_z_avg > '-0.2'.
            DATA(lv_chk) = ( lv_lat + lv_lon ) MOD 2.
            DATA(lv_light) = '0.4' + '0.6' * ( lv_z_avg + 1 ) / 2.
            DATA lv_color TYPE string.
            IF lv_chk = 0.
              DATA(lv_rv) = i2h( CONV i( 220 * lv_light ) ).
              lv_color = |#{ lv_rv }1818|.
            ELSE.
              DATA(lv_wv) = i2h( CONV i( 240 * lv_light ) ).
              lv_color = |#{ lv_wv }{ lv_wv }{ lv_wv }|.
            ENDIF.

            DATA lv_ex TYPE f. DATA lv_ey TYPE f.
            IF lv_explode > 0.
              DATA(lv_exp_angle) = ( CONV f( lv_lat * lv_lon_n + lv_lon ) / ( lv_lat_n * lv_lon_n ) ) * lv_pi * 2.
              lv_ex = cos( lv_exp_angle ) * lv_explode * 150.
              lv_ey = sin( lv_exp_angle ) * lv_explode * 100 + lv_explode * lv_explode * 200.
            ELSE.
              lv_ex = 0. lv_ey = 0.
            ENDIF.

            DATA(lv_sx0) = lv_cx + lv_rx00 * lv_r * lv_sq_x + lv_ex. DATA(lv_sy0) = lv_cy + lv_y00 * lv_r * lv_sq_y + lv_ey.
            DATA(lv_sx1) = lv_cx + lv_rx10 * lv_r * lv_sq_x + lv_ex. DATA(lv_sy1) = lv_cy + lv_y10 * lv_r * lv_sq_y + lv_ey.
            DATA(lv_sx2) = lv_cx + lv_rx11 * lv_r * lv_sq_x + lv_ex. DATA(lv_sy2) = lv_cy + lv_y11 * lv_r * lv_sq_y + lv_ey.
            DATA(lv_sx3) = lv_cx + lv_rx01 * lv_r * lv_sq_x + lv_ex. DATA(lv_sy3) = lv_cy + lv_y01 * lv_r * lv_sq_y + lv_ey.

            APPEND VALUE ty_tri_z( z = lv_z_avg tri = VALUE #(
              x1 = lv_sx0 y1 = lv_sy0 x2 = lv_sx1 y2 = lv_sy1 x3 = lv_sx2 y3 = lv_sy2 fill = lv_color )
            ) TO lt_tris.
            APPEND VALUE ty_tri_z( z = lv_z_avg tri = VALUE #(
              x1 = lv_sx0 y1 = lv_sy0 x2 = lv_sx2 y2 = lv_sy2 x3 = lv_sx3 y3 = lv_sy3 fill = lv_color )
            ) TO lt_tris.
          ENDIF.
          lv_lon = lv_lon + 1.
        ENDWHILE.
        lv_lat = lv_lat + 1.
      ENDWHILE.

      SORT lt_tris BY z ASCENDING.
      LOOP AT lt_tris INTO DATA(ls_t).
        APPEND ls_t-tri TO rs_frame-triangles.
      ENDLOOP.
    ELSE.
      " Phase 3: Scattered particles
      DATA(lv_fade) = lv_phase_prog.
      DATA(lv_seed) = CONV f( '0.391' ).
      DO 40 TIMES.
        DATA(lv_i) = sy-index.
        lv_seed = frac( sin( lv_seed * 12345 + CONV f( lv_i ) ) * 43758 ).
        DATA(lv_px) = lv_cx + ( lv_seed - '0.5' ) * 300.
        lv_seed = frac( sin( lv_seed * 54321 ) * 43758 ).
        DATA(lv_py) = lv_cy + lv_seed * 100 + lv_fade * 200.
        DATA(lv_psize) = CONV i( 6 * ( 1 - lv_fade ) ).
        IF lv_psize > 0 AND lv_py < lv_h.
          DATA(lv_pchk) = lv_i MOD 2.
          DATA(lv_pcolor) = COND string( WHEN lv_pchk = 0 THEN '#DD2020' ELSE '#EEEEEE' ).
          APPEND VALUE zif_o4d_effect=>ty_rect(
            x = lv_px y = lv_py w = lv_psize h = lv_psize fill = lv_pcolor
          ) TO rs_frame-rects.
        ENDIF.
      ENDDO.
    ENDIF.

    " Title with cycle info
    DATA(lv_title) = COND string(
      WHEN lv_phase = 0 THEN |SPIN UP { CONV i( lv_phase_prog * 100 ) }%|
      WHEN lv_phase = 1 THEN 'FASTER! FASTER!'
      WHEN lv_phase = 2 THEN 'BOOM!'
      ELSE 'RIP' ).
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_w / 2 y = 25 text = lv_title color = '#ffffff' size = 14 align = 'center'
    ) TO rs_frame-texts.

    " Beat flash during spin-up
    IF lv_phase = 0 AND is_ctx-gbi-pulse > '0.5'.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = '0.1' * lv_phase_prog.
      rs_frame-flash-r = 1. rs_frame-flash-g = '0.3'. rs_frame-flash-b = '0.3'.
    ENDIF.

    " Explosion flash
    IF lv_phase = 2 AND lv_phase_prog < '0.3'.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = '0.5' * ( 1 - lv_phase_prog / '0.3' ).
      rs_frame-flash-r = 1. rs_frame-flash-g = 1. rs_frame-flash-b = '0.8'.
    ENDIF.

    rs_frame-debug-vars = |\{"x":{ lv_cx }, "y":{ lv_cy }\}|.
  ENDMETHOD.


  METHOD i2h.
    DATA lv_hex TYPE x LENGTH 1.
    DATA(lv_v) = nmax( val1 = 0 val2 = nmin( val1 = 255 val2 = iv_i ) ).
    lv_hex = lv_v. rv = |{ lv_hex }|.
  ENDMETHOD.
ENDCLASS.
