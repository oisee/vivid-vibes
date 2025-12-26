"! @title IGNITION - Expanding particle explosion from center
"!
"! ╔══════════════════════════════════════════════════════════════════╗
"! ║                         TWEAK GUIDE                              ║
"! ╠══════════════════════════════════════════════════════════════════╣
"! ║  PARAMETER      │ LOCATION           │ EFFECT                   ║
"! ╠══════════════════════════════════════════════════════════════════╣
"! ║  lv_num_parts   │ 20 + t*30 (max100) │ Particle count over time ║
"! ║  lv_speed       │ 50 + seed*100      │ Particle speed (50-150)  ║
"! ║  lv_dist wrap   │ MOD 200            │ Distance wrap (respawn)  ║
"! ║  lv_y scale     │ * 0.6              │ Vertical squeeze (oval)  ║
"! ║  lv_alpha       │ 1 - dist/200       │ Fade with distance       ║
"! ║  lv_r base      │ 200 + seed*55      │ Red channel (200-255)    ║
"! ║  lv_g           │ (100+seed*100)*a   │ Green with alpha         ║
"! ║  lv_b           │ seed*50*alpha      │ Blue (sparse)            ║
"! ║  lv_glow_r      │ 20 + sin(t*10)*5   │ Center glow pulsation    ║
"! ║  outer rays     │ 8 lines, #FF8000   │ Orange outer glow        ║
"! ║  inner rays     │ 6 lines, #FFCC00   │ Yellow inner glow        ║
"! ║  core size      │ 8x8 white rect     │ Bright center            ║
"! ╠══════════════════════════════════════════════════════════════════╣
"! ║  TIP: Increase max particles (100→200) for denser explosion     ║
"! ║  TIP: Change wrap distance (200→400) for longer trails          ║
"! ╚══════════════════════════════════════════════════════════════════╝
CLASS zcl_o4d_ignition DEFINITION
  PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.

  PRIVATE SECTION.
    METHODS hsv_to_hex
      IMPORTING iv_h         TYPE f
                iv_s         TYPE f
                iv_v         TYPE f
      RETURNING VALUE(rv_hex) TYPE string.

ENDCLASS.



CLASS ZCL_O4D_IGNITION IMPLEMENTATION.


  METHOD zif_o4d_effect~get_name.
    rv_name = 'ignition'.
  ENDMETHOD.


  METHOD zif_o4d_effect~get_params.
  ENDMETHOD.


  METHOD zif_o4d_effect~set_param.
  ENDMETHOD.


  METHOD zif_o4d_effect~render.
    DATA(ls_ctx) = VALUE zif_o4d_effect=>ty_render_ctx(
      t  = is_sync-time  gt = is_sync-time
      bi = VALUE #( time = is_sync-time bar = is_sync-bar beat = is_sync-beat
                    bar_phase = is_sync-bar_phase pulse = is_sync-intensity )
    ).
    rs_frame = zif_o4d_effect~render_frame( ls_ctx ).
  ENDMETHOD.


  METHOD zif_o4d_effect~render_frame.
    CONSTANTS: lc_pi TYPE f VALUE '3.14159265'.
    DATA(lv_t) = is_ctx-t.
    DATA(lv_w) = CONV f( zif_o4d_effect=>c_width ).
    DATA(lv_h) = CONV f( zif_o4d_effect=>c_height ).
    DATA(lv_cx) = lv_w / 2.
    DATA(lv_cy) = lv_h / 2.

    "--- TWEAK: Particle count (grows over time, capped at 100) ---
    "DATA(lv_num_particles) = nmin( val1 = 100 val2 = CONV i( 20 + lv_t * 30 ) ).
    DATA(lv_num_particles) = nmin( val1 = 500 val2 = CONV i( 100 + lv_t * 60 ) ).
    DATA(lv_seed) = CONV f( '0.7391' ).  "--- TWEAK: Initial random seed

    DO lv_num_particles TIMES.
      DATA(lv_i) = sy-index.

      " Pseudo-random angle and speed using seed chaining
      lv_seed = frac( sin( lv_seed * 12345 + CONV f( lv_i ) ) * 43758 ).
      DATA(lv_angle) = lv_seed * 2 * lc_pi.

      lv_seed = frac( sin( lv_seed * 54321 ) * 43758 ).
      DATA(lv_speed) = 50 + lv_seed * 100.  "--- TWEAK: Speed range 50-150

      "--- TWEAK: Distance with wrap-around (respawns at center) ---
      DATA(lv_dist) = lv_t * lv_speed.
      lv_dist = lv_dist - floor( lv_dist / 200 ) * 200.  " Wrap at 200

      "--- TWEAK: Particle position (0.6 = oval squeeze) ---
      DATA(lv_x) = lv_cx + cos( lv_angle ) * lv_dist.
      DATA(lv_y) = lv_cy + sin( lv_angle ) * lv_dist * CONV f( '0.6' ).

      "--- TWEAK: Alpha fadeout with distance ---
      DATA(lv_alpha) = nmax( val1 = CONV f( 0 ) val2 = 1 - lv_dist / 200 ).

      "--- TWEAK: Fire colors (orange/yellow) ---
      lv_seed = frac( sin( lv_seed * 98765 ) * 43758 ).
      DATA(lv_r) = nmin( val1 = 255 val2 = CONV i( 200 + lv_seed * 55 ) ).  " 200-255 red
      DATA(lv_g) = CONV i( ( 100 + lv_seed * 100 ) * lv_alpha ).            " Green fades
      DATA(lv_b) = CONV i( lv_seed * 50 * lv_alpha ).                       " Sparse blue

      " Size based on alpha
      DATA(lv_size) = nmax( val1 = 1 val2 = CONV i( 3 * lv_alpha ) ).

      IF lv_alpha > CONV f( '0.05' ).
        DATA: lv_rh TYPE x LENGTH 1, lv_gh TYPE x LENGTH 1, lv_bh TYPE x LENGTH 1.
        lv_rh = lv_r. lv_gh = lv_g. lv_bh = lv_b.
        DATA(lv_color) = |#{ lv_rh }{ lv_gh }{ lv_bh }|.

        APPEND VALUE zif_o4d_effect=>ty_rect(
          x = lv_x  y = lv_y  w = lv_size  h = lv_size
          fill = lv_color
        ) TO rs_frame-rects.
      ENDIF.
    ENDDO.

    "--- TWEAK: Center glow radius (pulsates) ---
    DATA(lv_glow_r) = 20 + sin( lv_t * 10 ) * 5.

    "--- TWEAK: Outer glow rays (8 orange lines) ---
    DO 8 TIMES.
      DATA(lv_ga) = CONV f( sy-index - 1 ) / 8 * 2 * lc_pi.
      DATA(lv_gx) = lv_cx + cos( lv_ga ) * lv_glow_r.
      DATA(lv_gy) = lv_cy + sin( lv_ga ) * lv_glow_r.
      APPEND VALUE zif_o4d_effect=>ty_line(
        x1 = lv_cx  y1 = lv_cy  x2 = lv_gx  y2 = lv_gy
        color = '#FF8000'  width = 4  "--- TWEAK: Orange color, width
      ) TO rs_frame-lines.
    ENDDO.

    "--- TWEAK: Inner glow rays (6 yellow rotating lines) ---
    DO 6 TIMES.
      lv_ga = CONV f( sy-index - 1 ) / 6 * 2 * lc_pi + lv_t.  " Rotates!
      lv_gx = lv_cx + cos( lv_ga ) * lv_glow_r * CONV f( '0.6' ).
      lv_gy = lv_cy + sin( lv_ga ) * lv_glow_r * CONV f( '0.6' ).
      APPEND VALUE zif_o4d_effect=>ty_line(
        x1 = lv_cx  y1 = lv_cy  x2 = lv_gx  y2 = lv_gy
        color = '#FFCC00'  width = 3  "--- TWEAK: Yellow color
      ) TO rs_frame-lines.
    ENDDO.

    "--- TWEAK: Bright white core ---
    APPEND VALUE zif_o4d_effect=>ty_rect(
      x = lv_cx - 4  y = lv_cy - 4  w = 8  h = 8
      fill = '#FFFFFF'
    ) TO rs_frame-rects.

    "--- FLASH on beat 2 of each bar (backbeat/snare) ---
    IF is_ctx-bi-pos_16 mod 16 = 8.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = 1.
      rs_frame-flash-r = CONV f( '1.0' ).
      rs_frame-flash-g = CONV f( '0.6' ).
      rs_frame-flash-b = CONV f( '0.2' ).
    ENDIF.

    " Debug vars
    rs_frame-debug-vars = |\{"particles":{ lv_num_particles },"max_particles":100,| &&
      |"glow_r":{ lv_glow_r },"speed_range":[50,150],| &&
      |"wrap_dist":200,"pulse":{ is_ctx-bi-pulse }\}|.
  ENDMETHOD.


  METHOD hsv_to_hex.
    DATA: lv_r TYPE f, lv_g TYPE f, lv_b TYPE f.
    DATA(lv_hue) = iv_h / 60.
    DATA(lv_i) = floor( lv_hue ).
    DATA(lv_f) = lv_hue - lv_i.
    DATA(lv_p) = iv_v * ( 1 - iv_s ).
    DATA(lv_q) = iv_v * ( 1 - lv_f * iv_s ).
    DATA(lv_t) = iv_v * ( 1 - ( 1 - lv_f ) * iv_s ).

    CASE lv_i MOD 6.
      WHEN 0. lv_r = iv_v. lv_g = lv_t. lv_b = lv_p.
      WHEN 1. lv_r = lv_q. lv_g = iv_v. lv_b = lv_p.
      WHEN 2. lv_r = lv_p. lv_g = iv_v. lv_b = lv_t.
      WHEN 3. lv_r = lv_p. lv_g = lv_q. lv_b = iv_v.
      WHEN 4. lv_r = lv_t. lv_g = lv_p. lv_b = iv_v.
      WHEN 5. lv_r = iv_v. lv_g = lv_p. lv_b = lv_q.
    ENDCASE.

    DATA(lv_ri) = CONV i( lv_r * 255 ).
    DATA(lv_gi) = CONV i( lv_g * 255 ).
    DATA(lv_bi) = CONV i( lv_b * 255 ).

    DATA: lv_rh TYPE x LENGTH 1, lv_gh TYPE x LENGTH 1, lv_bh TYPE x LENGTH 1.
    lv_rh = lv_ri. lv_gh = lv_gi. lv_bh = lv_bi.
    rv_hex = |#{ lv_rh }{ lv_gh }{ lv_bh }|.
  ENDMETHOD.
ENDCLASS.
