"! @title IGNITE_EMIT - Particle explosion with 2x burst on kick
CLASS zcl_o4d_ignite_emit DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_O4D_IGNITE_EMIT IMPLEMENTATION.


  METHOD zif_o4d_effect~get_name.
    rv_name = 'ignite_emit'.
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
    DATA(lv_pulse) = is_ctx-gbi-pulse.
    DATA(lv_w) = CONV f( zif_o4d_effect=>c_width ).
    DATA(lv_h) = CONV f( zif_o4d_effect=>c_height ).
    DATA(lv_cx) = lv_w / 2.
    DATA(lv_cy) = lv_h / 2.

    "--- BASE particles + BURST on kick ---
    DATA(lv_base_particles) = nmin( val1 = 300 val2 = CONV i( 80 + lv_t * 40 ) ).

    " On kick (pulse > 0.7): emit 2x more particles!
    DATA(lv_kick) = COND f( WHEN lv_pulse > CONV f( '0.7' ) THEN lv_pulse ELSE 0 ).
    DATA(lv_burst_particles) = CONV i( lv_base_particles * lv_kick ).
    DATA(lv_num_particles) = lv_base_particles + lv_burst_particles.

    DATA(lv_seed) = CONV f( '0.7391' ).

    DO lv_num_particles TIMES.
      DATA(lv_i) = sy-index.
      DATA(lv_is_burst) = xsdbool( lv_i > lv_base_particles ).

      " Pseudo-random angle and speed
      lv_seed = frac( sin( lv_seed * 12345 + CONV f( lv_i ) ) * 43758 ).
      DATA(lv_angle) = lv_seed * 2 * lc_pi.

      lv_seed = frac( sin( lv_seed * 54321 ) * 43758 ).

      " Burst particles are FASTER
      DATA(lv_speed) = COND f(
        WHEN lv_is_burst = abap_true
        THEN 100 + lv_seed * 150  " Fast burst: 100-250
        ELSE 50 + lv_seed * 100   " Normal: 50-150
      ).

      " Distance with wrap-around
      DATA(lv_dist) = lv_t * lv_speed.
      DATA(lv_wrap) = COND f( WHEN lv_is_burst = abap_true THEN 150 ELSE 200 ).
      lv_dist = lv_dist - floor( lv_dist / lv_wrap ) * lv_wrap.

      " Position
      DATA(lv_x) = lv_cx + cos( lv_angle ) * lv_dist.
      DATA(lv_y) = lv_cy + sin( lv_angle ) * lv_dist * CONV f( '0.6' ).

      " Alpha fadeout
      DATA(lv_alpha) = nmax( val1 = CONV f( 0 ) val2 = 1 - lv_dist / lv_wrap ).

      " Colors - burst particles are BRIGHTER (white-hot)
      lv_seed = frac( sin( lv_seed * 98765 ) * 43758 ).
      DATA(lv_r) = CONV i( 0 ).
      DATA(lv_g) = CONV i( 0 ).
      DATA(lv_b) = CONV i( 0 ).

      IF lv_is_burst = abap_true.
        " White-yellow burst
        lv_r = 255.
        lv_g = nmin( val1 = 255 val2 = CONV i( 200 + lv_seed * 55 ) ).
        lv_b = CONV i( 150 * lv_alpha ).
      ELSE.
        " Normal orange/red
        lv_r = nmin( val1 = 255 val2 = CONV i( 200 + lv_seed * 55 ) ).
        lv_g = CONV i( ( 100 + lv_seed * 100 ) * lv_alpha ).
        lv_b = CONV i( lv_seed * 50 * lv_alpha ).
      ENDIF.

      DATA(lv_size) = nmax( val1 = 1 val2 = CONV i( 3 * lv_alpha ) ).
      IF lv_is_burst = abap_true. lv_size = lv_size + 1. ENDIF.

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

    " Center glow - BIGGER on kick
    DATA(lv_glow_r) = 20 + sin( lv_t * 10 ) * 5 + lv_kick * 15.

    " Outer glow rays
    DO 8 TIMES.
      DATA(lv_ga) = CONV f( sy-index - 1 ) / 8 * 2 * lc_pi.
      DATA(lv_gx) = lv_cx + cos( lv_ga ) * lv_glow_r.
      DATA(lv_gy) = lv_cy + sin( lv_ga ) * lv_glow_r.
      APPEND VALUE zif_o4d_effect=>ty_line(
        x1 = lv_cx  y1 = lv_cy  x2 = lv_gx  y2 = lv_gy
        color = '#FF8000'  width = 4
      ) TO rs_frame-lines.
    ENDDO.

    " Inner rotating rays - MORE on kick
    DATA(lv_inner_rays) = COND i( WHEN lv_kick > 0 THEN 12 ELSE 6 ).
    DO lv_inner_rays TIMES.
      lv_ga = CONV f( sy-index - 1 ) / lv_inner_rays * 2 * lc_pi + lv_t.
      lv_gx = lv_cx + cos( lv_ga ) * lv_glow_r * CONV f( '0.6' ).
      lv_gy = lv_cy + sin( lv_ga ) * lv_glow_r * CONV f( '0.6' ).
      DATA(lv_ray_color) = COND string( WHEN lv_kick > 0 THEN '#FFFFFF' ELSE '#FFCC00' ).
      APPEND VALUE zif_o4d_effect=>ty_line(
        x1 = lv_cx  y1 = lv_cy  x2 = lv_gx  y2 = lv_gy
        color = lv_ray_color  width = 3
      ) TO rs_frame-lines.
    ENDDO.

    " Bright core - BIGGER on kick
    DATA(lv_core_size) = COND i( WHEN lv_kick > 0 THEN 12 ELSE 8 ).
    APPEND VALUE zif_o4d_effect=>ty_rect(
      x = lv_cx - lv_core_size / 2  y = lv_cy - lv_core_size / 2
      w = lv_core_size  h = lv_core_size
      fill = '#FFFFFF'
    ) TO rs_frame-rects.

    " FLASH on beat 2 of each bar (backbeat/snare)
    IF is_ctx-bi-pos_16 mod 16 = 8.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = 1.
      rs_frame-flash-r = CONV f( '1.0' ).
      rs_frame-flash-g = CONV f( '0.6' ).
      rs_frame-flash-b = CONV f( '0.2' ).
    ENDIF.

    " Debug
    rs_frame-debug-vars = |\{"base":{ lv_base_particles },"burst":{ lv_burst_particles },| &&
      |"total":{ lv_num_particles },"kick":{ lv_kick },"pulse":{ lv_pulse }\}|.
  ENDMETHOD.
ENDCLASS.
