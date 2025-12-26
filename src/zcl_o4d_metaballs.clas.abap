"! @title METABALLS - Classic blob effect with distance field
"!
"! ╔══════════════════════════════════════════════════════════════════╗
"! ║                         TWEAK GUIDE                              ║
"! ╠══════════════════════════════════════════════════════════════════╣
"! ║  PARAMETER      │ LOCATION           │ EFFECT                   ║
"! ╠══════════════════════════════════════════════════════════════════╣
"! ║  mv_num_balls   │ constructor/6      │ Number of metaballs      ║
"! ║  mv_scale       │ constructor/16     │ Pixel block size         ║
"! ║  mv_threshold   │ DATA/1.0           │ Blob visibility threshold║
"! ║  speed_x        │ 0.3 + (i%3)*0.2    │ Horizontal movement speed║
"! ║  speed_y        │ 0.3 + ((i+1)%3)*15 │ Vertical movement speed  ║
"! ║  radius         │ 0.1 + (i%4)*0.03   │ Ball influence radius    ║
"! ║  hue            │ i * 360/num_balls  │ Color per ball           ║
"! ║  ball_x/y       │ 0.5 + 0.4*sin(t*)  │ Ball orbit pattern       ║
"! ║  lv_contrib     │ r²/dist²           │ Distance field formula   ║
"! ║  saturation     │ 0.6 + 0.4*intens   │ Color saturation         ║
"! ║  val base       │ 0.4 + 0.6*intens   │ Base brightness          ║
"! ║  val pulse      │ * (0.8 + pulse*0.2)│ Beat brightness boost    ║
"! ╠══════════════════════════════════════════════════════════════════╣
"! ║  BEAT SYNC: pulse boosts overall brightness                     ║
"! ║  TIP: Lower threshold (0.5) = larger blobs                      ║
"! ║  TIP: Higher threshold (1.5) = smaller, sharper blobs           ║
"! ║  TIP: Increase radius for gooier, more merged blobs             ║
"! ╚══════════════════════════════════════════════════════════════════╝
CLASS zcl_o4d_metaballs DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    METHODS constructor
      IMPORTING iv_num_balls TYPE i DEFAULT 6
                iv_scale TYPE i DEFAULT 16.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_ball,
             speed_x TYPE f,
             speed_y TYPE f,
             radius  TYPE f,
             hue     TYPE f,
           END OF ty_ball,
           tt_balls TYPE STANDARD TABLE OF ty_ball WITH EMPTY KEY.

    "--- TWEAK: Core parameters ---
    DATA: mv_num_balls TYPE i,          " Number of balls (4-10)
          mv_scale     TYPE i,          " Pixel block size (8-24)
          mt_balls     TYPE tt_balls,   " Ball configurations
          mv_threshold TYPE f VALUE '1.0', " Visibility threshold
          mv_vary_size TYPE abap_bool VALUE abap_true. " Vary circle size by intensity

    METHODS: init_balls,
             hsv_to_rgb IMPORTING iv_h TYPE f iv_s TYPE f iv_v TYPE f
                        EXPORTING ev_r TYPE i ev_g TYPE i ev_b TYPE i.
ENDCLASS.

CLASS zcl_o4d_metaballs IMPLEMENTATION.
  METHOD constructor.
    mv_num_balls = iv_num_balls.
    mv_scale = iv_scale.
    init_balls( ).
  ENDMETHOD.

  METHOD init_balls.
    CLEAR mt_balls.
    DO mv_num_balls TIMES.
      DATA(lv_i) = sy-index - 1.
      "--- TWEAK: Ball movement and size parameters ---
      APPEND VALUE ty_ball(
        speed_x = CONV f( '0.3' ) + ( lv_i MOD 3 ) * CONV f( '0.2' )    " X speed variation
        speed_y = CONV f( '0.3' ) + ( ( lv_i + 1 ) MOD 3 ) * CONV f( '0.15' ) " Y speed
        radius = CONV f( '0.1' ) + ( lv_i MOD 4 ) * CONV f( '0.03' )    " Size variation
        hue = CONV f( lv_i ) * 360 / mv_num_balls                       " Color spread
      ) TO mt_balls.
    ENDDO.
  ENDMETHOD.

  METHOD zif_o4d_effect~get_name. rv_name = 'metaballs'. ENDMETHOD.
  METHOD zif_o4d_effect~get_params. ENDMETHOD.
  METHOD zif_o4d_effect~set_param.
    CASE iv_name.
      WHEN 'num_balls'. mv_num_balls = CONV i( iv_value ). init_balls( ).
      WHEN 'scale'. mv_scale = CONV i( iv_value ).
      WHEN 'vary_size'. mv_vary_size = xsdbool( iv_value = 'true' OR iv_value = '1' OR iv_value = 'X' ).
    ENDCASE.
  ENDMETHOD.

  METHOD zif_o4d_effect~render.
    DATA(ls_ctx) = VALUE zif_o4d_effect=>ty_render_ctx(
      t = is_sync-time gt = is_sync-time bi = VALUE #( pulse = is_sync-intensity )
    ).
    rs_frame = zif_o4d_effect~render_frame( ls_ctx ).
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    DATA: lv_r TYPE i, lv_g TYPE i, lv_b TYPE i.
    DATA(lv_t) = is_ctx-t.
    DATA(lv_pulse) = is_ctx-bi-pulse.

    " Scan screen in blocks
    DATA(lv_cy) = 0.
    WHILE lv_cy < zif_o4d_effect=>c_height.
      DATA(lv_cx) = 0.
      WHILE lv_cx < zif_o4d_effect=>c_width.
        " Normalize coordinates to 0-1 range
        DATA(lv_px) = CONV f( lv_cx ) / zif_o4d_effect=>c_width.
        DATA(lv_py) = CONV f( lv_cy ) / zif_o4d_effect=>c_height.

        DATA(lv_total) = CONV f( 0 ).     " Total field strength
        DATA(lv_weighted) = CONV f( 0 ).  " Weighted hue sum

        "--- Calculate distance field contribution from each ball ---
        LOOP AT mt_balls INTO DATA(ls_ball).
          "--- TWEAK: Ball orbit pattern (Lissajous-like) ---
          DATA(lv_ball_x) = CONV f( '0.5' ) +
                           CONV f( '0.4' ) * sin( lv_t * ls_ball-speed_x * 2 + ls_ball-hue * CONV f( '0.01' ) ).
          DATA(lv_ball_y) = CONV f( '0.5' ) +
                           CONV f( '0.4' ) * cos( lv_t * ls_ball-speed_y * 2 + ls_ball-hue * CONV f( '0.02' ) ).

          " Distance from pixel to ball center
          DATA(lv_dx) = lv_px - lv_ball_x.
          DATA(lv_dy) = lv_py - lv_ball_y.
          DATA(lv_dist_sq) = lv_dx * lv_dx + lv_dy * lv_dy.
          IF lv_dist_sq < CONV f( '0.001' ). lv_dist_sq = CONV f( '0.001' ). ENDIF.

          "--- TWEAK: Metaball formula: contribution = radius² / distance² ---
          DATA(lv_contrib) = ( ls_ball-radius * ls_ball-radius ) / lv_dist_sq.
          lv_total = lv_total + lv_contrib.
          lv_weighted = lv_weighted + lv_contrib * ls_ball-hue.
        ENDLOOP.

        "--- TWEAK: Threshold determines blob edge ---
        IF lv_total > mv_threshold.
          DATA(lv_avg_hue) = CONV f( CONV i( lv_weighted / lv_total ) MOD 360 ).
          DATA(lv_intensity) = nmin( val1 = CONV f( 2 ) val2 = lv_total - mv_threshold ) / 2.

          "--- TWEAK: Color calculation with beat pulse ---
          DATA(lv_val) = ( CONV f( '0.4' ) + CONV f( '0.6' ) * lv_intensity ) *
                         ( CONV f( '0.8' ) + lv_pulse * CONV f( '0.2' ) ).

          hsv_to_rgb( EXPORTING iv_h = lv_avg_hue
                                iv_s = CONV f( '0.6' ) + CONV f( '0.4' ) * lv_intensity
                                iv_v = lv_val
                      IMPORTING ev_r = lv_r ev_g = lv_g ev_b = lv_b ).

          " Scale radius based on intensity (0.5 to 1.0 of base size)
          DATA(lv_radius) = COND f(
            WHEN mv_vary_size = abap_true
            THEN ( mv_scale / 2 ) * ( CONV f( '0.5' ) + CONV f( '0.5' ) * lv_intensity )
            ELSE mv_scale / 2 ).

          APPEND VALUE zif_o4d_effect=>ty_circle(
            x = lv_cx + mv_scale / 2
            y = lv_cy + mv_scale / 2
            radius = lv_radius
            fill = |rgb({ lv_r },{ lv_g },{ lv_b })|
          ) TO rs_frame-circles.
        ENDIF.

        lv_cx = lv_cx + mv_scale.
      ENDWHILE.
      lv_cy = lv_cy + mv_scale.
    ENDWHILE.

    " Debug vars - metaball state
    DATA(lv_ball0_x) = CONV f( '0.5' ) + CONV f( '0.4' ) * sin( lv_t * CONV f( '0.6' ) ).
    DATA(lv_ball0_y) = CONV f( '0.5' ) + CONV f( '0.4' ) * cos( lv_t * CONV f( '0.6' ) ).
    rs_frame-debug-vars = |\{"num_balls":{ mv_num_balls },"scale":{ mv_scale },| &&
      |"threshold":{ mv_threshold },"pulse":{ lv_pulse },| &&
      |"ball0_x":{ lv_ball0_x },"ball0_y":{ lv_ball0_y },| &&
      |"orbit_amp":0.4,"center":0.5,"radii":[0.1,0.13,0.16,0.1]\}|.
  ENDMETHOD.

  METHOD hsv_to_rgb.
    DATA: lv_c TYPE f, lv_x TYPE f, lv_m TYPE f, lv_r TYPE f, lv_g TYPE f, lv_b TYPE f.
    lv_c = iv_v * iv_s.
    DATA(lv_hp) = iv_h / 60.
    DATA(lv_hi) = floor( lv_hp ).
    lv_x = lv_c * ( 1 - abs( frac( lv_hp / 2 ) * 2 - 1 ) ).
    lv_m = iv_v - lv_c.
    CASE lv_hi MOD 6.
      WHEN 0. lv_r = lv_c. lv_g = lv_x. lv_b = 0.
      WHEN 1. lv_r = lv_x. lv_g = lv_c. lv_b = 0.
      WHEN 2. lv_r = 0. lv_g = lv_c. lv_b = lv_x.
      WHEN 3. lv_r = 0. lv_g = lv_x. lv_b = lv_c.
      WHEN 4. lv_r = lv_x. lv_g = 0. lv_b = lv_c.
      WHEN 5. lv_r = lv_c. lv_g = 0. lv_b = lv_x.
    ENDCASE.
    ev_r = CONV i( ( lv_r + lv_m ) * 255 ).
    ev_g = CONV i( ( lv_g + lv_m ) * 255 ).
    ev_b = CONV i( ( lv_b + lv_m ) * 255 ).
  ENDMETHOD.
ENDCLASS.
