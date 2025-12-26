"! @title PLASMA Effect - Classic demoscene plasma
"!
"! ╔══════════════════════════════════════════════════════════════════╗
"! ║                         TWEAK GUIDE                              ║
"! ╠══════════════════════════════════════════════════════════════════╣
"! ║  PARAMETER      │ LOCATION           │ EFFECT                   ║
"! ╠══════════════════════════════════════════════════════════════════╣
"! ║  mv_scale       │ constructor/20     │ Pixel block size (20=    ║
"! ║                 │                    │ chunky, 10=smooth)       ║
"! ║  lv_v1 divisor  │ render_frame/5     │ Horizontal wave freq     ║
"! ║  lv_v2 divisor  │ render_frame/3     │ Vertical wave freq       ║
"! ║  lv_v3 divisor  │ render_frame/5     │ Diagonal wave freq       ║
"! ║  lv_v4 divisor  │ render_frame/5     │ Radial wave freq         ║
"! ║  lv_t1 multiply │ render_frame/*1.5  │ Wave 1-3 animation speed ║
"! ║  lv_t2 multiply │ render_frame/*1.2  │ Wave 4 animation speed   ║
"! ║  hue offset     │ render_frame/*25   │ Color cycling speed      ║
"! ║  saturation     │ hsv_to_rgb/0.9     │ Color intensity          ║
"! ║  val base       │ render_frame/0.65  │ Base brightness          ║
"! ╠══════════════════════════════════════════════════════════════════╣
"! ║  BEAT SYNC: lv_pulse (0-1) boosts brightness on beat            ║
"! ║  Formula: lv_val = 0.65 + pulse * 0.35                          ║
"! ╚══════════════════════════════════════════════════════════════════╝
CLASS zcl_o4d_plasma DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    METHODS constructor
      IMPORTING iv_width TYPE i DEFAULT 640 iv_height TYPE i DEFAULT 400 iv_scale TYPE i DEFAULT 20.

  PRIVATE SECTION.
    DATA: mv_width TYPE i, mv_height TYPE i, mv_scale TYPE i,
          mt_sin TYPE STANDARD TABLE OF f WITH EMPTY KEY.
    METHODS: hsv_to_rgb IMPORTING iv_h TYPE f iv_s TYPE f iv_v TYPE f
                        EXPORTING ev_r TYPE i ev_g TYPE i ev_b TYPE i,
             init_sin_table.
ENDCLASS.



CLASS ZCL_O4D_PLASMA IMPLEMENTATION.


  METHOD constructor.
    mv_width = iv_width. mv_height = iv_height. mv_scale = iv_scale.
    init_sin_table( ).
  ENDMETHOD.


  METHOD init_sin_table.
    DO 256 TIMES.
      APPEND sin( ( sy-index - 1 ) * CONV f( '6.283185' ) / 256 ) TO mt_sin.
    ENDDO.
  ENDMETHOD.


  METHOD zif_o4d_effect~get_name. rv_name = 'plasma'. ENDMETHOD.


  METHOD zif_o4d_effect~get_params. ENDMETHOD.


  METHOD zif_o4d_effect~set_param.
    IF iv_name = 'scale'. mv_scale = CONV i( iv_value ). ENDIF.
  ENDMETHOD.


  METHOD zif_o4d_effect~render.
    DATA(ls_ctx) = VALUE zif_o4d_effect=>ty_render_ctx(
      t = is_sync-time gt = is_sync-time
      bi = VALUE #( pulse = is_sync-intensity )
    ).
    rs_frame = zif_o4d_effect~render_frame( ls_ctx ).
  ENDMETHOD.


  METHOD zif_o4d_effect~render_frame.
    DATA: lv_v1 TYPE f, lv_v2 TYPE f, lv_v3 TYPE f, lv_v4 TYPE f,
          lv_r TYPE i, lv_g TYPE i, lv_b TYPE i, lv_idx TYPE i,
          lv_cx TYPE i, lv_cy TYPE i, lv_x TYPE i, lv_y TYPE i.

    "--- TWEAK: Animation speeds ---
    DATA(lv_t1) = is_ctx-t * CONV f( '1.5' ).  " Wave 1-3 speed
    DATA(lv_t2) = is_ctx-t * CONV f( '1.2' ).  " Wave 4 speed
    IF is_ctx-gbi-pos_16 mod 16 = 8.
      DATA(lv_pulse) = '1.5'. "is_ctx-bi-pulse.
    ELSE.
      lv_pulse = '0.4'. "is_ctx-bi-pulse.
    ENDIF.

    lv_cy = 0.
    WHILE lv_cy < mv_height.
      lv_cx = 0.
      WHILE lv_cx < mv_width.
        lv_x = lv_cx - mv_width / 2.
        lv_y = lv_cy - mv_height / 2.

        "--- TWEAK: Plasma wave frequencies (divisors) ---
        " Wave 1: Horizontal (divisor 5 = medium frequency)
        lv_idx = ( lv_x / 5 + 128 ) MOD 256. IF lv_idx < 0. lv_idx = lv_idx + 256. ENDIF.
        READ TABLE mt_sin INDEX lv_idx + 1 INTO lv_v1.

        " Wave 2: Vertical + time (divisor 3 = higher frequency)
        lv_idx = ( lv_y / 3 + CONV i( lv_t1 * 35 ) ) MOD 256. IF lv_idx < 0. lv_idx = lv_idx + 256. ENDIF.
        READ TABLE mt_sin INDEX lv_idx + 1 INTO lv_v2.

        " Wave 3: Diagonal (x+y)
        lv_idx = ( ( lv_x + lv_y ) / 5 + CONV i( lv_t2 * 25 ) ) MOD 256. IF lv_idx < 0. lv_idx = lv_idx + 256. ENDIF.
        READ TABLE mt_sin INDEX lv_idx + 1 INTO lv_v3.

        " Wave 4: Radial (distance from center)
        lv_idx = CONV i( sqrt( CONV f( lv_x * lv_x + lv_y * lv_y ) ) ) / 5.
        lv_idx = ( lv_idx + CONV i( lv_t1 * 18 ) ) MOD 256. IF lv_idx < 0. lv_idx = lv_idx + 256. ENDIF.
        READ TABLE mt_sin INDEX lv_idx + 1 INTO lv_v4.

        "--- TWEAK: Color calculation ---
        DATA(lv_color) = ( lv_v1 + lv_v2 + lv_v3 + lv_v4 + 4 ) / 8.
        DATA(lv_hue) = CONV i( lv_color * 360 + is_ctx-t * 25 ) MOD 360.  " *25 = color cycle speed
        DATA(lv_val) = CONV f( '0.65' ) + lv_pulse * CONV f( '0.35' ).    " Beat brightness boost

        hsv_to_rgb( EXPORTING iv_h = CONV f( lv_hue ) iv_s = CONV f( '0.9' ) iv_v = lv_val
                    IMPORTING ev_r = lv_r ev_g = lv_g ev_b = lv_b ).

        APPEND VALUE zif_o4d_effect=>ty_rect(
          x = lv_cx y = lv_cy w = mv_scale h = mv_scale
          fill = |rgb({ lv_r },{ lv_g },{ lv_b })|
        ) TO rs_frame-rects.

        lv_cx = lv_cx + mv_scale.
      ENDWHILE.
      lv_cy = lv_cy + mv_scale.
    ENDWHILE.

    " CENTER TEXT: [ DEMO ]
    APPEND VALUE zif_o4d_effect=>ty_rect(
      x = mv_width / 2 - 120  y = mv_height / 2 - 18
      w = 240  h = 36  fill = 'rgba(0,0,0,0.7)'
    ) TO rs_frame-rects.
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = mv_width / 2  y = mv_height / 2 + 6
      text = '[ DEMO for SAP ABAP ]'  color = '#FFFFFF'  size = 20  align = 'center'
    ) TO rs_frame-texts.

    " Debug vars - tweakable parameters and derived values
    DATA(lv_hue_cycle) = CONV i( is_ctx-t * 25 ) MOD 360.
    rs_frame-debug-vars = |\{"scale":{ mv_scale },"t1":{ lv_t1 },"t2":{ lv_t2 },| &&
      |"pulse":{ lv_pulse },"hue_cycle":{ lv_hue_cycle },"grid_w":{ mv_width / mv_scale },| &&
      |"grid_h":{ mv_height / mv_scale },"wave_freqs":[5,3,5,5],"color_speed":25\}|.
  ENDMETHOD.


  METHOD hsv_to_rgb.
    DATA: lv_c TYPE f, lv_x TYPE f, lv_m TYPE f, lv_r TYPE f, lv_g TYPE f, lv_b TYPE f.
    lv_c = iv_v * iv_s.
    DATA(lv_hp) = iv_h / 60. DATA(lv_hi) = floor( lv_hp ).
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
    ev_r = CONV i( ( lv_r + lv_m ) * 255 ). ev_g = CONV i( ( lv_g + lv_m ) * 255 ). ev_b = CONV i( ( lv_b + lv_m ) * 255 ).
  ENDMETHOD.
ENDCLASS.
