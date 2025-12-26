"! @title COPPERBARS - Classic Amiga copper bars effect
"!
"! ╔══════════════════════════════════════════════════════════════════╗
"! ║                         TWEAK GUIDE                              ║
"! ╠══════════════════════════════════════════════════════════════════╣
"! ║  PARAMETER      │ LOCATION           │ EFFECT                   ║
"! ╠══════════════════════════════════════════════════════════════════╣
"! ║  mv_num_bars    │ constructor/8      │ Number of bars (4-16)    ║
"! ║  mv_bar_height  │ constructor/30     │ Bar thickness in pixels  ║
"! ║  mv_speed       │ constructor/2.0    │ Vertical oscillation spd ║
"! ║  lv_phase       │ 2*PI/num_bars      │ Phase offset between bars║
"! ║  lv_y_center    │ height/3           │ Oscillation amplitude    ║
"! ║  hue offset     │ lv_i * 45          │ Color separation (45°)   ║
"! ║  hue cycle      │ lv_t * 50          │ Color cycling speed      ║
"! ║  saturation     │ 0.8                │ Color saturation         ║
"! ║  val base       │ 0.7                │ Base brightness          ║
"! ║  val pulse      │ + pulse * 0.3      │ Beat brightness boost    ║
"! ╠══════════════════════════════════════════════════════════════════╣
"! ║  BEAT SYNC: pulse boosts brightness of all bars                 ║
"! ║  TIP: Try mv_num_bars=12, mv_speed=3.0 for faster animation     ║
"! ╚══════════════════════════════════════════════════════════════════╝
CLASS zcl_o4d_copperbars DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    METHODS constructor
      IMPORTING iv_num_bars TYPE i DEFAULT 8
                iv_bar_height TYPE i DEFAULT 30
                iv_speed TYPE f DEFAULT '2.0'.

  PRIVATE SECTION.
    CONSTANTS: c_pi TYPE f VALUE '3.14159265'.
    "--- TWEAK: Core parameters ---
    DATA: mv_num_bars   TYPE i,  " Number of bars (4-16 recommended)
          mv_bar_height TYPE i,  " Bar height in pixels
          mv_speed      TYPE f.  " Oscillation speed
    METHODS hsv_to_rgb
      IMPORTING iv_h TYPE f iv_s TYPE f iv_v TYPE f
      EXPORTING ev_r TYPE i ev_g TYPE i ev_b TYPE i.
ENDCLASS.



CLASS ZCL_O4D_COPPERBARS IMPLEMENTATION.


  METHOD constructor.
    mv_num_bars = iv_num_bars.
    mv_bar_height = iv_bar_height.
    mv_speed = iv_speed.
  ENDMETHOD.


  METHOD zif_o4d_effect~get_name. rv_name = 'copperbars'. ENDMETHOD.


  METHOD zif_o4d_effect~get_params. ENDMETHOD.


  METHOD zif_o4d_effect~set_param.
    CASE iv_name.
      WHEN 'num_bars'. mv_num_bars = CONV i( iv_value ).
      WHEN 'bar_height'. mv_bar_height = CONV i( iv_value ).
      WHEN 'speed'. mv_speed = CONV f( iv_value ).
    ENDCASE.
  ENDMETHOD.


  METHOD zif_o4d_effect~render.
    DATA(ls_ctx) = VALUE zif_o4d_effect=>ty_render_ctx(
      t = is_sync-time gt = is_sync-time
      bi = VALUE #( pulse = is_sync-intensity )
    ).
    rs_frame = zif_o4d_effect~render_frame( ls_ctx ).
  ENDMETHOD.


  METHOD zif_o4d_effect~render_frame.
    DATA: lv_r TYPE i, lv_g TYPE i, lv_b TYPE i.
    DATA(lv_t) = is_ctx-t.
    DATA(lv_pulse) = is_ctx-bi-pulse.

    data(lv_speed) = mv_speed.

    DO mv_num_bars TIMES.
      DATA(lv_i) = sy-index - 1.

      "--- TWEAK: Phase offset creates wave pattern ---
      DATA(lv_phase) = CONV f( lv_i ) * 2 * c_pi / mv_num_bars .

      "--- TWEAK: Vertical oscillation (amplitude = height/3) ---
      DATA(lv_y_center) = zif_o4d_effect=>c_height / 2 +
                          sin( lv_t * lv_speed + lv_phase ) * ( zif_o4d_effect=>c_height / 3 ).

      " Draw each scanline of the bar
      DATA(lv_dy) = 0 - mv_bar_height / 2.
      WHILE lv_dy < mv_bar_height / 2.
        DATA(lv_y) = CONV i( lv_y_center ) + lv_dy.
        IF lv_y >= 0 AND lv_y < zif_o4d_effect=>c_height.
          "--- TWEAK: Intensity gradient (brightest at center) ---
          DATA(lv_intensity) = 1 - abs( CONV f( lv_dy ) ) / ( CONV f( mv_bar_height ) / 2 ).

          "--- TWEAK: Color calculation ---
          DATA(lv_hue) = CONV i( CONV f( lv_i ) * 15 + lv_t * 50 ) MOD 360.  " 45° per bar + cycling
          DATA(lv_val) = lv_intensity * ( CONV f( '0.7' ) + lv_pulse * CONV f( '0.3' ) ).

          hsv_to_rgb( EXPORTING iv_h = CONV f( lv_hue ) iv_s = CONV f( '0.8' ) iv_v = lv_val
                      IMPORTING ev_r = lv_r ev_g = lv_g ev_b = lv_b ).

          APPEND VALUE zif_o4d_effect=>ty_line(
            x1 = 0 y1 = lv_y x2 = zif_o4d_effect=>c_width y2 = lv_y
            width = 1 color = |rgb({ lv_r },{ lv_g },{ lv_b })|
          ) TO rs_frame-lines.
        ENDIF.
        lv_dy = lv_dy + 1.
      ENDWHILE.
    ENDDO.

    "--- FLASH on beat 2 of each bar (backbeat/snare) ---
    IF is_ctx-bi-pos_16 mod 16 = 8.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = '0.5' . "is_ctx-gbi-pulse * CONV f( '0.5' ).
      rs_frame-flash-r = CONV f( '1.0' ).
      rs_frame-flash-g = CONV f( '0.8' ).
      rs_frame-flash-b = CONV f( '0.6' ).  " Warm copper flash
    ENDIF.

    " CENTER TEXT: [ VIVID VIBES ]
    DATA(lv_txt_y) = zif_o4d_effect=>c_height / 2.
    APPEND VALUE zif_o4d_effect=>ty_rect(
      x = zif_o4d_effect=>c_width / 2 - 95  y = lv_txt_y - 18
      w = 190  h = 36  fill = 'rgba(0,0,0,0.7)'
    ) TO rs_frame-rects.
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = zif_o4d_effect=>c_width / 2  y = lv_txt_y + 6
      text = '[ VIVID VIBES ]'  color = '#FFFFFF'  size = 20  align = 'center'
    ) TO rs_frame-texts.

    " Debug vars - copper bar state
    DATA(lv_bar0_y) = zif_o4d_effect=>c_height / 2 + sin( lv_t * lv_speed ) * ( zif_o4d_effect=>c_height / 3 ).
    DATA(lv_hue0) = CONV i( lv_t * 50 ) MOD 360.
    rs_frame-debug-vars = |\{"num_bars":{ mv_num_bars },"bar_height":{ mv_bar_height },| &&
      |"speed":{ lv_speed },"pulse":{ lv_pulse },"bar0_y":{ lv_bar0_y },| &&
      |"hue":{ lv_hue0 },"hue_step":45,"amplitude":{ zif_o4d_effect=>c_height / 3 }\}|.
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
