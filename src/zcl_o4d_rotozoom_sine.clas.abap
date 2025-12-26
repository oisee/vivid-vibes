"! @title ROTOZOOM_SINE - Classic rotozoom with wandering center (Lissajous path)
CLASS zcl_o4d_rotozoom_sine DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    METHODS constructor
      IMPORTING iv_wander_x TYPE f DEFAULT '1.5'    " X sine frequency
                iv_wander_y TYPE f DEFAULT '2.0'    " Y sine frequency
                iv_amplitude TYPE f DEFAULT '100'.  " Wander amplitude
  PRIVATE SECTION.
    DATA: mv_wander_x  TYPE f,
          mv_wander_y  TYPE f,
          mv_amplitude TYPE f.
    METHODS hsv_to_hex
      IMPORTING iv_h TYPE f iv_s TYPE f iv_v TYPE f
      RETURNING VALUE(rv_hex) TYPE string.
    METHODS xor_int
      IMPORTING iv_a TYPE i iv_b TYPE i
      RETURNING VALUE(rv_xor) TYPE i.
ENDCLASS.



CLASS ZCL_O4D_ROTOZOOM_SINE IMPLEMENTATION.


  METHOD constructor.
    mv_wander_x = iv_wander_x.
    mv_wander_y = iv_wander_y.
    mv_amplitude = iv_amplitude.
  ENDMETHOD.


  METHOD zif_o4d_effect~get_name. rv_name = 'rotozoom_sine'. ENDMETHOD.


  METHOD zif_o4d_effect~get_params. ENDMETHOD.


  METHOD zif_o4d_effect~set_param.
    CASE iv_name.
      WHEN 'wander_x'. mv_wander_x = CONV f( iv_value ).
      WHEN 'wander_y'. mv_wander_y = CONV f( iv_value ).
      WHEN 'amplitude'. mv_amplitude = CONV f( iv_value ).
    ENDCASE.
  ENDMETHOD.


  METHOD zif_o4d_effect~render.
    rs_frame = zif_o4d_effect~render_frame( VALUE #(
      t = is_sync-time gt = is_sync-time bi = VALUE #( pulse = is_sync-intensity ) ) ).
  ENDMETHOD.


  METHOD zif_o4d_effect~render_frame.
    DATA(lv_t) = is_ctx-t.
    DATA(lv_pulse) = '1.0'. "is_ctx-gbi-pulse.
    DATA(lv_w) = CONV f( zif_o4d_effect=>c_width ).
    DATA(lv_h) = CONV f( zif_o4d_effect=>c_height ).


*    "data(lv_pos) = is_ctx-gbi-pos_16 mod 16 .
*    data(lv_pos) = is_ctx-gbi-pos_8 mod 8 .
*    if lv_pos = 4.
*      data(lv_boost) = conv f( '1.4' ).
*    ELSE.
*      lv_boost = '1'.
*    ENDIF.
*
*    "--- TWEAK: Rotation and zoom ---
*    DATA(lv_angle) = lv_t * CONV f( '1.2' ) * sin( ( lv_t ) / 3 ).           " Rotation speed
*    "DATA(lv_zoom) = CONV f( '0.8' ) + sin( lv_t ) * CONV f( '0.3' ).  " Zoom oscillation
*    DATA(lv_zoom) = CONV f( '4.8' ) + sin( lv_t / 4 ) * CONV f( '6' ) * lv_boost.  " Zoom oscillation
*    DATA(lv_cos_a) = cos( lv_angle ).
*    DATA(lv_sin_a) = sin( lv_angle ).

    " Texture and pixel size
    DATA(lv_tex_size) = 64.
    DATA(lv_scale) = 8.

    " === CENTER WANDERS ON LISSAJOUS PATH ===
    DATA(lv_cx) = lv_w / 2 + sin( lv_t * mv_wander_x ) * mv_amplitude.
    DATA(lv_cy) = lv_h / 2 + sin( lv_t * mv_wander_y ) * mv_amplitude * CONV f( '0.6' ).

    " Boost amplitude on kick
    DATA(lv_kick_amp) = '1.0'. "COND f( WHEN lv_pulse > CONV f( '0.7' ) THEN lv_pulse * 30 ELSE 0 ).
    lv_cx = lv_cx + sin( lv_t * 5 ) * lv_kick_amp.
    lv_cy = lv_cy + cos( lv_t * 5 ) * lv_kick_amp * CONV f( '0.6' ).

    " Rotation - direction changes with sine
    DATA(lv_rot_dir) = sin( lv_t / 3 ).
    DATA(lv_angle) = lv_t * CONV f( '1.5' ) * lv_rot_dir.

    " Zoom oscillates + pulse boost
    "DATA(lv_zoom_base) = CONV f( '3.0' ) + sin( lv_t * CONV f( '0.8' ) ) * CONV f( '2.0' ).
    DATA(lv_zoom_base) = CONV f( '4.8' ) + sin( lv_t / 4 ) * CONV f( '6.0' ).
    "DATA(lv_zoom) =     CONV f( '4.8' ) + sin( lv_t / 4 ) * CONV f( '6' )
    DATA(lv_zoom) = lv_zoom_base + lv_pulse * CONV f( '1.5' ).

    DATA(lv_cos_a) = cos( lv_angle ).
    DATA(lv_sin_a) = sin( lv_angle ).

    " Render XOR texture
    DATA(lv_y) = 0.
    WHILE lv_y < lv_h.
      DATA(lv_x) = 0.
      WHILE lv_x < lv_w.
        " Transform from wandering center
        DATA(lv_dx) = CONV f( lv_x ) - lv_cx.
        DATA(lv_dy) = CONV f( lv_y ) - lv_cy.

        DATA(lv_u) = ( lv_dx * lv_cos_a - lv_dy * lv_sin_a ) / lv_zoom.
        DATA(lv_v) = ( lv_dx * lv_sin_a + lv_dy * lv_cos_a ) / lv_zoom.

        " Texture coordinates with wrap
        DATA(lv_tx) = CONV i( lv_u ) MOD lv_tex_size.
        DATA(lv_ty) = CONV i( lv_v ) MOD lv_tex_size.
        IF lv_tx < 0. lv_tx = lv_tx + lv_tex_size. ENDIF.
        IF lv_ty < 0. lv_ty = lv_ty + lv_tex_size. ENDIF.

        " XOR pattern
        DATA(lv_xor) = xor_int( iv_a = lv_tx iv_b = lv_ty ).

        " Color cycling + distance from center affects hue
        DATA(lv_dist) = sqrt( lv_dx * lv_dx + lv_dy * lv_dy ).
        DATA(lv_hue) = CONV f( ( lv_xor * 4 + CONV i( lv_t * 25 ) + CONV i( lv_dist / 3 ) ) MOD 360 ).
        DATA(lv_light) = CONV f( 35 + lv_xor ) / 100.
        IF lv_light > 1. lv_light = 1. ENDIF.

        " Pulse brightens
        lv_light = lv_light + lv_pulse * CONV f( '0.15' ).
        IF lv_light > 1. lv_light = 1. ENDIF.

        DATA(lv_color) = hsv_to_hex( iv_h = lv_hue iv_s = CONV f( '0.85' ) iv_v = lv_light ).

        APPEND VALUE zif_o4d_effect=>ty_rect(
          x = lv_x y = lv_y w = lv_scale h = lv_scale
          fill = lv_color
        ) TO rs_frame-rects.

        lv_x = lv_x + lv_scale.
      ENDWHILE.
      lv_y = lv_y + lv_scale.
    ENDWHILE.

*    " Center marker (follows the Lissajous)
*    APPEND VALUE zif_o4d_effect=>ty_rect(
*      x = lv_cx - 3 y = lv_cy - 3 w = 6 h = 6
*      fill = '#FFFFFF'
*    ) TO rs_frame-rects.

    " Title
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_w / 2 y = 20 text = 'SINE ROTOZOOM'
      color = '#FFFFFF' size = 14 align = 'center'
    ) TO rs_frame-texts.

    "--- FLASH on beat 2 of each bar (backbeat/snare) ---
*    IF is_ctx-bi-pos_16 mod 16 = 8.
*      rs_frame-flash-active = abap_true.
*      rs_frame-flash-intensity = 1.
*      rs_frame-flash-r = CONV f( '1.0' ).
*      rs_frame-flash-g = CONV f( '0.6' ).
*      rs_frame-flash-b = CONV f( '0.2' ).
*    ENDIF.

    " Debug
    rs_frame-debug-vars = |\{"cx":{ lv_cx },"cy":{ lv_cy },"zoom":{ lv_zoom },| &&
      |"angle":{ lv_angle },"wander_x":{ mv_wander_x },"wander_y":{ mv_wander_y }\}|.
  ENDMETHOD.


  METHOD xor_int.
    DATA: lv_xa TYPE x LENGTH 1, lv_xb TYPE x LENGTH 1, lv_xr TYPE x LENGTH 1.
    lv_xa = iv_a MOD 256. lv_xb = iv_b MOD 256.
    lv_xr = lv_xa BIT-XOR lv_xb.
    rv_xor = lv_xr.
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
    DATA: lv_rh TYPE x LENGTH 1, lv_gh TYPE x LENGTH 1, lv_bh TYPE x LENGTH 1.
    lv_rh = CONV i( lv_r * 255 ). lv_gh = CONV i( lv_g * 255 ). lv_bh = CONV i( lv_b * 255 ).
    rv_hex = |#{ lv_rh }{ lv_gh }{ lv_bh }|.
  ENDMETHOD.
ENDCLASS.
