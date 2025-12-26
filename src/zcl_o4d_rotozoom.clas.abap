CLASS zcl_o4d_rotozoom DEFINITION PUBLIC FINAL CREATE PUBLIC.
*======================================================================
* ROTOZOOM XOR - Classic demoscene rotating/zooming texture
*======================================================================
* TWEAK GUIDE:
*   lv_tex_size  = 64    - Texture size (try 32, 128 for different patterns)
*   lv_scale     = 8     - Pixel block size (4=detailed, 16=chunky retro)
*   lv_angle * 1.2       - Rotation speed (0.5=slow, 3.0=fast spin)
*   lv_zoom base 0.8     - Base zoom level (0.5=zoomed in, 2.0=zoomed out)
*   sin(t)*0.3           - Zoom oscillation amount (0.1=subtle, 0.8=wild)
*   lv_xor * 5           - Hue multiplier (1=subtle, 10=rainbow chaos)
*   t * 20               - Color cycling speed (5=slow, 50=rave mode)
*======================================================================
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
  PRIVATE SECTION.
    METHODS hsv_to_hex
      IMPORTING iv_h         TYPE f
                iv_s         TYPE f
                iv_v         TYPE f
      RETURNING VALUE(rv_hex) TYPE string.
    METHODS xor_int
      IMPORTING iv_a         TYPE i
                iv_b         TYPE i
      RETURNING VALUE(rv_xor) TYPE i.
ENDCLASS.



CLASS ZCL_O4D_ROTOZOOM IMPLEMENTATION.


  METHOD zif_o4d_effect~get_name. rv_name = 'rotozoom'. ENDMETHOD.


  METHOD zif_o4d_effect~get_params. ENDMETHOD.


  METHOD zif_o4d_effect~set_param. ENDMETHOD.


  METHOD zif_o4d_effect~render.
    rs_frame = zif_o4d_effect~render_frame( VALUE #(
      t = is_sync-time gt = is_sync-time bi = VALUE #( pulse = is_sync-intensity ) ) ).
  ENDMETHOD.


  METHOD zif_o4d_effect~render_frame.
    DATA(lv_t) = is_ctx-t.
    DATA(lv_w) = CONV f( zif_o4d_effect=>c_width ).
    DATA(lv_h) = CONV f( zif_o4d_effect=>c_height ).

    "--- TWEAK: Texture and pixel size ---
    DATA(lv_tex_size) = 64.   " XOR pattern repeats (32=fine, 128=coarse)
    DATA(lv_scale) = 6.       " Pixel block size (smaller=more detail, slower)

    "   DATA(lv_boost) = CONV f( 1 + is_ctx-gbi-f4 * '1.5' ).

    "data(lv_pos) = is_ctx-gbi-pos_16 mod 16 .
*    DATA(lv_pos) = is_ctx-gbi-pos_8 MOD 8 .
*    IF lv_pos = 4.
*      DATA(lv_boost) = CONV f( '1.4' ).
*    ELSE.
*      lv_boost = '1'.
*    ENDIF.
    data(lv_boost) = conv f( '1' ).
    "--- TWEAK: Rotation and zoom ---
    DATA(lv_angle) = lv_t * CONV f( '1.2' ) * sin( ( lv_t ) / 3 ).           " Rotation speed
    "DATA(lv_zoom) = CONV f( '0.8' ) + sin( lv_t ) * CONV f( '0.3' ).  " Zoom oscillation
    DATA(lv_zoom) = CONV f( '4.8' ) + sin( lv_t / 4 ) * CONV f( '6' ) * lv_boost.  " Zoom oscillation
    DATA(lv_cos_a) = cos( lv_angle ).
    DATA(lv_sin_a) = sin( lv_angle ).

    DATA(lv_y) = 0.
    WHILE lv_y < lv_h.
      DATA(lv_x) = 0.
      WHILE lv_x < lv_w.
        " Transform screen coords to texture coords
        DATA(lv_dx) = CONV f( lv_x ) - 160.  " Center X
        DATA(lv_dy) = CONV f( lv_y ) - 100.  " Center Y

        DATA(lv_u) = ( lv_dx * lv_cos_a - lv_dy * lv_sin_a ) / lv_zoom.
        DATA(lv_v) = ( lv_dx * lv_sin_a + lv_dy * lv_cos_a ) / lv_zoom.

        " Texture coordinates with wrap
        DATA(lv_tx) = CONV i( lv_u ) MOD lv_tex_size.
        DATA(lv_ty) = CONV i( lv_v ) MOD lv_tex_size.
        IF lv_tx < 0. lv_tx = lv_tx + lv_tex_size. ENDIF.
        IF lv_ty < 0. lv_ty = lv_ty + lv_tex_size. ENDIF.

        "--- TWEAK: XOR pattern and colors ---
        DATA(lv_xor) = xor_int( iv_a = lv_tx iv_b = lv_ty ).
        DATA(lv_hue) = CONV f( ( lv_xor * 5 + CONV i( lv_t * 20 ) ) MOD 360 ).  " Color cycle
        DATA(lv_light) = CONV f( 30 + lv_xor ) / 100.  " Brightness from XOR
        IF lv_light > 1. lv_light = 1. ENDIF.

        DATA(lv_color) = hsv_to_hex( iv_h = lv_hue iv_s = CONV f( '0.8' ) iv_v = lv_light ).

        APPEND VALUE zif_o4d_effect=>ty_rect(
          x    = lv_x y = lv_y w = lv_scale h = lv_scale
          fill = lv_color
        ) TO rs_frame-rects.

        lv_x = lv_x + lv_scale.
      ENDWHILE.
      lv_y = lv_y + lv_scale.
    ENDWHILE.

    APPEND VALUE zif_o4d_effect=>ty_text(
      x     = lv_w / 2  y    = 20 text  = 'ROTOZOOM XOR'
      color = '#FFFFFF' size = 14 align = 'center'
    ) TO rs_frame-texts.

    " Beat flash
    "--- FLASH on beat 2 of each bar (backbeat/snare) ---
    IF is_ctx-bi-pos_16 MOD 16 = 8.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = '0.5' . "is_ctx-gbi-pulse * CONV f( '0.5' ).
      rs_frame-flash-r = CONV f( '1.0' ).
      rs_frame-flash-g = CONV f( '0.8' ).
      rs_frame-flash-b = CONV f( '0.6' ).  " Warm copper flash
    ENDIF.

    " Debug vars
    DATA(lv_angle_deg) = CONV i( lv_angle * 180 / CONV f( '3.14159' ) ) MOD 360.
    DATA(lv_angle_raw) = lv_angle.
    DATA(lv_rot_deriv) = CONV f( '1.2' ) * ( sin( lv_t / 3 ) + lv_t / 3 * cos( lv_t / 3 ) ).
    rs_frame-debug-vars = |\{"tex_size":{ lv_tex_size },"scale":{ lv_scale },| &&
      |"angle_deg":{ lv_angle_deg },"angle_raw":{ lv_angle_raw },"zoom":{ lv_zoom },| &&
      |"boost":{ lv_boost },"t":{ lv_t },"rot_deriv":{ lv_rot_deriv }\}|.
*      |"lv_boost": { lv_boost } | &&
*      |"is_ctx-gbi-f4 ": { is_ctx-gbi-f4 } |.
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
