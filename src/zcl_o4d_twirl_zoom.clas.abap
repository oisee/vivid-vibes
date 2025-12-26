"! @title TWIRL_ZOOM - Rotozoom with spiral/twist distortion based on distance
CLASS zcl_o4d_twirl_zoom DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    METHODS constructor
      IMPORTING iv_twist_amount TYPE f DEFAULT '3.0'     " Сила закручивания
                iv_twist_type   TYPE i DEFAULT 1         " 1=linear, 2=quadratic, 3=sin, 4=swirl
                iv_twist_speed  TYPE f DEFAULT '1.0'.    " Скорость анимации твиста
  PRIVATE SECTION.
    DATA: mv_twist_amount TYPE f,
          mv_twist_type   TYPE i,
          mv_twist_speed  TYPE f.
    METHODS hsv_to_hex
      IMPORTING iv_h TYPE f iv_s TYPE f iv_v TYPE f
      RETURNING VALUE(rv_hex) TYPE string.
    METHODS xor_int
      IMPORTING iv_a TYPE i iv_b TYPE i
      RETURNING VALUE(rv_xor) TYPE i.
    METHODS calc_twist
      IMPORTING iv_dist TYPE f iv_t TYPE f
      RETURNING VALUE(rv_twist) TYPE f.
ENDCLASS.

CLASS zcl_o4d_twirl_zoom IMPLEMENTATION.

  METHOD constructor.
    mv_twist_amount = iv_twist_amount.
    mv_twist_type = iv_twist_type.
    mv_twist_speed = iv_twist_speed.
  ENDMETHOD.

  METHOD zif_o4d_effect~get_name. rv_name = 'twirl_zoom'. ENDMETHOD.

  METHOD zif_o4d_effect~get_params. ENDMETHOD.

  METHOD zif_o4d_effect~set_param.
    CASE iv_name.
      WHEN 'twist_amount'. mv_twist_amount = CONV f( iv_value ).
      WHEN 'twist_type'. mv_twist_type = CONV i( iv_value ).
      WHEN 'twist_speed'. mv_twist_speed = CONV f( iv_value ).
    ENDCASE.
  ENDMETHOD.

  METHOD zif_o4d_effect~render.
    rs_frame = zif_o4d_effect~render_frame( VALUE #(
      t = is_sync-time gt = is_sync-time bi = VALUE #( pulse = is_sync-intensity ) ) ).
  ENDMETHOD.

  METHOD calc_twist.
    " Вычисляем дополнительный угол в зависимости от расстояния
    DATA(lv_norm_dist) = iv_dist / 200.  " Нормализуем к ~0..1.5
    DATA(lv_anim) = sin( iv_t * mv_twist_speed ).  " Анимированный множитель

    CASE mv_twist_type.
      WHEN 1.  " LINEAR - равномерная спираль
        rv_twist = lv_norm_dist * mv_twist_amount * ( 1 + lv_anim * CONV f( '0.5' ) ).

      WHEN 2.  " QUADRATIC - края сильнее
        rv_twist = lv_norm_dist * lv_norm_dist * mv_twist_amount * ( 1 + lv_anim * CONV f( '0.5' ) ).

      WHEN 3.  " SIN - волновые кольца
        rv_twist = sin( lv_norm_dist * CONV f( '6.28' ) * 2 + iv_t * 2 ) * mv_twist_amount * lv_norm_dist.

      WHEN 4.  " SWIRL - вихрь к центру (сильнее в центре)
        DATA(lv_inv) = 1 / ( lv_norm_dist + CONV f( '0.1' ) ).
        rv_twist = lv_inv * mv_twist_amount * CONV f( '0.3' ) * ( 1 + lv_anim ).

      WHEN OTHERS.
        rv_twist = lv_norm_dist * mv_twist_amount.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    DATA(lv_t) = is_ctx-t.
    DATA(lv_pulse) = is_ctx-gbi-pulse.
    DATA(lv_w) = CONV f( zif_o4d_effect=>c_width ).
    DATA(lv_h) = CONV f( zif_o4d_effect=>c_height ).
    DATA(lv_cx) = lv_w / 2.
    DATA(lv_cy) = lv_h / 2.

    " Texture settings
    DATA(lv_tex_size) = 64.
    DATA(lv_scale) = 8.

    " Base rotation (медленнее чтобы твист был виден)
    DATA(lv_base_angle) = lv_t * CONV f( '0.8' ).

    " Zoom oscillates + pulse
    DATA(lv_zoom) = CONV f( '2.5' ) + sin( lv_t * CONV f( '0.6' ) ) * CONV f( '1.5' ) + lv_pulse.

    DATA(lv_cos_base) = cos( lv_base_angle ).
    DATA(lv_sin_base) = sin( lv_base_angle ).

    " Render with twist distortion
    DATA(lv_y) = 0.
    WHILE lv_y < lv_h.
      DATA(lv_x) = 0.
      WHILE lv_x < lv_w.
        " Vector from center
        DATA(lv_dx) = CONV f( lv_x ) - lv_cx.
        DATA(lv_dy) = CONV f( lv_y ) - lv_cy.

        " Distance from center
        DATA(lv_dist) = sqrt( lv_dx * lv_dx + lv_dy * lv_dy ).

        " === TWIST: дополнительный угол зависит от расстояния ===
        DATA(lv_twist) = calc_twist( iv_dist = lv_dist iv_t = lv_t ).

        " Total angle = base rotation + distance-based twist
        DATA(lv_total_angle) = lv_base_angle + lv_twist.
        DATA(lv_cos_a) = cos( lv_total_angle ).
        DATA(lv_sin_a) = sin( lv_total_angle ).

        " Transform to texture space
        DATA(lv_u) = ( lv_dx * lv_cos_a - lv_dy * lv_sin_a ) / lv_zoom.
        DATA(lv_v) = ( lv_dx * lv_sin_a + lv_dy * lv_cos_a ) / lv_zoom.

        " Texture coordinates with wrap
        DATA(lv_tx) = CONV i( lv_u ) MOD lv_tex_size.
        DATA(lv_ty) = CONV i( lv_v ) MOD lv_tex_size.
        IF lv_tx < 0. lv_tx = lv_tx + lv_tex_size. ENDIF.
        IF lv_ty < 0. lv_ty = lv_ty + lv_tex_size. ENDIF.

        " XOR pattern + colors
        DATA(lv_xor) = xor_int( iv_a = lv_tx iv_b = lv_ty ).

        " Hue varies with twist amount for psychedelic effect
        DATA(lv_hue) = CONV f( ( lv_xor * 4 + CONV i( lv_t * 30 ) + CONV i( lv_twist * 20 ) ) MOD 360 ).
        DATA(lv_light) = CONV f( 30 + lv_xor ) / 100.
        IF lv_light > 1. lv_light = 1. ENDIF.
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

    " Title shows twist type
    DATA(lv_type_name) = SWITCH string( mv_twist_type
      WHEN 1 THEN 'LINEAR'
      WHEN 2 THEN 'QUADRATIC'
      WHEN 3 THEN 'SINE WAVE'
      WHEN 4 THEN 'SWIRL'
      ELSE 'CUSTOM' ).
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_w / 2 y = 20 text = |TWIRL { lv_type_name }|
      color = '#FFFFFF' size = 14 align = 'center'
    ) TO rs_frame-texts.

    " FLASH on beat 2 of each bar (backbeat/snare)
    IF lv_pulse > CONV f( '0.7' ) AND is_ctx-gbi-beat = 2.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = lv_pulse * CONV f( '0.4' ).
      rs_frame-flash-r = CONV f( '0.6' ).
      rs_frame-flash-g = CONV f( '1.0' ).
      rs_frame-flash-b = CONV f( '0.8' ).  " Cyan flash
    ENDIF.

    " Debug
    rs_frame-debug-vars = |\{"twist_type":{ mv_twist_type },"twist_amount":{ mv_twist_amount },| &&
      |"twist_speed":{ mv_twist_speed },"zoom":{ lv_zoom },"pulse":{ lv_pulse }\}|.
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
