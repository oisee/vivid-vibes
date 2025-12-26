CLASS zcl_o4d_starburst DEFINITION
  PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_ray,
        angle   TYPE f,
        speed   TYPE f,
        length  TYPE f,
        hue     TYPE f,
        delay   TYPE f,
      END OF ty_ray,
      tt_rays TYPE STANDARD TABLE OF ty_ray WITH EMPTY KEY.

    CONSTANTS:
      c_num_rays TYPE i VALUE 32.

    DATA:
      mv_initialized TYPE abap_bool,
      mt_rays        TYPE tt_rays.

    METHODS:
      init_rays,
      hsv_to_hex
        IMPORTING iv_h         TYPE f
                  iv_s         TYPE f
                  iv_v         TYPE f
        RETURNING VALUE(rv_hex) TYPE string.

ENDCLASS.

CLASS zcl_o4d_starburst IMPLEMENTATION.

  METHOD zif_o4d_effect~get_name.
    rv_name = 'starburst'.
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
      gbi = VALUE #( time = is_sync-time bar = is_sync-bar beat = is_sync-beat
                     bar_phase = is_sync-bar_phase pulse = is_sync-intensity )
    ).
    rs_frame = zif_o4d_effect~render_frame( ls_ctx ).
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    IF mv_initialized = abap_false.
      init_rays( ).
      mv_initialized = abap_true.
    ENDIF.

    DATA(lv_t) = is_ctx-t.
    DATA(ls_bi) = is_ctx-bi.
    DATA(lv_w) = CONV f( zif_o4d_effect=>c_width ).
    DATA(lv_h) = CONV f( zif_o4d_effect=>c_height ).
    DATA(lv_cx) = lv_w / 2.
    DATA(lv_cy) = lv_h / 2.

    " Burst cycle - 2 seconds per burst
    DATA(lv_cycle) = CONV f( 2 ).
    DATA(lv_phase) = frac( lv_t / lv_cycle ).

    " Center glow on beat
    DATA(lv_glow_r) = 20 + ls_bi-pulse * 30.
    DO 8 TIMES.
      DATA(lv_gr) = lv_glow_r * ( 1 - CONV f( sy-index ) / 10 ).
      DATA(lv_glow_color) = hsv_to_hex(
        iv_h = frac( lv_t * CONV f( '0.1' ) )
        iv_s = CONV f( '0.8' )
        iv_v = CONV f( 1 ) - CONV f( sy-index ) / 10
      ).
      " Draw glow as cross
      APPEND VALUE zif_o4d_effect=>ty_line(
        x1 = lv_cx - lv_gr  y1 = lv_cy
        x2 = lv_cx + lv_gr  y2 = lv_cy
        color = lv_glow_color  width = 3
      ) TO rs_frame-lines.
      APPEND VALUE zif_o4d_effect=>ty_line(
        x1 = lv_cx  y1 = lv_cy - lv_gr
        x2 = lv_cx  y2 = lv_cy + lv_gr
        color = lv_glow_color  width = 3
      ) TO rs_frame-lines.
    ENDDO.

    " Draw rays
    LOOP AT mt_rays ASSIGNING FIELD-SYMBOL(<ray>).
      " Delayed start for staggered effect
      DATA(lv_ray_phase) = lv_phase - <ray>-delay.
      IF lv_ray_phase < 0.
        lv_ray_phase = lv_ray_phase + 1.
      ENDIF.

      " Ray expands outward
      DATA(lv_dist) = lv_ray_phase * <ray>-speed * 200.
      DATA(lv_len) = <ray>-length * ( 1 - lv_ray_phase * CONV f( '0.5' ) ).

      IF lv_dist < 400 AND lv_len > 0.
        DATA(lv_cos) = cos( <ray>-angle ).
        DATA(lv_sin) = sin( <ray>-angle ).

        DATA(lv_x1) = lv_cx + lv_cos * lv_dist.
        DATA(lv_y1) = lv_cy + lv_sin * lv_dist.
        DATA(lv_x2) = lv_cx + lv_cos * ( lv_dist + lv_len ).
        DATA(lv_y2) = lv_cy + lv_sin * ( lv_dist + lv_len ).

        " Fade out as ray moves outward
        DATA(lv_alpha) = 1 - lv_ray_phase.
        DATA(lv_color) = hsv_to_hex(
          iv_h = frac( <ray>-hue + lv_t * CONV f( '0.2' ) )
          iv_s = CONV f( '0.9' )
          iv_v = lv_alpha
        ).

        DATA(lv_width) = 2 + ls_bi-pulse * 2.
        APPEND VALUE zif_o4d_effect=>ty_line(
          x1 = lv_x1  y1 = lv_y1
          x2 = lv_x2  y2 = lv_y2
          color = lv_color  width = lv_width
        ) TO rs_frame-lines.
      ENDIF.
    ENDLOOP.

    " Title
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_cx  y = 30
      text = 'STARBURST'
      color = '#FFFFFF'  size = 12  align = 'center'
    ) TO rs_frame-texts.
  ENDMETHOD.

  METHOD init_rays.
    DATA: lv_seed TYPE f VALUE '0.4821'.

    DO c_num_rays TIMES.
      DATA(lv_angle) = CONV f( sy-index - 1 ) / c_num_rays * 2 * zif_o4d_effect=>c_pi.

      lv_seed = frac( sin( lv_seed * 12345 + sy-index ) * 43758 ).
      DATA(lv_speed) = CONV f( '0.8' ) + lv_seed * CONV f( '0.4' ).

      lv_seed = frac( sin( lv_seed * 12345 + sy-index ) * 43758 ).
      DATA(lv_length) = 20 + lv_seed * 40.

      lv_seed = frac( sin( lv_seed * 12345 + sy-index ) * 43758 ).
      DATA(lv_hue) = lv_seed.

      lv_seed = frac( sin( lv_seed * 12345 + sy-index ) * 43758 ).
      DATA(lv_delay) = lv_seed * CONV f( '0.3' ).

      APPEND VALUE ty_ray(
        angle = lv_angle
        speed = lv_speed
        length = lv_length
        hue = lv_hue
        delay = lv_delay
      ) TO mt_rays.
    ENDDO.
  ENDMETHOD.

  METHOD hsv_to_hex.
    DATA: lv_r TYPE f, lv_g TYPE f, lv_b TYPE f.
    DATA(lv_hh) = iv_h * 6.
    DATA(lv_i) = floor( lv_hh ).
    DATA(lv_f) = lv_hh - lv_i.
    DATA(lv_p) = iv_v * ( 1 - iv_s ).
    DATA(lv_q) = iv_v * ( 1 - lv_f * iv_s ).
    DATA(lv_tt) = iv_v * ( 1 - ( 1 - lv_f ) * iv_s ).
    CASE lv_i MOD 6.
      WHEN 0. lv_r = iv_v. lv_g = lv_tt. lv_b = lv_p.
      WHEN 1. lv_r = lv_q. lv_g = iv_v. lv_b = lv_p.
      WHEN 2. lv_r = lv_p. lv_g = iv_v. lv_b = lv_tt.
      WHEN 3. lv_r = lv_p. lv_g = lv_q. lv_b = iv_v.
      WHEN 4. lv_r = lv_tt. lv_g = lv_p. lv_b = iv_v.
      WHEN 5. lv_r = iv_v. lv_g = lv_p. lv_b = lv_q.
    ENDCASE.
    DATA(lv_ri) = CONV i( nmin( val1 = 255 val2 = nmax( val1 = 0 val2 = lv_r * 255 ) ) ).
    DATA(lv_gi) = CONV i( nmin( val1 = 255 val2 = nmax( val1 = 0 val2 = lv_g * 255 ) ) ).
    DATA(lv_bi) = CONV i( nmin( val1 = 255 val2 = nmax( val1 = 0 val2 = lv_b * 255 ) ) ).
    rv_hex = |#{ lv_ri ALIGN = RIGHT WIDTH = 2 PAD = '0' }{ lv_gi ALIGN = RIGHT WIDTH = 2 PAD = '0' }{ lv_bi ALIGN = RIGHT WIDTH = 2 PAD = '0' }|.
  ENDMETHOD.

ENDCLASS.
