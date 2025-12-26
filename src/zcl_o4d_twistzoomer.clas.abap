"! @title TWISTZOOMER - Spiraling zoom with twist distortion
CLASS zcl_o4d_twistzoomer DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    METHODS constructor
      IMPORTING iv_twist_speed TYPE f DEFAULT '2.0'
                iv_zoom_speed  TYPE f DEFAULT '1.5'
                iv_segments    TYPE i DEFAULT 16.
  PRIVATE SECTION.
    CONSTANTS: c_pi TYPE f VALUE '3.14159265'.
    DATA: mv_twist_speed TYPE f,
          mv_zoom_speed  TYPE f,
          mv_segments    TYPE i.
    METHODS hsv_to_hex
      IMPORTING iv_h TYPE f iv_s TYPE f iv_v TYPE f
      RETURNING VALUE(rv_hex) TYPE string.
ENDCLASS.

CLASS zcl_o4d_twistzoomer IMPLEMENTATION.

  METHOD constructor.
    mv_twist_speed = iv_twist_speed.
    mv_zoom_speed = iv_zoom_speed.
    mv_segments = iv_segments.
  ENDMETHOD.

  METHOD zif_o4d_effect~get_name.
    rv_name = 'twistzoomer'.
  ENDMETHOD.

  METHOD zif_o4d_effect~get_params.
  ENDMETHOD.

  METHOD zif_o4d_effect~set_param.
    CASE iv_name.
      WHEN 'twist_speed'. mv_twist_speed = CONV f( iv_value ).
      WHEN 'zoom_speed'. mv_zoom_speed = CONV f( iv_value ).
      WHEN 'segments'. mv_segments = CONV i( iv_value ).
    ENDCASE.
  ENDMETHOD.

  METHOD zif_o4d_effect~render.
    DATA(ls_ctx) = VALUE zif_o4d_effect=>ty_render_ctx(
      t = is_sync-time gt = is_sync-time
      bi = VALUE #( pulse = is_sync-intensity bar = is_sync-bar )
    ).
    rs_frame = zif_o4d_effect~render_frame( ls_ctx ).
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    DATA(lv_t) = is_ctx-t.
    DATA(lv_pulse) = is_ctx-gbi-pulse.
    DATA(lv_w) = CONV f( zif_o4d_effect=>c_width ).
    DATA(lv_h) = CONV f( zif_o4d_effect=>c_height ).
    DATA(lv_cx) = lv_w / 2.
    DATA(lv_cy) = lv_h / 2.

    " Zoom factor oscillates
    DATA(lv_zoom_base) = CONV f( '0.5' ) + sin( lv_t * mv_zoom_speed ) * CONV f( '0.3' ).
    DATA(lv_zoom) = lv_zoom_base + lv_pulse * CONV f( '0.2' ).

    " Number of concentric rings
    DATA(lv_num_rings) = 20 + CONV i( lv_pulse * 10 ).
    DATA(lv_max_radius) = CONV f( 350 ).

    " Draw concentric twisted rings
    DO lv_num_rings TIMES.
      DATA(lv_ring) = sy-index.
      DATA(lv_ring_pct) = CONV f( lv_ring ) / lv_num_rings.

      " Ring radius with zoom
      DATA(lv_radius) = lv_ring_pct * lv_max_radius * lv_zoom.

      " Twist amount increases with radius (outer rings twist more)
      DATA(lv_twist) = lv_t * mv_twist_speed + lv_ring_pct * c_pi * 2.

      " Additional spiral offset based on time
      DATA(lv_spiral) = lv_t * CONV f( '0.5' ) * lv_ring_pct.

      " Draw segments of this ring
      DO mv_segments TIMES.
        DATA(lv_seg) = sy-index - 1.
        DATA(lv_seg_pct) = CONV f( lv_seg ) / mv_segments.

        " Angle with twist distortion
        DATA(lv_angle1) = lv_seg_pct * 2 * c_pi + lv_twist + lv_spiral.
        DATA(lv_angle2) = ( lv_seg_pct + 1 / CONV f( mv_segments ) ) * 2 * c_pi + lv_twist + lv_spiral.

        " Wave distortion on radius
        DATA(lv_wave) = sin( lv_angle1 * 3 + lv_t * 4 ) * 10 * lv_ring_pct.
        DATA(lv_r1) = lv_radius + lv_wave.
        DATA(lv_r2) = lv_radius + sin( lv_angle2 * 3 + lv_t * 4 ) * 10 * lv_ring_pct.

        " Points
        DATA(lv_x1) = lv_cx + cos( lv_angle1 ) * lv_r1.
        DATA(lv_y1) = lv_cy + sin( lv_angle1 ) * lv_r1 * CONV f( '0.7' ).  " Squash Y
        DATA(lv_x2) = lv_cx + cos( lv_angle2 ) * lv_r2.
        DATA(lv_y2) = lv_cy + sin( lv_angle2 ) * lv_r2 * CONV f( '0.7' ).

        " Color: hue based on angle + ring + time
        DATA(lv_hue) = ( lv_seg_pct + lv_ring_pct + lv_t * CONV f( '0.1' ) ) * 360.
        lv_hue = lv_hue - floor( lv_hue / 360 ) * 360.
        DATA(lv_sat) = CONV f( '0.8' ).
        DATA(lv_val) = CONV f( '0.5' ) + lv_ring_pct * CONV f( '0.5' ) + lv_pulse * CONV f( '0.2' ).
        IF lv_val > 1. lv_val = 1. ENDIF.

        DATA(lv_color) = hsv_to_hex( iv_h = lv_hue iv_s = lv_sat iv_v = lv_val ).

        " Only draw if on screen
        IF lv_x1 >= -50 AND lv_x1 <= lv_w + 50 AND
           lv_y1 >= -50 AND lv_y1 <= lv_h + 50.
          APPEND VALUE zif_o4d_effect=>ty_line(
            x1 = lv_x1 y1 = lv_y1 x2 = lv_x2 y2 = lv_y2
            color = lv_color width = 2
          ) TO rs_frame-lines.
        ENDIF.
      ENDDO.
    ENDDO.

    " Radial spokes that twist
    DATA(lv_num_spokes) = 8 + CONV i( lv_pulse * 8 ).
    DO lv_num_spokes TIMES.
      DATA(lv_spoke) = sy-index - 1.
      DATA(lv_spoke_angle) = CONV f( lv_spoke ) / lv_num_spokes * 2 * c_pi + lv_t * mv_twist_speed * CONV f( '0.5' ).

      " Spoke curves outward with twist
      DATA(lv_spoke_len) = lv_max_radius * lv_zoom * CONV f( '0.8' ).
      DATA(lv_spoke_twist) = sin( lv_t * 3 ) * CONV f( '0.3' ).

      DATA(lv_sx1) = lv_cx.
      DATA(lv_sy1) = lv_cy.
      DATA(lv_sx2) = lv_cx + cos( lv_spoke_angle + lv_spoke_twist ) * lv_spoke_len.
      DATA(lv_sy2) = lv_cy + sin( lv_spoke_angle + lv_spoke_twist ) * lv_spoke_len * CONV f( '0.7' ).

      DATA(lv_spoke_hue) = ( CONV f( lv_spoke ) / lv_num_spokes + lv_t * CONV f( '0.2' ) ) * 360.
      lv_spoke_hue = lv_spoke_hue - floor( lv_spoke_hue / 360 ) * 360.
      DATA(lv_spoke_color) = hsv_to_hex( iv_h = lv_spoke_hue iv_s = CONV f( '0.6' ) iv_v = CONV f( '0.9' ) ).

      APPEND VALUE zif_o4d_effect=>ty_line(
        x1 = lv_sx1 y1 = lv_sy1 x2 = lv_sx2 y2 = lv_sy2
        color = lv_spoke_color width = 3
      ) TO rs_frame-lines.
    ENDDO.

    " Center dot
    APPEND VALUE zif_o4d_effect=>ty_rect(
      x = lv_cx - 4 y = lv_cy - 4 w = 8 h = 8
      fill = '#FFFFFF'
    ) TO rs_frame-rects.

    " CENTER TEXT: by Oisee / (Alice V)
    APPEND VALUE zif_o4d_effect=>ty_rect(
      x = lv_cx - 80  y = lv_cy - 30
      w = 160  h = 56  fill = 'rgba(0,0,0,0.7)'
    ) TO rs_frame-rects.
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_cx  y = lv_cy - 8
      text = 'by Oisee'  color = '#FFFFFF'  size = 18  align = 'center'
    ) TO rs_frame-texts.
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_cx  y = lv_cy + 16
      text = '(Alice V)'  color = '#CCCCCC'  size = 14  align = 'center'
    ) TO rs_frame-texts.

    " FLASH on beat 2 of each bar (backbeat/snare)
    IF lv_pulse > CONV f( '0.7' ) AND is_ctx-gbi-beat = 2.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = lv_pulse * CONV f( '0.4' ).
      rs_frame-flash-r = CONV f( '0.8' ).
      rs_frame-flash-g = CONV f( '0.5' ).
      rs_frame-flash-b = CONV f( '1.0' ).  " Purple flash
    ENDIF.

    " Debug
    rs_frame-debug-vars = |\{"zoom":{ lv_zoom },"rings":{ lv_num_rings },| &&
      |"spokes":{ lv_num_spokes },"twist_speed":{ mv_twist_speed },"pulse":{ lv_pulse }\}|.
  ENDMETHOD.

  METHOD hsv_to_hex.
    DATA: lv_r TYPE f, lv_g TYPE f, lv_b TYPE f.
    DATA(lv_c) = iv_v * iv_s.
    DATA(lv_hp) = iv_h / 60.
    DATA(lv_x) = lv_c * ( 1 - abs( frac( lv_hp / 2 ) * 2 - 1 ) ).
    DATA(lv_m) = iv_v - lv_c.
    DATA(lv_hi) = floor( lv_hp ).
    CASE lv_hi MOD 6.
      WHEN 0. lv_r = lv_c. lv_g = lv_x. lv_b = 0.
      WHEN 1. lv_r = lv_x. lv_g = lv_c. lv_b = 0.
      WHEN 2. lv_r = 0. lv_g = lv_c. lv_b = lv_x.
      WHEN 3. lv_r = 0. lv_g = lv_x. lv_b = lv_c.
      WHEN 4. lv_r = lv_x. lv_g = 0. lv_b = lv_c.
      WHEN 5. lv_r = lv_c. lv_g = 0. lv_b = lv_x.
    ENDCASE.
    DATA(lv_ri) = CONV i( ( lv_r + lv_m ) * 255 ).
    DATA(lv_gi) = CONV i( ( lv_g + lv_m ) * 255 ).
    DATA(lv_bi) = CONV i( ( lv_b + lv_m ) * 255 ).
    DATA: lv_rh TYPE x LENGTH 1, lv_gh TYPE x LENGTH 1, lv_bh TYPE x LENGTH 1.
    lv_rh = lv_ri. lv_gh = lv_gi. lv_bh = lv_bi.
    rv_hex = |#{ lv_rh }{ lv_gh }{ lv_bh }|.
  ENDMETHOD.

ENDCLASS.
