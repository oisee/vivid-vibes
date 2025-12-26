CLASS zcl_o4d_tunnel DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    METHODS constructor.

  PRIVATE SECTION.
    DATA mv_name TYPE string VALUE 'TUNNEL'.
    CONSTANTS: c_width    TYPE i VALUE 320,
               c_height   TYPE i VALUE 200,
               c_rings    TYPE i VALUE 25,
               c_segments TYPE i VALUE 16.
    CONSTANTS c_pi TYPE f VALUE '3.14159265358979'.

ENDCLASS.

CLASS zcl_o4d_tunnel IMPLEMENTATION.

  METHOD constructor.
  ENDMETHOD.

  METHOD zif_o4d_effect~get_name.
    rv_name = mv_name.
  ENDMETHOD.

  METHOD zif_o4d_effect~get_params.
    CLEAR rt_params.
  ENDMETHOD.

  METHOD zif_o4d_effect~set_param.
  ENDMETHOD.

  METHOD zif_o4d_effect~render.
    DATA ls_ctx TYPE zif_o4d_effect=>ty_render_ctx.
    ls_ctx-t = is_sync-time.
    ls_ctx-gbi-time = is_sync-time.
    ls_ctx-gbi-bar = is_sync-bar.
    ls_ctx-gbi-beat = is_sync-beat.
    ls_ctx-gbi-bar_phase = is_sync-bar_phase.
    ls_ctx-gbi-pulse = is_sync-intensity.
    rs_frame = zif_o4d_effect~render_frame( ls_ctx ).
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    DATA: lv_t TYPE f, lv_speed TYPE f, lv_twist TYPE f.
    DATA: lv_r TYPE i, lv_s TYPE i.
    DATA: lv_depth TYPE f, lv_radius TYPE f.
    DATA: lv_a1 TYPE f, lv_a2 TYPE f.
    DATA: lv_x1 TYPE f, lv_y1 TYPE f, lv_x2 TYPE f, lv_y2 TYPE f.
    DATA: lv_bright TYPE i, lv_checker TYPE i.
    DATA: lv_cx TYPE f, lv_cy TYPE f.
    DATA: lv_hue TYPE i.
    DATA ls_line TYPE zif_o4d_effect=>ty_line.
    DATA ls_tri TYPE zif_o4d_effect=>ty_triangle.
    DATA ls_rect TYPE zif_o4d_effect=>ty_rect.
    DATA ls_text TYPE zif_o4d_effect=>ty_text.

    lv_t = is_ctx-gbi-time.
    lv_speed = 5.
    lv_twist = '0.3'.
    lv_cx = c_width / 2.
    lv_cy = c_height / 2.

    " Black background
    CLEAR ls_rect.
    ls_rect-x = 0. ls_rect-y = 0. ls_rect-w = c_width. ls_rect-h = c_height.
    ls_rect-fill = '#000000'.
    APPEND ls_rect TO rs_frame-rects.

    " === TUNNEL RINGS ===
    " Draw from back (smallest) to front (largest) for proper layering
    DO c_rings TIMES.
      lv_r = c_rings - sy-index + 1.

      " Depth cycles with time
      lv_depth = ( lv_r + lv_t * lv_speed ) MOD c_rings.
      lv_radius = 20 + lv_depth * 6.

      " Brightness by depth (closer = brighter)
      lv_bright = 50 + ( c_rings - lv_depth ) * 8.
      IF lv_bright > 255. lv_bright = 255. ENDIF.

      " Draw segments forming the ring
      DO c_segments TIMES.
        lv_s = sy-index - 1.

        " Angle with twist that increases with depth
        DATA lv_twist_offset TYPE f.
        lv_twist_offset = lv_t * lv_twist * ( 1 + lv_depth / c_rings ).

        lv_a1 = ( lv_s / c_segments ) * 2 * c_pi + lv_twist_offset.
        lv_a2 = ( ( lv_s + 1 ) / c_segments ) * 2 * c_pi + lv_twist_offset.

        " Ellipse for 3D perspective (squashed vertically)
        lv_x1 = lv_cx + cos( lv_a1 ) * lv_radius.
        lv_y1 = lv_cy + sin( lv_a1 ) * lv_radius * '0.6'.
        lv_x2 = lv_cx + cos( lv_a2 ) * lv_radius.
        lv_y2 = lv_cy + sin( lv_a2 ) * lv_radius * '0.6'.

        " Checkerboard pattern
        lv_checker = ( lv_s + CONV i( lv_depth ) ) MOD 2.

        IF lv_checker = 0.
          " Draw filled segment as triangle pair
          DATA lv_inner_radius TYPE f.
          lv_inner_radius = lv_radius - 6.
          IF lv_inner_radius < 20. lv_inner_radius = 20. ENDIF.

          DATA: lv_ix1 TYPE f, lv_iy1 TYPE f, lv_ix2 TYPE f, lv_iy2 TYPE f.
          lv_ix1 = lv_cx + cos( lv_a1 ) * lv_inner_radius.
          lv_iy1 = lv_cy + sin( lv_a1 ) * lv_inner_radius * '0.6'.
          lv_ix2 = lv_cx + cos( lv_a2 ) * lv_inner_radius.
          lv_iy2 = lv_cy + sin( lv_a2 ) * lv_inner_radius * '0.6'.

          " Color varies with time and position
          lv_hue = ( lv_t * 30 + lv_s * 20 + lv_depth * 10 ) MOD 360.

          " Triangle 1: outer1, outer2, inner2
          CLEAR ls_tri.
          ls_tri-x1 = lv_x1. ls_tri-y1 = lv_y1.
          ls_tri-x2 = lv_x2. ls_tri-y2 = lv_y2.
          ls_tri-x3 = lv_ix2. ls_tri-y3 = lv_iy2.
          ls_tri-fill = |hsl({ lv_hue },80%,{ lv_bright / 5 }%)|.
          APPEND ls_tri TO rs_frame-triangles.

          " Triangle 2: outer1, inner2, inner1
          CLEAR ls_tri.
          ls_tri-x1 = lv_x1. ls_tri-y1 = lv_y1.
          ls_tri-x2 = lv_ix2. ls_tri-y2 = lv_iy2.
          ls_tri-x3 = lv_ix1. ls_tri-y3 = lv_iy1.
          ls_tri-fill = |hsl({ lv_hue },80%,{ lv_bright / 5 }%)|.
          APPEND ls_tri TO rs_frame-triangles.
        ENDIF.

        " Ring outline
        CLEAR ls_line.
        ls_line-x1 = lv_x1. ls_line-y1 = lv_y1.
        ls_line-x2 = lv_x2. ls_line-y2 = lv_y2.
        ls_line-color = |rgb({ lv_bright },{ lv_bright },{ lv_bright })|.
        ls_line-width = 1.
        APPEND ls_line TO rs_frame-lines.
      ENDDO.
    ENDDO.

    " Center glow
    DATA lv_glow_size TYPE f.
    lv_glow_size = 15 + sin( lv_t * 3 ) * 5 + is_ctx-gbi-pulse * 10.
    CLEAR ls_rect.
    ls_rect-x = lv_cx - lv_glow_size / 2.
    ls_rect-y = lv_cy - lv_glow_size / 2 * '0.6'.
    ls_rect-w = lv_glow_size. ls_rect-h = lv_glow_size * '0.6'.
    ls_rect-fill = '#FFFFFF'.
    APPEND ls_rect TO rs_frame-rects.

    " Title
    CLEAR ls_text.
    ls_text-x = 160. ls_text-y = 190.
    ls_text-text = 'TUNNEL'.
    ls_text-color = '#00FFFF'. ls_text-size = 10. ls_text-align = 'center'.
    APPEND ls_text TO rs_frame-texts.

    " Flash on bar
    IF is_ctx-gbi-on_bar = abap_true.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = '0.25'.
      rs_frame-flash-r = 1. rs_frame-flash-g = 1. rs_frame-flash-b = 1.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
