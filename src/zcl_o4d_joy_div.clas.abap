CLASS zcl_o4d_joy_div DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    METHODS constructor.

  PRIVATE SECTION.
    DATA mv_name TYPE string VALUE 'JOY_DIV'.
    CONSTANTS: c_num_points TYPE i VALUE 64,
               c_num_lines  TYPE i VALUE 20.
    METHODS int_to_hex IMPORTING iv_int TYPE i RETURNING VALUE(rv_hex) TYPE string.

ENDCLASS.

CLASS zcl_o4d_joy_div IMPLEMENTATION.

  METHOD constructor.
  ENDMETHOD.

  METHOD int_to_hex.
    DATA lv_hex TYPE x LENGTH 1.
    lv_hex = iv_int.
    rv_hex = |{ lv_hex }|.
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
    DATA: lv_t TYPE f, lv_line_t TYPE f.
    DATA: lv_line TYPE i, lv_i TYPE i, lv_x TYPE f, lv_y TYPE f, lv_base_y TYPE f.
    DATA: lv_noise TYPE f, lv_amp TYPE f, lv_dist TYPE f, lv_gauss TYPE f.
    DATA: lv_wave1 TYPE f, lv_wave2 TYPE f, lv_wave3 TYPE f.
    DATA: lv_gray TYPE i, lv_hex TYPE string.
    DATA ls_line TYPE zif_o4d_effect=>ty_line.
    DATA ls_tri TYPE zif_o4d_effect=>ty_triangle.
    DATA ls_rect TYPE zif_o4d_effect=>ty_rect.
    DATA ls_text TYPE zif_o4d_effect=>ty_text.
    DATA: lv_prev_x TYPE f, lv_prev_y TYPE f.

    " Type for storing line points
    TYPES: BEGIN OF ty_point,
             x TYPE f,
             y TYPE f,
           END OF ty_point.
    DATA: lt_points TYPE STANDARD TABLE OF ty_point WITH EMPTY KEY,
          ls_point TYPE ty_point.

    lv_t = is_ctx-gbi-time.

    " Black background
    CLEAR ls_rect.
    ls_rect-x = 0. ls_rect-y = 0. ls_rect-w = 320. ls_rect-h = 200.
    ls_rect-fill = '#000000'.
    APPEND ls_rect TO rs_frame-rects.

    " === UNKNOWN PLEASURES ===
    " Draw lines from back to front (bottom to top) for proper layering
    " Each line is drawn with black fill below it

    DO c_num_lines TIMES.
      lv_line = c_num_lines - sy-index.  " Bottom to top
      lv_base_y = 20 + lv_line * 8.

      " Each line has a time offset to simulate history
      lv_line_t = lv_t - lv_line * '0.15'.

      " Amplitude varies: strongest in middle lines
      lv_amp = 30 * ( 1 - abs( lv_line - c_num_lines / 2 ) / ( c_num_lines / 2 ) ).
      lv_amp = lv_amp * ( '0.7' + is_ctx-gbi-pulse * '0.3' ).

      " Calculate all points for this line
      CLEAR lt_points.
      DO c_num_points TIMES.
        lv_i = sy-index - 1.
        lv_x = lv_i * 320 / ( c_num_points - 1 ).

        " Gaussian envelope - strongest in center
        lv_dist = ( lv_x - 160 ) / 80.
        lv_gauss = exp( -1 * lv_dist * lv_dist / 2 ).

        " Multiple wave components
        lv_wave1 = sin( lv_x * '0.05' + lv_line_t + lv_line * '0.3' ).
        lv_wave2 = sin( lv_x * '0.02' - lv_line_t * '0.5' ).
        lv_wave3 = sin( lv_x * '0.08' + lv_line_t * '1.5' + lv_line * '0.2' ) * '0.5'.

        lv_noise = ( lv_wave1 * lv_wave2 + lv_wave3 ) * lv_gauss * lv_amp.
        lv_y = lv_base_y - abs( lv_noise ).

        ls_point-x = lv_x.
        ls_point-y = lv_y.
        APPEND ls_point TO lt_points.
      ENDDO.

      " Draw black fill below line using triangles (two per segment)
      DATA: lv_idx TYPE i, ls_p1 TYPE ty_point, ls_p2 TYPE ty_point.
      lv_idx = 0.
      LOOP AT lt_points INTO ls_p1.
        lv_idx = lv_idx + 1.
        IF lv_idx >= lines( lt_points ).
          EXIT.
        ENDIF.
        READ TABLE lt_points INTO ls_p2 INDEX lv_idx + 1.

        " Triangle 1: top-left, top-right, bottom-right
        CLEAR ls_tri.
        ls_tri-x1 = ls_p1-x. ls_tri-y1 = ls_p1-y.
        ls_tri-x2 = ls_p2-x. ls_tri-y2 = ls_p2-y.
        ls_tri-x3 = ls_p2-x. ls_tri-y3 = lv_base_y + 5.
        ls_tri-fill = '#000000'.
        APPEND ls_tri TO rs_frame-triangles.

        " Triangle 2: top-left, bottom-right, bottom-left
        CLEAR ls_tri.
        ls_tri-x1 = ls_p1-x. ls_tri-y1 = ls_p1-y.
        ls_tri-x2 = ls_p2-x. ls_tri-y2 = lv_base_y + 5.
        ls_tri-x3 = ls_p1-x. ls_tri-y3 = lv_base_y + 5.
        ls_tri-fill = '#000000'.
        APPEND ls_tri TO rs_frame-triangles.
      ENDLOOP.

      " Draw white line on top
      lv_gray = 100 + 155 * lv_line / c_num_lines.  " Brighter toward front
      lv_hex = int_to_hex( lv_gray ).

      lv_idx = 0.
      LOOP AT lt_points INTO ls_p1.
        lv_idx = lv_idx + 1.
        IF lv_idx >= lines( lt_points ).
          EXIT.
        ENDIF.
        READ TABLE lt_points INTO ls_p2 INDEX lv_idx + 1.

        CLEAR ls_line.
        ls_line-x1 = ls_p1-x. ls_line-y1 = ls_p1-y.
        ls_line-x2 = ls_p2-x. ls_line-y2 = ls_p2-y.
        ls_line-color = |#{ lv_hex }{ lv_hex }{ lv_hex }|.
        ls_line-width = 1.
        APPEND ls_line TO rs_frame-lines.
      ENDLOOP.
    ENDDO.

    " Title
    CLEAR ls_text.
    ls_text-x = 160. ls_text-y = 190.
    ls_text-text = 'UNKNOWN PLEASURES'.
    ls_text-color = '#FFFFFF'. ls_text-size = 10. ls_text-align = 'center'.
    APPEND ls_text TO rs_frame-texts.

    " Flash on bar
    IF is_ctx-gbi-on_bar = abap_true.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = '0.2'.
      rs_frame-flash-r = 1. rs_frame-flash-g = 1. rs_frame-flash-b = 1.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
