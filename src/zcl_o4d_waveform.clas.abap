CLASS zcl_o4d_waveform DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    METHODS constructor.

  PRIVATE SECTION.
    DATA mv_name TYPE string VALUE 'WAVEFORM'.
    METHODS int_to_hex IMPORTING iv_int TYPE i RETURNING VALUE(rv_hex) TYPE string.

ENDCLASS.

CLASS zcl_o4d_waveform IMPLEMENTATION.

  METHOD constructor.
  ENDMETHOD.

  METHOD int_to_hex.
    DATA lv_hex TYPE x LENGTH 1.
    DATA lv_val TYPE i.
    lv_val = iv_int.
    IF lv_val < 0. lv_val = 0. ENDIF.
    IF lv_val > 255. lv_val = 255. ENDIF.
    lv_hex = lv_val.
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
    DATA: lv_t TYPE f, lv_i TYPE i, lv_x TYPE f, lv_y TYPE f.
    DATA: lv_wave1 TYPE f, lv_wave2 TYPE f, lv_wave3 TYPE f, lv_val TYPE f.
    DATA: lv_prev_x TYPE f, lv_prev_y TYPE f, lv_first TYPE abap_bool.
    DATA: lv_hue TYPE i, lv_r TYPE i, lv_g TYPE i, lv_b TYPE i.
    DATA: lv_hex_r TYPE string, lv_hex_g TYPE string, lv_hex_b TYPE string.
    DATA: lv_amp TYPE f, lv_freq_mult TYPE f.
    DATA ls_line TYPE zif_o4d_effect=>ty_line.
    DATA ls_rect TYPE zif_o4d_effect=>ty_rect.
    DATA ls_text TYPE zif_o4d_effect=>ty_text.

    lv_t = is_ctx-gbi-time.

    " Background
    CLEAR ls_rect.
    ls_rect-x = 0. ls_rect-y = 0. ls_rect-w = 320. ls_rect-h = 200.
    ls_rect-fill = '#000000'.
    APPEND ls_rect TO rs_frame-rects.

    " Amplitude based on beat
    lv_amp = 30 + is_ctx-gbi-pulse * 40.

    " Draw multiple waveform layers
    DO 3 TIMES.
      DATA lv_layer TYPE i.
      lv_layer = sy-index.
      lv_freq_mult = '0.5' + lv_layer * '0.3'.

      " Color for each layer
      CASE lv_layer.
        WHEN 1.
          lv_r = 0. lv_g = 255. lv_b = 100.  " Green
        WHEN 2.
          lv_r = 0. lv_g = 200. lv_b = 255.  " Cyan
        WHEN 3.
          lv_r = 255. lv_g = 100. lv_b = 255.  " Magenta
      ENDCASE.

      lv_hex_r = int_to_hex( lv_r ).
      lv_hex_g = int_to_hex( lv_g ).
      lv_hex_b = int_to_hex( lv_b ).

      lv_first = abap_true.
      DO 64 TIMES.
        lv_i = sy-index - 1.
        lv_x = lv_i * 5.

        " Complex waveform
        lv_wave1 = sin( lv_i * '0.15' * lv_freq_mult + lv_t * 3 ).
        lv_wave2 = sin( lv_i * '0.25' * lv_freq_mult + lv_t * 5 + lv_layer ) * '0.5'.
        lv_wave3 = sin( lv_i * '0.08' * lv_freq_mult + lv_t * 2 ) * '0.3'.

        lv_val = ( lv_wave1 + lv_wave2 + lv_wave3 ) / '1.8'.
        lv_y = 100 + lv_val * lv_amp.

        IF lv_first = abap_true.
          lv_prev_x = lv_x. lv_prev_y = lv_y.
          lv_first = abap_false.
        ELSE.
          CLEAR ls_line.
          ls_line-x1 = lv_prev_x. ls_line-y1 = lv_prev_y.
          ls_line-x2 = lv_x. ls_line-y2 = lv_y.
          ls_line-color = |#{ lv_hex_r }{ lv_hex_g }{ lv_hex_b }|.
          ls_line-width = 2.
          APPEND ls_line TO rs_frame-lines.
          lv_prev_x = lv_x. lv_prev_y = lv_y.
        ENDIF.
      ENDDO.
    ENDDO.

    " Center line (dimmed)
    CLEAR ls_line.
    ls_line-x1 = 0. ls_line-y1 = 100.
    ls_line-x2 = 320. ls_line-y2 = 100.
    ls_line-color = '#333333'.
    ls_line-width = 1.
    APPEND ls_line TO rs_frame-lines.

    " Grid lines
    DO 5 TIMES.
      DATA lv_grid_y TYPE f.
      lv_grid_y = sy-index * 40.

      CLEAR ls_line.
      ls_line-x1 = 0. ls_line-y1 = lv_grid_y.
      ls_line-x2 = 320. ls_line-y2 = lv_grid_y.
      ls_line-color = '#222222'.
      ls_line-width = 1.
      APPEND ls_line TO rs_frame-lines.
    ENDDO.

    " Vertical grid
    DO 8 TIMES.
      DATA lv_grid_x TYPE f.
      lv_grid_x = sy-index * 40.

      CLEAR ls_line.
      ls_line-x1 = lv_grid_x. ls_line-y1 = 0.
      ls_line-x2 = lv_grid_x. ls_line-y2 = 200.
      ls_line-color = '#222222'.
      ls_line-width = 1.
      APPEND ls_line TO rs_frame-lines.
    ENDDO.

    " Title
    CLEAR ls_text.
    ls_text-x = 160. ls_text-y = 15.
    ls_text-text = 'WAVEFORM'.
    ls_text-color = '#00FF66'. ls_text-size = 12. ls_text-align = 'center'.
    APPEND ls_text TO rs_frame-texts.

    " BPM indicator
    CLEAR ls_text.
    ls_text-x = 10. ls_text-y = 190.
    ls_text-text = |BPM: 76 BEAT: { is_ctx-gbi-beat }|.
    ls_text-color = '#666666'. ls_text-size = 8.
    APPEND ls_text TO rs_frame-texts.

    " Flash on beat 2
    IF is_ctx-gbi-beat = 2 AND is_ctx-gbi-pulse > '0.7'.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = '0.2'.
      rs_frame-flash-r = 0. rs_frame-flash-g = 1. rs_frame-flash-b = '0.5'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
