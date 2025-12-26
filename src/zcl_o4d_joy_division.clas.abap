CLASS zcl_o4d_joy_division DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_history_line,
        points TYPE STANDARD TABLE OF f WITH EMPTY KEY,
      END OF ty_history_line,
      tt_history TYPE STANDARD TABLE OF ty_history_line WITH EMPTY KEY.

    DATA mv_num_bars TYPE i VALUE 12.
    DATA mv_history TYPE tt_history.
    DATA mv_max_history TYPE i VALUE 40.
    DATA mv_last_beat TYPE i VALUE -1.

    " Phase bar boundaries (configurable)
    DATA mv_intro_bars TYPE i VALUE 1.
    DATA mv_corporate_bars TYPE i VALUE 1.
    DATA mv_transform_bars TYPE i VALUE 2.

    METHODS get_phase
      IMPORTING is_bi TYPE zif_o4d_effect=>ty_beat_info
      RETURNING VALUE(rv_phase) TYPE string.

    METHODS render_intro
      IMPORTING is_ctx TYPE zif_o4d_effect=>ty_render_ctx
      RETURNING VALUE(rs_frame) TYPE zif_o4d_effect=>ty_frame.

    METHODS render_corporate
      IMPORTING is_ctx TYPE zif_o4d_effect=>ty_render_ctx
      RETURNING VALUE(rs_frame) TYPE zif_o4d_effect=>ty_frame.

    METHODS render_transform
      IMPORTING is_ctx TYPE zif_o4d_effect=>ty_render_ctx
      RETURNING VALUE(rs_frame) TYPE zif_o4d_effect=>ty_frame.

    METHODS render_joy_division
      IMPORTING is_ctx TYPE zif_o4d_effect=>ty_render_ctx
      RETURNING VALUE(rs_frame) TYPE zif_o4d_effect=>ty_frame.

    METHODS get_bar_color
      IMPORTING iv_index TYPE i
      RETURNING VALUE(rv_color) TYPE string.

ENDCLASS.

CLASS zcl_o4d_joy_division IMPLEMENTATION.

  METHOD zif_o4d_effect~get_name.
    rv_name = 'Joy Division'.
  ENDMETHOD.

  METHOD zif_o4d_effect~get_params.
    rt_params = VALUE #(
      ( name = 'num_bars' type = 'int' value = |{ mv_num_bars }| min = '4' max = '24' descr = 'Chart columns' )
      ( name = 'max_history' type = 'int' value = |{ mv_max_history }| min = '10' max = '80' descr = 'History depth' )
      ( name = 'intro_bars' type = 'int' value = |{ mv_intro_bars }| min = '1' max = '4' descr = 'Intro phase bars' )
      ( name = 'corporate_bars' type = 'int' value = |{ mv_corporate_bars }| min = '1' max = '4' descr = 'Corporate phase bars' )
      ( name = 'transform_bars' type = 'int' value = |{ mv_transform_bars }| min = '1' max = '4' descr = 'Transform phase bars' )
    ).
  ENDMETHOD.

  METHOD zif_o4d_effect~set_param.
    CASE iv_name.
      WHEN 'num_bars'.
        mv_num_bars = CONV i( iv_value ).
      WHEN 'max_history'.
        mv_max_history = CONV i( iv_value ).
      WHEN 'intro_bars'.
        mv_intro_bars = CONV i( iv_value ).
      WHEN 'corporate_bars'.
        mv_corporate_bars = CONV i( iv_value ).
      WHEN 'transform_bars'.
        mv_transform_bars = CONV i( iv_value ).
    ENDCASE.
  ENDMETHOD.

  METHOD zif_o4d_effect~render.
    rs_frame = zif_o4d_effect~render_frame(
      VALUE #( t = is_sync-time bi = VALUE #( bar = is_sync-bar bar_phase = is_sync-bar_phase ) )
    ).
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    CASE get_phase( is_ctx-bi ).
      WHEN 'intro'.
        rs_frame = render_intro( is_ctx ).
      WHEN 'corporate'.
        rs_frame = render_corporate( is_ctx ).
      WHEN 'transform'.
        rs_frame = render_transform( is_ctx ).
      WHEN 'joy_division'.
        rs_frame = render_joy_division( is_ctx ).
    ENDCASE.
  ENDMETHOD.

  METHOD get_phase.
    DATA(lv_bar) = is_bi-bar.
    DATA(lv_intro_end) = mv_intro_bars.
    DATA(lv_corp_end) = lv_intro_end + mv_corporate_bars.
    DATA(lv_trans_end) = lv_corp_end + mv_transform_bars.

    IF lv_bar < lv_intro_end.
      rv_phase = 'intro'.
    ELSEIF lv_bar < lv_corp_end.
      rv_phase = 'corporate'.
    ELSEIF lv_bar < lv_trans_end.
      rv_phase = 'transform'.
    ELSE.
      rv_phase = 'joy_division'.
    ENDIF.
  ENDMETHOD.

  METHOD get_bar_color.
    CASE iv_index MOD 4.
      WHEN 0. rv_color = '#2266aa'.
      WHEN 1. rv_color = '#3377bb'.
      WHEN 2. rv_color = '#4488cc'.
      WHEN 3. rv_color = '#5599dd'.
    ENDCASE.
  ENDMETHOD.

  METHOD render_intro.
    " Progress: bar + bar_phase within intro
    DATA(lv_bar) = is_ctx-bi-bar.
    DATA(lv_progress) = ( CONV f( lv_bar ) + is_ctx-bi-bar_phase ) / mv_intro_bars.
    IF lv_progress > 1. lv_progress = 1. ENDIF.

    " Smooth easing
    DATA(lv_ease) = lv_progress * lv_progress * ( 3 - 2 * lv_progress ).

    " Use same parameters as corporate phase for consistency
    DATA(lv_bar_w) = 40.
    DATA(lv_gap) = 10.
    DATA(lv_total_w) = mv_num_bars * ( lv_bar_w + lv_gap ) - lv_gap.
    DATA(lv_start_x) = 320 - lv_total_w / 2.
    DATA(lv_cy) = 220.  " Same as corporate

    " Show center bar (same as first bar in corporate)
    DATA(lv_center_idx) = mv_num_bars / 2.
    DATA(lv_x) = lv_start_x + lv_center_idx * ( lv_bar_w + lv_gap ).

    " Same height formula as corporate
    DATA(lv_seed) = lv_center_idx * 7 + 13.
    DATA(lv_max_h) = 60 + ( lv_seed MOD 100 ).
    DATA(lv_h) = lv_max_h * lv_ease.

    " Beat pulse effect
    DATA(lv_pulse) = is_ctx-bi-pulse.
    lv_h = lv_h * ( 1 + lv_pulse * '0.1' ).

    APPEND VALUE zif_o4d_effect=>ty_rect(
      x = lv_x
      y = lv_cy - lv_h / 2
      w = lv_bar_w
      h = lv_h
      fill = '#4488cc'
    ) TO rs_frame-rects.

    DATA(lv_alpha) = COND f( WHEN lv_progress > '0.5' THEN 1 ELSE lv_progress * 2 ).
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = 320 y = 50 text = 'Q1 SALES' color = |rgba(255,255,255,{ lv_alpha })| size = 24 align = 'center'
    ) TO rs_frame-texts.
  ENDMETHOD.

  METHOD render_corporate.
    " Progress within corporate phase
    DATA(lv_bar) = is_ctx-bi-bar - mv_intro_bars.
    DATA(lv_progress) = ( CONV f( lv_bar ) + is_ctx-bi-bar_phase ) / mv_corporate_bars.
    IF lv_progress > 1. lv_progress = 1. ENDIF.

    DATA(lv_bar_w) = 40.
    DATA(lv_gap) = 10.
    DATA(lv_total_w) = mv_num_bars * ( lv_bar_w + lv_gap ) - lv_gap.
    DATA(lv_start_x) = 320 - lv_total_w / 2.
    DATA(lv_cy) = 220.

    " Beat pulse
    DATA(lv_beat_pulse) = 1 + is_ctx-bi-pulse * '0.1'.

    DO mv_num_bars TIMES.
      DATA(lv_i) = sy-index - 1.
      DATA(lv_x) = lv_start_x + lv_i * ( lv_bar_w + lv_gap ).

      " Staggered appearance
      DATA(lv_delay) = CONV f( lv_i ) / mv_num_bars * '0.75'.
      DATA(lv_bar_prog) = COND f(
        WHEN lv_progress < lv_delay THEN 0
        ELSE ( lv_progress - lv_delay ) / ( 1 - lv_delay )
      ).
      IF lv_bar_prog > 1. lv_bar_prog = 1. ENDIF.

      DATA(lv_seed) = lv_i * 7 + 13.
      DATA(lv_h) = ( 60 + ( lv_seed MOD 100 ) ) * lv_bar_prog * lv_beat_pulse.

      APPEND VALUE zif_o4d_effect=>ty_rect(
        x = lv_x
        y = lv_cy - lv_h / 2
        w = lv_bar_w
        h = lv_h
        fill = get_bar_color( lv_i )
      ) TO rs_frame-rects.
    ENDDO.

    APPEND VALUE zif_o4d_effect=>ty_text(
      x = 320 y = 50 text = 'QUARTERLY SALES REPORT' color = '#ffffff' size = 20 align = 'center'
    ) TO rs_frame-texts.

    APPEND VALUE zif_o4d_effect=>ty_text(
      x = 320 y = 350 text = 'Fiscal Year 2025' color = '#888888' size = 14 align = 'center'
    ) TO rs_frame-texts.
  ENDMETHOD.

  METHOD render_transform.
    CONSTANTS lc_pi TYPE f VALUE '3.14159265358979'.

    " Progress within transform phase
    DATA(lv_phase_bar) = is_ctx-bi-bar - mv_intro_bars - mv_corporate_bars.
    DATA(lv_progress) = ( CONV f( lv_phase_bar ) + is_ctx-bi-bar_phase ) / mv_transform_bars.
    IF lv_progress > 1. lv_progress = 1. ENDIF.

    " Continuous beat counter for animation (bar * 4 + beat + beat_phase)
    DATA(lv_beat_pos) = CONV f( is_ctx-bi-bar * 4 + is_ctx-bi-beat ) + is_ctx-bi-beat_phase.

    " Number of bars increases with progress
    DATA(lv_num) = CONV i( mv_num_bars + lv_progress * ( 48 - mv_num_bars ) ).
    DATA(lv_bar_w) = 640 / lv_num - 2.
    DATA(lv_cy) = 200.

    " Chaos increases with progress
    DATA(lv_chaos) = lv_progress * lv_progress.

    DO lv_num TIMES.
      DATA(lv_i) = sy-index - 1.
      DATA(lv_x) = CONV f( lv_i ) * 640 / lv_num.

      " Wave based on beat position
      DATA(lv_phase) = CONV f( lv_i ) * '0.3' + lv_beat_pos * lc_pi * '0.5'.
      DATA(lv_wave) = sin( lv_phase ) * 60 * lv_chaos.
      DATA(lv_h) = 80 + lv_wave + sin( lv_beat_pos * lc_pi + lv_i ) * 40 * lv_chaos.

      " Beat pulse
      lv_h = lv_h * ( 1 + is_ctx-bi-pulse * '0.2' ).

      " Color shifts from blue to white
      DATA(lv_r) = CONV i( 68 + lv_progress * ( 255 - 68 ) ).
      DATA(lv_g) = CONV i( 136 + lv_progress * ( 255 - 136 ) ).
      DATA(lv_b) = CONV i( 204 + lv_progress * ( 255 - 204 ) ).
      DATA(lv_alpha) = '1.0' - lv_progress * '0.3'.

      APPEND VALUE zif_o4d_effect=>ty_rect(
        x = lv_x
        y = lv_cy - lv_h / 2
        w = lv_bar_w
        h = lv_h
        fill = |rgba({ lv_r },{ lv_g },{ lv_b },{ lv_alpha })|
      ) TO rs_frame-rects.
    ENDDO.

    " Glitching title synced to beat
    DATA(lv_glitch) = COND string(
      WHEN lv_progress > '0.8' THEN 'UNKNOWN PLEASURES'
      WHEN lv_progress > '0.6' THEN 'UNKN0WN PL34SUR3S'
      WHEN lv_progress > '0.4' THEN 'QU4RT3RLY S4L3S'
      ELSE 'QUARTERLY SALES'
    ).
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = 320 y = 50 text = lv_glitch color = '#ffffff' size = 20 align = 'center'
    ) TO rs_frame-texts.
  ENDMETHOD.

  METHOD render_joy_division.
    CONSTANTS lc_pi TYPE f VALUE '3.14159265358979'.

    " Continuous beat position for wave animation
    DATA(lv_beat_pos) = CONV f( is_ctx-bi-bar * 4 + is_ctx-bi-beat ) + is_ctx-bi-beat_phase.
    DATA(lv_global_beat) = is_ctx-bi-i4.  " Global 1/4 note position

    DATA lt_current TYPE STANDARD TABLE OF f WITH EMPTY KEY.
    DATA(lv_num_points) = 64.

    " Beat intensity affects waveform
    DATA(lv_beat_amp) = 1 + is_ctx-bi-pulse * '0.5'.

    DO lv_num_points TIMES.
      DATA(lv_i) = sy-index - 1.
      DATA(lv_x_norm) = CONV f( lv_i ) / ( lv_num_points - 1 ).

      " Waves driven by beat position
      DATA(lv_wave1) = sin( lv_x_norm * lc_pi * 4 + lv_beat_pos * lc_pi * '0.5' ) * 30 * lv_beat_amp.
      DATA(lv_wave2) = sin( lv_x_norm * lc_pi * 8 + lv_beat_pos * lc_pi * '0.8' ) * 15.
      DATA(lv_wave3) = sin( lv_x_norm * lc_pi * 2 + lv_beat_pos * lc_pi * '0.3' ) * 20.

      " Center peak
      DATA(lv_center_dist) = abs( lv_x_norm - '0.5' ) * 2.
      DATA(lv_peak) = ( 1 - lv_center_dist * lv_center_dist ) * 50 * lv_beat_amp.

      DATA(lv_y) = lv_wave1 + lv_wave2 + lv_wave3 + lv_peak.
      APPEND lv_y TO lt_current.
    ENDDO.

    " Add to history on each new beat (using global beat counter)
    IF lv_global_beat <> mv_last_beat.
      INSERT VALUE ty_history_line( points = lt_current ) INTO mv_history INDEX 1.
      IF lines( mv_history ) > mv_max_history.
        DELETE mv_history INDEX mv_max_history + 1.
      ENDIF.
      mv_last_beat = lv_global_beat.
    ENDIF.

    DATA(lv_base_y) = 350.
    DATA(lv_line_spacing) = 6.

    LOOP AT mv_history INTO DATA(ls_line).
      DATA(lv_line_idx) = sy-tabix - 1.
      DATA(lv_y_offset) = lv_base_y - lv_line_idx * lv_line_spacing.

      DATA(lv_alpha) = CONV f( 1 ) - CONV f( lv_line_idx ) / mv_max_history.

      DATA lv_prev_x TYPE f.
      DATA lv_prev_y TYPE f.
      DATA(lv_first) = abap_true.

      LOOP AT ls_line-points INTO DATA(lv_point_y).
        DATA(lv_pt_idx) = sy-tabix - 1.
        DATA(lv_px) = 60 + CONV f( lv_pt_idx ) / ( lv_num_points - 1 ) * 520.
        DATA(lv_py) = lv_y_offset - lv_point_y.

        IF lv_first = abap_false.
          APPEND VALUE zif_o4d_effect=>ty_line(
            x1 = lv_prev_x y1 = lv_prev_y
            x2 = lv_px y2 = lv_py
            color = |rgba(255,255,255,{ lv_alpha })|
            width = '1.5'
          ) TO rs_frame-lines.
        ENDIF.

        lv_prev_x = lv_px.
        lv_prev_y = lv_py.
        lv_first = abap_false.
      ENDLOOP.
    ENDLOOP.

    APPEND VALUE zif_o4d_effect=>ty_text(
      x = 320 y = 30 text = 'UNKNOWN PLEASURES' color = '#ffffff' size = 16 align = 'center'
    ) TO rs_frame-texts.
  ENDMETHOD.

ENDCLASS.
