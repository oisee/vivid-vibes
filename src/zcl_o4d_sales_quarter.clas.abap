"! @title SAP SALES - Quarterly sales chart that transforms
"!
"! Phases:
"! 1. intro (0-2s) - Single centered chart grows from center
"! 2. corporate (2-4s) - Full "boring" corporate chart
"! 3. transform (4-6s) - Bars multiply and start dancing
"! 4. joy_division (6s+) - Joy Division style trails
CLASS zcl_o4d_sales_quarter DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.

  PRIVATE SECTION.
    TYPES:
      tt_values TYPE STANDARD TABLE OF f WITH EMPTY KEY,
      BEGIN OF ty_history_line,
        values TYPE tt_values,
      END OF ty_history_line,
      tt_history TYPE STANDARD TABLE OF ty_history_line WITH EMPTY KEY.

    CONSTANTS:
      c_width  TYPE f VALUE '640',
      c_height TYPE f VALUE '400'.

    DATA:
      mv_initialized   TYPE abap_bool,
      mt_history       TYPE tt_history,
      mv_history_lines TYPE i VALUE 30.

    METHODS:
      render_intro
        IMPORTING iv_t     TYPE f
                  is_bi    TYPE zif_o4d_effect=>ty_beat_info
        CHANGING  cs_frame TYPE zif_o4d_effect=>ty_frame,

      render_corporate
        IMPORTING iv_t     TYPE f
                  is_bi    TYPE zif_o4d_effect=>ty_beat_info
        CHANGING  cs_frame TYPE zif_o4d_effect=>ty_frame,

      render_transform
        IMPORTING iv_t     TYPE f
                  is_bi    TYPE zif_o4d_effect=>ty_beat_info
        CHANGING  cs_frame TYPE zif_o4d_effect=>ty_frame,

      render_joy_division
        IMPORTING iv_t     TYPE f
                  is_bi    TYPE zif_o4d_effect=>ty_beat_info
        CHANGING  cs_frame TYPE zif_o4d_effect=>ty_frame,

      hsv_to_hex
        IMPORTING iv_h         TYPE f
                  iv_s         TYPE f
                  iv_v         TYPE f
        RETURNING VALUE(rv_hex) TYPE string.

ENDCLASS.



CLASS ZCL_O4D_SALES_QUARTER IMPLEMENTATION.


  METHOD zif_o4d_effect~get_name.
    rv_name = 'sales_quarter'.
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
    ).
    rs_frame = zif_o4d_effect~render_frame( ls_ctx ).
  ENDMETHOD.


  METHOD zif_o4d_effect~render_frame.
    DATA(lv_t)  = is_ctx-bi-bar.
    DATA(ls_bi) = is_ctx-bi.

    " Phase selection
    IF lv_t < 2.
      render_intro( EXPORTING iv_t = is_ctx-t is_bi = ls_bi CHANGING cs_frame = rs_frame ).
    ELSEIF lv_t < 4.
      render_corporate( EXPORTING iv_t = is_ctx-t - 2 is_bi = ls_bi CHANGING cs_frame = rs_frame ).
    ELSEIF lv_t < 6.
      render_transform( EXPORTING iv_t = is_ctx-t - 4 is_bi = ls_bi CHANGING cs_frame = rs_frame ).
    ELSE.
      render_joy_division( EXPORTING iv_t = is_ctx-t - 6 is_bi = ls_bi CHANGING cs_frame = rs_frame ).
    ENDIF.

    " Debug vars
    DATA(lv_phase) = COND string(
      WHEN lv_t < 2 THEN 'intro'
      WHEN lv_t < 4 THEN 'corporate'
      WHEN lv_t < 6 THEN 'transform'
      ELSE 'joy_division' ).
    rs_frame-debug-vars = |\{"phase":"{ lv_phase }","t":{ lv_t },"history_lines":{ lines( mt_history ) }\}|.
  ENDMETHOD.


  METHOD render_intro.
    " Phase 1: Single chart grows from center
    DATA(lv_progress) = iv_t / 2.  " 0 to 1
    DATA(lv_scale) = lv_progress.

    " Center position
    DATA(lv_cx) = c_width / 2.
    DATA(lv_cy) = c_height / 2.

    " Chart dimensions (grow from center)
    DATA(lv_chart_w) = 200 * lv_scale.
    DATA(lv_chart_h) = 150 * lv_scale.

    " Title fades in
    IF lv_progress > '0.3'.
      DATA(lv_title_alpha) = ( lv_progress - '0.3' ) / '0.3'.
      DATA(lv_gray) = CONV i( 100 * lv_title_alpha ).
      APPEND VALUE zif_o4d_effect=>ty_text(
        x = lv_cx  y = lv_cy - lv_chart_h / 2 - 20
        text = |SALES BY QUARTER|
        color = |rgb({ lv_gray },{ lv_gray },{ lv_gray + 20 })|
        size = CONV i( 12 * lv_scale + 4 )  align = 'center'
      ) TO cs_frame-texts.
    ENDIF.

    " 4 bars grow from center
    DATA(lt_values) = VALUE tt_values( ( CONV f( '0.3' ) ) ( CONV f( '0.5' ) )
                                       ( CONV f( '0.4' ) ) ( CONV f( '0.7' ) ) ).
    DATA(lt_labels) = VALUE string_table( ( `Q1` ) ( `Q2` ) ( `Q3` ) ( `Q4` ) ).

    DATA(lv_bar_w) = lv_chart_w / 5.
    DATA(lv_gap) = lv_chart_w / 20.

    LOOP AT lt_values INTO DATA(lv_val).
      DATA(lv_idx) = sy-tabix - 1.

      " Bar position relative to center
      DATA(lv_bar_x) = lv_cx - lv_chart_w / 2 + lv_gap + lv_idx * ( lv_bar_w + lv_gap ).
      DATA(lv_bar_h) = lv_val * lv_chart_h * lv_scale.

      " Grow from center Y
      DATA(lv_bar_y) = lv_cy - lv_bar_h / 2.

      " Gray corporate color
      DATA(lv_brightness) = CONV i( 80 + lv_val * 40 ).
      DATA(lv_color) = |rgb({ lv_brightness },{ lv_brightness },{ lv_brightness + 20 })|.

      APPEND VALUE zif_o4d_effect=>ty_rect(
        x = lv_bar_x  y = lv_bar_y  w = lv_bar_w  h = lv_bar_h
        fill = lv_color
      ) TO cs_frame-rects.

      " Labels appear late
      IF lv_progress > '0.6'.
        READ TABLE lt_labels INDEX lv_idx + 1 INTO DATA(lv_label).
        APPEND VALUE zif_o4d_effect=>ty_text(
          x = lv_bar_x + lv_bar_w / 2  y = lv_cy + lv_chart_h / 2 + 15
          text = lv_label  color = '#666666'
          size = CONV i( 8 * lv_scale + 4 )  align = 'center'
        ) TO cs_frame-texts.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD render_corporate.
    " Phase 2: Full corporate chart - boring but complete
    DATA(lv_cx) = c_width / 2.
    DATA(lv_cy) = c_height / 2.
    DATA(lv_chart_w) = CONV f( 200 ).
    DATA(lv_chart_h) = CONV f( 150 ).

    " Title
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_cx  y = lv_cy - lv_chart_h / 2 - 20
      text = |SALES BY QUARTER|  color = '#8888AA'
      size = 16  align = 'center'
    ) TO cs_frame-texts.

    DATA(lt_values) = VALUE tt_values( ( CONV f( '0.3' ) ) ( CONV f( '0.5' ) )
                                       ( CONV f( '0.4' ) ) ( CONV f( '0.7' ) ) ).
    DATA(lt_labels) = VALUE string_table( ( `Q1` ) ( `Q2` ) ( `Q3` ) ( `Q4` ) ).

    DATA(lv_bar_w) = lv_chart_w / 5.
    DATA(lv_gap) = lv_chart_w / 20.

    LOOP AT lt_values INTO DATA(lv_val).
      DATA(lv_idx) = sy-tabix - 1.

      " Beat pulse on current quarter
      DATA(lv_pulse) = CONV f( 1 ).
      IF lv_idx = is_bi-beat MOD 4.
        lv_pulse = 1 + is_bi-pulse * CONV f( '0.15' ).
      ENDIF.

      DATA(lv_bar_x) = lv_cx - lv_chart_w / 2 + lv_gap + lv_idx * ( lv_bar_w + lv_gap ).
      DATA(lv_bar_h) = lv_val * lv_chart_h * lv_pulse.
      DATA(lv_bar_y) = lv_cy + lv_chart_h / 2 - lv_bar_h.

      DATA(lv_brightness) = CONV i( 80 + lv_val * 40 ).
      DATA(lv_color) = |rgb({ lv_brightness },{ lv_brightness },{ lv_brightness + 20 })|.

      APPEND VALUE zif_o4d_effect=>ty_rect(
        x = lv_bar_x  y = lv_bar_y  w = lv_bar_w  h = lv_bar_h
        fill = lv_color
      ) TO cs_frame-rects.

      READ TABLE lt_labels INDEX lv_idx + 1 INTO DATA(lv_label).
      APPEND VALUE zif_o4d_effect=>ty_text(
        x = lv_bar_x + lv_bar_w / 2  y = lv_cy + lv_chart_h / 2 + 15
        text = lv_label  color = '#666666'
        size = 12  align = 'center'
      ) TO cs_frame-texts.
    ENDLOOP.
  ENDMETHOD.


  METHOD render_transform.
    " Phase 3: Bars multiply and start dancing
    DATA(lv_progress) = iv_t / 2.  " 0 to 1

    " Number of bars increases
    DATA(lv_num_bars) = 4 + CONV i( lv_progress * 28 ).  " 4 → 32

    " Base values that will "dance"
    DATA lt_values TYPE tt_values.
    DO lv_num_bars TIMES.
      DATA(lv_i) = sy-index - 1.
      DATA(lv_val) = CONV f( '0.3' ) + CONV f( '0.4' ) * sin( iv_t * 3 + lv_i * CONV f( '0.5' ) ).
      APPEND lv_val TO lt_values.
    ENDDO.

    " Chart spans more of screen
    DATA(lv_chart_w) = 200 + lv_progress * 400.  " 200 → 600
    DATA(lv_bar_w) = nmax( val1 = CONV f( 4 ) val2 = lv_chart_w / lv_num_bars - 2 ).
    DATA(lv_base_y) = c_height - 40.
    DATA(lv_max_h) = c_height - 80.

    LOOP AT lt_values INTO DATA(lv_v).
      lv_i = sy-tabix - 1.
      DATA(lv_bar_x) = ( c_width - lv_chart_w ) / 2 + lv_i * ( lv_chart_w / lv_num_bars ).
      DATA(lv_bar_h) = lv_v * lv_max_h * ( CONV f( '0.5' ) + CONV f( '0.5' ) * lv_progress ).
      DATA(lv_bar_y) = lv_base_y - lv_bar_h.

      " Transition from gray to white
      DATA(lv_gray) = CONV i( 100 + 155 * lv_progress ).
      DATA(lv_color) = |rgb({ lv_gray },{ lv_gray },{ lv_gray })|.

      APPEND VALUE zif_o4d_effect=>ty_rect(
        x = lv_bar_x  y = lv_bar_y  w = lv_bar_w  h = lv_bar_h
        fill = lv_color
      ) TO cs_frame-rects.
    ENDLOOP.
  ENDMETHOD.


  METHOD render_joy_division.
    " Phase 4: Joy Division style with history trails
    DATA(lv_num_bars) = 32.

    " Generate current spectrum (dancing values)
    DATA lt_current TYPE tt_values.
    DO lv_num_bars TIMES.
      DATA(lv_i) = sy-index - 1.
      DATA(lv_val) = CONV f( '0.3' ) +
        CONV f( '0.4' ) * sin( iv_t * 2 + lv_i * CONV f( '0.3' ) ) +
        CONV f( '0.2' ) * sin( iv_t * 5 + lv_i * CONV f( '0.7' ) ).
      " Beat pulse
      IF is_bi-on_beat = abap_true.
        lv_val = lv_val * ( 1 + is_bi-pulse * CONV f( '0.3' ) ).
      ENDIF.
      APPEND lv_val TO lt_current.
    ENDDO.

    " Add to history
    APPEND VALUE ty_history_line( values = lt_current ) TO mt_history.
    IF lines( mt_history ) > mv_history_lines.
      DELETE mt_history INDEX 1.
    ENDIF.

    " Render history lines (Joy Division style)
    DATA(lv_line_spacing) = ( c_height - 60 ) / mv_history_lines.

    LOOP AT mt_history INTO DATA(ls_hist).
      DATA(lv_line_idx) = sy-tabix - 1.
      DATA(lv_y_base) = CONV f( 40 ) + lv_line_idx * lv_line_spacing.

      " Older lines are dimmer
      DATA(lv_age) = CONV f( lv_line_idx ) / lines( mt_history ).
      DATA(lv_brightness) = CONV i( 50 + 200 * lv_age ).

      " Draw line segments
      DATA(lv_prev_x) = CONV f( 0 ).
      DATA(lv_prev_y) = CONV f( 0 ).
      DATA(lv_first) = abap_true.

      LOOP AT ls_hist-values INTO lv_val.
        lv_i = sy-tabix - 1.
        DATA(lv_x) = ( CONV f( lv_i ) + CONV f( '0.5' ) ) * c_width / lv_num_bars.
        DATA(lv_y) = lv_y_base - lv_val * lv_line_spacing * 2.

        IF lv_first = abap_false.
          " Draw line from prev to current
          DATA(lv_color) = |rgb({ lv_brightness },{ lv_brightness },{ lv_brightness })|.
          APPEND VALUE zif_o4d_effect=>ty_line(
            x1 = lv_prev_x  y1 = lv_prev_y
            x2 = lv_x       y2 = lv_y
            color = lv_color  width = 1
          ) TO cs_frame-lines.
        ENDIF.

        lv_prev_x = lv_x.
        lv_prev_y = lv_y.
        lv_first = abap_false.
      ENDLOOP.
    ENDLOOP.
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

    DATA(lv_ri) = CONV i( lv_r * 255 ).
    DATA(lv_gi) = CONV i( lv_g * 255 ).
    DATA(lv_bi) = CONV i( lv_b * 255 ).

    DATA: lv_rh TYPE x LENGTH 1, lv_gh TYPE x LENGTH 1, lv_bh TYPE x LENGTH 1.
    lv_rh = lv_ri. lv_gh = lv_gi. lv_bh = lv_bi.
    rv_hex = |#{ lv_rh }{ lv_gh }{ lv_bh }|.
  ENDMETHOD.
ENDCLASS.
