CLASS zcl_o4d_sales_dance DEFINITION
  PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    METHODS constructor.

  PRIVATE SECTION.
    TYPES:
      tt_float TYPE STANDARD TABLE OF f WITH EMPTY KEY.

    CONSTANTS lc_pi TYPE f VALUE '3.14159265358979'.

    DATA mv_start_bars TYPE i VALUE 4.      " Initial bar count
    DATA mv_end_bars TYPE i VALUE 32.       " Final bar count
    DATA mv_intro_bars TYPE i VALUE 3.      " Phase 1: bars appear
    DATA mv_divide_bars TYPE i VALUE 3.     " Phase 2: slow division
    DATA mt_quarterly TYPE tt_float.

    METHODS get_phase
      IMPORTING is_ctx TYPE zif_o4d_effect=>ty_render_ctx
      RETURNING VALUE(rv_phase) TYPE string.

    METHODS get_dancing_values
      IMPORTING iv_num_bars TYPE i
                is_ctx TYPE zif_o4d_effect=>ty_render_ctx
      RETURNING VALUE(rt_values) TYPE tt_float.

    METHODS render_intro
      IMPORTING is_ctx TYPE zif_o4d_effect=>ty_render_ctx
      RETURNING VALUE(rs_frame) TYPE zif_o4d_effect=>ty_frame.

    METHODS render_divide
      IMPORTING is_ctx TYPE zif_o4d_effect=>ty_render_ctx
      RETURNING VALUE(rs_frame) TYPE zif_o4d_effect=>ty_frame.

    METHODS render_dance
      IMPORTING is_ctx TYPE zif_o4d_effect=>ty_render_ctx
      RETURNING VALUE(rs_frame) TYPE zif_o4d_effect=>ty_frame.

ENDCLASS.



CLASS ZCL_O4D_SALES_DANCE IMPLEMENTATION.


  METHOD constructor.
    APPEND CONV f( '0.3' ) TO mt_quarterly.
    APPEND CONV f( '0.5' ) TO mt_quarterly.
    APPEND CONV f( '0.4' ) TO mt_quarterly.
    APPEND CONV f( '0.7' ) TO mt_quarterly.
  ENDMETHOD.


  METHOD zif_o4d_effect~get_name.
    rv_name = 'Sales Dance'.
  ENDMETHOD.


  METHOD zif_o4d_effect~get_params.
    rt_params = VALUE #( ).
  ENDMETHOD.


  METHOD zif_o4d_effect~set_param.
  ENDMETHOD.


  METHOD zif_o4d_effect~render.
    rs_frame = zif_o4d_effect~render_frame(
      VALUE #( t = is_sync-time bi = VALUE #( bar = is_sync-bar bar_phase = is_sync-bar_phase ) )
    ).
  ENDMETHOD.


  METHOD zif_o4d_effect~render_frame.
    CASE get_phase( is_ctx ).
      WHEN 'intro'.
        rs_frame = render_intro( is_ctx ).
      WHEN 'divide'.
        rs_frame = render_divide( is_ctx ).
      WHEN 'dance'.
        rs_frame = render_dance( is_ctx ).
    ENDCASE.
  ENDMETHOD.


  METHOD get_phase.
    DATA(lv_bar) = is_ctx-bi-bar.
    IF lv_bar < mv_intro_bars.
      rv_phase = 'intro'.
    ELSEIF lv_bar < mv_intro_bars + mv_divide_bars.
      rv_phase = 'divide'.
    ELSE.
      rv_phase = 'dance'.
    ENDIF.
  ENDMETHOD.


  METHOD get_dancing_values.
    DATA(lv_beat_pos) = CONV f( is_ctx-bi-bar * 4 + is_ctx-bi-beat ) + is_ctx-bi-beat_phase.
    DATA(lv_pulse) = is_ctx-bi-pulse.

    DO iv_num_bars TIMES.
      DATA(lv_i) = sy-index - 1.
      DATA(lv_q_idx) = lv_i MOD 4 + 1.
      DATA(lv_base) = mt_quarterly[ lv_q_idx ].

      " Dancing waves synced to beat
      DATA(lv_wave) = sin( lv_beat_pos * lc_pi * '0.5' + lv_i * '0.5' ) * '0.25'.
      DATA(lv_wave2) = sin( lv_beat_pos * lc_pi + lv_i * '0.3' ) * '0.15'.
      DATA(lv_beat_boost) = lv_pulse * '0.3'.

      DATA(lv_val) = lv_base + lv_wave + lv_wave2 + lv_beat_boost.
      IF lv_val < '0.1'. lv_val = '0.1'. ENDIF.
      IF lv_val > 1. lv_val = 1. ENDIF.

      APPEND lv_val TO rt_values.
    ENDDO.
  ENDMETHOD.


  METHOD render_intro.
    " Phase 1: 4 boring corporate bars appear
    DATA(lv_progress) = ( CONV f( is_ctx-bi-bar ) + is_ctx-bi-bar_phase ) / mv_intro_bars.
    IF lv_progress > 1. lv_progress = 1. ENDIF.

    DATA(lv_w) = CONV f( zif_o4d_effect=>c_width ).
    DATA(lv_h) = CONV f( zif_o4d_effect=>c_height ).
    DATA(lv_bar_width) = lv_w / 6.
    DATA(lv_spacing) = lv_w / 5.
    DATA(lv_max_height) = lv_h - 80.
    DATA(lv_bottom) = lv_h - 40.

    " Title
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = 320 y = 25 text = 'SALES BY QUARTER'
      color = '#888888' size = 18 align = 'center'
    ) TO rs_frame-texts.

    " Draw 4 bars with staggered appearance
    DO mv_start_bars TIMES.
      DATA(lv_i) = sy-index - 1.
      DATA(lv_q_idx) = lv_i + 1.
      DATA(lv_val) = mt_quarterly[ lv_q_idx ].

      " Staggered delay for each bar
      DATA(lv_delay) = CONV f( lv_i ) / mv_start_bars * '0.6'.
      DATA(lv_bar_prog) = COND f(
        WHEN lv_progress < lv_delay THEN 0
        ELSE ( lv_progress - lv_delay ) / ( 1 - lv_delay )
      ).
      IF lv_bar_prog > 1. lv_bar_prog = 1. ENDIF.

      DATA(lv_bar_h) = lv_val * lv_max_height * lv_bar_prog.
      DATA(lv_x) = lv_spacing + lv_i * lv_spacing - lv_bar_width / 2.
      DATA(lv_y) = lv_bottom - lv_bar_h.

      " Corporate gray bars
      APPEND VALUE zif_o4d_effect=>ty_rect(
        x = lv_x y = lv_y w = lv_bar_width h = lv_bar_h
        fill = '#666688'
      ) TO rs_frame-rects.

      " Quarter labels
      IF lv_bar_prog > '0.5' ."AND is_ctx-bi-on_8th = abap_true.
        APPEND VALUE zif_o4d_effect=>ty_text(
          x = lv_x + lv_bar_width / 2 y = lv_h - 20
          text = |Q{ lv_q_idx }|
          color = '#888888' size = 14 align = 'center'
        ) TO rs_frame-texts.
      ENDIF.
    ENDDO.
  ENDMETHOD.


  METHOD render_divide.
    " Phase 2: Slow division from 4 to 32 bars
    DATA(lv_phase_bar) = is_ctx-bi-bar - mv_intro_bars.
    DATA(lv_progress) = ( CONV f( lv_phase_bar ) + is_ctx-bi-bar_phase ) / mv_divide_bars.
    IF lv_progress > 1. lv_progress = 1. ENDIF.

    " Smooth interpolation of bar count
    DATA(lv_num_bars) = CONV i( mv_start_bars + ( mv_end_bars - mv_start_bars ) * lv_progress ).
    IF lv_num_bars < mv_start_bars. lv_num_bars = mv_start_bars. ENDIF.

    DATA(lv_w) = CONV f( zif_o4d_effect=>c_width ).
    DATA(lv_h) = CONV f( zif_o4d_effect=>c_height ).
    DATA(lv_bar_width) = lv_w / lv_num_bars - 2.
    IF lv_bar_width < 3. lv_bar_width = 3. ENDIF.
    DATA(lv_max_height) = lv_h - 80.
    DATA(lv_bottom) = lv_h - 40.

    " Title morphing
    DATA(lv_title) = COND string(
      WHEN lv_progress < '0.5' THEN 'SALES BY QUARTER'
      ELSE '>>> MULTIPLYING <<<'
    ).
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = 320 y = 25 text = lv_title
      color = '#AAAAAA' size = 16 align = 'center'
    ) TO rs_frame-texts.

    " Draw dividing bars
    DO lv_num_bars TIMES.
      DATA(lv_i) = sy-index - 1.
      DATA(lv_q_idx) = lv_i MOD 4 + 1.
      DATA(lv_val) = mt_quarterly[ lv_q_idx ].

      DATA(lv_x) = ( CONV f( lv_i ) + '0.5' ) * lv_w / lv_num_bars - lv_bar_width / 2.
      DATA(lv_bar_h) = lv_val * lv_max_height.
      DATA(lv_y) = lv_bottom - lv_bar_h.

      " Color gets brighter as we divide
      DATA(lv_bright) = CONV i( 100 + 100 * lv_progress ).
      APPEND VALUE zif_o4d_effect=>ty_rect(
        x = lv_x y = lv_y w = lv_bar_width h = lv_bar_h
        fill = |rgb({ lv_bright },{ lv_bright },{ lv_bright + 30 })|
      ) TO rs_frame-rects.
    ENDDO.
  ENDMETHOD.


  METHOD render_dance.
    " Phase 3: Dancing bars with flash on kick!
    DATA(lv_w) = CONV f( zif_o4d_effect=>c_width ).
    DATA(lv_h) = CONV f( zif_o4d_effect=>c_height ).
    DATA(lv_num_bars) = mv_end_bars.

    DATA(lt_values) = get_dancing_values( iv_num_bars = lv_num_bars is_ctx = is_ctx ).

    DATA(lv_bar_width) = lv_w / lv_num_bars - 1.
    IF lv_bar_width < 2. lv_bar_width = 2. ENDIF.
    DATA(lv_max_height) = lv_h - 60.
    DATA(lv_bottom) = lv_h - 30.

    " Title
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = 320 y = 20 text = '>>> DANCE! <<<'
      color = '#FFFFFF' size = 18 align = 'center'
    ) TO rs_frame-texts.

    " Draw dancing bars
    LOOP AT lt_values INTO DATA(lv_val).
      DATA(lv_i) = sy-tabix - 1.
      DATA(lv_x) = ( CONV f( lv_i ) + '0.5' ) * lv_w / lv_num_bars - lv_bar_width / 2.
      DATA(lv_bar_h) = lv_val * lv_max_height.
      DATA(lv_y) = lv_bottom - lv_bar_h.

      " Color based on height - more colorful when dancing
      DATA(lv_hue) = CONV i( lv_i * 360 / lv_num_bars ).
      DATA(lv_sat) = CONV i( 50 + lv_val * 50 ).
      DATA(lv_light) = CONV i( 40 + lv_val * 30 ).

      APPEND VALUE zif_o4d_effect=>ty_rect(
        x = lv_x y = lv_y w = lv_bar_width h = lv_bar_h
        fill = |hsl({ lv_hue },{ lv_sat }%,{ lv_light }%)|
      ) TO rs_frame-rects.
    ENDLOOP.

    " FLASH on beat 2 of each bar (backbeat/snare)
*    IF is_ctx-gbi-pulse > CONV f( '0.7' ) AND is_ctx-gbi-beat = 2.
*      rs_frame-flash-active = abap_true.
*      rs_frame-flash-intensity = is_ctx-gbi-pulse * CONV f( '0.5' ).
*      rs_frame-flash-r = CONV f( '1.0' ).
*      rs_frame-flash-g = CONV f( '1.0' ).
*      rs_frame-flash-b = CONV f( '0.9' ).
*    ENDIF.

    " Debug
    rs_frame-debug-vars = |\{"phase":"dance","bars":{ lv_num_bars },"pulse":{ is_ctx-bi-pulse }\}|.
  ENDMETHOD.
ENDCLASS.
