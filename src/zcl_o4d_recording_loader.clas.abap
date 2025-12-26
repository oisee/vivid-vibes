"! Pattern Mapper Recording Loader
CLASS zcl_o4d_recording_loader DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_mouse_point,
        t       TYPE f,
        x       TYPE f,
        y       TYPE f,
        pressed TYPE abap_bool,
      END OF ty_mouse_point,
      tt_mouse_points TYPE STANDARD TABLE OF ty_mouse_point WITH EMPTY KEY,

      BEGIN OF ty_trigger_event,
        t   TYPE f,
        key TYPE string,
        bit TYPE i,
      END OF ty_trigger_event,
      tt_trigger_events TYPE STANDARD TABLE OF ty_trigger_event WITH EMPTY KEY.

    METHODS:
      constructor
        IMPORTING
          iv_bpm    TYPE f DEFAULT '76'
          iv_width  TYPE i DEFAULT 640
          iv_height TYPE i DEFAULT 400,

      enable_beat_triggers
        IMPORTING
          iv_kick_bit  TYPE i DEFAULT 0
          iv_snare_bit TYPE i DEFAULT 1,

      add_trigger
        IMPORTING
          iv_time TYPE f
          iv_bit  TYPE i
          iv_key  TYPE string DEFAULT 'Manual',

      add_point
        IMPORTING
          iv_time    TYPE f
          iv_x       TYPE f
          iv_y       TYPE f
          iv_pressed TYPE abap_bool DEFAULT abap_false,

      set_effect_range
        IMPORTING
          iv_start TYPE f
          iv_end   TYPE f,

      get_frame
        IMPORTING
          iv_time  TYPE f
          iv_frame TYPE i DEFAULT 0
        RETURNING
          VALUE(rs_frame) TYPE zif_o4d_effect=>ty_input_frame,

      has_recording
        RETURNING VALUE(rv_has) TYPE abap_bool.

  PRIVATE SECTION.
    DATA:
      mv_bpm           TYPE f,
      mv_beat_dur      TYPE f,
      mv_bar_dur       TYPE f,
      mv_width         TYPE i,
      mv_height        TYPE i,
      mt_mouse_points  TYPE tt_mouse_points,
      mt_triggers      TYPE tt_trigger_events,
      mv_effect_start  TYPE f,
      mv_effect_end    TYPE f,
      mv_beat_triggers TYPE abap_bool,
      mv_kick_bit      TYPE i,
      mv_snare_bit     TYPE i.

    METHODS:
      interpolate_position
        IMPORTING iv_time TYPE f
        EXPORTING ev_x TYPE f ev_y TYPE f ev_pressed TYPE abap_bool,

      get_triggers_at
        IMPORTING iv_time TYPE f
        RETURNING VALUE(rv_triggers) TYPE i,

      get_beat_triggers
        IMPORTING iv_time TYPE f
        RETURNING VALUE(rv_triggers) TYPE i,

      set_bit
        IMPORTING iv_value TYPE i iv_bit TYPE i
        RETURNING VALUE(rv_result) TYPE i.
ENDCLASS.

CLASS zcl_o4d_recording_loader IMPLEMENTATION.
  METHOD constructor.
    mv_bpm = iv_bpm.
    mv_beat_dur = 60 / mv_bpm.
    mv_bar_dur = mv_beat_dur * 4.
    mv_width = iv_width.
    mv_height = iv_height.
    mv_effect_start = 0.
    mv_effect_end = 99999.
    mv_beat_triggers = abap_false.
  ENDMETHOD.

  METHOD enable_beat_triggers.
    mv_beat_triggers = abap_true.
    mv_kick_bit = iv_kick_bit.
    mv_snare_bit = iv_snare_bit.
  ENDMETHOD.

  METHOD add_trigger.
    APPEND VALUE #( t = iv_time key = iv_key bit = iv_bit ) TO mt_triggers.
    SORT mt_triggers BY t ASCENDING.
  ENDMETHOD.

  METHOD add_point.
    APPEND VALUE #( t = iv_time x = iv_x y = iv_y pressed = iv_pressed ) TO mt_mouse_points.
    SORT mt_mouse_points BY t ASCENDING.
  ENDMETHOD.

  METHOD set_effect_range.
    mv_effect_start = iv_start.
    mv_effect_end = iv_end.
  ENDMETHOD.

  METHOD get_frame.
    DATA: lv_x TYPE f, lv_y TYPE f, lv_pressed TYPE abap_bool.

    interpolate_position( EXPORTING iv_time = iv_time
                          IMPORTING ev_x = lv_x ev_y = lv_y ev_pressed = lv_pressed ).

    DATA(lv_triggers) = get_triggers_at( iv_time ).
    IF mv_beat_triggers = abap_true.
      DATA(lv_beat_tr) = get_beat_triggers( iv_time ).
      lv_triggers = lv_triggers + lv_beat_tr.
    ENDIF.

    DATA(lv_local_time) = nmax( val1 = 0 val2 = iv_time - mv_effect_start ).
    DATA(lv_tick) = CONV i( lv_local_time * 30 ).

    rs_frame-x = CONV i( lv_x * mv_width ).
    rs_frame-y = CONV i( lv_y * mv_height ).
    rs_frame-x_norm = lv_x.
    rs_frame-y_norm = lv_y.
    rs_frame-pressed = lv_pressed.
    rs_frame-triggers = lv_triggers.
    rs_frame-t_global = iv_time.
    rs_frame-t_local = lv_local_time.
    rs_frame-frame = iv_frame.
    rs_frame-tick = lv_tick.

    rs_frame-x = nmax( val1 = 0 val2 = nmin( val1 = mv_width - 1 val2 = rs_frame-x ) ).
    rs_frame-y = nmax( val1 = 0 val2 = nmin( val1 = mv_height - 1 val2 = rs_frame-y ) ).

    rs_frame-global_rec = CORRESPONDING #( mt_mouse_points ).
    LOOP AT mt_mouse_points INTO DATA(ls_pt)
         WHERE t >= mv_effect_start AND t <= mv_effect_end.
      APPEND CORRESPONDING #( ls_pt ) TO rs_frame-local_rec.
    ENDLOOP.
    LOOP AT mt_triggers INTO DATA(ls_tr)
         WHERE t >= mv_effect_start AND t <= mv_effect_end.
      APPEND CORRESPONDING #( ls_tr ) TO rs_frame-trigger_events.
    ENDLOOP.
  ENDMETHOD.

  METHOD interpolate_position.
    ev_x = CONV f( '0.5' ).
    ev_y = CONV f( '0.5' ).
    ev_pressed = abap_false.

    IF mt_mouse_points IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_idx) = 0.
    LOOP AT mt_mouse_points INTO DATA(ls_pt).
      IF ls_pt-t >= iv_time.
        lv_idx = sy-tabix.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_idx <= 1.
      DATA(ls_first) = mt_mouse_points[ 1 ].
      ev_x = ls_first-x.
      ev_y = ls_first-y.
      ev_pressed = ls_first-pressed.
      RETURN.
    ENDIF.

    IF lv_idx = 0.
      DATA(ls_last) = mt_mouse_points[ lines( mt_mouse_points ) ].
      ev_x = ls_last-x.
      ev_y = ls_last-y.
      ev_pressed = ls_last-pressed.
      RETURN.
    ENDIF.

    DATA(ls_p1) = mt_mouse_points[ lv_idx - 1 ].
    DATA(ls_p2) = mt_mouse_points[ lv_idx ].

    IF ls_p2-t = ls_p1-t.
      ev_x = ls_p2-x.
      ev_y = ls_p2-y.
      ev_pressed = ls_p2-pressed.
      RETURN.
    ENDIF.

    DATA(lv_alpha) = ( iv_time - ls_p1-t ) / ( ls_p2-t - ls_p1-t ).
    ev_x = ls_p1-x + lv_alpha * ( ls_p2-x - ls_p1-x ).
    ev_y = ls_p1-y + lv_alpha * ( ls_p2-y - ls_p1-y ).
    ev_pressed = xsdbool( ls_p1-pressed = abap_true OR ls_p2-pressed = abap_true ).
  ENDMETHOD.

  METHOD get_triggers_at.
    rv_triggers = 0.
    DATA(lv_window) = CONV f( '0.05' ).
    LOOP AT mt_triggers INTO DATA(ls_tr)
         WHERE t >= iv_time - lv_window AND t <= iv_time + lv_window.
      rv_triggers = set_bit( iv_value = rv_triggers iv_bit = ls_tr-bit ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_beat_triggers.
    rv_triggers = 0.
    DATA(lv_beat_pos) = iv_time / mv_beat_dur.
    DATA(lv_beat_phase) = frac( lv_beat_pos ).

    IF lv_beat_phase < CONV f( '0.05' ) OR lv_beat_phase > CONV f( '0.95' ).
      rv_triggers = set_bit( iv_value = rv_triggers iv_bit = mv_kick_bit ).
    ENDIF.

    DATA(lv_beat_in_bar) = CONV i( lv_beat_pos ) MOD 4.
    IF ( lv_beat_in_bar = 1 OR lv_beat_in_bar = 3 ) AND
       ( lv_beat_phase < CONV f( '0.05' ) OR lv_beat_phase > CONV f( '0.95' ) ).
      rv_triggers = set_bit( iv_value = rv_triggers iv_bit = mv_snare_bit ).
    ENDIF.
  ENDMETHOD.

  METHOD set_bit.
    DATA(lv_mask) = 1.
    DO iv_bit TIMES.
      lv_mask = lv_mask * 2.
    ENDDO.
    IF iv_value MOD ( lv_mask * 2 ) < lv_mask.
      rv_result = iv_value + lv_mask.
    ELSE.
      rv_result = iv_value.
    ENDIF.
  ENDMETHOD.

  METHOD has_recording.
    rv_has = xsdbool( mt_mouse_points IS NOT INITIAL OR mt_triggers IS NOT INITIAL ).
  ENDMETHOD.
ENDCLASS.
