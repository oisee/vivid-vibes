CLASS zcl_o4d_tornado_greetings DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    METHODS constructor.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_greeting,
             text   TYPE string,
             angle  TYPE f,
             height TYPE f,
             size   TYPE i,
             hue    TYPE i,
           END OF ty_greeting,
           tt_greetings TYPE STANDARD TABLE OF ty_greeting WITH EMPTY KEY.

    TYPES: BEGIN OF ty_flyout,
             text      TYPE string,
             start_t   TYPE f,
             duration  TYPE f,
             from_x    TYPE f,
             from_y    TYPE f,
             from_size TYPE i,
           END OF ty_flyout.

    DATA: mt_greetings   TYPE tt_greetings,
          mv_rotation    TYPE f,
          ms_flyout      TYPE ty_flyout,
          mv_frozen      TYPE abap_bool,
          mv_freeze_t    TYPE f,
          mv_last_beat   TYPE i.

    CONSTANTS: c_cx        TYPE i VALUE 320,
               c_cy        TYPE i VALUE 200,
               c_radius    TYPE f VALUE '120.0',
               c_height    TYPE f VALUE '300.0',
               c_freeze_dur TYPE f VALUE '0.15'.

    METHODS: init_greetings,
             calc_tornado_pos IMPORTING iv_angle  TYPE f
                                        iv_height TYPE f
                                        iv_time   TYPE f
                              EXPORTING ev_x      TYPE f
                                        ev_y      TYPE f
                                        ev_scale  TYPE f.
ENDCLASS.

CLASS zcl_o4d_tornado_greetings IMPLEMENTATION.

  METHOD constructor.
    init_greetings( ).
    mv_rotation = 0.
    mv_frozen = abap_false.
    mv_last_beat = -1.
  ENDMETHOD.

  METHOD init_greetings.
    DATA: lt_names TYPE string_table.
    APPEND 'OISEE' TO lt_names.
    APPEND 'CLAUDE' TO lt_names.
    APPEND 'ANTHROPIC' TO lt_names.
    APPEND 'SAP' TO lt_names.
    APPEND 'ABAP' TO lt_names.
    APPEND 'DEMOSCENE' TO lt_names.
    APPEND 'REVISION' TO lt_names.
    APPEND 'ASSEMBLY' TO lt_names.
    APPEND 'POUET' TO lt_names.
    APPEND 'FARBRAUSCH' TO lt_names.
    APPEND 'CONSPIRACY' TO lt_names.
    APPEND 'ASD' TO lt_names.
    APPEND 'STILL' TO lt_names.
    APPEND 'RAZOR1911' TO lt_names.
    APPEND 'FAIRLIGHT' TO lt_names.
    APPEND 'TPOLM' TO lt_names.

    DATA(lv_count) = lines( lt_names ).
    LOOP AT lt_names INTO DATA(lv_name).
      DATA(lv_idx) = sy-tabix - 1.
      APPEND VALUE ty_greeting(
        text   = lv_name
        angle  = ( CONV f( lv_idx ) / lv_count ) * 2 * '3.14159265'
        height = CONV f( lv_idx MOD 5 ) / 5
        size   = 16 + ( lv_idx MOD 3 ) * 4
        hue    = ( lv_idx * 23 ) MOD 360
      ) TO mt_greetings.
    ENDLOOP.
  ENDMETHOD.

  METHOD calc_tornado_pos.
    DATA(lv_tornado_r) = c_radius * ( '0.3' + iv_height * '0.7' ).
    DATA(lv_rot_angle) = iv_angle + iv_time * '2.5'.
    lv_rot_angle = lv_rot_angle + iv_height * sin( iv_time * 3 ) * '0.5'.

    DATA(lv_x3d) = lv_tornado_r * cos( lv_rot_angle ).
    DATA(lv_z3d) = lv_tornado_r * sin( lv_rot_angle ).
    DATA(lv_y3d) = ( iv_height - '0.5' ) * c_height.

    DATA(lv_depth) = lv_z3d + 300.
    DATA(lv_scale) = 250 / lv_depth.

    ev_x = c_cx + lv_x3d * lv_scale.
    ev_y = c_cy - lv_y3d * lv_scale + 50.
    ev_scale = lv_scale.
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    DATA: lt_items TYPE TABLE OF ty_greeting,
          lv_x TYPE f, lv_y TYPE f, lv_scale TYPE f,
          lv_bright TYPE i.

    DATA(lv_t) = is_ctx-t.

    " Check for freeze frame on beat
    IF is_ctx-bi-on_beat = abap_true AND is_ctx-bi-beat <> mv_last_beat.
      mv_last_beat = is_ctx-bi-beat.
      IF is_ctx-bi-beat = 0 OR is_ctx-bi-beat = 2.
        mv_frozen = abap_true.
        mv_freeze_t = lv_t.
        DATA(lv_pick) = is_ctx-bi-bar MOD lines( mt_greetings ).
        READ TABLE mt_greetings INTO DATA(ls_pick) INDEX lv_pick + 1.
        IF sy-subrc = 0.
          calc_tornado_pos( EXPORTING iv_angle = ls_pick-angle
                                       iv_height = ls_pick-height
                                       iv_time = lv_t
                            IMPORTING ev_x = lv_x ev_y = lv_y ev_scale = lv_scale ).
          ms_flyout = VALUE #( text = ls_pick-text start_t = lv_t duration = '0.4'
                               from_x = lv_x from_y = lv_y from_size = ls_pick-size ).
        ENDIF.
      ENDIF.
    ENDIF.

    " Unfreeze after short duration
    DATA(lv_elapsed) = lv_t - mv_freeze_t.
    IF mv_frozen = abap_true AND lv_elapsed > c_freeze_dur.
      mv_frozen = abap_false.
    ENDIF.

    DATA(lv_render_t) = COND f( WHEN mv_frozen = abap_true THEN mv_freeze_t ELSE lv_t ).

    " Background gradient
    DO 20 TIMES.
      DATA(lv_i) = sy-index - 1.
      DATA(lv_by) = lv_i * 20.
      lv_bright = 5 + lv_i.
      APPEND VALUE #( x = 0 y = lv_by w = 640 h = 20
                      fill = |rgb({ lv_bright },{ lv_bright },{ lv_bright + 10 })| ) TO rs_frame-rects.
    ENDDO.

    " Sort greetings by depth
    lt_items = mt_greetings.
    LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<ls_g>).
      DATA(lv_z) = c_radius * sin( <ls_g>-angle + lv_render_t * '2.5' ).
      <ls_g>-height = <ls_g>-height + lv_z / 1000.
    ENDLOOP.
    SORT lt_items BY height ASCENDING.

    " Render tornado greetings
    LOOP AT lt_items INTO DATA(ls_g).
      DATA(lv_orig_h) = ( sy-tabix - 1 ) MOD 5.
      DATA(lv_h) = CONV f( lv_orig_h ) / 5.
      lv_h = frac( lv_h + lv_render_t * '0.15' ).

      calc_tornado_pos( EXPORTING iv_angle = ls_g-angle
                                   iv_height = lv_h
                                   iv_time = lv_render_t
                        IMPORTING ev_x = lv_x ev_y = lv_y ev_scale = lv_scale ).

      IF lv_scale < '0.2' OR lv_scale > 3 OR lv_x < -50 OR lv_x > 690.
        CONTINUE.
      ENDIF.

      DATA(lv_size) = CONV i( ls_g-size * lv_scale ).
      IF lv_size < 8. lv_size = 8. ENDIF.
      IF lv_size > 48. lv_size = 48. ENDIF.

      lv_bright = CONV i( 50 + lv_scale * 150 ).
      IF lv_bright > 255. lv_bright = 255. ENDIF.
      DATA(lv_hue) = ( ls_g-hue + CONV i( lv_render_t * 30 ) ) MOD 360.

      APPEND VALUE #( x = lv_x y = lv_y text = ls_g-text
                      size = lv_size
                      color = |hsl({ lv_hue },80%,{ lv_bright / 5 }%)| ) TO rs_frame-texts.
    ENDLOOP.

    " Render flyout greeting
    IF ms_flyout-text IS NOT INITIAL.
      DATA(lv_fly_progress) = ( lv_t - ms_flyout-start_t ) / ms_flyout-duration.
      IF lv_fly_progress >= 0 AND lv_fly_progress <= 1.
        DATA(lv_ease) = 1 - ( 1 - lv_fly_progress ) ** 3.

        DATA(lv_fly_x) = ms_flyout-from_x + ( c_cx - ms_flyout-from_x ) * lv_ease.
        DATA(lv_fly_y) = ms_flyout-from_y + ( c_cy - 50 - ms_flyout-from_y ) * lv_ease.
        DATA(lv_fly_size) = CONV i( ms_flyout-from_size + ( 64 - ms_flyout-from_size ) * lv_ease ).
        DATA(lv_fly_alpha) = CONV i( 255 * ( 1 - lv_fly_progress ) ).

        APPEND VALUE #( x = lv_fly_x y = lv_fly_y text = ms_flyout-text
                        size = lv_fly_size + 4
                        color = |rgba(255,255,0,{ lv_fly_alpha / 3 })| ) TO rs_frame-texts.
        APPEND VALUE #( x = lv_fly_x y = lv_fly_y text = ms_flyout-text
                        size = lv_fly_size
                        color = |rgba(255,255,255,{ lv_fly_alpha })| ) TO rs_frame-texts.
      ELSEIF lv_fly_progress > 1.
        CLEAR ms_flyout.
      ENDIF.
    ENDIF.

    " Freeze frame flash
    IF mv_frozen = abap_true.
      DATA(lv_flash) = CONV i( 100 * ( 1 - lv_elapsed / c_freeze_dur ) ).
      IF lv_flash > 0.
        APPEND VALUE #( x = 0 y = 0 w = 640 h = 400
                        fill = |rgba(255,255,255,{ lv_flash })| ) TO rs_frame-rects.
      ENDIF.
    ENDIF.

    APPEND VALUE #( x = 320 y = 380 text = 'GREETINGS TO...'
                    size = 14 color = '#888888' ) TO rs_frame-texts.

    rs_frame-debug-vars = |\{"frozen":{ COND string( WHEN mv_frozen = abap_true THEN 'true' ELSE 'false' ) },| &&
                          |"flyout":"{ ms_flyout-text }","greetings":{ lines( mt_greetings ) }\}|.
  ENDMETHOD.

  METHOD zif_o4d_effect~get_name.
    rv_name = 'tornado_greetings'.
  ENDMETHOD.

  METHOD zif_o4d_effect~set_param.
  ENDMETHOD.

ENDCLASS.
