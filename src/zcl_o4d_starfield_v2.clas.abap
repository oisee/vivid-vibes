CLASS zcl_o4d_starfield_v2 DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.

    TYPES: BEGIN OF ty_star,
             sx TYPE f,
             sy TYPE f,
             sz TYPE f,
           END OF ty_star.

    CONSTANTS: c_num_stars TYPE i VALUE 120.
    METHODS constructor.

  PRIVATE SECTION.
    DATA mt_stars TYPE STANDARD TABLE OF ty_star.
    DATA mv_initialized TYPE abap_bool.
    DATA mv_name TYPE string VALUE 'STARFIELD_V2'.
    METHODS init_stars.
    METHODS int_to_hex IMPORTING iv_int TYPE i RETURNING VALUE(rv_hex) TYPE string.

ENDCLASS.

CLASS zcl_o4d_starfield_v2 IMPLEMENTATION.

  METHOD constructor.
    mv_initialized = abap_false.
  ENDMETHOD.

  METHOD int_to_hex.
    DATA lv_hex TYPE x LENGTH 1.
    lv_hex = iv_int.
    rv_hex = |{ lv_hex }|.
  ENDMETHOD.

  METHOD init_stars.
    DATA ls_star TYPE ty_star.
    DATA lv_seed TYPE i.
    CLEAR mt_stars.
    DO c_num_stars TIMES.
      lv_seed = sy-index * 73 + 17.
      ls_star-sx = ( lv_seed * 31 MOD 640 ) - 320.
      ls_star-sy = ( lv_seed * 47 MOD 400 ) - 200.
      ls_star-sz = lv_seed * 23 MOD 500.
      APPEND ls_star TO mt_stars.
    ENDDO.
    mv_initialized = abap_true.
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
    DATA: lv_t TYPE f, lv_cycle_t TYPE f, lv_phase TYPE string,
          lv_progress TYPE f, lv_speed TYPE f, lv_num_stars TYPE i.
    DATA: lv_sz TYPE f, lv_factor TYPE f, lv_x TYPE f, lv_y TYPE f,
          lv_size TYPE f, lv_bright TYPE i, lv_x2 TYPE f, lv_y2 TYPE f,
          lv_blue TYPE i, lv_line_len TYPE f, lv_pct TYPE i.
    DATA: lv_hex_r TYPE string, lv_hex_g TYPE string, lv_hex_b TYPE string.
    DATA ls_line TYPE zif_o4d_effect=>ty_line.
    DATA ls_rect TYPE zif_o4d_effect=>ty_rect.
    DATA ls_text TYPE zif_o4d_effect=>ty_text.
    FIELD-SYMBOLS <star> TYPE ty_star.

    IF mv_initialized = abap_false.
      init_stars( ).
    ENDIF.

    lv_t = is_ctx-gbi-time.
    lv_cycle_t = lv_t - floor( lv_t / 16 ) * 16.

    IF lv_cycle_t < 4.
      lv_phase = 'intro'.
      lv_progress = lv_cycle_t / 4.
      lv_speed = '0.2' + lv_progress * '0.8'.
    ELSEIF lv_cycle_t < 8.
      lv_phase = 'cruise'.
      lv_progress = ( lv_cycle_t - 4 ) / 4.
      lv_speed = 1.
    ELSEIF lv_cycle_t < 12.
      lv_phase = 'tunnel'.
      lv_progress = ( lv_cycle_t - 8 ) / 4.
      lv_speed = 1 + lv_progress * 4.
    ELSE.
      lv_phase = 'arrive'.
      lv_progress = ( lv_cycle_t - 12 ) / 4.
      lv_speed = 5 - lv_progress * '4.7'.
      IF lv_speed < '0.3'. lv_speed = '0.3'. ENDIF.
    ENDIF.

    lv_num_stars = COND #( WHEN lv_phase = 'tunnel' THEN 120 ELSE 100 ).

    DATA lv_idx TYPE i VALUE 0.
    LOOP AT mt_stars ASSIGNING <star>.
      lv_idx = lv_idx + 1.
      IF lv_idx > lv_num_stars. EXIT. ENDIF.

      lv_sz = 500 - ( ( <star>-sz + lv_t * 100 * lv_speed ) MOD 500 ) + 20.
      lv_factor = 200 / lv_sz.
      lv_x = 160 + <star>-sx * lv_factor.
      lv_y = 100 + <star>-sy * lv_factor.

      IF lv_x < 0 OR lv_x > 320 OR lv_y < 0 OR lv_y > 200. CONTINUE. ENDIF.

      lv_size = 4 - lv_sz / 150.
      IF lv_size < 1. lv_size = 1. ENDIF.
      lv_size = lv_size + is_ctx-gbi-pulse * '0.5'.

      lv_bright = 300 - lv_sz * '0.4'.
      IF lv_bright > 255. lv_bright = 255. ENDIF.
      IF lv_bright < 50. lv_bright = 50. ENDIF.

      lv_hex_r = int_to_hex( lv_bright ).
      lv_hex_g = int_to_hex( lv_bright ).

      IF lv_phase = 'tunnel'.
        lv_line_len = lv_speed * 3.
        lv_x2 = lv_x + ( lv_x - 160 ) * lv_line_len / 50.
        lv_y2 = lv_y + ( lv_y - 100 ) * lv_line_len / 50.
        lv_blue = lv_bright + 50.
        IF lv_blue > 255. lv_blue = 255. ENDIF.
        lv_hex_b = int_to_hex( lv_blue ).

        CLEAR ls_line.
        ls_line-x1 = lv_x.
        ls_line-y1 = lv_y.
        ls_line-x2 = lv_x2.
        ls_line-y2 = lv_y2.
        ls_line-color = |#{ lv_hex_r }{ lv_hex_g }{ lv_hex_b }|.
        ls_line-width = lv_size / 2.
        APPEND ls_line TO rs_frame-lines.
      ELSE.
        lv_hex_b = int_to_hex( lv_bright ).
        CLEAR ls_rect.
        ls_rect-x = lv_x - lv_size / 2.
        ls_rect-y = lv_y - lv_size / 2.
        ls_rect-w = lv_size.
        ls_rect-h = lv_size.
        ls_rect-fill = |#{ lv_hex_r }{ lv_hex_g }{ lv_hex_b }|.
        APPEND ls_rect TO rs_frame-rects.
      ENDIF.
    ENDLOOP.

    CASE lv_phase.
      WHEN 'intro'.
        IF lv_t < 4.
          CLEAR ls_text.
          ls_text-x = 20. ls_text-y = 30.
          ls_text-text = 'ABAP/4 DEMO SYSTEM V1.0'.
          ls_text-color = '#00FF00'. ls_text-size = 10.
          APPEND ls_text TO rs_frame-texts.
          ls_text-y = 45. ls_text-text = 'INITIALIZING...'.
          APPEND ls_text TO rs_frame-texts.
          IF lv_t > 1. ls_text-y = 60. ls_text-text = 'MEMORY OK: 640K'. APPEND ls_text TO rs_frame-texts. ENDIF.
          IF lv_t > 2. ls_text-y = 75. ls_text-text = 'LOADING EFFECTS...'. APPEND ls_text TO rs_frame-texts. ENDIF.
          IF lv_t > 3. ls_text-y = 90. ls_text-text = 'ENGAGING J-DRIVE...'. APPEND ls_text TO rs_frame-texts. ENDIF.
        ENDIF.
      WHEN 'tunnel'.
        CLEAR ls_text. ls_text-x = 160. ls_text-y = 190.
        ls_text-text = 'HYPERSPACE'. ls_text-color = '#00FFFF'.
        ls_text-size = 14. ls_text-align = 'center'.
        APPEND ls_text TO rs_frame-texts.
      WHEN 'arrive'.
        lv_pct = ( 1 - lv_progress ) * 100.
        CLEAR ls_text. ls_text-x = 120. ls_text-y = 190.
        ls_text-text = |ARRIVING... { lv_pct }%|.
        ls_text-color = '#00FF00'. ls_text-size = 10.
        APPEND ls_text TO rs_frame-texts.
    ENDCASE.

    IF is_ctx-gbi-beat = 2 AND is_ctx-gbi-pulse > '0.7'.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = '0.4'.
      rs_frame-flash-r = 0. rs_frame-flash-g = '0.8'. rs_frame-flash-b = 1.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
