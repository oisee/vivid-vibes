CLASS zcl_o4d_cat_ears DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    METHODS constructor
      IMPORTING iv_num_bars TYPE i DEFAULT 32
                iv_history  TYPE i DEFAULT 15.

  PRIVATE SECTION.
    CONSTANTS: c_width TYPE i VALUE 640, c_height TYPE i VALUE 400.
    TYPES: tt_floats TYPE STANDARD TABLE OF f WITH EMPTY KEY,
           tt_history TYPE STANDARD TABLE OF tt_floats WITH EMPTY KEY.
    DATA: mv_num_bars TYPE i, mv_history TYPE i,
          mt_sin TYPE tt_floats, mt_history TYPE tt_history,
          mv_last_sample TYPE f.
    METHODS: init_sin_table,
             get_spectrum IMPORTING iv_time TYPE f RETURNING VALUE(rt_values) TYPE tt_floats.
ENDCLASS.

CLASS zcl_o4d_cat_ears IMPLEMENTATION.

  METHOD constructor.
    mv_num_bars = iv_num_bars. mv_history = iv_history. mv_last_sample = -1.
    init_sin_table( ).
  ENDMETHOD.

  METHOD init_sin_table.
    DO 256 TIMES.
      APPEND sin( ( sy-index - 1 ) * CONV f( '6.283185' ) / 256 ) TO mt_sin.
    ENDDO.
  ENDMETHOD.

  METHOD get_spectrum.
    DATA: lv_val TYPE f, lv_idx TYPE i, lv_sin TYPE f, lv_seed TYPE i, lv_rnd TYPE i.
    DATA: lv_ear_sin TYPE f, lv_ear_idx TYPE i.
    lv_seed = CONV i( iv_time * 500 ).

    DO mv_num_bars TIMES.
      DATA(lv_i) = sy-index - 1.

      lv_idx = CONV i( iv_time * 30 + lv_i * 5 ) MOD 256.
      READ TABLE mt_sin INDEX lv_idx + 1 INTO lv_sin.
      lv_val = CONV f( '0.3' ) + lv_sin * CONV f( '0.2' ).

      lv_idx = CONV i( iv_time * 45 + lv_i * 10 ) MOD 256.
      READ TABLE mt_sin INDEX lv_idx + 1 INTO lv_sin.
      lv_val = lv_val + lv_sin * CONV f( '0.15' ).

      lv_idx = CONV i( iv_time * 15 + lv_i * 3 ) MOD 256.
      READ TABLE mt_sin INDEX lv_idx + 1 INTO lv_sin.
      lv_val = lv_val + lv_sin * CONV f( '0.1' ).

      " === CAT EARS: high frequency double peaks ===
      lv_ear_idx = CONV i( iv_time * 80 + lv_i * 25 ) MOD 256.
      READ TABLE mt_sin INDEX lv_ear_idx + 1 INTO lv_ear_sin.
      lv_val = lv_val + abs( lv_ear_sin ) * CONV f( '0.25' ).

      lv_rnd = ( lv_seed + lv_i * 17 ) MOD 150.
      IF lv_rnd < 5.
        lv_val = lv_val + CONV f( lv_rnd + 2 ) / 10.
      ENDIF.

      DATA(lv_cd) = abs( CONV f( lv_i ) - CONV f( mv_num_bars ) / 2 ).
      lv_val = lv_val + ( 1 - lv_cd / ( CONV f( mv_num_bars ) / 2 ) ) * CONV f( '0.2' ).

      lv_val = nmax( val1 = CONV f( 0 ) val2 = nmin( val1 = CONV f( 1 ) val2 = lv_val ) ).
      APPEND lv_val TO rt_values.
    ENDDO.
  ENDMETHOD.

  METHOD zif_o4d_effect~get_name. rv_name = 'cat_ears'. ENDMETHOD.
  METHOD zif_o4d_effect~get_params. ENDMETHOD.

  METHOD zif_o4d_effect~set_param.
    CASE iv_name.
      WHEN 'num_bars'. mv_num_bars = CONV i( iv_value ).
      WHEN 'history'. mv_history = CONV i( iv_value ).
    ENDCASE.
  ENDMETHOD.

  METHOD zif_o4d_effect~render.
    DATA(ls_ctx) = VALUE zif_o4d_effect=>ty_render_ctx(
      t = is_sync-time gt = is_sync-time
      bi = VALUE #( pulse = is_sync-intensity bar_phase = is_sync-bar_phase )
    ).
    rs_frame = zif_o4d_effect~render_frame( ls_ctx ).
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    DATA: lt_spectrum TYPE tt_floats.
    DATA(lv_t) = is_ctx-t.

    DATA(lv_sample_time) = floor( lv_t / CONV f( '0.15' ) ) * CONV f( '0.15' ).
    IF lv_sample_time <> mv_last_sample.
      mv_last_sample = lv_sample_time.
      lt_spectrum = get_spectrum( lv_t ).
      INSERT lt_spectrum INTO mt_history INDEX 1.
      IF lines( mt_history ) > mv_history. DELETE mt_history INDEX mv_history + 1. ENDIF.
    ENDIF.

    DATA(lv_pulse) = is_ctx-bi-pulse.
    DATA(lv_amp_scale) = CONV f( 1 ) + lv_pulse * CONV f( '0.5' ).
    DATA(lv_bright_boost) = CONV i( lv_pulse * 80 ).

    DATA(lv_line_spacing) = CONV f( c_height - 80 ) / mv_history.
    LOOP AT mt_history INTO DATA(lt_hist_line).
      DATA(lv_line_idx) = sy-tabix.
      DATA(lv_y_base) = 50 + CONV f( lv_line_idx ) * lv_line_spacing.
      DATA(lv_age) = CONV f( lv_line_idx ) / lines( mt_history ).
      DATA(lv_gray) = nmin( val1 = 255 val2 = 40 + CONV i( 180 * lv_age ) + lv_bright_boost ).
      DATA(lv_prev_x) = CONV f( -1 ). DATA(lv_prev_y) = CONV f( -1 ).
      LOOP AT lt_hist_line INTO DATA(lv_val).
        DATA(lv_pi) = sy-tabix - 1.
        DATA(lv_x) = 40 + CONV f( lv_pi ) * ( c_width - 80 ) / mv_num_bars.
        DATA(lv_y) = lv_y_base - lv_val * lv_line_spacing * CONV f( '1.8' ) * lv_amp_scale.
        IF lv_prev_x >= 0.
          APPEND VALUE zif_o4d_effect=>ty_line(
            x1 = lv_prev_x y1 = lv_prev_y x2 = lv_x y2 = lv_y
            width = 2 color = |rgb({ lv_gray },{ lv_gray },{ lv_gray })|
          ) TO rs_frame-lines.
        ENDIF.
        lv_prev_x = lv_x. lv_prev_y = lv_y.
      ENDLOOP.
    ENDLOOP.

    APPEND VALUE zif_o4d_effect=>ty_text(
      x = c_width / 2 y = 25
      text = 'UNKNOWN PURRRRRS' color = '#888' size = 10 align = 'center'
    ) TO rs_frame-texts.

    rs_frame-debug-vars = |\{"num_bars":{ mv_num_bars },"history":{ mv_history },| &&
      |"history_lines":{ lines( mt_history ) },"line_spacing":{ lv_line_spacing },| &&
      |"pulse":{ lv_pulse },"amp_scale":{ lv_amp_scale },"bright_boost":{ lv_bright_boost },| &&
      |"lines":{ lines( rs_frame-lines ) },"t":{ lv_t }\}|.
  ENDMETHOD.
ENDCLASS.
