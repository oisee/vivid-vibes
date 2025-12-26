CLASS zcl_o4d_firegreetings DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
  PRIVATE SECTION.
    CLASS-METHODS int_to_hex IMPORTING iv_int TYPE i RETURNING VALUE(rv_hex) TYPE string.
ENDCLASS.

CLASS zcl_o4d_firegreetings IMPLEMENTATION.
  METHOD zif_o4d_effect~get_name. rv_name = 'fire_greetings'. ENDMETHOD.
  METHOD zif_o4d_effect~get_params. ENDMETHOD.
  METHOD zif_o4d_effect~set_param. ENDMETHOD.

  METHOD zif_o4d_effect~render.
    DATA(ls_ctx) = VALUE zif_o4d_effect=>ty_render_ctx(
      t = is_sync-time gt = is_sync-time bi = VALUE #( pulse = is_sync-intensity )
    ).
    rs_frame = zif_o4d_effect~render_frame( ls_ctx ).
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    DATA(lv_t) = is_ctx-t.
    DATA(lv_w) = zif_o4d_effect=>c_width.
    DATA(lv_h) = zif_o4d_effect=>c_height.

    " Fire background - simplified with horizontal gradient lines
    DATA lv_y TYPE i.
    lv_y = 0.
    WHILE lv_y < lv_h.
      DATA lv_x TYPE i.
      lv_x = 0.
      WHILE lv_x < lv_w.
        " Noise based on position and time
        DATA(lv_n1) = sin( CONV f( lv_x ) * CONV f( '0.05' ) + lv_t * 3 ) *
                      cos( CONV f( lv_y ) * CONV f( '0.08' ) - lv_t * 5 ).
        DATA(lv_n2) = sin( CONV f( lv_x ) * CONV f( '0.02' ) - lv_t * 2 ) *
                      sin( CONV f( lv_y ) * CONV f( '0.05' ) + lv_t * 4 ).
        DATA(lv_noise) = ( lv_n1 + lv_n2 ) * CONV f( '0.5' ) + CONV f( '0.5' ).

        DATA(lv_heat) = nmax( val1 = CONV f( 0 ) val2 = 1 - CONV f( lv_y ) / lv_h + lv_noise * CONV f( '0.5' ) - CONV f( '0.3' ) ).
        DATA(lv_flicker) = sin( CONV f( lv_x ) * CONV f( '0.1' ) + CONV f( lv_y ) * CONV f( '0.05' ) + lv_t * 10 ) *
                           CONV f( '0.2' ) + CONV f( '0.8' ).
        DATA(lv_intensity) = lv_heat * lv_flicker.

        " Fire palette
        DATA lv_r TYPE i.
        DATA lv_g TYPE i.
        DATA lv_b TYPE i.
        IF lv_intensity < CONV f( '0.3' ).
          lv_r = CONV i( lv_intensity * 3 * 180 ).
          lv_g = 0.
          lv_b = 0.
        ELSEIF lv_intensity < CONV f( '0.6' ).
          lv_r = CONV i( 180 + ( lv_intensity - CONV f( '0.3' ) ) * 3 * 75 ).
          lv_g = CONV i( ( lv_intensity - CONV f( '0.3' ) ) * 3 * 100 ).
          lv_b = 0.
        ELSE.
          lv_r = 255.
          lv_g = CONV i( 100 + ( lv_intensity - CONV f( '0.6' ) ) * CONV f( '2.5' ) * 155 ).
          lv_b = CONV i( ( lv_intensity - CONV f( '0.6' ) ) * CONV f( '2.5' ) * 200 ).
        ENDIF.

        lv_r = nmin( val1 = 255 val2 = lv_r ).
        lv_g = nmin( val1 = 255 val2 = lv_g ).
        lv_b = nmin( val1 = 255 val2 = lv_b ).

        APPEND VALUE zif_o4d_effect=>ty_line(
          x1 = lv_x y1 = lv_y x2 = lv_x + 8 y2 = lv_y
          width = 8 color = |#{ int_to_hex( lv_r ) }{ int_to_hex( lv_g ) }{ int_to_hex( lv_b ) }|
        ) TO rs_frame-lines.

        lv_x = lv_x + 8.
      ENDWHILE.
      lv_y = lv_y + 8.
    ENDWHILE.

    " Greetings text scrolling up from bottom
    DATA(lt_greets) = VALUE string_table(
      ( `>>> GREETINGS <<<` )
      ( `` )
      ( `ALL DEMOSCENERS` )
      ( `SAP COMMUNITY` )
      ( `ABAP DEVELOPERS` )
      ( `HANA EXPERTS` )
      ( `` )
      ( `RETRO COMPUTING` )
      ( `C64 // AMIGA // ATARI` )
      ( `ZX SPECTRUM CREW` )
      ( `DOS // WINDOWS 3.1` )
      ( `` )
      ( `OISEE + CLAUDE` )
      ( `ANTHROPIC TEAM` )
      ( `` )
      ( `>>> STAY COOL <<<` )
    ).

    DATA(lv_scroll_y) = 0 - lv_t * 25.  " Скорость скролла
    DATA lv_idx TYPE i.
    lv_idx = 1.
    LOOP AT lt_greets INTO DATA(lv_text).
      DATA(lv_ty) = lv_scroll_y + ( lv_idx - 1 ) * 28 + lv_h + 20.  " Начинаем ПОД экраном!
      IF lv_ty > -20 AND lv_ty < lv_h + 20.  " Полная высота экрана
        DATA(lv_wave) = sin( lv_t * 2 + CONV f( lv_idx ) * CONV f( '0.3' ) ) * 10.
        DATA(lv_screen_pos) = CONV f( 1 ) - CONV f( lv_ty ) / CONV f( lv_h ).

        " Color based on position
        DATA lv_tcolor TYPE string.
        IF lv_screen_pos < CONV f( '0.3' ).
          lv_tcolor = '#ffff00'.  " Bottom - bright yellow
        ELSEIF lv_screen_pos < CONV f( '0.6' ).
          lv_tcolor = '#ff8800'.  " Middle - orange
        ELSE.
          lv_tcolor = '#ff2200'.  " Top - burnt red
        ENDIF.

        DATA(lv_tx) = CONV f( lv_w ) / 2 + lv_wave.

        " Black outline
        APPEND VALUE zif_o4d_effect=>ty_text( x = lv_tx - 2 y = lv_ty text = lv_text color = '#000000' size = 14 align = 'center' ) TO rs_frame-texts.
        APPEND VALUE zif_o4d_effect=>ty_text( x = lv_tx + 2 y = lv_ty text = lv_text color = '#000000' size = 14 align = 'center' ) TO rs_frame-texts.
        " Main text
        APPEND VALUE zif_o4d_effect=>ty_text( x = lv_tx y = lv_ty text = lv_text color = lv_tcolor size = 14 align = 'center' ) TO rs_frame-texts.
      ENDIF.
      lv_idx = lv_idx + 1.
    ENDLOOP.
  ENDMETHOD.

  METHOD int_to_hex.
    DATA lv_hex TYPE x LENGTH 1.
    DATA(lv_val) = nmax( val1 = 0 val2 = nmin( val1 = 255 val2 = iv_int ) ).
    lv_hex = lv_val.
    rv_hex = |{ lv_hex }|.
    TRANSLATE rv_hex TO LOWER CASE.
  ENDMETHOD.
ENDCLASS.
