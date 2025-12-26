CLASS zcl_o4d_greetings DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
  PRIVATE SECTION.
    CONSTANTS c_pi TYPE f VALUE '3.14159265'.
ENDCLASS.



CLASS ZCL_O4D_GREETINGS IMPLEMENTATION.


  METHOD zif_o4d_effect~get_name. rv_name = 'greetings'. ENDMETHOD.


  METHOD zif_o4d_effect~get_params. ENDMETHOD.


  METHOD zif_o4d_effect~set_param. ENDMETHOD.


  METHOD zif_o4d_effect~render.
    DATA(ls_ctx) = VALUE zif_o4d_effect=>ty_render_ctx(
      t = is_sync-time gt = is_sync-time bi = VALUE #( pulse = is_sync-intensity )
    ).
    rs_frame = zif_o4d_effect~render_frame( ls_ctx ).
  ENDMETHOD.


  METHOD zif_o4d_effect~render_frame.
    "======================================================================
    " GREETINGS - Sine wave text scroller
    "======================================================================
    DATA(lv_t) = is_ctx-t.                         " Время эффекта (секунды)
    DATA(lv_w) = zif_o4d_effect=>c_width.          " Ширина экрана (640)
    DATA(lv_h) = zif_o4d_effect=>c_height.         " Высота экрана (400)

    "----------------------------------------------------------------------
    " БАЗОВАЯ ПОЗИЦИЯ ТЕКСТА ПО ВЕРТИКАЛИ
    " lv_h / 2 = центр экрана (200)
    " Чтобы текст был НИЖЕ - увеличь множитель (например 0.7 = 280px)
    " Чтобы текст был ВЫШЕ - уменьши множитель (например 0.3 = 120px)
    "----------------------------------------------------------------------
    DATA(lv_cy) = CONV f( lv_h ) * CONV f( '0.65' ).  " 65% от высоты = 260px

    "----------------------------------------------------------------------
    " ТЕКСТ ДЛЯ СКРОЛЛЕРА
    " Пробелы в начале/конце создают паузу между повторами
    "----------------------------------------------------------------------
    DATA(lv_scroll_text) = `    GREETINGS TO ALL DEMOSCENERS!  ` &&
                           `SAP COMMUNITY - ABAP DEVELOPERS - ` &&
                           `C64 CREW - AMIGA FOREVER - ATARI ST - ` &&
                           `KEEP THE SCENE ALIVE!    `.

    DATA(lv_text_len) = strlen( lv_scroll_text ).

    "----------------------------------------------------------------------
    " СКОРОСТЬ СКРОЛЛА
    " lv_t * 60 = 60 пикселей/секунду (увеличь для быстрее)
    "----------------------------------------------------------------------
    DATA(lv_scroll_offset) = CONV i( lv_t * 60 ) MOD ( lv_text_len * 12 ).

    " Начальная позиция символа на экране
    DATA lv_char_idx TYPE i.
    DATA lv_screen_x TYPE i.
    lv_screen_x = 0 - lv_scroll_offset MOD 12.    " Субпиксельный скролл
    lv_char_idx = lv_scroll_offset / 12.          " Индекс первого символа

    " Рисуем каждый видимый символ
    WHILE lv_screen_x < lv_w + 20.
      DATA(lv_actual_idx) = lv_char_idx MOD lv_text_len.  " Зацикливаем текст
      DATA(lv_char) = substring( val = lv_scroll_text off = lv_actual_idx len = 1 ).

      IF lv_char <> ` `.
        "----------------------------------------------------------------------
        " АМПЛИТУДА СИНУСОИДЫ
        " * 30 = высота волны в пикселях (увеличь для больше волны)
        " 0.03 = частота волны (увеличь для чаще волны)
        "----------------------------------------------------------------------
        DATA(lv_wave_y) = sin( ( CONV f( lv_screen_x ) + lv_t * 100 ) * CONV f( '0.03' ) ) * 30.
        DATA(lv_y) = lv_cy + lv_wave_y.           " Финальная Y = база + волна

        " Rainbow цвет - hue крутится по позиции и времени
        DATA(lv_hue) = CONV i( CONV f( lv_screen_x ) + lv_t * 50 ) MOD 360.

        APPEND VALUE zif_o4d_effect=>ty_text(
          x = lv_screen_x
          y = lv_y
          text = lv_char
          size = 20                               " Размер шрифта
          color = |hsl({ lv_hue },80%,60%)|       " HSL: hue, saturation, lightness
          align = 'left'
        ) TO rs_frame-texts.
      ENDIF.

      lv_screen_x = lv_screen_x + 12.             " Расстояние между буквами
      lv_char_idx = lv_char_idx + 1.
    ENDWHILE.

    " Заголовок сверху
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = CONV f( lv_w ) / 2 y = 25 text = '>>> GREETINGS <<<'
      color = '#ffffff' size = 14 align = 'center'
    ) TO rs_frame-texts.

    " Подпись снизу
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = CONV f( lv_w ) / 2 y = CONV f( lv_h ) - 20 text = 'OISEE 2025'
      color = '#888888' size = 10 align = 'center'
    ) TO rs_frame-texts.

    " Debug vars - greetings scroller state
    DATA(lv_cur_hue) = CONV i( lv_t * 50 ) MOD 360.
    rs_frame-debug-vars = |\{"scroll_offset":{ lv_scroll_offset },"scroll_speed":60,| &&
      |"text_len":{ lv_text_len },"char_idx":{ lv_char_idx },| &&
      |"center_y":{ lv_cy },"wave_amp":30,"wave_freq":0.03,| &&
      |"hue":{ lv_cur_hue },"font_size":20,"char_spacing":12\}|.
  ENDMETHOD.
ENDCLASS.
