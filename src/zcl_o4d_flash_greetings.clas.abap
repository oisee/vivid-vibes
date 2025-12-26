CLASS zcl_o4d_flash_greetings DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_o4d_effect .

    METHODS constructor
      IMPORTING
        !iv_subtitle  TYPE string DEFAULT 'GREETINGS TO...'
        !iv_greetings TYPE string OPTIONAL .
  PRIVATE SECTION.

    DATA mt_greetings TYPE string_table .
    DATA mv_current_idx TYPE i .
    DATA mv_flash_t TYPE f .
    DATA mv_last_i4 TYPE i .
    CONSTANTS c_flash_dur TYPE f VALUE '0.2' ##NO_TEXT.
    DATA mv_subtitle TYPE string .
ENDCLASS.



CLASS ZCL_O4D_FLASH_GREETINGS IMPLEMENTATION.


  METHOD constructor.
    mv_subtitle = iv_subtitle.
    IF iv_greetings IS INITIAL.
      APPEND 'OISEE' TO mt_greetings.
      APPEND 'CLAUDE' TO mt_greetings.
      APPEND 'ANTHROPIC' TO mt_greetings.
      APPEND 'SAP' TO mt_greetings.
      APPEND 'ABAP' TO mt_greetings.
      APPEND 'DEMOSCENE' TO mt_greetings.
      APPEND 'REVISION' TO mt_greetings.
      APPEND 'ASSEMBLY' TO mt_greetings.
      APPEND 'POUET' TO mt_greetings.
      APPEND 'FARBRAUSCH' TO mt_greetings.
      APPEND 'CONSPIRACY' TO mt_greetings.
      APPEND 'ASD' TO mt_greetings.
      APPEND 'RAZOR1911' TO mt_greetings.
      APPEND 'FAIRLIGHT' TO mt_greetings.
    ELSE.
      SPLIT iv_greetings AT ',' INTO TABLE mt_greetings.
    ENDIF.
    mv_current_idx = 1.
    mv_last_i4 = -1.
  ENDMETHOD.


  METHOD zif_o4d_effect~render_frame.
    DATA: lv_text        TYPE string, lv_flash_phase TYPE f.

    DATA(lv_t) = is_ctx-t / 2.
    DATA(lv_count) = lines( mt_greetings ).

    " Trigger on every beat (i4)
    IF is_ctx-bi-i4 <> mv_last_i4.
      mv_last_i4 = is_ctx-bi-i4.
      mv_flash_t = lv_t.
      mv_current_idx = ( mv_current_idx MOD lv_count ) + 1.
    ENDIF.

    " Get current greeting
    READ TABLE mt_greetings INTO lv_text INDEX mv_current_idx.
    IF sy-subrc <> 0. lv_text = 'GREETS'. ENDIF.

    " Calculate flash phase (0 = start of flash, 1 = end)
    lv_flash_phase = ( lv_t - mv_flash_t ) / c_flash_dur.
    IF lv_flash_phase > 1. lv_flash_phase = 1. ENDIF.
    IF lv_flash_phase < 0. lv_flash_phase = 0. ENDIF.

    " Background: white fading to black
    DATA(lv_bg_bright) = CONV i( 255 * ( 1 - lv_flash_phase ) ).
    APPEND VALUE #( x    = 0 y = 0 w = 640 h = 400
                    fill = |rgb({ lv_bg_bright },{ lv_bg_bright },{ lv_bg_bright })| ) TO rs_frame-rects.

    " The greeting: starts dark, becomes bright (inverse of background)
    DATA(lv_txt_bright) = CONV i( 50 + 205 * lv_flash_phase ).
    DATA(lv_hue) = ( mv_current_idx * 25 + CONV i( lv_t * 50 ) ) MOD 360.

    " Size pulses with beat
    DATA(lv_size_base) = 48.
    DATA(lv_size_pulse) = CONV i( 24 * ( 1 - lv_flash_phase ) ).
    DATA(lv_size) = lv_size_base + lv_size_pulse.

    " Center position with slight movement
    DATA(lv_x) = 320 + CONV i( sin( lv_t * 2 ) * 20 ).
    DATA(lv_y) = 200 + CONV i( cos( lv_t * 3 ) * 10 ).

    " Shadow/outline for contrast during white flash
    IF lv_bg_bright > 100.
      APPEND VALUE #( x    = lv_x + 3 y     = lv_y + 3 text = lv_text
                      size = lv_size  color = '#000000' ) TO rs_frame-texts.
    ENDIF.

    " Main greeting text
    DATA(lv_color) = COND string(
      WHEN lv_flash_phase < '0.5'
      THEN |hsl({ lv_hue },100%,{ 20 + CONV i( lv_flash_phase * 60 ) }%)|
      ELSE |hsl({ lv_hue },90%,{ lv_txt_bright / 5 }%)| ).

    APPEND VALUE #( x    = lv_x    y     = lv_y text = lv_text
                    size = lv_size color = lv_color ) TO rs_frame-texts.

    " Glow effect during flash
    IF lv_flash_phase < '0.3'.
      DATA(lv_glow_alpha) = CONV i( 150 * ( 1 - lv_flash_phase / '0.3' ) ).
      APPEND VALUE #( x     = lv_x y = lv_y text = lv_text
                      size  = lv_size + 8
                      color = |rgba(255,255,255,{ lv_glow_alpha })| ) TO rs_frame-texts.
    ENDIF.

    IF mv_subtitle IS NOT INITIAL.
      " Small subtitle
      APPEND VALUE #( x     = 320 y = 350 text = mv_subtitle
                      size  = 12
                      color = |rgb({ 100 - lv_bg_bright / 3 },{ 100 - lv_bg_bright / 3 },{ 100 - lv_bg_bright / 3 })| ) TO rs_frame-texts.
    ENDIF.
    " Counter
    APPEND VALUE #( x    = 320 y     = 380 text = |{ mv_current_idx }/{ lv_count }|
                    size = 10  color = '#666666' ) TO rs_frame-texts.

    rs_frame-debug-vars = |\{"greeting":"{ lv_text }","idx":{ mv_current_idx },"flash_phase":{ lv_flash_phase }\}|.
  ENDMETHOD.


  METHOD zif_o4d_effect~get_name.
    rv_name = 'flash_greetings'.
  ENDMETHOD.


  METHOD zif_o4d_effect~set_param.
  ENDMETHOD.
ENDCLASS.
