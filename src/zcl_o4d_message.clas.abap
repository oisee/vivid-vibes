CLASS zcl_o4d_message DEFINITION PUBLIC CREATE PRIVATE.
*======================================================================
* MESSAGE - Overlay text effect with customizable message
* Usage: zcl_o4d_message=>create( 'OOPS!' )
*        zcl_o4d_message=>create( iv_text = 'ERROR' iv_color = '#FF0000' )
*======================================================================
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.

    CLASS-METHODS new
      IMPORTING iv_text       TYPE string DEFAULT 'OOPS!'
                iv_subtext    TYPE string DEFAULT ''
                iv_color      TYPE string DEFAULT '#FF0000'
                iv_bg_color   TYPE string DEFAULT '#FF000044'
                iv_size       TYPE i DEFAULT 48
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_o4d_message.

    METHODS constructor
      IMPORTING iv_text     TYPE string
                iv_subtext  TYPE string
                iv_color    TYPE string
                iv_bg_color TYPE string
                iv_size     TYPE i.

  PRIVATE SECTION.
    CONSTANTS: lc_w TYPE i VALUE 640,
               lc_h TYPE i VALUE 400.

    DATA: mv_text     TYPE string,
          mv_subtext  TYPE string,
          mv_color    TYPE string,
          mv_bg_color TYPE string,
          mv_size     TYPE i.

ENDCLASS.

CLASS zcl_o4d_message IMPLEMENTATION.

  METHOD new.
    ro_obj = NEW #(
      iv_text     = iv_text
      iv_subtext  = iv_subtext
      iv_color    = iv_color
      iv_bg_color = iv_bg_color
      iv_size     = iv_size ).
  ENDMETHOD.

  METHOD constructor.
    mv_text     = iv_text.
    mv_subtext  = iv_subtext.
    mv_color    = iv_color.
    mv_bg_color = iv_bg_color.
    mv_size     = iv_size.
  ENDMETHOD.

  METHOD zif_o4d_effect~get_name. rv_name = 'message'. ENDMETHOD.
  METHOD zif_o4d_effect~get_params. ENDMETHOD.
  METHOD zif_o4d_effect~set_param. ENDMETHOD.

  METHOD zif_o4d_effect~render.
    rs_frame = zif_o4d_effect~render_frame( VALUE #(
      t = is_sync-time gt = is_sync-time
      gbi = VALUE #( time = is_sync-time bar_phase = is_sync-bar_phase
                     pulse = is_sync-intensity ) ) ).
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    DATA(lv_t) = is_ctx-gbi-time.
    DATA(lv_phase) = is_ctx-gbi-bar_phase.

    " Background overlay with pulse
    DATA(lv_alpha) = '0.3' + sin( lv_t * 8 ) * '0.15'.
    APPEND VALUE zif_o4d_effect=>ty_rect(
      x = 0 y = 0 w = lc_w h = lc_h
      fill = mv_bg_color
    ) TO rs_frame-rects.

    " Main text with shake effect
    DATA(lv_shake_x) = sin( lv_t * 30 ) * 3.
    DATA(lv_shake_y) = cos( lv_t * 25 ) * 2.

    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lc_w / 2 + lv_shake_x
      y = lc_h / 2 + lv_shake_y
      text = mv_text
      color = mv_color
      size = mv_size
      align = 'center'
    ) TO rs_frame-texts.

    " Subtext if provided
    IF mv_subtext IS NOT INITIAL.
      " Fade in subtext
      DATA(lv_sub_alpha) = nmin( val1 = 1 val2 = lv_t * 2 ).

      APPEND VALUE zif_o4d_effect=>ty_text(
        x = lc_w / 2
        y = lc_h / 2 + mv_size / 2 + 20
        text = mv_subtext
        color = mv_color
        size = 16
        align = 'center'
      ) TO rs_frame-texts.
    ENDIF.

    " Beat flash
    IF is_ctx-gbi-pulse > '0.7'.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = '0.3'.
      rs_frame-flash-r = 1. rs_frame-flash-g = '0.2'. rs_frame-flash-b = '0.2'.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
