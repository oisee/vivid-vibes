"! CURSOR_TRAIL Effect - Pattern Mapper demo
CLASS zcl_o4d_cursor_trail DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.

  PRIVATE SECTION.
    DATA: mv_glow_radius TYPE i VALUE 30,
          mv_trail_len   TYPE i VALUE 20,
          mv_flash_time  TYPE f VALUE 0.

    METHODS check_trigger
      IMPORTING iv_triggers TYPE i
                iv_bit      TYPE i
      RETURNING VALUE(rv_set) TYPE abap_bool.

    METHODS int_to_hex
      IMPORTING iv_int TYPE i
      RETURNING VALUE(rv_hex) TYPE string.
ENDCLASS.

CLASS zcl_o4d_cursor_trail IMPLEMENTATION.
  METHOD zif_o4d_effect~get_name. rv_name = 'cursor_trail'. ENDMETHOD.
  METHOD zif_o4d_effect~get_params. ENDMETHOD.
  METHOD zif_o4d_effect~set_param.
    CASE iv_name.
      WHEN 'glow_radius'. mv_glow_radius = CONV i( iv_value ).
      WHEN 'trail_len'. mv_trail_len = CONV i( iv_value ).
    ENDCASE.
  ENDMETHOD.

  METHOD zif_o4d_effect~render.
    DATA(ls_ctx) = VALUE zif_o4d_effect=>ty_render_ctx(
      t = is_sync-time gt = is_sync-time
      bi = VALUE #( pulse = is_sync-intensity )
    ).
    rs_frame = zif_o4d_effect~render_frame( ls_ctx ).
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    DATA(lv_t) = is_ctx-t.
    DATA(ls_input) = is_ctx-input.

    " Check for flash trigger (bit 0)
    IF check_trigger( iv_triggers = ls_input-triggers iv_bit = zif_o4d_effect=>c_trigger_flash ).
      mv_flash_time = lv_t.
    ENDIF.

    " Flash background effect
    DATA(lv_flash_alpha) = nmax( val1 = CONV f( 0 ) val2 = CONV f( 1 ) - ( lv_t - mv_flash_time ) * 10 ).
    IF lv_flash_alpha > 0.
      DATA(lv_flash_v) = CONV i( 255 * lv_flash_alpha ).
      DATA(lv_flash_hex) = int_to_hex( lv_flash_v ).
      APPEND VALUE zif_o4d_effect=>ty_rect(
        x = 0 y = 0 w = zif_o4d_effect=>c_width h = zif_o4d_effect=>c_height
        fill = |#{ lv_flash_hex }{ lv_flash_hex }{ lv_flash_hex }|
      ) TO rs_frame-rects.
    ENDIF.

    " Draw trail from local recording
    DATA(lv_rec_count) = lines( ls_input-local_rec ).
    IF lv_rec_count > 0.
      DATA(lv_start) = nmax( val1 = 1 val2 = lv_rec_count - mv_trail_len ).
      LOOP AT ls_input-local_rec INTO DATA(ls_pt) FROM lv_start.
        DATA(lv_idx) = sy-tabix - lv_start + 1.
        DATA(lv_fade) = CONV f( lv_idx ) / mv_trail_len.
        DATA(lv_trail_v) = CONV i( 100 * lv_fade ).
        DATA(lv_trail_hex) = int_to_hex( lv_trail_v ).
        DATA(lv_px) = CONV i( ls_pt-x * zif_o4d_effect=>c_width ).
        DATA(lv_py) = CONV i( ls_pt-y * zif_o4d_effect=>c_height ).
        APPEND VALUE zif_o4d_effect=>ty_rect(
          x = lv_px - 1 y = lv_py - 1 w = 3 h = 3
          fill = |#00{ lv_trail_hex }{ lv_trail_hex }|
        ) TO rs_frame-rects.
      ENDLOOP.
    ENDIF.

    " Draw glow at cursor position
    DATA lv_radius TYPE i.
    lv_radius = mv_glow_radius.
    IF ls_input-pressed = abap_true.
      lv_radius = lv_radius * 3 / 2.
    ENDIF.

    DATA(lv_cx) = ls_input-x_norm * zif_o4d_effect=>c_width.
    DATA(lv_cy) = ls_input-y_norm * zif_o4d_effect=>c_height.

    " Draw concentric circles for glow
    DO 5 TIMES.
      DATA(lv_ring) = sy-index.
      DATA(lv_ring_r) = CONV f( lv_radius * lv_ring ) / 5.
      DATA(lv_intensity) = CONV f( 1 ) - CONV f( lv_ring ) / 6.
      DATA(lv_glow_v) = CONV i( 255 * lv_intensity ).
      DATA(lv_glow_hex) = int_to_hex( lv_glow_v ).

      DO 36 TIMES.
        DATA(lv_angle) = CONV f( sy-index * 10 ) * zif_o4d_effect=>c_pi / 180.
        DATA(lv_angle2) = CONV f( ( sy-index + 1 ) * 10 ) * zif_o4d_effect=>c_pi / 180.
        APPEND VALUE zif_o4d_effect=>ty_line(
          x1 = lv_cx + cos( lv_angle ) * lv_ring_r
          y1 = lv_cy + sin( lv_angle ) * lv_ring_r
          x2 = lv_cx + cos( lv_angle2 ) * lv_ring_r
          y2 = lv_cy + sin( lv_angle2 ) * lv_ring_r
          color = |#{ lv_glow_hex }{ lv_glow_hex }{ lv_glow_hex }|
          width = 2
        ) TO rs_frame-lines.
      ENDDO.
    ENDDO.

    " Draw center dot
    APPEND VALUE zif_o4d_effect=>ty_rect(
      x = lv_cx - 3 y = lv_cy - 3 w = 6 h = 6 fill = '#FFFFFF'
    ) TO rs_frame-rects.

    " Info text
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = 320 y = 20
      text = |CURSOR TRAIL - x={ ls_input-x } y={ ls_input-y }|
      color = '#888888' size = 10 align = 'center'
    ) TO rs_frame-texts.
  ENDMETHOD.

  METHOD check_trigger.
    DATA lv_mask TYPE i VALUE 1.
    DO iv_bit TIMES.
      lv_mask = lv_mask * 2.
    ENDDO.
    rv_set = xsdbool( iv_triggers MOD ( lv_mask * 2 ) >= lv_mask ).
  ENDMETHOD.

  METHOD int_to_hex.
    DATA(lv_val) = nmax( val1 = 0 val2 = nmin( val1 = 255 val2 = iv_int ) ).
    IF lv_val < 16.
      rv_hex = |0{ CONV xstring( lv_val ) }|.
    ELSE.
      rv_hex = CONV xstring( lv_val ).
    ENDIF.
    TRANSLATE rv_hex TO LOWER CASE.
  ENDMETHOD.
ENDCLASS.
