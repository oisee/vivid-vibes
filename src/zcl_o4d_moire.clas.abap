CLASS zcl_o4d_moire DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    CLASS-METHODS new IMPORTING iv_lines TYPE i DEFAULT 30 iv_grids TYPE i DEFAULT 2
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_o4d_moire.
    METHODS constructor IMPORTING iv_lines TYPE i iv_grids TYPE i.
  PRIVATE SECTION.
    DATA mv_lines TYPE i.
    DATA mv_grids TYPE i.
ENDCLASS.

CLASS zcl_o4d_moire IMPLEMENTATION.
  METHOD new. ro_obj = NEW #( iv_lines = iv_lines iv_grids = iv_grids ). ENDMETHOD.
  METHOD constructor. mv_lines = iv_lines. mv_grids = iv_grids. ENDMETHOD.
  METHOD zif_o4d_effect~get_name. rv_name = 'moire'. ENDMETHOD.
  METHOD zif_o4d_effect~get_params. ENDMETHOD.
  METHOD zif_o4d_effect~set_param. ENDMETHOD.
  METHOD zif_o4d_effect~render.
    rs_frame = zif_o4d_effect~render_frame( VALUE #( t = is_sync-time gt = is_sync-time
      gbi = VALUE #( bar = is_sync-bar bar_phase = is_sync-bar_phase pulse = is_sync-intensity ) ) ).
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    DATA: lv_g TYPE i, lv_i TYPE i, lv_dx TYPE f, lv_dy TYPE f, lv_px TYPE f, lv_py TYPE f, lv_off TYPE f.
    DATA: lv_cos TYPE f, lv_sin TYPE f, lv_angle TYPE f, lv_base_rot TYPE f, lv_speed TYPE f.
    DATA: lv_hue TYPE i, lv_half TYPE i.
    DATA lv_color TYPE string.
    DATA(lv_w) = CONV f( zif_o4d_effect=>c_width ). DATA(lv_h) = CONV f( zif_o4d_effect=>c_height ).
    DATA(lv_cx) = lv_w / 2. DATA(lv_cy) = lv_h / 2. DATA(lv_t) = is_ctx-gt.
    DATA(lv_pi) = zif_o4d_effect=>c_pi.
    DATA(lv_diag) = sqrt( lv_w * lv_w + lv_h * lv_h ). DATA(lv_spacing) = lv_diag / mv_lines.
    APPEND VALUE zif_o4d_effect=>ty_rect( x = 0 y = 0 w = lv_w h = lv_h fill = '#000' ) TO rs_frame-rects.
    lv_g = 0.
    WHILE lv_g < mv_grids.
      lv_base_rot = CONV f( lv_g ) / mv_grids * lv_pi. lv_speed = '0.3' + CONV f( lv_g ) * '0.15'.
      lv_angle = lv_base_rot + lv_t * lv_speed. lv_cos = cos( lv_angle ). lv_sin = sin( lv_angle ).
      lv_hue = lv_g * 360 / mv_grids. lv_color = |hsla({ lv_hue }, 100%, 50%, 0.5)|.
      lv_half = mv_lines / 2. lv_i = 0 - lv_half.
      WHILE lv_i <= lv_half.
        lv_off = CONV f( lv_i ) * lv_spacing. lv_px = lv_cx + lv_cos * lv_off. lv_py = lv_cy + lv_sin * lv_off.
        lv_dx = lv_sin * lv_diag * -1. lv_dy = lv_cos * lv_diag.
        APPEND VALUE zif_o4d_effect=>ty_line( x1 = lv_px - lv_dx y1 = lv_py - lv_dy
          x2 = lv_px + lv_dx y2 = lv_py + lv_dy width = 1 color = lv_color ) TO rs_frame-lines.
        lv_i = lv_i + 1.
      ENDWHILE.
      lv_g = lv_g + 1.
    ENDWHILE.
    APPEND VALUE zif_o4d_effect=>ty_text( x = lv_cx y = 25 text = 'MOIRE' color = '#fff' size = 18 align = 'center' ) TO rs_frame-texts.
    IF is_ctx-gbi-pulse > '0.5'. rs_frame-flash-active = abap_true. rs_frame-flash-intensity = '0.08'.
      rs_frame-flash-r = 1. rs_frame-flash-g = 1. rs_frame-flash-b = 1.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
