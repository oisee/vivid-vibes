CLASS zcl_o4d_scanmorph DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    CLASS-METHODS new
      IMPORTING iv_text TYPE string DEFAULT 'SAP'
                iv_bars TYPE i DEFAULT 4
                iv_scanlines TYPE i DEFAULT 64
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_o4d_scanmorph.
    METHODS constructor
      IMPORTING iv_text TYPE string
                iv_bars TYPE i
                iv_scanlines TYPE i.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_seg, y TYPE i, s TYPE f, e TYPE f, END OF ty_seg.
    TYPES tt_seg TYPE STANDARD TABLE OF ty_seg WITH EMPTY KEY.
    TYPES: BEGIN OF ty_shape, letter TYPE c LENGTH 1, segs TYPE tt_seg, END OF ty_shape.
    TYPES tt_shapes TYPE STANDARD TABLE OF ty_shape WITH EMPTY KEY.
    DATA mv_text TYPE string.
    DATA mv_bars TYPE i.
    DATA mv_lines TYPE i.
    DATA mt_shapes TYPE tt_shapes.
    METHODS build_shapes.
    METHODS get_shape IMPORTING iv_c TYPE c RETURNING VALUE(rt) TYPE tt_seg.
ENDCLASS.



CLASS ZCL_O4D_SCANMORPH IMPLEMENTATION.


  METHOD new. ro_obj = NEW #( iv_text = iv_text iv_bars = iv_bars iv_scanlines = iv_scanlines ). ENDMETHOD.


  METHOD constructor. mv_text = iv_text. mv_bars = iv_bars. mv_lines = iv_scanlines. build_shapes( ). ENDMETHOD.


  METHOD build_shapes.
    DATA lv_c TYPE c LENGTH 1.
    DATA(lv_len) = strlen( mv_text ).
    DO lv_len TIMES.
      DATA(lv_i) = sy-index - 1.
      lv_c = mv_text+lv_i(1).
      APPEND VALUE ty_shape( letter = lv_c segs = get_shape( lv_c ) ) TO mt_shapes.
    ENDDO.
  ENDMETHOD.


  METHOD get_shape.
    DATA: lv_y TYPE i, lv_s TYPE f, lv_e TYPE f, lv_yf TYPE f, lv_w TYPE f, lv_b TYPE f.
    DATA(lv_n) = mv_lines.
    CASE iv_c.
      WHEN 'S'.
        lv_y = 0.
        WHILE lv_y < lv_n.
          lv_yf = CONV f( lv_y ) / lv_n.
          IF lv_yf < '0.2'. lv_s = 20. lv_e = 80.
          ELSEIF lv_yf < '0.35'. lv_s = 20. lv_e = 35.
          ELSEIF lv_yf < '0.5'. lv_s = 20 + ( lv_yf - '0.35' ) / '0.15' * 45. lv_e = 35 + ( lv_yf - '0.35' ) / '0.15' * 45.
          ELSEIF lv_yf < '0.65'. lv_s = 35. lv_e = 80.
          ELSEIF lv_yf < '0.8'. lv_s = 65. lv_e = 80.
          ELSE. lv_s = 20. lv_e = 80.
          ENDIF.
          APPEND VALUE #( y = lv_y s = lv_s e = lv_e ) TO rt. lv_y = lv_y + 1.
        ENDWHILE.
      WHEN 'A'.
        lv_y = 0.
        WHILE lv_y < lv_n.
          lv_yf = CONV f( lv_y ) / lv_n. lv_w = 10 + lv_yf * 60. lv_s = 50 - lv_w / 2. lv_e = 50 + lv_w / 2.
*          IF lv_yf > '0.5'.
*            APPEND VALUE #( y = lv_y s = lv_s e = lv_s + 15 ) TO rt. APPEND VALUE #( y = lv_y s = lv_e - 15 e = lv_e ) TO rt.
*          ELSE.
            APPEND VALUE #( y = lv_y s = lv_s e = lv_e ) TO rt.
*          ENDIF. lv_y = lv_y + 1.
        ENDWHILE.
      WHEN 'P'.
        lv_y = 0.
        WHILE lv_y < lv_n.
          lv_yf = CONV f( lv_y ) / lv_n.
          IF lv_yf < '0.5'. lv_b = sin( lv_yf * 2 * '3.14159' ) * 40. lv_s = 25. lv_e = 40 + lv_b.
          ELSE. lv_s = 25. lv_e = 40.
          ENDIF. APPEND VALUE #( y = lv_y s = lv_s e = lv_e ) TO rt. lv_y = lv_y + 1.
        ENDWHILE.
      WHEN 'B'.
        lv_y = 0.
        WHILE lv_y < lv_n.
          lv_yf = CONV f( lv_y ) / lv_n.
          IF lv_yf < '0.45'. lv_b = sin( lv_yf / '0.45' * '3.14159' ) * 35. lv_s = 25. lv_e = 40 + lv_b.
          ELSEIF lv_yf < '0.55'. lv_s = 25. lv_e = 60.
          ELSE. lv_b = sin( ( lv_yf - '0.55' ) / '0.45' * '3.14159' ) * 40. lv_s = 25. lv_e = 45 + lv_b.
          ENDIF. APPEND VALUE #( y = lv_y s = lv_s e = lv_e ) TO rt. lv_y = lv_y + 1.
        ENDWHILE.
      WHEN OTHERS.
        lv_y = 0. WHILE lv_y < lv_n. APPEND VALUE #( y = lv_y s = 20 e = 80 ) TO rt. lv_y = lv_y + 1. ENDWHILE.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_o4d_effect~get_name. rv_name = 'scanmorph'. ENDMETHOD.


  METHOD zif_o4d_effect~get_params. ENDMETHOD.


  METHOD zif_o4d_effect~set_param. ENDMETHOD.


  METHOD zif_o4d_effect~render.
    rs_frame = zif_o4d_effect~render_frame( VALUE #( t = is_sync-time gt = is_sync-time
      gbi = VALUE #( bar = is_sync-bar bar_phase = is_sync-bar_phase pulse = is_sync-intensity ) ) ).
  ENDMETHOD.


  METHOD zif_o4d_effect~render_frame.
    DATA: lv_i TYPE i, ls_f TYPE ty_seg, ls_t TYPE ty_seg, lv_s TYPE f, lv_e TYPE f, lv_x1 TYPE f, lv_x2 TYPE f, lv_y TYPE f, lv_h TYPE f.
    DATA(lv_w) = CONV f( zif_o4d_effect=>c_width ). DATA(lv_ht) = CONV f( zif_o4d_effect=>c_height ).
    DATA(lv_n) = lines( mt_shapes ). IF lv_n = 0. RETURN. ENDIF.
    DATA(lv_bar) = is_ctx-gbi-bar. DATA(lv_bp) = is_ctx-gbi-bar_phase.
    DATA(lv_tot) = mv_bars * lv_n. DATA(lv_cb) = lv_bar MOD lv_tot.
    DATA(lv_cp) = CONV f( lv_cb ) + lv_bp. DATA(lv_si) = lv_cb / mv_bars.
    DATA(lv_sp) = ( lv_cp - lv_si * mv_bars ) / mv_bars.
    DATA(lv_fi) = lv_si MOD lv_n + 1. DATA(lv_ti) = ( lv_si + 1 ) MOD lv_n + 1.
    DATA(ls_from) = mt_shapes[ lv_fi ]. DATA(ls_to) = mt_shapes[ lv_ti ].
    DATA(lv_tt) = COND f( WHEN lv_sp < '0.5' THEN 4 * lv_sp * lv_sp * lv_sp ELSE 1 - ( -2 * lv_sp + 2 ) ** 3 / 2 ).
    DATA(lv_sh) = CONV f( 300 ). DATA(lv_sw) = CONV f( 200 ).
    DATA(lv_ox) = ( lv_w - lv_sw ) / 2. DATA(lv_oy) = ( lv_ht - lv_sh ) / 2. DATA(lv_lh) = lv_sh / mv_lines.
    lv_i = 0.
    WHILE lv_i < mv_lines.
      CLEAR: ls_f, ls_t.
      LOOP AT ls_from-segs INTO ls_f WHERE y = lv_i. EXIT. ENDLOOP.
      LOOP AT ls_to-segs INTO ls_t WHERE y = lv_i. EXIT. ENDLOOP.
      lv_s = ls_f-s + ( ls_t-s - ls_f-s ) * lv_tt. lv_e = ls_f-e + ( ls_t-e - ls_f-e ) * lv_tt.
      IF lv_e - lv_s > 1.
        lv_x1 = lv_ox + lv_s / 100 * lv_sw. lv_x2 = lv_ox + lv_e / 100 * lv_sw.
        lv_y = lv_oy + CONV f( lv_i ) * lv_lh.
        lv_h = ( CONV f( lv_i ) / mv_lines * 60 + is_ctx-gt * 30 ) MOD 360.
        APPEND VALUE zif_o4d_effect=>ty_rect( x = lv_x1 y = lv_y w = lv_x2 - lv_x1 h = lv_lh
          fill = |hsl({ CONV i( lv_h ) }, 80%, 60%)| ) TO rs_frame-rects.
      ENDIF.
      lv_i = lv_i + 1.
    ENDWHILE.
    DATA(lv_let) = COND string( WHEN lv_sp < '0.3' THEN CONV string( ls_from-letter )
      WHEN lv_sp > '0.7' THEN CONV string( ls_to-letter ) ELSE |{ ls_from-letter }{ ls_to-letter }| ).
    APPEND VALUE zif_o4d_effect=>ty_text( x = lv_w / 2 y = 30 text = lv_let color = '#ffffff' size = 20 align = 'center' ) TO rs_frame-texts.
    IF is_ctx-gbi-pulse > '0.5'. rs_frame-flash-active = abap_true. rs_frame-flash-intensity = '0.15'.
      rs_frame-flash-r = 1. rs_frame-flash-g = 1. rs_frame-flash-b = 1.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
