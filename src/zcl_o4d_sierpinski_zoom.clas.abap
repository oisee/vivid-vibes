CLASS zcl_o4d_sierpinski_zoom DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
  PRIVATE SECTION.
    CONSTANTS c_pi TYPE f VALUE '3.14159265'.
    CLASS-METHODS hsv_to_hex IMPORTING iv_h TYPE f iv_s TYPE f iv_v TYPE f RETURNING VALUE(rv_hex) TYPE string.
    CLASS-METHODS int_to_hex IMPORTING iv_int TYPE i RETURNING VALUE(rv_hex) TYPE string.
ENDCLASS.

CLASS zcl_o4d_sierpinski_zoom IMPLEMENTATION.
  METHOD zif_o4d_effect~get_name. rv_name = 'sierpinski_zoom'. ENDMETHOD.
  METHOD zif_o4d_effect~get_params. ENDMETHOD.
  METHOD zif_o4d_effect~set_param. ENDMETHOD.

  METHOD zif_o4d_effect~render.
    DATA(ls_ctx) = VALUE zif_o4d_effect=>ty_render_ctx(
      t = is_sync-time gt = is_sync-time bi = VALUE #( pulse = is_sync-intensity ) ).
    rs_frame = zif_o4d_effect~render_frame( ls_ctx ).
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    DATA(lv_t) = is_ctx-t.
    DATA(lv_w) = CONV f( zif_o4d_effect=>c_width ).
    DATA(lv_h) = CONV f( zif_o4d_effect=>c_height ).
    DATA(lv_cx) = lv_w / 2.
    DATA(lv_cy) = lv_h / 2.

    " Infinite zoom - exponential growth looping
    DATA(lv_zoom_cycle) = CONV f( '3.0' ).  " seconds per zoom cycle
    DATA(lv_zoom_phase) = frac( lv_t / lv_zoom_cycle ).
    DATA(lv_zoom) = exp( lv_zoom_phase * CONV f( '1.1' ) ).  " 1.0 -> 3.0

    " Offset to zoom into corner hole (bottom-left of center triangle)
    DATA(lv_offset_x) = CONV f( '0.25' ) * ( 1 - lv_zoom_phase ).
    DATA(lv_offset_y) = CONV f( '0.15' ) * ( 1 - lv_zoom_phase ).

    " Draw multiple layers for seamless loop
    DATA lv_layer TYPE i.
    lv_layer = 0.
    WHILE lv_layer < 3.
      DATA(lv_layer_zoom) = lv_zoom * ( CONV f( '0.5' ) ** lv_layer ).
      DATA(lv_layer_alpha) = CONV f( 1 ).
      IF lv_layer = 0 AND lv_zoom_phase > CONV f( '0.7' ).
        lv_layer_alpha = ( 1 - lv_zoom_phase ) / CONV f( '0.3' ).
      ENDIF.
      IF lv_layer = 2 AND lv_zoom_phase < CONV f( '0.3' ).
        lv_layer_alpha = lv_zoom_phase / CONV f( '0.3' ).
      ENDIF.

      IF lv_layer_alpha > CONV f( '0.1' ).
        " Draw Sierpinski at this zoom level
        DATA(lv_size) = 400 * lv_layer_zoom.
        DATA(lv_ocx) = lv_cx - lv_offset_x * lv_size.
        DATA(lv_ocy) = lv_cy + lv_offset_y * lv_size.

        " Generate triangles iteratively
        TYPES: BEGIN OF ty_tri, x1 TYPE f, y1 TYPE f, x2 TYPE f, y2 TYPE f, x3 TYPE f, y3 TYPE f, d TYPE i, END OF ty_tri.
        DATA lt_q TYPE STANDARD TABLE OF ty_tri WITH EMPTY KEY.
        DATA lt_n TYPE STANDARD TABLE OF ty_tri WITH EMPTY KEY.

        " Base triangle
        APPEND VALUE ty_tri(
          x1 = lv_ocx y1 = lv_ocy - lv_size * CONV f( '0.866' )
          x2 = lv_ocx - lv_size / 2 y2 = lv_ocy + lv_size * CONV f( '0.433' )
          x3 = lv_ocx + lv_size / 2 y3 = lv_ocy + lv_size * CONV f( '0.433' )
          d = 0
        ) TO lt_q.

        WHILE lines( lt_q ) > 0.
          CLEAR lt_n.
          LOOP AT lt_q INTO DATA(ls_t).
            DATA(lv_edge) = sqrt( ( ls_t-x2 - ls_t-x1 ) ** 2 + ( ls_t-y2 - ls_t-y1 ) ** 2 ).

            " Only draw if on screen and reasonable size
            IF lv_edge > 3 AND lv_edge < 2000.
              IF ls_t-d >= 6 OR lv_edge < 10.
                " Draw triangle with depth-based color
                DATA(lv_hue) = CONV f( ls_t-d * 40 + lv_t * 30 + lv_layer * 120 ).
                DATA(lv_val) = CONV f( '0.5' ) + CONV f( '0.5' ) * lv_layer_alpha.
                DATA(lv_color) = hsv_to_hex( iv_h = lv_hue iv_s = CONV f( '0.8' ) iv_v = lv_val ).
                APPEND VALUE zif_o4d_effect=>ty_line( x1 = ls_t-x1 y1 = ls_t-y1 x2 = ls_t-x2 y2 = ls_t-y2 width = 1 color = lv_color ) TO rs_frame-lines.
                APPEND VALUE zif_o4d_effect=>ty_line( x1 = ls_t-x2 y1 = ls_t-y2 x2 = ls_t-x3 y2 = ls_t-y3 width = 1 color = lv_color ) TO rs_frame-lines.
                APPEND VALUE zif_o4d_effect=>ty_line( x1 = ls_t-x3 y1 = ls_t-y3 x2 = ls_t-x1 y2 = ls_t-y1 width = 1 color = lv_color ) TO rs_frame-lines.
              ELSE.
                " Subdivide
                DATA(lv_mx1) = ( ls_t-x1 + ls_t-x2 ) / 2. DATA(lv_my1) = ( ls_t-y1 + ls_t-y2 ) / 2.
                DATA(lv_mx2) = ( ls_t-x2 + ls_t-x3 ) / 2. DATA(lv_my2) = ( ls_t-y2 + ls_t-y3 ) / 2.
                DATA(lv_mx3) = ( ls_t-x3 + ls_t-x1 ) / 2. DATA(lv_my3) = ( ls_t-y3 + ls_t-y1 ) / 2.
                DATA(lv_nd) = ls_t-d + 1.
                APPEND VALUE ty_tri( x1 = ls_t-x1 y1 = ls_t-y1 x2 = lv_mx1 y2 = lv_my1 x3 = lv_mx3 y3 = lv_my3 d = lv_nd ) TO lt_n.
                APPEND VALUE ty_tri( x1 = lv_mx1 y1 = lv_my1 x2 = ls_t-x2 y2 = ls_t-y2 x3 = lv_mx2 y3 = lv_my2 d = lv_nd ) TO lt_n.
                APPEND VALUE ty_tri( x1 = lv_mx3 y1 = lv_my3 x2 = lv_mx2 y2 = lv_my2 x3 = ls_t-x3 y3 = ls_t-y3 d = lv_nd ) TO lt_n.
              ENDIF.
            ENDIF.
          ENDLOOP.
          lt_q = lt_n.
        ENDWHILE.
      ENDIF.
      lv_layer = lv_layer + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD hsv_to_hex.
    DATA: lv_c TYPE f, lv_x TYPE f, lv_m TYPE f, lv_r TYPE f, lv_g TYPE f, lv_b TYPE f.
    DATA(lv_h) = CONV f( CONV i( iv_h ) MOD 360 ). lv_c = iv_v * iv_s.
    lv_x = lv_c * ( 1 - abs( frac( lv_h / 120 ) * 2 - 1 ) ). lv_m = iv_v - lv_c.
    IF lv_h < 60. lv_r = lv_c. lv_g = lv_x. lv_b = 0.
    ELSEIF lv_h < 120. lv_r = lv_x. lv_g = lv_c. lv_b = 0.
    ELSEIF lv_h < 180. lv_r = 0. lv_g = lv_c. lv_b = lv_x.
    ELSEIF lv_h < 240. lv_r = 0. lv_g = lv_x. lv_b = lv_c.
    ELSEIF lv_h < 300. lv_r = lv_x. lv_g = 0. lv_b = lv_c.
    ELSE. lv_r = lv_c. lv_g = 0. lv_b = lv_x. ENDIF.
    rv_hex = |#{ int_to_hex( CONV i( ( lv_r + lv_m ) * 255 ) ) }{ int_to_hex( CONV i( ( lv_g + lv_m ) * 255 ) ) }{ int_to_hex( CONV i( ( lv_b + lv_m ) * 255 ) ) }|.
  ENDMETHOD.

  METHOD int_to_hex.
    DATA lv_hex TYPE x LENGTH 1. DATA(lv_val) = nmax( val1 = 0 val2 = nmin( val1 = 255 val2 = iv_int ) ).
    lv_hex = lv_val. rv_hex = |{ lv_hex }|. TRANSLATE rv_hex TO LOWER CASE.
  ENDMETHOD.
ENDCLASS.
