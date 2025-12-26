CLASS zcl_o4d_swirl DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
  PRIVATE SECTION.
    CONSTANTS c_pi TYPE f VALUE '3.14159265'.
    CLASS-METHODS hsv_to_hex IMPORTING iv_h TYPE f iv_s TYPE f iv_v TYPE f RETURNING VALUE(rv_hex) TYPE string.
    CLASS-METHODS int_to_hex IMPORTING iv_int TYPE i RETURNING VALUE(rv_hex) TYPE string.
ENDCLASS.

CLASS zcl_o4d_swirl IMPLEMENTATION.
  METHOD zif_o4d_effect~get_name. rv_name = 'swirl'. ENDMETHOD.
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
    DATA(lv_cx) = CONV f( zif_o4d_effect=>c_width ) / 2.
    DATA(lv_cy) = CONV f( zif_o4d_effect=>c_height ) / 2.

    " Concentric rings with twist
    DATA lv_ring TYPE i.
    lv_ring = 0.
    WHILE lv_ring < 20.
      DATA(lv_r) = CONV f( 10 + lv_ring * 10 ).
      DATA(lv_hue) = CONV f( CONV i( CONV f( lv_ring ) * 20 + lv_t * 50 ) MOD 360 ).
      DATA(lv_color) = hsv_to_hex( iv_h = lv_hue iv_s = CONV f( '0.9' ) iv_v = CONV f( '0.8' ) ).

      " Draw ring segments with twist
      DATA lv_seg TYPE i.
      lv_seg = 0.
      WHILE lv_seg < 32.
        DATA(lv_twist) = sin( lv_r * CONV f( '0.1' ) - lv_t * 3 ) * CONV f( '0.5' ).
        DATA(lv_a1) = ( CONV f( lv_seg ) / 32 ) * c_pi * 2 + lv_twist.
        DATA(lv_a2) = ( CONV f( lv_seg + 1 ) / 32 ) * c_pi * 2 + lv_twist.

        DATA(lv_x1) = lv_cx + cos( lv_a1 ) * lv_r.
        DATA(lv_y1) = lv_cy + sin( lv_a1 ) * lv_r * CONV f( '0.8' ).
        DATA(lv_x2) = lv_cx + cos( lv_a2 ) * lv_r.
        DATA(lv_y2) = lv_cy + sin( lv_a2 ) * lv_r * CONV f( '0.8' ).

        APPEND VALUE zif_o4d_effect=>ty_line(
          x1 = lv_x1 y1 = lv_y1 x2 = lv_x2 y2 = lv_y2
          width = 1 color = lv_color
        ) TO rs_frame-lines.

        lv_seg = lv_seg + 1.
      ENDWHILE.
      lv_ring = lv_ring + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD hsv_to_hex.
    DATA: lv_c TYPE f, lv_x TYPE f, lv_m TYPE f, lv_r TYPE f, lv_g TYPE f, lv_b TYPE f.
    DATA(lv_h) = CONV f( CONV i( iv_h ) MOD 360 ).
    lv_c = iv_v * iv_s.
    lv_x = lv_c * ( 1 - abs( frac( lv_h / 120 ) * 2 - 1 ) ).
    lv_m = iv_v - lv_c.
    IF lv_h < 60. lv_r = lv_c. lv_g = lv_x. lv_b = 0.
    ELSEIF lv_h < 120. lv_r = lv_x. lv_g = lv_c. lv_b = 0.
    ELSEIF lv_h < 180. lv_r = 0. lv_g = lv_c. lv_b = lv_x.
    ELSEIF lv_h < 240. lv_r = 0. lv_g = lv_x. lv_b = lv_c.
    ELSEIF lv_h < 300. lv_r = lv_x. lv_g = 0. lv_b = lv_c.
    ELSE. lv_r = lv_c. lv_g = 0. lv_b = lv_x. ENDIF.
    rv_hex = |#{ int_to_hex( CONV i( ( lv_r + lv_m ) * 255 ) ) }{ int_to_hex( CONV i( ( lv_g + lv_m ) * 255 ) ) }{ int_to_hex( CONV i( ( lv_b + lv_m ) * 255 ) ) }|.
  ENDMETHOD.

  METHOD int_to_hex.
    DATA lv_hex TYPE x LENGTH 1.
    DATA(lv_val) = nmax( val1 = 0 val2 = nmin( val1 = 255 val2 = iv_int ) ).
    lv_hex = lv_val.
    rv_hex = |{ lv_hex }|.
    TRANSLATE rv_hex TO LOWER CASE.
  ENDMETHOD.
ENDCLASS.
