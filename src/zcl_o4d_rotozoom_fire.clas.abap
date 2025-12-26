CLASS zcl_o4d_rotozoom_fire DEFINITION PUBLIC FINAL CREATE PUBLIC.
*======================================================================
* ROTOZOOM FIRE - Rotating/zooming fire texture
* Classic demoscene fire with upward propagation
*======================================================================
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
  PRIVATE SECTION.
    " Fire buffer (stateful between frames)
    DATA mt_fire TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    DATA mv_initialized TYPE abap_bool.
    DATA mv_tex_w TYPE i VALUE 64.
    DATA mv_tex_h TYPE i VALUE 64.
    DATA mv_last_frame TYPE i VALUE -1.
    DATA mv_seed TYPE i VALUE 12345.

    METHODS init_fire.
    METHODS step_fire.
    METHODS rand RETURNING VALUE(rv_val) TYPE i.
    METHODS get_fire_color
      IMPORTING iv_val        TYPE i
      RETURNING VALUE(rv_hex) TYPE string.
ENDCLASS.

CLASS zcl_o4d_rotozoom_fire IMPLEMENTATION.

  METHOD zif_o4d_effect~get_name. rv_name = 'rotozoom_fire'. ENDMETHOD.
  METHOD zif_o4d_effect~get_params. ENDMETHOD.
  METHOD zif_o4d_effect~set_param. ENDMETHOD.

  METHOD zif_o4d_effect~render.
    rs_frame = zif_o4d_effect~render_frame( VALUE #(
      t = is_sync-time gt = is_sync-time gf = CONV i( is_sync-time * 30 )
      bi = VALUE #( pulse = is_sync-intensity ) ) ).
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    DATA(lv_t) = is_ctx-t.
    DATA(lv_frame) = is_ctx-gf.
    DATA(lv_w) = CONV f( zif_o4d_effect=>c_width ).
    DATA(lv_h) = CONV f( zif_o4d_effect=>c_height ).

    " Initialize fire buffer once
    IF mv_initialized = abap_false.
      init_fire( ).
      mv_initialized = abap_true.
    ENDIF.

    " Step fire simulation (once per frame)
    IF lv_frame <> mv_last_frame.
      step_fire( ).
      mv_last_frame = lv_frame.
    ENDIF.

    " Rotozoom parameters
    DATA(lv_scale) = 8.
    DATA(lv_angle) = lv_t * CONV f( '0.8' ) + sin( lv_t / 2 ) * CONV f( '0.5' ).
    DATA(lv_zoom) = CONV f( '2.0' ) + sin( lv_t / 3 ) * CONV f( '1.5' ).

    " Beat boost
    IF is_ctx-gbi-pulse > CONV f( '0.5' ).
      lv_zoom = lv_zoom * CONV f( '1.3' ).
    ENDIF.

    DATA(lv_cos_a) = cos( lv_angle ).
    DATA(lv_sin_a) = sin( lv_angle ).
    DATA(lv_cx) = lv_w / 2.
    DATA(lv_cy) = lv_h / 2.

    " Render rotozoom
    DATA(lv_y) = 0.
    WHILE lv_y < lv_h.
      DATA(lv_x) = 0.
      WHILE lv_x < lv_w.
        " Transform to texture coords
        DATA(lv_dx) = CONV f( lv_x ) - lv_cx.
        DATA(lv_dy) = CONV f( lv_y ) - lv_cy.

        DATA(lv_u) = ( lv_dx * lv_cos_a - lv_dy * lv_sin_a ) / lv_zoom.
        DATA(lv_v) = ( lv_dx * lv_sin_a + lv_dy * lv_cos_a ) / lv_zoom.

        " Wrap texture coordinates
        DATA(lv_tx) = CONV i( lv_u ) MOD mv_tex_w.
        DATA(lv_ty) = CONV i( lv_v ) MOD mv_tex_h.
        IF lv_tx < 0. lv_tx = lv_tx + mv_tex_w. ENDIF.
        IF lv_ty < 0. lv_ty = lv_ty + mv_tex_h. ENDIF.

        " Get fire value from buffer
        DATA(lv_idx) = lv_ty * mv_tex_w + lv_tx + 1.
        DATA(lv_fire_val) = 0.
        IF lv_idx > 0 AND lv_idx <= lines( mt_fire ).
          lv_fire_val = mt_fire[ lv_idx ].
        ENDIF.

        DATA(lv_color) = get_fire_color( lv_fire_val ).

        APPEND VALUE zif_o4d_effect=>ty_rect(
          x = lv_x y = lv_y w = lv_scale h = lv_scale
          fill = lv_color
        ) TO rs_frame-rects.

        lv_x = lv_x + lv_scale.
      ENDWHILE.
      lv_y = lv_y + lv_scale.
    ENDWHILE.

    " Title
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_w / 2 y = 20 text = 'FIRE ROTOZOOM'
      color = '#FFFF00' size = 14 align = 'center'
    ) TO rs_frame-texts.

  ENDMETHOD.

  METHOD init_fire.
    " Initialize fire buffer with zeros
    CLEAR mt_fire.
    DO mv_tex_w * mv_tex_h TIMES.
      APPEND 0 TO mt_fire.
    ENDDO.
  ENDMETHOD.

  METHOD rand.
    " Simple Linear Congruential Generator
    mv_seed = ( mv_seed * 1103515245 + 12345 ) MOD 2147483648.
    rv_val = mv_seed MOD 256.
  ENDMETHOD.

  METHOD step_fire.
    DATA: lv_x      TYPE i,
          lv_y      TYPE i,
          lv_idx    TYPE i,
          lv_sum    TYPE i,
          lv_avg    TYPE i,
          lv_x1     TYPE i,
          lv_x2     TYPE i,
          lv_x3     TYPE i,
          lv_y1     TYPE i,
          lv_cool   TYPE i.

    " === SEED BOTTOM ROW WITH RANDOM FIRE ===
    lv_y = mv_tex_h - 1.
    lv_x = 0.
    WHILE lv_x < mv_tex_w.
      lv_idx = lv_y * mv_tex_w + lv_x + 1.
      " Random hot spots
      IF rand( ) > 180.
        mt_fire[ lv_idx ] = 255.
      ELSEIF rand( ) > 100.
        mt_fire[ lv_idx ] = 200 + rand( ) MOD 55.
      ELSE.
        mt_fire[ lv_idx ] = 150 + rand( ) MOD 50.
      ENDIF.
      lv_x = lv_x + 1.
    ENDWHILE.

    " === PROPAGATE FIRE UPWARD ===
    " Process from top to bottom-1
    lv_y = 0.
    WHILE lv_y < mv_tex_h - 1.
      lv_x = 0.
      WHILE lv_x < mv_tex_w.
        " Sample 4 pixels below (with horizontal wrap)
        lv_x1 = ( lv_x - 1 + mv_tex_w ) MOD mv_tex_w.
        lv_x2 = lv_x.
        lv_x3 = ( lv_x + 1 ) MOD mv_tex_w.
        lv_y1 = lv_y + 1.

        lv_sum = mt_fire[ lv_y1 * mv_tex_w + lv_x1 + 1 ]
               + mt_fire[ lv_y1 * mv_tex_w + lv_x2 + 1 ]
               + mt_fire[ lv_y1 * mv_tex_w + lv_x3 + 1 ].

        " Add center pixel from 2 rows below for smoother flames
        IF lv_y < mv_tex_h - 2.
          lv_sum = lv_sum + mt_fire[ ( lv_y + 2 ) * mv_tex_w + lv_x2 + 1 ].
          lv_avg = lv_sum / 4.
        ELSE.
          lv_avg = lv_sum / 3.
        ENDIF.

        " Cooling factor - fire fades as it rises
        lv_cool = 2 + rand( ) MOD 3.
        lv_avg = lv_avg - lv_cool.
        IF lv_avg < 0.
          lv_avg = 0.
        ENDIF.

        lv_idx = lv_y * mv_tex_w + lv_x + 1.
        mt_fire[ lv_idx ] = lv_avg.

        lv_x = lv_x + 1.
      ENDWHILE.
      lv_y = lv_y + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD get_fire_color.
    " Classic fire palette: black -> red -> orange -> yellow -> white
    DATA: lv_r TYPE i, lv_g TYPE i, lv_b TYPE i.

    IF iv_val < 64.
      " Black to dark red
      lv_r = iv_val * 4.
      lv_g = 0.
      lv_b = 0.
    ELSEIF iv_val < 128.
      " Dark red to orange
      lv_r = 255.
      lv_g = ( iv_val - 64 ) * 3.
      lv_b = 0.
    ELSEIF iv_val < 192.
      " Orange to yellow
      lv_r = 255.
      lv_g = 192 + ( iv_val - 128 ).
      lv_b = 0.
    ELSE.
      " Yellow to white
      lv_r = 255.
      lv_g = 255.
      lv_b = ( iv_val - 192 ) * 4.
    ENDIF.

    " Clamp
    IF lv_r > 255. lv_r = 255. ENDIF.
    IF lv_g > 255. lv_g = 255. ENDIF.
    IF lv_b > 255. lv_b = 255. ENDIF.

    DATA: lv_rh TYPE x LENGTH 1, lv_gh TYPE x LENGTH 1, lv_bh TYPE x LENGTH 1.
    lv_rh = lv_r. lv_gh = lv_g. lv_bh = lv_b.
    rv_hex = |#{ lv_rh }{ lv_gh }{ lv_bh }|.
  ENDMETHOD.

ENDCLASS.
