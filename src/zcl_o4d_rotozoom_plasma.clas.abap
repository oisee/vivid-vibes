CLASS zcl_o4d_rotozoom_plasma DEFINITION PUBLIC FINAL CREATE PUBLIC.
*======================================================================
* ROTOZOOM PLASMA - Recursive rotozoomed texture
*======================================================================
* The texture being rotozoomed is ITSELF a rotozoom pattern!
* Multiple layers of rotating/zooming sine waves = psychedelic plasma
*======================================================================
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    CLASS-METHODS new
      IMPORTING iv_layers TYPE i DEFAULT 3
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_o4d_rotozoom_plasma.
  PRIVATE SECTION.
    DATA mv_layers TYPE i VALUE 3.
    DATA mv_scale TYPE i VALUE 8.

    METHODS get_plasma_color
      IMPORTING iv_u TYPE f iv_v TYPE f iv_t TYPE f
      RETURNING VALUE(rv_hex) TYPE string.
ENDCLASS.



CLASS ZCL_O4D_ROTOZOOM_PLASMA IMPLEMENTATION.


  METHOD new.
    ro_obj = NEW #( ).
    ro_obj->mv_layers = iv_layers.
  ENDMETHOD.


  METHOD zif_o4d_effect~get_name. rv_name = 'rotozoom_plasma'. ENDMETHOD.


  METHOD zif_o4d_effect~get_params. ENDMETHOD.


  METHOD zif_o4d_effect~set_param. ENDMETHOD.


  METHOD zif_o4d_effect~render.
    rs_frame = zif_o4d_effect~render_frame( VALUE #(
      t = is_sync-time gt = is_sync-time gf = CONV i( is_sync-time * 30 )
      gbi = VALUE #( pulse = is_sync-intensity ) ) ).
  ENDMETHOD.


  METHOD zif_o4d_effect~render_frame.
    DATA(lv_t) = is_ctx-gt.
    DATA(lv_w) = CONV f( zif_o4d_effect=>c_width ).
    DATA(lv_h) = CONV f( zif_o4d_effect=>c_height ).
    DATA(lv_cx) = lv_w / 2.
    DATA(lv_cy) = lv_h / 2.

    " === LAYER 1: Outer rotozoom ===
    DATA(lv_angle1) = lv_t * '0.4'.
    DATA(lv_zoom1) = '1.5' + sin( lv_t * '0.3' ) * '0.5'.
    DATA(lv_cos1) = cos( lv_angle1 ).
    DATA(lv_sin1) = sin( lv_angle1 ).

    " === LAYER 2: Inner rotozoom (counter-rotating) ===
    DATA(lv_angle2) = lv_t * '-0.6' + '1.5'.
    DATA(lv_zoom2) = '2.0' + sin( lv_t * '0.5' ) * '1.0'.
    DATA(lv_cos2) = cos( lv_angle2 ).
    DATA(lv_sin2) = sin( lv_angle2 ).

    " === LAYER 3: Deepest layer (fast rotate) ===
    DATA(lv_angle3) = lv_t * '0.8'.
    DATA(lv_zoom3) = '3.0' + cos( lv_t * '0.4' ) * '1.5'.
    DATA(lv_cos3) = cos( lv_angle3 ).
    DATA(lv_sin3) = sin( lv_angle3 ).

    " Beat boost
    DATA(lv_beat_boost) = CONV f( 1 ).
*    IF is_ctx-gbi-pulse > '0.5'.
*      lv_beat_boost = '1.3'.
*    ENDIF.

    " Render plasma grid
    DATA: lv_y TYPE i, lv_x TYPE i.
    DATA: lv_dx TYPE f, lv_dy TYPE f.
    DATA: lv_u1 TYPE f, lv_v1 TYPE f, lv_u2 TYPE f, lv_v2 TYPE f, lv_u3 TYPE f, lv_v3 TYPE f.

    lv_y = 0.
    WHILE lv_y < lv_h.
      lv_x = 0.
      WHILE lv_x < lv_w.
        " Screen to centered coords
        lv_dx = ( CONV f( lv_x ) - lv_cx ) / 50.
        lv_dy = ( CONV f( lv_y ) - lv_cy ) / 50.

        " === Apply rotozoom layer 1 ===
        lv_u1 = ( lv_dx * lv_cos1 - lv_dy * lv_sin1 ) * lv_zoom1 * lv_beat_boost.
        lv_v1 = ( lv_dx * lv_sin1 + lv_dy * lv_cos1 ) * lv_zoom1 * lv_beat_boost.

        " === Apply rotozoom layer 2 on result of layer 1 ===
        lv_u2 = ( lv_u1 * lv_cos2 - lv_v1 * lv_sin2 ) * lv_zoom2.
        lv_v2 = ( lv_u1 * lv_sin2 + lv_v1 * lv_cos2 ) * lv_zoom2.

        " === Apply rotozoom layer 3 on result of layer 2 ===
        IF mv_layers >= 3.
          lv_u3 = ( lv_u2 * lv_cos3 - lv_v2 * lv_sin3 ) * lv_zoom3.
          lv_v3 = ( lv_u2 * lv_sin3 + lv_v2 * lv_cos3 ) * lv_zoom3.
        ELSE.
          lv_u3 = lv_u2.
          lv_v3 = lv_v2.
        ENDIF.

        " Get plasma color from final coordinates
        DATA(lv_color) = get_plasma_color( iv_u = lv_u3 iv_v = lv_v3 iv_t = lv_t ).

        APPEND VALUE zif_o4d_effect=>ty_rect(
          x = lv_x y = lv_y w = mv_scale h = mv_scale
          fill = lv_color
        ) TO rs_frame-rects.

        lv_x = lv_x + mv_scale.
      ENDWHILE.
      lv_y = lv_y + mv_scale.
    ENDWHILE.

    " Title with glow effect
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_w / 2 y = 20 text = 'ROTOZOOM PLASMA'
      color = '#fff' size = 14 align = 'center'
    ) TO rs_frame-texts.

    " Beat flash
*    IF is_ctx-gbi-pulse > '0.7'.
*      rs_frame-flash-active = abap_true.
*      rs_frame-flash-intensity = '0.2'.
*      rs_frame-flash-r = 1. rs_frame-flash-g = 0. rs_frame-flash-b = 1.
*    ENDIF.
  ENDMETHOD.


  METHOD get_plasma_color.
    " === THE MAGIC: Plasma from recursive rotozoom coordinates ===
    " Multiple overlapping sine waves create the plasma pattern
    DATA: lv_v1 TYPE f, lv_v2 TYPE f, lv_v3 TYPE f, lv_v4 TYPE f.
    DATA: lv_val TYPE f.
    DATA: lv_r TYPE i, lv_g TYPE i, lv_b TYPE i.

    " Wave 1: Horizontal ripple
    lv_v1 = sin( iv_u + iv_t * 2 ).

    " Wave 2: Vertical ripple
    lv_v2 = sin( iv_v + iv_t * '1.5' ).

    " Wave 3: Diagonal wave
    lv_v3 = sin( ( iv_u + iv_v ) * '0.7' + iv_t ).

    " Wave 4: Circular ripple from center
    lv_v4 = sin( sqrt( iv_u * iv_u + iv_v * iv_v ) * 2 - iv_t * 3 ).

    " Combine waves (range -4 to +4, normalize to 0-1)
    lv_val = ( lv_v1 + lv_v2 + lv_v3 + lv_v4 + 4 ) / 8.

    " === Psychedelic color mapping ===
    " Use sine waves for RGB with phase offsets
    lv_r = CONV i( ( sin( lv_val * '6.283' + iv_t ) + 1 ) * 127 ).
    lv_g = CONV i( ( sin( lv_val * '6.283' + iv_t + '2.094' ) + 1 ) * 127 ).
    lv_b = CONV i( ( sin( lv_val * '6.283' + iv_t + '4.188' ) + 1 ) * 127 ).

    " Boost saturation
    DATA(lv_max) = nmax( val1 = lv_r val2 = nmax( val1 = lv_g val2 = lv_b ) ).
    IF lv_max > 0.
      DATA(lv_boost) = CONV f( 255 ) / lv_max.
      IF lv_boost > '1.5'. lv_boost = '1.5'. ENDIF.
      lv_r = CONV i( lv_r * lv_boost ).
      lv_g = CONV i( lv_g * lv_boost ).
      lv_b = CONV i( lv_b * lv_boost ).
    ENDIF.

    " Clamp
    IF lv_r > 255. lv_r = 255. ENDIF.
    IF lv_g > 255. lv_g = 255. ENDIF.
    IF lv_b > 255. lv_b = 255. ENDIF.
    IF lv_r < 0. lv_r = 0. ENDIF.
    IF lv_g < 0. lv_g = 0. ENDIF.
    IF lv_b < 0. lv_b = 0. ENDIF.

    " Convert to hex
    DATA: lv_rh TYPE x LENGTH 1, lv_gh TYPE x LENGTH 1, lv_bh TYPE x LENGTH 1.
    lv_rh = lv_r. lv_gh = lv_g. lv_bh = lv_b.
    rv_hex = |#{ lv_rh }{ lv_gh }{ lv_bh }|.
  ENDMETHOD.
ENDCLASS.
