CLASS zcl_o4d_voxel_land DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    METHODS constructor.

private section.

  aliases C_HEIGHT
    for ZIF_O4D_EFFECT~C_HEIGHT .
  aliases C_WIDTH
    for ZIF_O4D_EFFECT~C_WIDTH .

  data MV_NAME type STRING value 'VOXEL_LAND' ##NO_TEXT.
  constants C_MAX_DIST type I value 60 ##NO_TEXT.
  constants C_HORIZON type I value 100 ##NO_TEXT.
  constants C_CAM_H type I value 80 ##NO_TEXT.  " Half resolution for speed

  methods INT_TO_HEX
    importing
      !IV_INT type I
    returning
      value(RV_HEX) type STRING .
  methods GET_HEIGHT
    importing
      !IV_X type F
      !IV_Z type F
    returning
      value(RV_H) type F .
ENDCLASS.



CLASS ZCL_O4D_VOXEL_LAND IMPLEMENTATION.


  METHOD constructor.
  ENDMETHOD.


  METHOD int_to_hex.
    DATA lv_hex TYPE x LENGTH 1.
    lv_hex = iv_int.
    rv_hex = |{ lv_hex }|.
  ENDMETHOD.


  METHOD get_height.
    " Procedural terrain height using waves
    rv_h = sin( iv_x * '0.02' + iv_z * '0.01' ) * 30
         + sin( iv_z * '0.03' ) * 20
         + sin( iv_x * '0.01' - iv_z * '0.02' ) * 15
         + 50.
  ENDMETHOD.


  METHOD zif_o4d_effect~get_name.
    rv_name = mv_name.
  ENDMETHOD.


  METHOD zif_o4d_effect~get_params.
    CLEAR rt_params.
  ENDMETHOD.


  METHOD zif_o4d_effect~set_param.
  ENDMETHOD.


  METHOD zif_o4d_effect~render.
    DATA ls_ctx TYPE zif_o4d_effect=>ty_render_ctx.
    ls_ctx-t = is_sync-time.
    ls_ctx-gbi-time = is_sync-time.
    ls_ctx-gbi-bar = is_sync-bar.
    ls_ctx-gbi-beat = is_sync-beat.
    ls_ctx-gbi-bar_phase = is_sync-bar_phase.
    ls_ctx-gbi-pulse = is_sync-intensity.
    rs_frame = zif_o4d_effect~render_frame( ls_ctx ).
  ENDMETHOD.


  METHOD zif_o4d_effect~render_frame.
    DATA: lv_t TYPE f, lv_cam_z TYPE f.
    DATA: lv_d TYPE i, lv_i TYPE i.
    DATA: lv_world_x TYPE f, lv_world_z TYPE f.
    DATA: lv_h TYPE f, lv_screen_y TYPE i, lv_prev_y TYPE i.
    DATA: lv_bright TYPE i, lv_green TYPE i.
    DATA: lv_hex_r TYPE string, lv_hex_g TYPE string, lv_hex_b TYPE string.
    DATA ls_rect TYPE zif_o4d_effect=>ty_rect.
    DATA ls_text TYPE zif_o4d_effect=>ty_text.

    " Y-buffer for each screen column
    TYPES: BEGIN OF ty_ybuf,
             y TYPE i,
           END OF ty_ybuf.
    DATA: lt_ybuf TYPE STANDARD TABLE OF ty_ybuf WITH EMPTY KEY,
          ls_ybuf TYPE ty_ybuf.

    lv_t = is_ctx-gbi-time.
    lv_cam_z = lv_t * 20.  " Camera moves forward

    " Initialize y-buffer with screen height
    DO c_width TIMES.
      ls_ybuf-y = c_height.
      APPEND ls_ybuf TO lt_ybuf.
    ENDDO.

    " Sky gradient
    DATA lv_sky_y TYPE i.
    DO 5 TIMES.
      lv_sky_y = ( sy-index - 1 ) * 20.
      lv_bright = 10 + ( 5 - sy-index ) * 8.
      CLEAR ls_rect.
      ls_rect-x = 0. ls_rect-y = lv_sky_y. ls_rect-w = c_width. ls_rect-h = 20.
      ls_rect-fill = |rgb(0,{ lv_bright },{ lv_bright + 20 })|.
      APPEND ls_rect TO rs_frame-rects.
    ENDDO.

    " === COMANCHE-STYLE RAYCAST ===
    " For each depth step (near to far)
    DO c_max_dist TIMES.
      lv_d = sy-index.

      " For each screen column
      DO c_width TIMES.
        lv_i = sy-index.

        " Read current y-buffer
        READ TABLE lt_ybuf INTO ls_ybuf INDEX lv_i.
        lv_prev_y = ls_ybuf-y.

        " Calculate world position
        lv_world_x = ( lv_i - c_width / 2 ) * lv_d / 20.
        lv_world_z = lv_cam_z + lv_d.

        " Get terrain height
        lv_h = get_height( iv_x = lv_world_x iv_z = lv_world_z ).

        " Project to screen (perspective)
        DATA lv_height_diff TYPE f.
        lv_height_diff = c_cam_h - lv_h.
        lv_screen_y = c_horizon + lv_height_diff * 100 / lv_d.

        " Clamp to screen
        IF lv_screen_y < 0. lv_screen_y = 0. ENDIF.
        IF lv_screen_y > c_height. lv_screen_y = c_height. ENDIF.

        " Draw column if above y-buffer (occlusion)
        IF lv_screen_y < lv_prev_y.
          " Calculate color based on depth and height
          lv_bright = 50 + ( c_max_dist - lv_d ) * 3.
          IF lv_bright > 200. lv_bright = 200. ENDIF.

          " Green tint varies with height
          lv_green = lv_bright + lv_h / 2.
          IF lv_green > 230. lv_green = 230. ENDIF.

          lv_hex_r = int_to_hex( lv_bright / 3 ).
          lv_hex_g = int_to_hex( lv_green ).
          lv_hex_b = int_to_hex( lv_bright / 3 ).

          " Draw vertical line segment (2 pixels wide for 320 screen)
          CLEAR ls_rect.
          ls_rect-x = ( lv_i - 1 ) * 2.  " Double width for 320 screen
          ls_rect-y = lv_screen_y.
          ls_rect-w = 2.
          ls_rect-h = lv_prev_y - lv_screen_y.
          ls_rect-fill = |#{ lv_hex_r }{ lv_hex_g }{ lv_hex_b }|.
          APPEND ls_rect TO rs_frame-rects.

          " Update y-buffer
          ls_ybuf-y = lv_screen_y.
          MODIFY lt_ybuf FROM ls_ybuf INDEX lv_i.
        ENDIF.
      ENDDO.
    ENDDO.

    " Title
    CLEAR ls_text.
    ls_text-x = 160. ls_text-y = 15.
    ls_text-text = 'VOXEL LANDSCAPE'.
    ls_text-color = '#00FF66'. ls_text-size = 10. ls_text-align = 'center'.
    APPEND ls_text TO rs_frame-texts.

    " Flash on beat
*    IF is_ctx-gbi-on_beat = abap_true.
*      rs_frame-flash-active = abap_true.
*      rs_frame-flash-intensity = '0.15'.
*      rs_frame-flash-r = 0. rs_frame-flash-g = 1. rs_frame-flash-b = '0.5'.
*    ENDIF.
  ENDMETHOD.
ENDCLASS.
