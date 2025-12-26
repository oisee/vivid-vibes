CLASS zcl_o4d_voxel_city DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    METHODS constructor.

  PRIVATE SECTION.
    DATA mv_name TYPE string VALUE 'VOXEL_CITY'.
    METHODS int_to_hex IMPORTING iv_int TYPE i RETURNING VALUE(rv_hex) TYPE string.
    METHODS hash IMPORTING iv_x TYPE i iv_y TYPE i RETURNING VALUE(rv_val) TYPE f.

ENDCLASS.

CLASS zcl_o4d_voxel_city IMPLEMENTATION.

  METHOD constructor.
  ENDMETHOD.

  METHOD int_to_hex.
    DATA lv_hex TYPE x LENGTH 1.
    DATA lv_val TYPE i.
    lv_val = iv_int.
    IF lv_val < 0. lv_val = 0. ENDIF.
    IF lv_val > 255. lv_val = 255. ENDIF.
    lv_hex = lv_val.
    rv_hex = |{ lv_hex }|.
  ENDMETHOD.

  METHOD hash.
    " Simple hash function
    DATA lv_val TYPE f.
    lv_val = sin( iv_x * '127.1' + iv_y * '311.7' ) * '43758.5453'.
    rv_val = lv_val - floor( lv_val ).
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
    DATA: lv_t TYPE f, lv_cam_z TYPE f, lv_cam_y TYPE f.
    DATA: lv_z TYPE i, lv_grid_x TYPE i, lv_grid_z TYPE i.
    DATA: lv_h TYPE f, lv_building_y TYPE f, lv_ray_y TYPE f.
    DATA: lv_shade TYPE f, lv_window_x TYPE f, lv_window_y TYPE f.
    DATA: lv_screen_x TYPE f, lv_screen_y TYPE f.
    DATA: lv_r TYPE i, lv_g TYPE i, lv_b TYPE i.
    DATA: lv_hex_r TYPE string, lv_hex_g TYPE string, lv_hex_b TYPE string.
    DATA: lv_road_x TYPE f.
    DATA ls_rect TYPE zif_o4d_effect=>ty_rect.
    DATA ls_text TYPE zif_o4d_effect=>ty_text.

    lv_t = is_ctx-gbi-time.

    " Camera
    lv_cam_z = lv_t * 2.
    lv_cam_y = '0.3'.

    " Sky gradient (night sky)
    DO 8 TIMES.
      DATA lv_y_pos TYPE f.
      lv_y_pos = ( sy-index - 1 ) / 8.

      lv_r = '0.0' * 255 + lv_y_pos * 10.
      lv_g = '0.0' * 255 + lv_y_pos * 5.
      lv_b = '0.1' * 255 + lv_y_pos * 25.

      lv_hex_r = int_to_hex( lv_r ).
      lv_hex_g = int_to_hex( lv_g ).
      lv_hex_b = int_to_hex( lv_b ).

      CLEAR ls_rect.
      ls_rect-x = 0.
      ls_rect-y = ( sy-index - 1 ) * 10.
      ls_rect-w = 320.
      ls_rect-h = 10.
      ls_rect-fill = |#{ lv_hex_r }{ lv_hex_g }{ lv_hex_b }|.
      APPEND ls_rect TO rs_frame-rects.
    ENDDO.

    " Stars
    DO 30 TIMES.
      DATA lv_star_x TYPE f.
      DATA lv_star_y TYPE f.
      DATA lv_star_bright TYPE i.

      lv_star_x = hash( iv_x = sy-index iv_y = 0 ) * 320.
      lv_star_y = hash( iv_x = sy-index iv_y = 1 ) * 80.
      lv_star_bright = 100 + sin( lv_t * 2 + sy-index ) * 50.

      lv_hex_r = int_to_hex( lv_star_bright ).

      CLEAR ls_rect.
      ls_rect-x = lv_star_x. ls_rect-y = lv_star_y.
      ls_rect-w = 2. ls_rect-h = 2.
      ls_rect-fill = |#{ lv_hex_r }{ lv_hex_r }{ lv_hex_r }|.
      APPEND ls_rect TO rs_frame-rects.
    ENDDO.

    " Buildings (back to front)
    DO 25 TIMES.
      lv_z = 26 - sy-index.  " Far to near

      DO 15 TIMES.
        lv_grid_x = sy-index - 8.  " -7 to 7
        lv_grid_z = floor( lv_cam_z * '0.5' ) + lv_z.

        " Building height from hash
        lv_h = hash( iv_x = lv_grid_x iv_y = lv_grid_z ).

        IF lv_h < '0.3'.
          CONTINUE.  " No building here
        ENDIF.

        lv_h = lv_h * '0.8'.

        " Project to screen
        DATA lv_proj_factor TYPE f.
        lv_proj_factor = 100 / lv_z.
        lv_screen_x = 160 + lv_grid_x * lv_proj_factor.
        lv_screen_y = 100 - lv_h * lv_proj_factor * 2.

        IF lv_screen_x < -20 OR lv_screen_x > 340.
          CONTINUE.
        ENDIF.

        DATA lv_bld_w TYPE f.
        DATA lv_bld_h TYPE f.
        lv_bld_w = lv_proj_factor * '0.8'.
        lv_bld_h = lv_h * lv_proj_factor * 2.

        " Building shade based on depth
        lv_shade = 1 - lv_z / 25.

        " Building base color (dark gray/blue)
        lv_r = 25 * lv_shade.
        lv_g = 25 * lv_shade.
        lv_b = 38 * lv_shade.

        lv_hex_r = int_to_hex( lv_r ).
        lv_hex_g = int_to_hex( lv_g ).
        lv_hex_b = int_to_hex( lv_b ).

        CLEAR ls_rect.
        ls_rect-x = lv_screen_x - lv_bld_w / 2.
        ls_rect-y = lv_screen_y.
        ls_rect-w = lv_bld_w.
        ls_rect-h = lv_bld_h.
        ls_rect-fill = |#{ lv_hex_r }{ lv_hex_g }{ lv_hex_b }|.
        APPEND ls_rect TO rs_frame-rects.

        " Windows (lit randomly)
        DATA lv_win_rows TYPE i.
        DATA lv_win_cols TYPE i.
        lv_win_rows = floor( lv_bld_h / 8 ).
        lv_win_cols = floor( lv_bld_w / 5 ).

        IF lv_win_rows > 0 AND lv_win_cols > 0 AND lv_z < 15.
          DO lv_win_rows TIMES.
            DATA lv_wy TYPE i.
            lv_wy = sy-index.
            DO lv_win_cols TIMES.
              DATA lv_wx TYPE i.
              lv_wx = sy-index.

              " Random window lit
              IF hash( iv_x = lv_grid_x + lv_wx * 10 iv_y = lv_grid_z + lv_wy * 10 ) > '0.5'.
                DATA lv_win_x TYPE f.
                DATA lv_win_y TYPE f.
                lv_win_x = lv_screen_x - lv_bld_w / 2 + lv_wx * 5.
                lv_win_y = lv_screen_y + lv_wy * 8.

                " Yellow/warm window light
                lv_r = 255 * lv_shade * '0.3'.
                lv_g = 230 * lv_shade * '0.3'.
                lv_b = 128 * lv_shade * '0.3'.

                lv_hex_r = int_to_hex( lv_r ).
                lv_hex_g = int_to_hex( lv_g ).
                lv_hex_b = int_to_hex( lv_b ).

                CLEAR ls_rect.
                ls_rect-x = lv_win_x. ls_rect-y = lv_win_y.
                ls_rect-w = 3. ls_rect-h = 5.
                ls_rect-fill = |#{ lv_hex_r }{ lv_hex_g }{ lv_hex_b }|.
                APPEND ls_rect TO rs_frame-rects.
              ENDIF.
            ENDDO.
          ENDDO.
        ENDIF.
      ENDDO.
    ENDDO.

    " Road at bottom
    CLEAR ls_rect.
    ls_rect-x = 0. ls_rect-y = 170.
    ls_rect-w = 320. ls_rect-h = 30.
    ls_rect-fill = '#0D0D0D'.
    APPEND ls_rect TO rs_frame-rects.

    " Road lines (moving)
    DO 5 TIMES.
      lv_road_x = ( ( sy-index - 1 ) * 80 - lv_t * 100 ) MOD 400 - 40.

      " Yellow center line
      lv_r = 255 * ( '0.5' + is_ctx-gbi-pulse * '0.5' ).
      lv_g = 255 * ( '0.5' + is_ctx-gbi-pulse * '0.5' ).
      lv_b = 0.

      lv_hex_r = int_to_hex( lv_r ).
      lv_hex_g = int_to_hex( lv_g ).
      lv_hex_b = int_to_hex( lv_b ).

      CLEAR ls_rect.
      ls_rect-x = lv_road_x. ls_rect-y = 183.
      ls_rect-w = 30. ls_rect-h = 3.
      ls_rect-fill = |#{ lv_hex_r }{ lv_hex_g }{ lv_hex_b }|.
      APPEND ls_rect TO rs_frame-rects.
    ENDDO.

    " Title
    CLEAR ls_text.
    ls_text-x = 160. ls_text-y = 10.
    ls_text-text = 'VOXEL CITY'.
    ls_text-color = '#44AAFF'. ls_text-size = 10. ls_text-align = 'center'.
    APPEND ls_text TO rs_frame-texts.

    " Flash on beat 2
    IF is_ctx-gbi-beat = 2 AND is_ctx-gbi-pulse > '0.7'.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = '0.25'.
      rs_frame-flash-r = '0.3'. rs_frame-flash-g = '0.5'. rs_frame-flash-b = 1.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
