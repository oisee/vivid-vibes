CLASS zcl_o4d_cave_tunnel DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    CLASS-METHODS new
      IMPORTING iv_layers     TYPE i DEFAULT 3
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_o4d_cave_tunnel.
    METHODS constructor
      IMPORTING iv_layers TYPE i DEFAULT 3.

  PRIVATE SECTION.
    CONSTANTS: c_map_len   TYPE i VALUE 2048,
               c_map_h     TYPE i VALUE 64,
               c_depth     TYPE i VALUE 80.

    TYPES: BEGIN OF ty_slice,
             floor   TYPE i,
             ceiling TYPE i,
           END OF ty_slice.
    TYPES tt_tunnel TYPE STANDARD TABLE OF ty_slice WITH EMPTY KEY.

    DATA mv_name   TYPE string VALUE 'CAVE'.
    DATA mv_layers TYPE i.
    DATA mt_tunnel TYPE tt_tunnel.

    METHODS generate_tunnel
      IMPORTING iv_seed TYPE i
      EXPORTING et_map  TYPE tt_tunnel.
    METHODS get_slice
      IMPORTING iv_x     TYPE i
                iv_layer TYPE i
      RETURNING VALUE(rs_slice) TYPE ty_slice.
    METHODS int_to_hex
      IMPORTING iv_int TYPE i
      RETURNING VALUE(rv_hex) TYPE string.

ENDCLASS.

CLASS zcl_o4d_cave_tunnel IMPLEMENTATION.

  METHOD new.
    ro_obj = NEW #( iv_layers = iv_layers ).
  ENDMETHOD.

  METHOD constructor.
    DATA lt_layer TYPE tt_tunnel.

    mv_layers = iv_layers.
    IF mv_layers < 1. mv_layers = 1. ENDIF.
    IF mv_layers > 5. mv_layers = 5. ENDIF.

    DO mv_layers TIMES.
      generate_tunnel( EXPORTING iv_seed = sy-index * 1000
                       IMPORTING et_map  = lt_layer ).
      APPEND LINES OF lt_layer TO mt_tunnel.
    ENDDO.
  ENDMETHOD.

  METHOD generate_tunnel.
    DATA: lv_x TYPE i, lv_floor TYPE f, lv_ceil TYPE f.
    DATA: lv_floor_vel TYPE f, lv_ceil_vel TYPE f.
    DATA: lv_noise TYPE f, lv_hash TYPE f, lv_mid TYPE f.
    DATA ls_slice TYPE ty_slice.

    CLEAR et_map.

    lv_floor = 10. lv_ceil = 54.
    lv_floor_vel = 0. lv_ceil_vel = 0.

    DO c_map_len TIMES.
      lv_x = sy-index - 1.

      lv_hash = sin( ( lv_x + iv_seed ) * '0.1' ) * '43758.5453'.
      lv_noise = lv_hash - floor( lv_hash ).

      lv_floor_vel = lv_floor_vel * '0.95' + ( lv_noise - '0.5' ) * '0.8'.
      lv_floor = lv_floor + lv_floor_vel.

      lv_hash = sin( ( lv_x + iv_seed + 500 ) * '0.13' ) * '43758.5453'.
      lv_noise = lv_hash - floor( lv_hash ).

      lv_ceil_vel = lv_ceil_vel * '0.95' + ( lv_noise - '0.5' ) * '0.8'.
      lv_ceil = lv_ceil + lv_ceil_vel.

      lv_hash = sin( ( lv_x + iv_seed ) * '0.02' ) * '43758.5453'.
      lv_noise = lv_hash - floor( lv_hash ).
      IF lv_noise > '0.85'.
        lv_floor = lv_floor - 3. lv_ceil = lv_ceil + 3.
      ELSEIF lv_noise < '0.15'.
        lv_floor = lv_floor + 2. lv_ceil = lv_ceil - 2.
      ENDIF.

      IF lv_floor < 5. lv_floor = 5. lv_floor_vel = abs( lv_floor_vel ). ENDIF.
      IF lv_floor > 30. lv_floor = 30. lv_floor_vel = -1 * abs( lv_floor_vel ). ENDIF.
      IF lv_ceil > 59. lv_ceil = 59. lv_ceil_vel = -1 * abs( lv_ceil_vel ). ENDIF.
      IF lv_ceil < 34. lv_ceil = 34. lv_ceil_vel = abs( lv_ceil_vel ). ENDIF.

      IF lv_ceil - lv_floor < 12.
        lv_mid = ( lv_floor + lv_ceil ) / 2.
        lv_floor = lv_mid - 6. lv_ceil = lv_mid + 6.
      ENDIF.

      ls_slice-floor = floor( lv_floor ).
      ls_slice-ceiling = floor( lv_ceil ).
      APPEND ls_slice TO et_map.
    ENDDO.
  ENDMETHOD.

  METHOD get_slice.
    DATA: lv_wx TYPE i, lv_idx TYPE i.

    lv_wx = iv_x MOD c_map_len.
    IF lv_wx < 0. lv_wx = lv_wx + c_map_len. ENDIF.

    lv_idx = iv_layer * c_map_len + lv_wx + 1.

    IF lv_idx > 0 AND lv_idx <= lines( mt_tunnel ).
      rs_slice = mt_tunnel[ lv_idx ].
    ELSE.
      rs_slice-floor = 20. rs_slice-ceiling = 44.
    ENDIF.
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
    ls_ctx-bi-pulse = is_sync-intensity.
    rs_frame = zif_o4d_effect~render_frame( ls_ctx ).
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    CONSTANTS: lc_w TYPE i VALUE 640, lc_h TYPE i VALUE 400.

    DATA: lv_t TYPE f, lv_cam_x TYPE f, lv_d TYPE i, lv_map_x TYPE i.
    DATA: lv_fog TYPE f, lv_depth_ratio TYPE f, lv_half_w TYPE f.
    DATA: lv_screen_y_floor TYPE i, lv_screen_y_ceil TYPE i.
    DATA: lv_x1 TYPE i, lv_x2 TYPE i, lv_layer TYPE i, lv_center_y TYPE i.
    DATA: lv_r TYPE i, lv_g TYPE i, lv_b TYPE i.
    DATA: lv_base_r TYPE i, lv_base_g TYPE i, lv_base_b TYPE i.
    DATA: lv_hex_r TYPE string, lv_hex_g TYPE string, lv_hex_b TYPE string.
    DATA: lv_layer_center TYPE i, lv_parallax TYPE f, lv_layer_cam_x TYPE f.
    DATA: lv_tunnel_half TYPE f, lv_vert_scale TYPE f, lv_color TYPE string.
    DATA ls_line TYPE zif_o4d_effect=>ty_line.
    DATA ls_rect TYPE zif_o4d_effect=>ty_rect.
    DATA ls_text TYPE zif_o4d_effect=>ty_text.
    DATA ls_slice TYPE ty_slice.

    lv_t = is_ctx-t.
    lv_cam_x = lv_t * 40.

    CLEAR ls_rect.
    ls_rect-x = 0. ls_rect-y = 0. ls_rect-w = lc_w. ls_rect-h = lc_h.
    ls_rect-fill = '#0a0810'.
    APPEND ls_rect TO rs_frame-rects.

    lv_center_y = lc_h / 2.

    DO mv_layers TIMES.
      lv_layer = sy-index - 1.

      lv_layer_center = ( lv_layer - 1 ) * 220.
      lv_parallax = 1 + lv_layer * '0.25'.
      lv_layer_cam_x = lv_cam_x * lv_parallax.

      CASE lv_layer.
        WHEN 0. lv_base_r = 130. lv_base_g = 75. lv_base_b = 45.
        WHEN 1. lv_base_r = 55. lv_base_g = 90. lv_base_b = 140.
        WHEN 2. lv_base_r = 45. lv_base_g = 120. lv_base_b = 75.
        WHEN OTHERS. lv_base_r = 100. lv_base_g = 100. lv_base_b = 110.
      ENDCASE.

      DO c_depth TIMES.
        lv_d = c_depth - sy-index + 1.
        lv_map_x = floor( lv_layer_cam_x ) + lv_d.

        ls_slice = get_slice( iv_x = lv_map_x iv_layer = lv_layer ).

        lv_depth_ratio = CONV f( c_depth - lv_d ) / c_depth.
        lv_fog = 1 - lv_depth_ratio.
        lv_fog = lv_fog * lv_fog.

        lv_half_w = 15 + lv_depth_ratio * 200.
        lv_x1 = lc_w / 2 + lv_layer_center - lv_half_w.
        lv_x2 = lc_w / 2 + lv_layer_center + lv_half_w.

        IF lv_x2 < 0 OR lv_x1 > lc_w. CONTINUE. ENDIF.

        lv_tunnel_half = ( ls_slice-ceiling - ls_slice-floor ) / 2.
        lv_vert_scale = 2 + lv_depth_ratio * 6.

        lv_screen_y_floor = lv_center_y + lv_tunnel_half * lv_vert_scale.
        lv_screen_y_ceil = lv_center_y - lv_tunnel_half * lv_vert_scale.

        IF lv_screen_y_floor > lc_h. lv_screen_y_floor = lc_h. ENDIF.
        IF lv_screen_y_ceil < 0. lv_screen_y_ceil = 0. ENDIF.

        lv_r = lv_base_r * ( 1 - lv_fog * '0.85' ).
        lv_g = lv_base_g * ( 1 - lv_fog * '0.85' ).
        lv_b = lv_base_b * ( 1 - lv_fog * '0.75' ).

        lv_r = lv_r * ( 1 + is_ctx-bi-pulse * '0.5' ).
        lv_g = lv_g * ( 1 + is_ctx-bi-pulse * '0.3' ).
        lv_b = lv_b * ( 1 + is_ctx-bi-pulse * '0.4' ).

        lv_hex_r = int_to_hex( lv_r ).
        lv_hex_g = int_to_hex( lv_g ).
        lv_hex_b = int_to_hex( lv_b ).

        lv_color = |#{ lv_hex_r }{ lv_hex_g }{ lv_hex_b }|.

        CLEAR ls_line.
        ls_line-x1 = lv_x1. ls_line-y1 = lv_screen_y_floor.
        ls_line-x2 = lv_x2. ls_line-y2 = lv_screen_y_floor.
        ls_line-color = lv_color. ls_line-width = 2 + lv_depth_ratio * 4.
        APPEND ls_line TO rs_frame-lines.

        CLEAR ls_line.
        ls_line-x1 = lv_x1. ls_line-y1 = lv_screen_y_ceil.
        ls_line-x2 = lv_x2. ls_line-y2 = lv_screen_y_ceil.
        ls_line-color = lv_color. ls_line-width = 2 + lv_depth_ratio * 4.
        APPEND ls_line TO rs_frame-lines.
      ENDDO.
    ENDDO.

    CLEAR ls_text.
    ls_text-x = lc_w / 2. ls_text-y = 25.
    ls_text-text = 'CAVE RUNNER'.
    ls_text-color = '#88AACC'. ls_text-size = 18. ls_text-align = 'center'.
    APPEND ls_text TO rs_frame-texts.

    rs_frame-state-line_cap = 'round'.
    rs_frame-state-line_join = 'round'.
  ENDMETHOD.

ENDCLASS.

