CLASS zcl_o4d_mountains_sunrise DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    CLASS-METHODS new
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_o4d_mountains_sunrise.
    METHODS constructor.

  PRIVATE SECTION.
    CONSTANTS: c_grid_w TYPE i VALUE 256,  " Width of precalculated grid
               c_layers TYPE i VALUE 4.

    TYPES: BEGIN OF ty_column,
             height TYPE f,  " Normalized height 0-1
           END OF ty_column.

    DATA mv_name TYPE string VALUE 'SUNRISE'.
    DATA mt_heights TYPE STANDARD TABLE OF ty_column WITH EMPTY KEY.  " c_grid_w * c_layers entries

    METHODS int_to_hex IMPORTING iv_int TYPE i RETURNING VALUE(rv_hex) TYPE string.
    METHODS noise IMPORTING iv_x TYPE f iv_y TYPE f RETURNING VALUE(rv_val) TYPE f.
    METHODS fbm IMPORTING iv_x TYPE f iv_y TYPE f RETURNING VALUE(rv_val) TYPE f.
    METHODS generate_terrain.
    METHODS get_height
      IMPORTING iv_x     TYPE i
                iv_layer TYPE i
      RETURNING VALUE(rv_h) TYPE f.

ENDCLASS.

CLASS zcl_o4d_mountains_sunrise IMPLEMENTATION.

  METHOD new.
    ro_obj = NEW #( ).
  ENDMETHOD.

  METHOD constructor.
    generate_terrain( ).
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

  METHOD noise.
    DATA: lv_ix TYPE i, lv_iy TYPE i, lv_hash TYPE f.
    lv_ix = floor( iv_x ).
    lv_iy = floor( iv_y ).
    lv_hash = sin( lv_ix * '127.1' + lv_iy * '311.7' ) * '43758.5453'.
    rv_val = lv_hash - floor( lv_hash ).
  ENDMETHOD.

  METHOD fbm.
    DATA: lv_f TYPE f VALUE 0, lv_amp TYPE f VALUE '0.5'.
    DATA: lv_x TYPE f, lv_y TYPE f.
    lv_x = iv_x. lv_y = iv_y.

    DO 5 TIMES.
      lv_f = lv_f + lv_amp * noise( iv_x = lv_x iv_y = lv_y ).
      lv_x = lv_x * 2.
      lv_y = lv_y * 2.
      lv_amp = lv_amp * '0.5'.
    ENDDO.

    rv_val = lv_f.
  ENDMETHOD.

  METHOD generate_terrain.
    " Precalculate mountain heights for all layers
    DATA: lv_x TYPE i, lv_layer TYPE i, lv_norm_x TYPE f.
    DATA: lv_h TYPE f, lv_mountain_x TYPE f, lv_peak_x TYPE f.
    DATA ls_col TYPE ty_column.

    CLEAR mt_heights.

    DO c_layers TIMES.
      lv_layer = sy-index - 1.

      DO c_grid_w TIMES.
        lv_x = sy-index - 1.
        lv_norm_x = CONV f( lv_x ) / c_grid_w.

        " Mountain profile calculation
        lv_mountain_x = lv_norm_x * ( 2 + lv_layer ).

        " Height from FBM - multiple octaves for detail
        lv_h = fbm( iv_x = lv_mountain_x iv_y = CONV f( lv_layer ) ) * '0.4'.
        lv_h = lv_h + fbm( iv_x = lv_mountain_x * 2 iv_y = CONV f( lv_layer ) * 2 ) * '0.2'.

        " Peak emphasis - sharper peaks
        lv_peak_x = lv_mountain_x * '0.5'.
        lv_h = lv_h + nmax( val1 = 0 val2 = '0.3' - abs( ( lv_peak_x - floor( lv_peak_x ) ) - '0.5' ) * 2 ) * '0.3'.

        " Secondary peaks
        lv_peak_x = lv_mountain_x * '0.8' + '0.3'.
        lv_h = lv_h + nmax( val1 = 0 val2 = '0.2' - abs( ( lv_peak_x - floor( lv_peak_x ) ) - '0.5' ) * 2 ) * '0.2'.

        ls_col-height = lv_h.
        APPEND ls_col TO mt_heights.
      ENDDO.
    ENDDO.
  ENDMETHOD.

  METHOD get_height.
    " Get precalculated height with wrapping
    DATA: lv_wx TYPE i, lv_idx TYPE i.

    lv_wx = iv_x MOD c_grid_w.
    IF lv_wx < 0. lv_wx = lv_wx + c_grid_w. ENDIF.

    lv_idx = iv_layer * c_grid_w + lv_wx + 1.

    IF lv_idx > 0 AND lv_idx <= lines( mt_heights ).
      rv_h = mt_heights[ lv_idx ]-height.
    ELSE.
      rv_h = '0.3'.
    ENDIF.
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
    CONSTANTS: lc_w TYPE f VALUE 640,
               lc_h TYPE f VALUE 400.

    DATA: lv_t TYPE f, lv_rise TYPE f.
    DATA: lv_sun1_x TYPE f, lv_sun1_y TYPE f, lv_sun2_x TYPE f, lv_sun2_y TYPE f.
    DATA: lv_y_pos TYPE f, lv_sky_r TYPE i, lv_sky_g TYPE i, lv_sky_b TYPE i.
    DATA: lv_glow1 TYPE f, lv_glow2 TYPE f.
    DATA: lv_hex_r TYPE string, lv_hex_g TYPE string, lv_hex_b TYPE string.
    DATA: lv_layer TYPE i, lv_x TYPE i, lv_depth TYPE f.
    DATA: lv_h TYPE f, lv_mountain_y TYPE f, lv_shade TYPE f.
    DATA: lv_r TYPE i, lv_g TYPE i, lv_b TYPE i.
    DATA: lv_dx1 TYPE f, lv_dy1 TYPE f, lv_dx2 TYPE f, lv_dy2 TYPE f.
    DATA: lv_scroll TYPE i, lv_grid_x TYPE i.
    DATA ls_rect TYPE zif_o4d_effect=>ty_rect.
    DATA ls_circle TYPE zif_o4d_effect=>ty_circle.
    DATA ls_text TYPE zif_o4d_effect=>ty_text.

    lv_t = is_ctx-t.

    " Sunrise progress (0 = below horizon, 1 = fully risen)
    lv_rise = nmin( val1 = 1 val2 = is_ctx-t / 12 ).  " Rise over 12 seconds

    " Two suns - left golden, right crimson
    lv_sun1_x = lc_w * '0.3'.
    lv_sun1_y = lc_h + 80 - lv_rise * 330.  " Rise from below

    lv_sun2_x = lc_w * '0.7'.
    lv_sun2_y = lc_h + 120 - lv_rise * 370.  " Slightly delayed

    " === SKY GRADIENT with sun glow ===
    DO 40 TIMES.
      lv_y_pos = ( sy-index - 1 ) / 40.
      DATA(lv_pixel_y) = ( sy-index - 1 ) * 10.

      " Base gradient: deep purple top -> warm horizon
      lv_sky_r = 10 + ( 220 - 10 ) * ( 1 - lv_y_pos ) * lv_rise.
      lv_sky_g = 5 + ( 120 - 5 ) * ( 1 - lv_y_pos ) * lv_rise.
      lv_sky_b = 35 + ( 80 - 35 ) * ( 1 - lv_y_pos ).

      " Sun 1 glow (golden)
      lv_dx1 = lc_w / 2 - lv_sun1_x.
      lv_dy1 = lv_pixel_y - lv_sun1_y.
      lv_glow1 = nmax( val1 = 0 val2 = 1 - sqrt( lv_dx1 * lv_dx1 + lv_dy1 * lv_dy1 ) / 350 ).
      lv_glow1 = lv_glow1 * lv_glow1 * lv_rise.

      " Sun 2 glow (crimson)
      lv_dx2 = lc_w / 2 - lv_sun2_x.
      lv_dy2 = lv_pixel_y - lv_sun2_y.
      lv_glow2 = nmax( val1 = 0 val2 = 1 - sqrt( lv_dx2 * lv_dx2 + lv_dy2 * lv_dy2 ) / 350 ).
      lv_glow2 = lv_glow2 * lv_glow2 * lv_rise.

      " Add glow to sky
      lv_sky_r = lv_sky_r + lv_glow1 * 200 + lv_glow2 * 220.
      lv_sky_g = lv_sky_g + lv_glow1 * 140 + lv_glow2 * 60.
      lv_sky_b = lv_sky_b + lv_glow1 * 40 + lv_glow2 * 90.

      lv_hex_r = int_to_hex( lv_sky_r ).
      lv_hex_g = int_to_hex( lv_sky_g ).
      lv_hex_b = int_to_hex( lv_sky_b ).

      CLEAR ls_rect.
      ls_rect-x = 0. ls_rect-y = lv_pixel_y.
      ls_rect-w = lc_w. ls_rect-h = 11.
      ls_rect-fill = |#{ lv_hex_r }{ lv_hex_g }{ lv_hex_b }|.
      APPEND ls_rect TO rs_frame-rects_back.  " Background layer
    ENDDO.

    " === SHADOW RAYS (crepuscular rays) ===
    " Dark rays from mountain peaks toward suns
    DO 32 TIMES.
      lv_x = ( sy-index - 1 ) * 20.
      lv_scroll = floor( lv_t * 8 ).  " Scroll speed for front layer
      lv_grid_x = floor( CONV f( lv_x ) / lc_w * c_grid_w ) + lv_scroll.

      " Get front mountain height
      lv_h = get_height( iv_x = lv_grid_x iv_layer = 3 ).
      DATA(lv_mtn_y) = ( '0.75' + lv_h * '0.25' ) * lc_h.

      " Ray toward sun 1
      lv_dx1 = lv_sun1_x - lv_x.
      lv_dy1 = lv_sun1_y - lv_mtn_y.
      IF lv_dy1 < -20 AND lv_sun1_y < lc_h.
        DATA(lv_ray_len) = sqrt( lv_dx1 * lv_dx1 + lv_dy1 * lv_dy1 ).
        DATA(lv_rdx) = lv_dx1 / lv_ray_len.
        DATA(lv_rdy) = lv_dy1 / lv_ray_len.

        DO 6 TIMES.
          DATA(lv_seg) = sy-index.
          DATA(lv_sx) = lv_x + lv_rdx * lv_seg * 40.
          DATA(lv_sy) = lv_mtn_y + lv_rdy * lv_seg * 40.

          IF lv_sy > 0 AND lv_sy < lv_mtn_y - 10.
            CLEAR ls_rect.
            ls_rect-x = lv_sx - 4. ls_rect-y = lv_sy.
            ls_rect-w = 8. ls_rect-h = 35.
            ls_rect-fill = '#00001030'.
            APPEND ls_rect TO rs_frame-rects_back.  " Background layer
          ENDIF.
        ENDDO.
      ENDIF.

      " Ray toward sun 2
      lv_dx2 = lv_sun2_x - lv_x.
      lv_dy2 = lv_sun2_y - lv_mtn_y.
      IF lv_dy2 < -20 AND lv_sun2_y < lc_h.
        lv_ray_len = sqrt( lv_dx2 * lv_dx2 + lv_dy2 * lv_dy2 ).
        lv_rdx = lv_dx2 / lv_ray_len.
        lv_rdy = lv_dy2 / lv_ray_len.

        DO 6 TIMES.
          lv_seg = sy-index.
          lv_sx = lv_x + lv_rdx * lv_seg * 40.
          lv_sy = lv_mtn_y + lv_rdy * lv_seg * 40.

          IF lv_sy > 0 AND lv_sy < lv_mtn_y - 10.
            CLEAR ls_rect.
            ls_rect-x = lv_sx - 4. ls_rect-y = lv_sy.
            ls_rect-w = 8. ls_rect-h = 35.
            ls_rect-fill = '#10000820'.
            APPEND ls_rect TO rs_frame-rects_back.  " Background layer
          ENDIF.
        ENDDO.
      ENDIF.
    ENDDO.

    " === SUNS (behind mountains) ===
    IF lv_sun1_y < lc_h.
      " Sun 1 - Golden
      CLEAR ls_circle.
      ls_circle-x = lv_sun1_x. ls_circle-y = lv_sun1_y.
      ls_circle-radius = 90. ls_circle-fill = '#FFAA2218'.
      APPEND ls_circle TO rs_frame-circles.

      CLEAR ls_circle.
      ls_circle-x = lv_sun1_x. ls_circle-y = lv_sun1_y.
      ls_circle-radius = 50. ls_circle-fill = '#FFCC44'.
      APPEND ls_circle TO rs_frame-circles.

      CLEAR ls_circle.
      ls_circle-x = lv_sun1_x. ls_circle-y = lv_sun1_y.
      ls_circle-radius = 28. ls_circle-fill = '#FFFFBB'.
      APPEND ls_circle TO rs_frame-circles.
    ENDIF.

    IF lv_sun2_y < lc_h.
      " Sun 2 - Crimson
      CLEAR ls_circle.
      ls_circle-x = lv_sun2_x. ls_circle-y = lv_sun2_y.
      ls_circle-radius = 80. ls_circle-fill = '#FF333318'.
      APPEND ls_circle TO rs_frame-circles.

      CLEAR ls_circle.
      ls_circle-x = lv_sun2_x. ls_circle-y = lv_sun2_y.
      ls_circle-radius = 45. ls_circle-fill = '#FF4444'.
      APPEND ls_circle TO rs_frame-circles.

      CLEAR ls_circle.
      ls_circle-x = lv_sun2_x. ls_circle-y = lv_sun2_y.
      ls_circle-radius = 22. ls_circle-fill = '#FFAAAA'.
      APPEND ls_circle TO rs_frame-circles.
    ENDIF.

    " === MOUNTAINS (back to front) ===
    DO 4 TIMES.
      lv_layer = sy-index - 1.  " 0, 1, 2, 3 (back first, front last)
      lv_depth = '0.3' + lv_layer * '0.12'.

      " Scroll speed varies by layer (parallax)
      DATA(lv_layer_speed) = ( 2 + lv_layer * 3 ).
      lv_scroll = floor( lv_t * lv_layer_speed ).

      " 64 columns to fill 640px
      DO 64 TIMES.
        lv_x = ( sy-index - 1 ) * 10.

        " Map screen x to grid with scroll offset
        lv_grid_x = floor( CONV f( lv_x ) / lc_w * c_grid_w ) + lv_scroll.

        lv_h = get_height( iv_x = lv_grid_x iv_layer = lv_layer ).
        lv_mountain_y = lv_depth + lv_h * ( '0.4' - lv_layer * '0.04' ).
        lv_mountain_y = lv_mountain_y * lc_h.

        lv_shade = 1 - lv_layer * '0.12'.

        " Silhouette colors - darker for front layers
        CASE lv_layer.
          WHEN 0.
            lv_r = 70 * lv_shade. lv_g = 40 * lv_shade. lv_b = 90 * lv_shade.
          WHEN 1.
            lv_r = 50 * lv_shade. lv_g = 30 * lv_shade. lv_b = 70 * lv_shade.
          WHEN 2.
            lv_r = 30 * lv_shade. lv_g = 22 * lv_shade. lv_b = 45 * lv_shade.
          WHEN OTHERS.
            lv_r = 12 * lv_shade. lv_g = 10 * lv_shade. lv_b = 18 * lv_shade.
        ENDCASE.

        " Rim lighting from suns
        DATA(lv_dist1) = abs( lv_x - lv_sun1_x ).
        DATA(lv_dist2) = abs( lv_x - lv_sun2_x ).
        DATA(lv_rim1) = nmax( val1 = 0 val2 = 1 - lv_dist1 / 180 ) * ( 1 - lv_layer / 5 ) * lv_rise.
        DATA(lv_rim2) = nmax( val1 = 0 val2 = 1 - lv_dist2 / 180 ) * ( 1 - lv_layer / 5 ) * lv_rise.

        lv_r = lv_r + lv_rim1 * 100 + lv_rim2 * 80.
        lv_g = lv_g + lv_rim1 * 60 + lv_rim2 * 25.
        lv_b = lv_b + lv_rim1 * 15 + lv_rim2 * 40.

        " Beat pulse
        lv_r = lv_r * ( 1 + is_ctx-bi-pulse * '0.2' ).
        lv_g = lv_g * ( 1 + is_ctx-bi-pulse * '0.15' ).
        lv_b = lv_b * ( 1 + is_ctx-bi-pulse * '0.1' ).

        lv_hex_r = int_to_hex( lv_r ).
        lv_hex_g = int_to_hex( lv_g ).
        lv_hex_b = int_to_hex( lv_b ).

        CLEAR ls_rect.
        ls_rect-x = lv_x. ls_rect-y = lv_mountain_y.
        ls_rect-w = 11. ls_rect-h = lc_h - lv_mountain_y + 1.
        ls_rect-fill = |#{ lv_hex_r }{ lv_hex_g }{ lv_hex_b }|.
        APPEND ls_rect TO rs_frame-rects.
      ENDDO.
    ENDDO.

    " Title
    CLEAR ls_text.
    ls_text-x = lc_w / 2. ls_text-y = 25.
    ls_text-text = 'TWIN SUNS'.
    ls_text-color = '#FFDD88'. ls_text-size = 20. ls_text-align = 'center'.
    APPEND ls_text TO rs_frame-texts.

    " Flash when first sun crests
    IF lv_rise > '0.25' AND lv_rise < '0.28'.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = '0.5'.
      rs_frame-flash-r = 1. rs_frame-flash-g = '0.85'. rs_frame-flash-b = '0.4'.
    ENDIF.

    " Second flash for second sun
    IF lv_rise > '0.35' AND lv_rise < '0.38'.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = '0.4'.
      rs_frame-flash-r = 1. rs_frame-flash-g = '0.5'. rs_frame-flash-b = '0.5'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

