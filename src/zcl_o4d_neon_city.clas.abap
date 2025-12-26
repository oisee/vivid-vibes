CLASS zcl_o4d_neon_city DEFINITION PUBLIC FINAL CREATE PUBLIC.
*======================================================================
* NEON CITY - Voxel-style flying over neon skyscrapers
*======================================================================
* PRECACHED building grid for smooth scrolling
* Blue/cyan neon aesthetic with window lights
*======================================================================
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    CLASS-METHODS new
      IMPORTING iv_grid_size TYPE i DEFAULT 64
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_o4d_neon_city.
  PRIVATE SECTION.
    CONSTANTS:
      c_cols TYPE i VALUE 40,
      c_rows TYPE i VALUE 25,
      c_block TYPE i VALUE 4.  " Building block spacing

    " Cached building grid
    TYPES: BEGIN OF ty_cell,
             height TYPE f,
             hue    TYPE f,
             window TYPE f,
           END OF ty_cell.
    DATA mt_grid TYPE STANDARD TABLE OF ty_cell WITH EMPTY KEY.
    DATA mv_grid_size TYPE i.

    METHODS init_city.
    METHODS get_cell IMPORTING iv_x TYPE i iv_z TYPE i RETURNING VALUE(rs_cell) TYPE ty_cell.
    METHODS hsv_to_hex
      IMPORTING iv_h TYPE f iv_s TYPE f iv_v TYPE f
      RETURNING VALUE(rv_hex) TYPE string.
ENDCLASS.



CLASS ZCL_O4D_NEON_CITY IMPLEMENTATION.


  METHOD new.
    ro_obj = NEW #( ).
    ro_obj->mv_grid_size = iv_grid_size.
    ro_obj->init_city( ).
  ENDMETHOD.


  METHOD init_city.
    " Precalculate entire city grid
    DATA: lv_x TYPE i, lv_z TYPE i, lv_idx TYPE i.
    DATA: lv_height TYPE f, lv_hue TYPE f, lv_window TYPE f.
    DATA: lv_hash TYPE f.

    CLEAR mt_grid.

    lv_z = 0.
    WHILE lv_z < mv_grid_size.
      lv_x = 0.
      WHILE lv_x < mv_grid_size.
        " Roads at block intervals
        IF lv_x MOD c_block = 0 OR lv_z MOD c_block = 0.
          lv_height = 0.  " Road
          lv_hue = 0.
          lv_window = 0.
        ELSE.
          " Hash for this building (deterministic)
          lv_hash = sin( CONV f( lv_x ) * '12.9898' + CONV f( lv_z ) * '78.233' ) * '43758.5453'.
          lv_hash = lv_hash - floor( lv_hash ).

          " Building height
          lv_height = lv_hash * lv_hash * '0.8' + '0.15'.
          IF lv_hash > '0.85'.
            lv_height = lv_height * '1.6'.  " Skyscraper
          ENDIF.

          " Color variation
          lv_hue = '0.52' + lv_hash * '0.12'.  " Cyan to blue range

          " Window brightness
          DATA(lv_hash2) = sin( CONV f( lv_x ) * '47.123' + CONV f( lv_z ) * '31.337' ) * '12345.6789'.
          lv_window = lv_hash2 - floor( lv_hash2 ).
        ENDIF.

        APPEND VALUE ty_cell( height = lv_height hue = lv_hue window = lv_window ) TO mt_grid.

        lv_x = lv_x + 1.
      ENDWHILE.
      lv_z = lv_z + 1.
    ENDWHILE.
  ENDMETHOD.


  METHOD get_cell.
    " Wrap coordinates and get cached cell
    DATA(lv_wx) = iv_x MOD mv_grid_size.
    DATA(lv_wz) = iv_z MOD mv_grid_size.
    IF lv_wx < 0. lv_wx = lv_wx + mv_grid_size. ENDIF.
    IF lv_wz < 0. lv_wz = lv_wz + mv_grid_size. ENDIF.

    DATA(lv_idx) = lv_wz * mv_grid_size + lv_wx + 1.
    IF lv_idx > 0 AND lv_idx <= lines( mt_grid ).
      rs_cell = mt_grid[ lv_idx ].
    ENDIF.
  ENDMETHOD.


  METHOD zif_o4d_effect~get_name. rv_name = 'neon_city'. ENDMETHOD.


  METHOD zif_o4d_effect~get_params. ENDMETHOD.


  METHOD zif_o4d_effect~set_param. ENDMETHOD.


  METHOD zif_o4d_effect~render.
    rs_frame = zif_o4d_effect~render_frame( VALUE #(
      t = is_sync-time gt = is_sync-time
      bi = VALUE #( pulse = is_sync-intensity bar_phase = is_sync-bar_phase )
      gbi = VALUE #( pulse = is_sync-intensity bar_phase = is_sync-bar_phase )
    ) ).
  ENDMETHOD.


  METHOD zif_o4d_effect~render_frame.
    DATA: lv_row TYPE i, lv_col TYPE i, lv_z TYPE f, lv_depth TYPE f,
          lv_scale TYPE f, lv_y_base TYPE f, lv_col_w TYPE f,
          lv_x TYPE f, lv_screen_x TYPE f, lv_screen_y TYPE f,
          lv_sat TYPE f, lv_val TYPE f, lv_color TYPE string,
          lv_rect_h TYPE f.

    DATA(lv_t) = is_ctx-gt.
    DATA(ls_bi) = is_ctx-bi.
    DATA(lv_w) = CONV f( zif_o4d_effect=>c_width ).
    DATA(lv_h) = CONV f( zif_o4d_effect=>c_height ).

    " Smooth scrolling offsets (sub-cell precision)
    DATA(lv_scroll_x) = 1. "sin( lv_t * '0.12' ) * 8.
    DATA(lv_scroll_z) = lv_t * 4.  " Forward motion

    " Integer and fractional parts for smooth scrolling
    DATA(lv_scroll_z_int) = CONV i( floor( lv_scroll_z ) ).
    DATA(lv_scroll_z_frac) = lv_scroll_z - lv_scroll_z_int.

    " Night sky gradient
    DATA: lv_sky_y TYPE f, lv_sky_val TYPE f, lv_sky_col TYPE string.
    DATA(lv_sky_h) = lv_h * '0.4'.
    DO 6 TIMES.
      lv_sky_y = ( sy-index - 1 ) * lv_sky_h / 6.
      lv_sky_val = '0.215' + CONV f( sy-index * 3 ) / 100.
      lv_sky_col = hsv_to_hex( iv_h = '0.62' iv_s = '0.9' iv_v = lv_sky_val ).
      APPEND VALUE zif_o4d_effect=>ty_rect(
        x = 0 y = lv_sky_y w = lv_w h = lv_sky_h / 6 + 1 fill = lv_sky_col
      ) TO rs_frame-rects.
    ENDDO.

    " Ground plane
    APPEND VALUE zif_o4d_effect=>ty_rect(
      x = 0 y = lv_h * '0.65' w = lv_w h = lv_h * '0.35' fill = '#080810'
    ) TO rs_frame-rects.

    " Draw buildings from back to front
    " Smooth scroll: subtract frac so rows move toward camera smoothly
    DO c_rows TIMES.
      lv_row = c_rows - sy-index.

      " Depth: subtract fractional scroll for smooth forward motion
      lv_depth = ( lv_row + 1 - lv_scroll_z_frac ) / ( c_rows + 1 ).
      IF lv_depth < '0.02'. CONTINUE. ENDIF.  " Skip rows that exited

      lv_scale = 1 / ( lv_depth * 3 + '0.25' ).
      lv_y_base = lv_h * '0.78' - lv_depth * lv_h * '0.35'.
      lv_col_w = lv_w / c_cols * lv_scale.
      IF lv_col_w < 1. lv_col_w = 1. ENDIF.

      DO c_cols TIMES.
        lv_col = sy-index - 1.
        lv_x = CONV f( lv_col ) + lv_scroll_x.

        " Get cached cell
        DATA(lv_gx) = CONV i( floor( lv_x ) ).
        DATA(lv_gz) = lv_row + lv_scroll_z_int.
        DATA(ls_cell) = get_cell( iv_x = lv_gx iv_z = lv_gz ).

        IF ls_cell-height > '0.05'.  " Skip roads
          lv_screen_x = ( CONV f( lv_col ) / c_cols - '0.5' ) * lv_w * lv_scale + lv_w / 2.
          lv_screen_y = lv_y_base - ls_cell-height * 140 * lv_scale.

          " Color from cached values
          lv_sat = '0.85'.

          " Window lights + depth fade
          IF ls_cell-window > '0.65'.
            lv_val = '0.75' + lv_depth * '0.25'.  " Bright window
          ELSE.
            "lv_val = '0.15' + lv_depth * '0.45' + ls_bi-pulse * '0.12'.
            lv_val = '0.15' + lv_depth * '0.45' + '0.5' * '0.12'.
          ENDIF.

          lv_color = hsv_to_hex( iv_h = ls_cell-hue iv_s = lv_sat iv_v = lv_val ).

          IF lv_screen_x > ( 0 - lv_col_w ) AND lv_screen_x < lv_w + lv_col_w.
            lv_rect_h = lv_y_base - lv_screen_y.
            IF lv_rect_h > 0.
              APPEND VALUE zif_o4d_effect=>ty_rect(
                x = lv_screen_x - lv_col_w / 2
                y = lv_screen_y
                w = lv_col_w + 1
                h = lv_rect_h
                fill = lv_color
              ) TO rs_frame-rects.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDDO.
    ENDDO.

*    " Horizon glow line
*    APPEND VALUE zif_o4d_effect=>ty_rect(
*      x = 0 y = lv_h * '0.4' - 2 w = lv_w h = 4
*      fill = '#002040'
*    ) TO rs_frame-rects.

    " Title
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_w / 2 y = 18 text = 'NEON CITY'
      color = '#00FFFF' size = 14 align = 'center'
    ) TO rs_frame-texts.

    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_w / 2 y = lv_h - 12
      text = |{ mv_grid_size }x{ mv_grid_size } grid|
      color = '#0088AA' size = 10 align = 'center'
    ) TO rs_frame-texts.

    " Beat flash
    "--- FLASH on beat 2 of each bar (backbeat/snare) ---
    IF is_ctx-bi-pos_16 mod 16 = 8.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = '0.5' . "is_ctx-gbi-pulse * CONV f( '0.5' ).
      rs_frame-flash-r = CONV f( '1.0' ).
      rs_frame-flash-g = CONV f( '0.8' ).
      rs_frame-flash-b = CONV f( '0.6' ).  " Warm copper flash
    ENDIF.
*    IF ls_bi-pulse > '0.6'.
*      rs_frame-flash-active = abap_true.
*      rs_frame-flash-intensity = '0.12'.
*      rs_frame-flash-r = 0. rs_frame-flash-g = '0.7'. rs_frame-flash-b = 1.
*    ENDIF.
  ENDMETHOD.


  METHOD hsv_to_hex.
    DATA: lv_r TYPE f, lv_g TYPE f, lv_b TYPE f.
    DATA(lv_hh) = iv_h * 6.
    DATA(lv_i) = floor( lv_hh ).
    DATA(lv_f) = lv_hh - lv_i.
    DATA(lv_p) = iv_v * ( 1 - iv_s ).
    DATA(lv_q) = iv_v * ( 1 - lv_f * iv_s ).
    DATA(lv_tt) = iv_v * ( 1 - ( 1 - lv_f ) * iv_s ).
    CASE lv_i MOD 6.
      WHEN 0. lv_r = iv_v. lv_g = lv_tt. lv_b = lv_p.
      WHEN 1. lv_r = lv_q. lv_g = iv_v. lv_b = lv_p.
      WHEN 2. lv_r = lv_p. lv_g = iv_v. lv_b = lv_tt.
      WHEN 3. lv_r = lv_p. lv_g = lv_q. lv_b = iv_v.
      WHEN 4. lv_r = lv_tt. lv_g = lv_p. lv_b = iv_v.
      WHEN 5. lv_r = iv_v. lv_g = lv_p. lv_b = lv_q.
    ENDCASE.
    DATA(lv_ri) = CONV i( nmin( val1 = 255 val2 = nmax( val1 = 0 val2 = lv_r * 255 ) ) ).
    DATA(lv_gi) = CONV i( nmin( val1 = 255 val2 = nmax( val1 = 0 val2 = lv_g * 255 ) ) ).
    DATA(lv_bi) = CONV i( nmin( val1 = 255 val2 = nmax( val1 = 0 val2 = lv_b * 255 ) ) ).
    rv_hex = |#{ lv_ri ALIGN = RIGHT WIDTH = 2 PAD = '0' }{ lv_gi ALIGN = RIGHT WIDTH = 2 PAD = '0' }{ lv_bi ALIGN = RIGHT WIDTH = 2 PAD = '0' }|.
  ENDMETHOD.
ENDCLASS.
