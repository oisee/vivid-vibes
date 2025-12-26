CLASS zcl_o4d_lowpoly DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_tri_z, z TYPE f, tri TYPE zif_o4d_effect=>ty_triangle, END OF ty_tri_z.
    CLASS-METHODS:
      get_height IMPORTING iv_x TYPE f iv_z TYPE f RETURNING VALUE(rv_h) TYPE f,
      hsv_hex IMPORTING iv_h TYPE f iv_s TYPE f iv_v TYPE f RETURNING VALUE(rv) TYPE string,
      i2h IMPORTING iv_i TYPE i RETURNING VALUE(rv) TYPE string.
ENDCLASS.

CLASS zcl_o4d_lowpoly IMPLEMENTATION.
  METHOD zif_o4d_effect~get_name. rv_name = 'lowpoly'. ENDMETHOD.
  METHOD zif_o4d_effect~get_params. ENDMETHOD.
  METHOD zif_o4d_effect~set_param. ENDMETHOD.
  METHOD zif_o4d_effect~render.
    rs_frame = zif_o4d_effect~render_frame( VALUE #(
      t = is_sync-time gt = is_sync-time bi = VALUE #( pulse = is_sync-intensity ) ) ).
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    TYPES tt_triz TYPE STANDARD TABLE OF ty_tri_z WITH EMPTY KEY.
    DATA lt_tris TYPE tt_triz.

    DATA(lv_t) = is_ctx-t.
    DATA(lv_w) = CONV f( zif_o4d_effect=>c_width ).
    DATA(lv_h) = CONV f( zif_o4d_effect=>c_height ).
    DATA(lv_horizon) = lv_h / 3.
    DATA(lv_cam_y) = CONV f( 15 ).

    " Grid size
    DATA(lv_grid) = 10.

    " Draw terrain from back to front
    DATA lv_gz TYPE i.
    DATA lv_gx TYPE i.
    lv_gz = lv_grid.
    WHILE lv_gz > 0.
      lv_gx = 0.
      WHILE lv_gx < lv_grid.
        " World coordinates
        DATA(lv_wx) = ( CONV f( lv_gx ) - CONV f( lv_grid ) / 2 ) * 30.
        DATA(lv_wz) = CONV f( lv_gz ) * 25 + 20.

        " Get 4 corners with heights
        DATA(lv_hx0) = CONV f( lv_gx ) + lv_t * 2.
        DATA(lv_hz0) = CONV f( lv_gz ).
        DATA(lv_h00) = get_height( iv_x = lv_hx0 iv_z = lv_hz0 ) * 30 - lv_cam_y.
        DATA(lv_h10) = get_height( iv_x = lv_hx0 + 1 iv_z = lv_hz0 ) * 30 - lv_cam_y.
        DATA(lv_h11) = get_height( iv_x = lv_hx0 + 1 iv_z = lv_hz0 + 1 ) * 30 - lv_cam_y.
        DATA(lv_h01) = get_height( iv_x = lv_hx0 iv_z = lv_hz0 + 1 ) * 30 - lv_cam_y.

        " Project 4 corners to screen
        DATA(lv_zd0) = nmax( val1 = CONV f( 10 ) val2 = lv_wz ).
        DATA(lv_zd1) = nmax( val1 = CONV f( 10 ) val2 = lv_wz + 25 ).

        DATA(lv_sx0) = lv_w / 2 + lv_wx * 150 / lv_zd0.
        DATA(lv_sy0) = lv_horizon + lv_h00 * 150 / lv_zd0.
        DATA(lv_sx1) = lv_w / 2 + ( lv_wx + 30 ) * 150 / lv_zd0.
        DATA(lv_sy1) = lv_horizon + lv_h10 * 150 / lv_zd0.
        DATA(lv_sx2) = lv_w / 2 + ( lv_wx + 30 ) * 150 / lv_zd1.
        DATA(lv_sy2) = lv_horizon + lv_h11 * 150 / lv_zd1.
        DATA(lv_sx3) = lv_w / 2 + lv_wx * 150 / lv_zd1.
        DATA(lv_sy3) = lv_horizon + lv_h01 * 150 / lv_zd1.

        " Color based on depth
        DATA(lv_depth) = 1 - CONV f( lv_gz ) / lv_grid.
        DATA(lv_hue) = 90 + lv_depth * 40.
        DATA(lv_sat) = CONV f( '0.6' ) + lv_depth * CONV f( '0.2' ).
        DATA(lv_val) = CONV f( '0.25' ) + lv_depth * CONV f( '0.35' ).
        DATA(lv_color) = hsv_hex( iv_h = lv_hue iv_s = lv_sat iv_v = lv_val ).

        " Two triangles per quad
        DATA(lv_zavg) = ( lv_zd0 + lv_zd1 ) / 2.
        APPEND VALUE ty_tri_z( z = lv_zavg tri = VALUE #(
          x1 = lv_sx0 y1 = lv_sy0 x2 = lv_sx1 y2 = lv_sy1 x3 = lv_sx2 y3 = lv_sy2 fill = lv_color )
        ) TO lt_tris.
        APPEND VALUE ty_tri_z( z = lv_zavg tri = VALUE #(
          x1 = lv_sx0 y1 = lv_sy0 x2 = lv_sx2 y2 = lv_sy2 x3 = lv_sx3 y3 = lv_sy3 fill = lv_color )
        ) TO lt_tris.

        lv_gx = lv_gx + 1.
      ENDWHILE.
      lv_gz = lv_gz - 1.
    ENDWHILE.

    " Sort back to front (higher z = further)
    SORT lt_tris BY z DESCENDING.

    " Add triangles
    LOOP AT lt_tris INTO DATA(ls_t).
      APPEND ls_t-tri TO rs_frame-triangles.
    ENDLOOP.

    " Sky gradient lines
    DATA lv_sky_y TYPE i.
    lv_sky_y = 0.
    WHILE lv_sky_y < lv_horizon.
      DATA(lv_sky_p) = CONV f( lv_sky_y ) / lv_horizon.
      DATA(lv_sky_r) = CONV i( 100 + lv_sky_p * 100 ).
      DATA(lv_sky_g) = CONV i( 150 + lv_sky_p * 70 ).
      DATA(lv_sky_b) = CONV i( 255 - lv_sky_p * 75 ).
      DATA(lv_sky_c) = |#{ i2h( lv_sky_r ) }{ i2h( lv_sky_g ) }{ i2h( lv_sky_b ) }|.
      APPEND VALUE zif_o4d_effect=>ty_line(
        x1 = 0 y1 = lv_sky_y x2 = lv_w y2 = lv_sky_y width = 1 color = lv_sky_c
      ) TO rs_frame-lines.
      lv_sky_y = lv_sky_y + 4.
    ENDWHILE.

    " Title
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_w / 2 y = 20 text = 'LOW-POLY TERRAIN' color = '#ffffff' size = 12 align = 'center'
    ) TO rs_frame-texts.
  ENDMETHOD.

  METHOD get_height.
    DATA(lv_nx) = CONV f( CONV i( iv_x ) MOD 17 ).
    DATA(lv_nz) = CONV f( CONV i( iv_z ) MOD 17 ).
    rv_h = sin( lv_nx * CONV f( '0.7' ) ) * CONV f( '0.5' ) +
           cos( lv_nz * CONV f( '0.5' ) ) * CONV f( '0.3' ) +
           sin( ( lv_nx + lv_nz ) * CONV f( '0.3' ) ) * CONV f( '0.2' ).
    rv_h = ( rv_h + 1 ) / 2.
  ENDMETHOD.

  METHOD hsv_hex.
    DATA: lv_c TYPE f, lv_x TYPE f, lv_m TYPE f, lv_r TYPE f, lv_g TYPE f, lv_b TYPE f.
    DATA(lv_h) = CONV f( CONV i( iv_h ) MOD 360 ). lv_c = iv_v * iv_s.
    lv_x = lv_c * ( 1 - abs( frac( lv_h / 120 ) * 2 - 1 ) ). lv_m = iv_v - lv_c.
    IF lv_h < 60. lv_r = lv_c. lv_g = lv_x. lv_b = 0.
    ELSEIF lv_h < 120. lv_r = lv_x. lv_g = lv_c. lv_b = 0.
    ELSEIF lv_h < 180. lv_r = 0. lv_g = lv_c. lv_b = lv_x.
    ELSEIF lv_h < 240. lv_r = 0. lv_g = lv_x. lv_b = lv_c.
    ELSEIF lv_h < 300. lv_r = lv_x. lv_g = 0. lv_b = lv_c.
    ELSE. lv_r = lv_c. lv_g = 0. lv_b = lv_x. ENDIF.
    rv = |#{ i2h( CONV i( ( lv_r + lv_m ) * 255 ) ) }{ i2h( CONV i( ( lv_g + lv_m ) * 255 ) ) }{ i2h( CONV i( ( lv_b + lv_m ) * 255 ) ) }|.
  ENDMETHOD.

  METHOD i2h.
    DATA lv_hex TYPE x LENGTH 1.
    DATA(lv_v) = nmax( val1 = 0 val2 = nmin( val1 = 255 val2 = iv_i ) ).
    lv_hex = lv_v. rv = |{ lv_hex }|. TRANSLATE rv TO LOWER CASE.
  ENDMETHOD.
ENDCLASS.

