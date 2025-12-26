CLASS zcl_o4d_amigaball DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_tri_z, z TYPE f, tri TYPE zif_o4d_effect=>ty_triangle, END OF ty_tri_z.
    CLASS-METHODS i2h IMPORTING iv_i TYPE i RETURNING VALUE(rv) TYPE string.
ENDCLASS.



CLASS ZCL_O4D_AMIGABALL IMPLEMENTATION.


  METHOD zif_o4d_effect~get_name. rv_name = 'amiga_ball'. ENDMETHOD.


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
    DATA(lv_pi) = zif_o4d_effect=>c_pi.

    " Sky gradient
    DATA lv_y TYPE i.
    lv_y = 0.
    WHILE lv_y < lv_h.
      DATA(lv_sky_b) = 80 + CONV i( CONV f( lv_y ) / lv_h * 120 ).
      APPEND VALUE zif_o4d_effect=>ty_line(
        x1    = 0 y1 = lv_y x2 = lv_w y2 = lv_y width = 1
        color = |#0050{ i2h( lv_sky_b ) }|
      ) TO rs_frame-lines.
      lv_y = lv_y + 4.
    ENDWHILE.

    " Ball parameters
    DATA(lv_r) = CONV f( 70 ).
    DATA(lv_cx) = lv_w / 2 + sin( lv_t * CONV f( '1.5' ) ) * 120.
    DATA(lv_bounce) = abs( sin( lv_t * 3 ) ) * 100.
    DATA(lv_cy) = lv_h - lv_r - 30 - lv_bounce.

    " Squash effect
    DATA(lv_sq_y) = COND f( WHEN lv_bounce < 20 THEN CONV f( '0.7' ) + CONV f( '0.3' ) * lv_bounce / 20 ELSE 1 ).
    DATA(lv_sq_x) = 1 + ( 1 - lv_sq_y ) * CONV f( '0.5' ).

    " Shadow ellipse
    DATA(lv_sr) = lv_r * ( CONV f( '0.6' ) + CONV f( '0.4' ) * ( 1 - lv_bounce / 120 ) ).
    DO 16 TIMES.
      DATA(lv_seg) = sy-index - 1.
      DATA(lv_a1) = CONV f( lv_seg ) / 16 * lv_pi * 2.
      DATA(lv_a2) = CONV f( lv_seg + 1 ) / 16 * lv_pi * 2.
      APPEND VALUE zif_o4d_effect=>ty_line(
        x1    = lv_cx + cos( lv_a1 ) * lv_sr y1    = lv_h - 15 + sin( lv_a1 ) * lv_sr / 4
        x2    = lv_cx + cos( lv_a2 ) * lv_sr y2    = lv_h - 15 + sin( lv_a2 ) * lv_sr / 4
        width = 4                            color = '#002244' ) TO rs_frame-lines.
    ENDDO.

    " Rotation
    DATA(lv_rot) = lv_t * 2.
    DATA(lv_cos_rot) = cos( lv_rot ).
    DATA(lv_sin_rot) = sin( lv_rot ).

    " Sphere mesh: 8 lat x 16 lon
    DATA(lv_lat_n) = 8.
    DATA(lv_lon_n) = 16.

    DATA lv_lat TYPE i.
    DATA lv_lon TYPE i.
    lv_lat = 0.
    WHILE lv_lat < lv_lat_n.
      lv_lon = 0.
      WHILE lv_lon < lv_lon_n.
        " Latitude angles (poles at -pi/2 and +pi/2)
        DATA(lv_phi1) = ( CONV f( lv_lat ) / lv_lat_n - CONV f( '0.5' ) ) * lv_pi.
        DATA(lv_phi2) = ( CONV f( lv_lat + 1 ) / lv_lat_n - CONV f( '0.5' ) ) * lv_pi.
        " Longitude angles
        DATA(lv_th1) = CONV f( lv_lon ) / lv_lon_n * lv_pi * 2.
        DATA(lv_th2) = CONV f( lv_lon + 1 ) / lv_lon_n * lv_pi * 2.

        " 4 corners (unrotated)
        DATA(lv_x00) = cos( lv_phi1 ) * cos( lv_th1 ). DATA(lv_y00) = sin( lv_phi1 ).
        DATA(lv_z00) = cos( lv_phi1 ) * sin( lv_th1 ).
        DATA(lv_x10) = cos( lv_phi1 ) * cos( lv_th2 ). DATA(lv_y10) = sin( lv_phi1 ).
        DATA(lv_z10) = cos( lv_phi1 ) * sin( lv_th2 ).
        DATA(lv_x11) = cos( lv_phi2 ) * cos( lv_th2 ). DATA(lv_y11) = sin( lv_phi2 ).
        DATA(lv_z11) = cos( lv_phi2 ) * sin( lv_th2 ).
        DATA(lv_x01) = cos( lv_phi2 ) * cos( lv_th1 ). DATA(lv_y01) = sin( lv_phi2 ).
        DATA(lv_z01) = cos( lv_phi2 ) * sin( lv_th1 ).

        " Rotate around Y axis
        DATA(lv_rx00) = lv_x00 * lv_cos_rot - lv_z00 * lv_sin_rot. DATA(lv_rz00) = lv_x00 * lv_sin_rot + lv_z00 * lv_cos_rot.
        DATA(lv_rx10) = lv_x10 * lv_cos_rot - lv_z10 * lv_sin_rot. DATA(lv_rz10) = lv_x10 * lv_sin_rot + lv_z10 * lv_cos_rot.
        DATA(lv_rx11) = lv_x11 * lv_cos_rot - lv_z11 * lv_sin_rot. DATA(lv_rz11) = lv_x11 * lv_sin_rot + lv_z11 * lv_cos_rot.
        DATA(lv_rx01) = lv_x01 * lv_cos_rot - lv_z01 * lv_sin_rot. DATA(lv_rz01) = lv_x01 * lv_sin_rot + lv_z01 * lv_cos_rot.

        " Average Z for backface culling
        DATA(lv_z_avg) = ( lv_rz00 + lv_rz10 + lv_rz11 + lv_rz01 ) / 4.

        IF lv_z_avg > CONV f( '-0.2' ).  " Show front faces
          " Checkerboard color
          DATA(lv_chk) = ( lv_lat + lv_lon ) MOD 2.
          DATA(lv_light) = CONV f( '0.4' ) + CONV f( '0.6' ) * ( lv_z_avg + 1 ) / 2.
          DATA lv_color TYPE string.
          IF lv_chk = 0.
            " Red
            DATA(lv_rv) = i2h( CONV i( 220 * lv_light ) ).
            lv_color = |#{ lv_rv }1818|.
          ELSE.
            " White
            DATA(lv_wv) = i2h( CONV i( 240 * lv_light ) ).
            lv_color = |#{ lv_wv }{ lv_wv }{ lv_wv }|.
          ENDIF.

          " Project to screen with squash
          DATA(lv_sx0) = lv_cx + lv_rx00 * lv_r * lv_sq_x. DATA(lv_sy0) = lv_cy + lv_y00 * lv_r * lv_sq_y.
          DATA(lv_sx1) = lv_cx + lv_rx10 * lv_r * lv_sq_x. DATA(lv_sy1) = lv_cy + lv_y10 * lv_r * lv_sq_y.
          DATA(lv_sx2) = lv_cx + lv_rx11 * lv_r * lv_sq_x. DATA(lv_sy2) = lv_cy + lv_y11 * lv_r * lv_sq_y.
          DATA(lv_sx3) = lv_cx + lv_rx01 * lv_r * lv_sq_x. DATA(lv_sy3) = lv_cy + lv_y01 * lv_r * lv_sq_y.

          " Two triangles per quad
          APPEND VALUE ty_tri_z( z = lv_z_avg tri = VALUE #(
                                                             x1 = lv_sx0 y1 = lv_sy0 x2 = lv_sx1 y2 = lv_sy1 x3 = lv_sx2 y3 = lv_sy2 fill = lv_color )
          ) TO lt_tris.
          APPEND VALUE ty_tri_z( z = lv_z_avg tri = VALUE #(
                                                             x1 = lv_sx0 y1 = lv_sy0 x2 = lv_sx2 y2 = lv_sy2 x3 = lv_sx3 y3 = lv_sy3 fill = lv_color )
          ) TO lt_tris.
        ENDIF.

        lv_lon = lv_lon + 1.
      ENDWHILE.
      lv_lat = lv_lat + 1.
    ENDWHILE.

    " Sort back to front
    SORT lt_tris BY z ASCENDING.

    " Add triangles
    LOOP AT lt_tris INTO DATA(ls_t).
      APPEND ls_t-tri TO rs_frame-triangles.
    ENDLOOP.

    " Title
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_w / 2 y = 25 text = 'AMIGA BOING BALL' color = '#ffffff' size = 14 align = 'center'
    ) TO rs_frame-texts.

    rs_frame-debug-vars = |\{"x":{ lv_cx }, "y":{ lv_cy }\}|.
  ENDMETHOD.


  METHOD i2h.
    DATA lv_hex TYPE x LENGTH 1.
    DATA(lv_v) = nmax( val1 = 0 val2 = nmin( val1 = 255 val2 = iv_i ) ).
    lv_hex = lv_v. rv = |{ lv_hex }|. TRANSLATE rv TO LOWER CASE.
  ENDMETHOD.
ENDCLASS.
