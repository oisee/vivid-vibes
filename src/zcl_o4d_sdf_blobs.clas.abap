CLASS zcl_o4d_sdf_blobs DEFINITION PUBLIC FINAL CREATE PUBLIC.
*======================================================================
* SDF BLOBS 3D - Ray Marching + Phong Lighting
*======================================================================
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    CLASS-METHODS new
      IMPORTING iv_num_blobs   TYPE i DEFAULT 5
                iv_resolution  TYPE i DEFAULT 72
                iv_smoothness  TYPE f DEFAULT 50
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_o4d_sdf_blobs.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_blob3d,
             x TYPE f, y TYPE f, z TYPE f, r TYPE f,
           END OF ty_blob3d.
    TYPES tt_blobs TYPE STANDARD TABLE OF ty_blob3d WITH EMPTY KEY.
    TYPES: BEGIN OF ty_v3, x TYPE f, y TYPE f, z TYPE f, END OF ty_v3.

    DATA mv_num_blobs TYPE i.
    DATA mv_resolution TYPE i.
    DATA mv_smoothness TYPE f.

    METHODS smooth_min
      IMPORTING iv_a TYPE f iv_b TYPE f iv_k TYPE f
      RETURNING VALUE(rv_min) TYPE f.

    METHODS scene_sdf
      IMPORTING iv_x TYPE f iv_y TYPE f iv_z TYPE f
                it_blobs TYPE tt_blobs
      RETURNING VALUE(rv_dist) TYPE f.

    METHODS calc_normal
      IMPORTING iv_x TYPE f iv_y TYPE f iv_z TYPE f
                it_blobs TYPE tt_blobs
      RETURNING VALUE(rs_n) TYPE ty_v3.
ENDCLASS.

CLASS zcl_o4d_sdf_blobs IMPLEMENTATION.

  METHOD new.
    ro_obj = NEW #( ).
    ro_obj->mv_num_blobs = iv_num_blobs.
    ro_obj->mv_resolution = iv_resolution.
    ro_obj->mv_smoothness = iv_smoothness.
  ENDMETHOD.

  METHOD smooth_min.
    DATA(lv_h) = '0.5' + '0.5' * ( iv_a - iv_b ) / iv_k.
    IF lv_h < 0. lv_h = 0. ENDIF.
    IF lv_h > 1. lv_h = 1. ENDIF.
    rv_min = iv_a * ( 1 - lv_h ) + iv_b * lv_h - iv_k * lv_h * ( 1 - lv_h ).
  ENDMETHOD.

  METHOD scene_sdf.
    rv_dist = 9999.
    LOOP AT it_blobs INTO DATA(ls_b).
      DATA(lv_dx) = iv_x - ls_b-x.
      DATA(lv_dy) = iv_y - ls_b-y.
      DATA(lv_dz) = iv_z - ls_b-z.
      DATA(lv_dist) = sqrt( lv_dx * lv_dx + lv_dy * lv_dy + lv_dz * lv_dz ) - ls_b-r.
      rv_dist = smooth_min( iv_a = rv_dist iv_b = lv_dist iv_k = mv_smoothness ).
    ENDLOOP.
  ENDMETHOD.

  METHOD calc_normal.
    " Gradient of SDF = surface normal
    DATA(lv_eps) = CONV f( '0.5' ).

    DATA(lv_dx) = scene_sdf( iv_x = iv_x + lv_eps iv_y = iv_y iv_z = iv_z it_blobs = it_blobs )
                - scene_sdf( iv_x = iv_x - lv_eps iv_y = iv_y iv_z = iv_z it_blobs = it_blobs ).
    DATA(lv_dy) = scene_sdf( iv_x = iv_x iv_y = iv_y + lv_eps iv_z = iv_z it_blobs = it_blobs )
                - scene_sdf( iv_x = iv_x iv_y = iv_y - lv_eps iv_z = iv_z it_blobs = it_blobs ).
    DATA(lv_dz) = scene_sdf( iv_x = iv_x iv_y = iv_y iv_z = iv_z + lv_eps it_blobs = it_blobs )
                - scene_sdf( iv_x = iv_x iv_y = iv_y iv_z = iv_z - lv_eps it_blobs = it_blobs ).

    " Normalize
    DATA(lv_len) = sqrt( lv_dx * lv_dx + lv_dy * lv_dy + lv_dz * lv_dz ).
    IF lv_len < '0.001'. lv_len = '0.001'. ENDIF.

    rs_n-x = lv_dx / lv_len.
    rs_n-y = lv_dy / lv_len.
    rs_n-z = lv_dz / lv_len.
  ENDMETHOD.

  METHOD zif_o4d_effect~get_name. rv_name = 'sdf_blobs'. ENDMETHOD.
  METHOD zif_o4d_effect~get_params. ENDMETHOD.
  METHOD zif_o4d_effect~set_param. ENDMETHOD.

  METHOD zif_o4d_effect~render.
    rs_frame = zif_o4d_effect~render_frame( VALUE #(
      t = is_sync-time gt = is_sync-time
      gbi = VALUE #( bar = is_sync-bar bar_phase = is_sync-bar_phase pulse = is_sync-intensity )
    ) ).
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    CONSTANTS: lc_w TYPE f VALUE 640, lc_h TYPE f VALUE 400.

    DATA(lv_t) = is_ctx-gt.

    " Full screen grid
    DATA(lv_aspect) = lc_h / lc_w.
    DATA(lv_res_y) = CONV i( mv_resolution * lv_aspect ).
    DATA(lv_pw) = lc_w / mv_resolution.
    DATA(lv_ph) = lc_h / lv_res_y.

    " Animate blobs - bigger amplitude, rotating constellation
    DATA lt_blobs TYPE tt_blobs.
    DATA lv_global_rot TYPE f.
    DATA lv_blob_x TYPE f.
    DATA lv_blob_y TYPE f.
    DATA lv_blob_z TYPE f.
    DATA lv_rotated_x TYPE f.
    DATA lv_rotated_z TYPE f.
    DATA lv_orbit_r TYPE f.

    lv_global_rot = lv_t * CONV f( '0.3' ).

    DO mv_num_blobs TIMES.
      DATA(lv_i) = sy-index.
      DATA(lv_phase) = CONV f( lv_i ) * CONV f( '1.256' ).
      lv_orbit_r = 70 + sin( lv_t * CONV f( '0.5' ) + lv_i ) * 40.

      " Base position with larger movement
      lv_blob_x = cos( lv_t * CONV f( '0.7' ) + lv_phase ) * lv_orbit_r.
      lv_blob_y = sin( lv_t * CONV f( '0.6' ) + lv_phase ) * lv_orbit_r * CONV f( '0.8' ).
      lv_blob_z = 50 + cos( lv_t * CONV f( '0.4' ) + lv_i * 2 ) * 80.

      " Rotate entire constellation around Y axis
      lv_rotated_x = lv_blob_x * cos( lv_global_rot ) + lv_blob_z * sin( lv_global_rot ).
      lv_rotated_z = - lv_blob_x * sin( lv_global_rot ) + lv_blob_z * cos( lv_global_rot ).

      APPEND VALUE ty_blob3d(
        x = lv_rotated_x
        y = lv_blob_y
        z = lv_rotated_z
        r = 45 + sin( lv_t * CONV f( '1.5' ) + lv_i ) * 15
      ) TO lt_blobs.
    ENDDO.

    " Light direction (top-right-front)
    DATA(lv_light_x) = CONV f( '0.5' ).
    DATA(lv_light_y) = CONV f( '-0.7' ).
    DATA(lv_light_z) = CONV f( '-0.5' ).
    DATA(lv_light_len) = sqrt( lv_light_x ** 2 + lv_light_y ** 2 + lv_light_z ** 2 ).
    lv_light_x = lv_light_x / lv_light_len.
    lv_light_y = lv_light_y / lv_light_len.
    lv_light_z = lv_light_z / lv_light_len.

    " Camera - closer
    DATA(lv_cam_z) = CONV f( -180 ).
    DATA(lv_focal) = CONV f( 250 ).

    DATA lv_py TYPE i.
    WHILE lv_py < lv_res_y.
      DATA lv_px TYPE i.
      lv_px = 0.

      WHILE lv_px < mv_resolution.
        " Ray direction
        DATA(lv_ray_x) = ( lv_px * lv_pw - lc_w / 2 ) / lv_focal.
        DATA(lv_ray_y) = ( lv_py * lv_ph - lc_h / 2 ) / lv_focal.
        DATA(lv_ray_z) = CONV f( 1 ).
        DATA(lv_ray_len) = sqrt( lv_ray_x ** 2 + lv_ray_y ** 2 + lv_ray_z ** 2 ).
        lv_ray_x = lv_ray_x / lv_ray_len.
        lv_ray_y = lv_ray_y / lv_ray_len.
        lv_ray_z = lv_ray_z / lv_ray_len.

        " Ray march
        DATA(lv_total) = CONV f( 0 ).
        DATA(lv_hit) = abap_false.
        DATA lv_pos_x TYPE f.
        DATA lv_pos_y TYPE f.
        DATA lv_pos_z TYPE f.

        DO 20 TIMES.
          lv_pos_x = lv_total * lv_ray_x.
          lv_pos_y = lv_total * lv_ray_y.
          lv_pos_z = lv_cam_z + lv_total * lv_ray_z.

          DATA(lv_dist) = scene_sdf( iv_x = lv_pos_x iv_y = lv_pos_y iv_z = lv_pos_z it_blobs = lt_blobs ).

          IF lv_dist < 2.
            lv_hit = abap_true.
            EXIT.
          ENDIF.
          IF lv_total > 400. EXIT. ENDIF.

          lv_total = lv_total + lv_dist.
        ENDDO.

        DATA lv_light_val TYPE f.

        IF lv_hit = abap_true.
          " Calculate normal for Phong lighting
          DATA(ls_n) = calc_normal( iv_x = lv_pos_x iv_y = lv_pos_y iv_z = lv_pos_z it_blobs = lt_blobs ).

          " Diffuse: N dot L
          DATA(lv_diffuse) = ls_n-x * lv_light_x + ls_n-y * lv_light_y + ls_n-z * lv_light_z.
          IF lv_diffuse < 0. lv_diffuse = 0. ENDIF.

          " Specular: reflection dot view (simplified)
          " R = 2*(N.L)*N - L
          DATA(lv_rx) = 2 * lv_diffuse * ls_n-x - lv_light_x.
          DATA(lv_ry) = 2 * lv_diffuse * ls_n-y - lv_light_y.
          DATA(lv_rz) = 2 * lv_diffuse * ls_n-z - lv_light_z.
          " View direction = -ray
          DATA(lv_spec) = - ( lv_rx * lv_ray_x + lv_ry * lv_ray_y + lv_rz * lv_ray_z ).
          IF lv_spec < 0. lv_spec = 0. ENDIF.
          lv_spec = lv_spec ** 8.  " Shininess

          " Ambient + Diffuse + Specular
          lv_light_val = '0.15' + lv_diffuse * '0.6' + lv_spec * '0.4'.
          IF lv_light_val > 1. lv_light_val = 1. ENDIF.

          " Голубой / Cyan shades
          DATA(lv_r) = CONV i( lv_light_val * 80 ).
          DATA(lv_g) = CONV i( lv_light_val * 200 + 40 ).
          DATA(lv_b) = CONV i( lv_light_val * 200 + 55 ).

          APPEND VALUE zif_o4d_effect=>ty_rect(
            x = lv_px * lv_pw  y = lv_py * lv_ph
            w = lv_pw + 1  h = lv_ph + 1
            fill = |rgb({ lv_r },{ lv_g },{ lv_b })|
          ) TO rs_frame-rects.
        ELSE.
          " Dark background
          APPEND VALUE zif_o4d_effect=>ty_rect(
            x = lv_px * lv_pw  y = lv_py * lv_ph
            w = lv_pw + 1  h = lv_ph + 1
            fill = '#0a0a12'
          ) TO rs_frame-rects.
        ENDIF.

        lv_px = lv_px + 1.
      ENDWHILE.
      lv_py = lv_py + 1.
    ENDWHILE.

    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lc_w / 2 y = 20 text = |3D METABALLS| color = '#aaccff' size = 14 align = 'center'
    ) TO rs_frame-texts.

    IF is_ctx-gbi-pulse > '0.7'.
      rs_frame-flash = VALUE #( active = abap_true intensity = '0.12' r = '0.8' g = '0.9' b = 1 ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

