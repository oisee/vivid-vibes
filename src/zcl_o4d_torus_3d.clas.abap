CLASS zcl_o4d_torus_3d DEFINITION PUBLIC FINAL CREATE PUBLIC.
*======================================================================
* 3D TORUS - Ray Marched Donut with Phong Shading
*======================================================================
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    CLASS-METHODS new
      IMPORTING iv_resolution   TYPE i DEFAULT 80
                iv_major_radius TYPE f DEFAULT 70
                iv_minor_radius TYPE f DEFAULT 28
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_o4d_torus_3d.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_v3, x TYPE f, y TYPE f, z TYPE f, END OF ty_v3.

    DATA mv_resolution TYPE i.
    DATA mv_major_r TYPE f.  " Big radius (center to tube center)
    DATA mv_minor_r TYPE f.  " Small radius (tube thickness)

    METHODS torus_sdf
      IMPORTING is_p TYPE ty_v3
      RETURNING VALUE(rv_dist) TYPE f.

    METHODS rotate_x
      IMPORTING is_p TYPE ty_v3 iv_a TYPE f
      RETURNING VALUE(rs_p) TYPE ty_v3.

    METHODS rotate_y
      IMPORTING is_p TYPE ty_v3 iv_a TYPE f
      RETURNING VALUE(rs_p) TYPE ty_v3.

    METHODS calc_normal
      IMPORTING is_p TYPE ty_v3
      RETURNING VALUE(rs_n) TYPE ty_v3.
ENDCLASS.

CLASS zcl_o4d_torus_3d IMPLEMENTATION.

  METHOD new.
    ro_obj = NEW #( ).
    ro_obj->mv_resolution = iv_resolution.
    ro_obj->mv_major_r = iv_major_radius.
    ro_obj->mv_minor_r = iv_minor_radius.
  ENDMETHOD.

  METHOD torus_sdf.
    " Torus SDF: distance to donut surface
    " q = (length(p.xz) - R, p.y)
    " return length(q) - r
    DATA(lv_q_x) = sqrt( is_p-x * is_p-x + is_p-z * is_p-z ) - mv_major_r.
    DATA(lv_q_y) = is_p-y.
    rv_dist = sqrt( lv_q_x * lv_q_x + lv_q_y * lv_q_y ) - mv_minor_r.
  ENDMETHOD.

  METHOD rotate_x.
    DATA(lv_c) = cos( iv_a ).
    DATA(lv_s) = sin( iv_a ).
    rs_p-x = is_p-x.
    rs_p-y = is_p-y * lv_c - is_p-z * lv_s.
    rs_p-z = is_p-y * lv_s + is_p-z * lv_c.
  ENDMETHOD.

  METHOD rotate_y.
    DATA(lv_c) = cos( iv_a ).
    DATA(lv_s) = sin( iv_a ).
    rs_p-x = is_p-x * lv_c + is_p-z * lv_s.
    rs_p-y = is_p-y.
    rs_p-z = - is_p-x * lv_s + is_p-z * lv_c.
  ENDMETHOD.

  METHOD calc_normal.
    DATA(lv_eps) = CONV f( '0.5' ).
    DATA(lv_dx) = torus_sdf( VALUE ty_v3( x = is_p-x + lv_eps y = is_p-y z = is_p-z ) )
                - torus_sdf( VALUE ty_v3( x = is_p-x - lv_eps y = is_p-y z = is_p-z ) ).
    DATA(lv_dy) = torus_sdf( VALUE ty_v3( x = is_p-x y = is_p-y + lv_eps z = is_p-z ) )
                - torus_sdf( VALUE ty_v3( x = is_p-x y = is_p-y - lv_eps z = is_p-z ) ).
    DATA(lv_dz) = torus_sdf( VALUE ty_v3( x = is_p-x y = is_p-y z = is_p-z + lv_eps ) )
                - torus_sdf( VALUE ty_v3( x = is_p-x y = is_p-y z = is_p-z - lv_eps ) ).

    DATA(lv_len) = sqrt( lv_dx * lv_dx + lv_dy * lv_dy + lv_dz * lv_dz ).
    IF lv_len < '0.001'. lv_len = '0.001'. ENDIF.

    rs_n-x = lv_dx / lv_len.
    rs_n-y = lv_dy / lv_len.
    rs_n-z = lv_dz / lv_len.
  ENDMETHOD.

  METHOD zif_o4d_effect~get_name. rv_name = 'torus_3d'. ENDMETHOD.
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

    " Rotation angles - smooth tumbling
    DATA(lv_rot_x) = lv_t * '0.4'.
    DATA(lv_rot_y) = lv_t * '0.6'.

    " Light direction (animate slightly)
    DATA(lv_light_angle) = lv_t * '0.2'.
    DATA ls_light TYPE ty_v3.
    ls_light-x = cos( lv_light_angle ) * '0.5'.
    ls_light-y = '-0.7'.
    ls_light-z = sin( lv_light_angle ) * '0.3' - '0.5'.
    DATA(lv_light_len) = sqrt( ls_light-x ** 2 + ls_light-y ** 2 + ls_light-z ** 2 ).
    ls_light-x = ls_light-x / lv_light_len.
    ls_light-y = ls_light-y / lv_light_len.
    ls_light-z = ls_light-z / lv_light_len.

    " Camera
    DATA(lv_cam_z) = CONV f( -220 ).
    DATA(lv_focal) = CONV f( 280 ).

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
        DATA ls_pos TYPE ty_v3.

        DO 32 TIMES.
          " World position
          ls_pos-x = lv_total * lv_ray_x.
          ls_pos-y = lv_total * lv_ray_y.
          ls_pos-z = lv_cam_z + lv_total * lv_ray_z.

          " Rotate point INTO torus space (inverse rotation)
          DATA(ls_rot) = rotate_x( is_p = ls_pos iv_a = - lv_rot_x ).
          ls_rot = rotate_y( is_p = ls_rot iv_a = - lv_rot_y ).

          DATA(lv_dist) = torus_sdf( ls_rot ).

          IF lv_dist < '1.5'.
            lv_hit = abap_true.
            EXIT.
          ENDIF.
          IF lv_total > 400. EXIT. ENDIF.

          lv_total = lv_total + lv_dist.
        ENDDO.

        DATA lv_r TYPE i.
        DATA lv_g TYPE i.
        DATA lv_b TYPE i.

        IF lv_hit = abap_true.
          " Get rotated position for normal calculation
          DATA(ls_rot_pos) = rotate_x( is_p = ls_pos iv_a = - lv_rot_x ).
          ls_rot_pos = rotate_y( is_p = ls_rot_pos iv_a = - lv_rot_y ).

          " Calculate normal in torus space
          DATA(ls_n_local) = calc_normal( ls_rot_pos ).

          " Rotate normal back to world space
          DATA(ls_n) = rotate_y( is_p = ls_n_local iv_a = lv_rot_y ).
          ls_n = rotate_x( is_p = ls_n iv_a = lv_rot_x ).

          " Diffuse: N · L
          DATA(lv_diffuse) = ls_n-x * ls_light-x + ls_n-y * ls_light-y + ls_n-z * ls_light-z.
          IF lv_diffuse < 0. lv_diffuse = 0. ENDIF.

          " Specular: R · V
          DATA(lv_rx) = 2 * lv_diffuse * ls_n-x - ls_light-x.
          DATA(lv_ry) = 2 * lv_diffuse * ls_n-y - ls_light-y.
          DATA(lv_rz) = 2 * lv_diffuse * ls_n-z - ls_light-z.
          DATA(lv_spec) = - ( lv_rx * lv_ray_x + lv_ry * lv_ray_y + lv_rz * lv_ray_z ).
          IF lv_spec < 0. lv_spec = 0. ENDIF.
          lv_spec = lv_spec ** 16.  " High shininess

          " Phong lighting
          DATA(lv_light_val) = '0.12' + lv_diffuse * '0.65' + lv_spec * '0.5'.
          IF lv_light_val > 1. lv_light_val = 1. ENDIF.

          " Голубой / Cyan color (like blobs)
          lv_r = nmin( val1 = 255 val2 = CONV i( lv_light_val * 80 ) ).
          lv_g = nmin( val1 = 255 val2 = CONV i( lv_light_val * 200 + 40 ) ).
          lv_b = nmin( val1 = 255 val2 = CONV i( lv_light_val * 200 + 55 + lv_spec * 100 ) ).

          IF lv_r > 255. lv_r = 255. ENDIF.
          IF lv_g > 255. lv_g = 255. ENDIF.
          IF lv_b > 255. lv_b = 255. ENDIF.
        ELSE.
          " Dark purple background
          lv_r = 12. lv_g = 8. lv_b = 20.
        ENDIF.

        APPEND VALUE zif_o4d_effect=>ty_rect(
          x = lv_px * lv_pw  y = lv_py * lv_ph
          w = lv_pw + 1  h = lv_ph + 1
          fill = |rgb({ lv_r },{ lv_g },{ lv_b })|
        ) TO rs_frame-rects.

        lv_px = lv_px + 1.
      ENDWHILE.
      lv_py = lv_py + 1.
    ENDWHILE.

    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lc_w / 2 y = 20 text = |3D TORUS| color = '#ffcc66' size = 14 align = 'center'
    ) TO rs_frame-texts.

    IF is_ctx-gbi-pulse > '0.6'.
      rs_frame-flash = VALUE #( active = abap_true intensity = '0.15' r = 1 g = '0.8' b = '0.3' ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

