CLASS zcl_o4d_twister DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    METHODS constructor IMPORTING iv_speed TYPE f DEFAULT '1.5'.

  PRIVATE SECTION.
    CONSTANTS: c_pi TYPE f VALUE '3.14159265', c_num_slots TYPE i VALUE 12.
    DATA: mv_speed TYPE f, mt_credits TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    METHODS init_credits.
ENDCLASS.

CLASS zcl_o4d_twister IMPLEMENTATION.
  METHOD constructor. mv_speed = iv_speed. init_credits( ). ENDMETHOD.

  METHOD init_credits.
    mt_credits = VALUE #(
      ( `CODE: CLAUDE + ALICE` )
      ( `MUSIC: OISEE` )
      ( `GFX: ABAP VECTORS` )
      ( `WEBGL: GPU POWER` )
      ( `2025 PRODUCTION` )
      ( `GREETS TO ALL` )
    ).
  ENDMETHOD.

  METHOD zif_o4d_effect~get_name. rv_name = 'twister'. ENDMETHOD.
  METHOD zif_o4d_effect~get_params. ENDMETHOD.
  METHOD zif_o4d_effect~set_param.
    IF iv_name = 'speed'. mv_speed = CONV f( iv_value ). ENDIF.
  ENDMETHOD.

  METHOD zif_o4d_effect~render.
    DATA(ls_ctx) = VALUE zif_o4d_effect=>ty_render_ctx(
      t = is_sync-time gt = is_sync-time bi = VALUE #( pulse = is_sync-intensity )
    ).
    rs_frame = zif_o4d_effect~render_frame( ls_ctx ).
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    DATA(lv_t) = is_ctx-t.
    DATA(lv_w) = zif_o4d_effect=>c_width.
    DATA(lv_h) = zif_o4d_effect=>c_height.
    DATA(lv_cx) = CONV f( lv_w ) / 2.
    DATA(lv_cy) = CONV f( lv_h ) / 2.
    DATA(lv_num_credits) = lines( mt_credits ).

    " Title
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_cx y = 15 text = '- CREDITS -'
      color = '#FFFFFF' size = 10 align = 'center'
    ) TO rs_frame-texts.

    " Vertical cylinder with 12 slots, rotating around horizontal axis
    " Each slot is at a fixed angle, text scrolls through slots
    DATA(lv_radius) = CONV f( 80 ).
    DATA(lv_rotation) = lv_t * mv_speed.

    DO c_num_slots TIMES.
      DATA(lv_slot) = sy-index - 1.

      " Angle for this slot on the cylinder (vertical distribution)
      DATA(lv_slot_angle) = ( CONV f( lv_slot ) / c_num_slots ) * 2 * c_pi + lv_rotation.

      " Z-depth determines visibility (front = visible)
      DATA(lv_z) = cos( lv_slot_angle ).

      " Only draw front-facing text (z > -0.2 for some wrap-around)
      IF lv_z > CONV f( '-0.2' ).
        " Y position on cylinder
        DATA(lv_y) = lv_cy + sin( lv_slot_angle ) * lv_radius.

        " Scale based on depth (closer = bigger)
        DATA(lv_depth_factor) = ( lv_z + 1 ) / 2.  " 0..1
        DATA(lv_scale) = CONV f( '0.4' ) + lv_depth_factor * CONV f( '0.6' ).

        " Which credit line to show in this slot
        DATA(lv_credit_idx) = ( lv_slot MOD lv_num_credits ) + 1.
        READ TABLE mt_credits INDEX lv_credit_idx INTO DATA(lv_text).

        " Rainbow hue based on slot + time
        DATA(lv_hue) = CONV i( CONV f( lv_slot ) * 30 + lv_t * 20 ) MOD 360.
        DATA(lv_bright) = 30 + CONV i( lv_depth_factor * 70 ).
        DATA(lv_sat) = 60 + CONV i( lv_depth_factor * 30 ).

        " Font size based on scale
        DATA(lv_size) = CONV i( 16 * lv_scale ).
        IF lv_size < 8. lv_size = 8. ENDIF.

        APPEND VALUE zif_o4d_effect=>ty_text(
          x = lv_cx
          y = lv_y
          text = lv_text
          size = lv_size
          color = |hsl({ lv_hue },{ lv_sat }%,{ lv_bright }%)|
          align = 'center'
        ) TO rs_frame-texts.
      ENDIF.
    ENDDO.

    " Footer
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_cx y = CONV f( lv_h ) - 12 text = 'BYTEBEAT-ABAP 2025'
      color = '#888888' size = 8 align = 'center'
    ) TO rs_frame-texts.

    " Debug vars
    DATA(lv_rot_angle) = lv_rotation - floor( lv_rotation / ( 2 * c_pi ) ) * 2 * c_pi.
    rs_frame-debug-vars = |\{"speed":{ mv_speed },"slots":{ c_num_slots },| &&
      |"credits":{ lv_num_credits },"rotation":{ lv_rot_angle },| &&
      |"radius":{ lv_radius },"visible_slots":6\}|.
  ENDMETHOD.
ENDCLASS.
