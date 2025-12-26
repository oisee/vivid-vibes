CLASS zcl_o4d_macro_demo DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    ALIASES: ty_frame FOR zif_o4d_effect~ty_frame,
             ty_sync FOR zif_o4d_effect~ty_sync,
             ty_render_ctx FOR zif_o4d_effect~ty_render_ctx,
             ty_macro FOR zif_o4d_effect~ty_macro.
ENDCLASS.

CLASS zcl_o4d_macro_demo IMPLEMENTATION.
  METHOD zif_o4d_effect~get_name.
    rv_name = 'MACRO_DEMO'.
  ENDMETHOD.

  METHOD zif_o4d_effect~get_params.
    " No editable params
  ENDMETHOD.

  METHOD zif_o4d_effect~set_param.
    " Not used
  ENDMETHOD.

  METHOD zif_o4d_effect~render.
    " Legacy method - redirect to render_frame
    DATA ls_ctx TYPE ty_render_ctx.
    ls_ctx-t = is_sync-time.
    ls_ctx-gt = is_sync-time.
    rs_frame = zif_o4d_effect~render_frame( ls_ctx ).
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    " Determine which macro to use based on bar
    DATA lv_bar TYPE i.
    lv_bar = is_ctx-gbi-bar MOD 8.

    " Send macro command - JS will render the effect!
    DATA ls_macro TYPE ty_macro.

    CASE lv_bar.
      WHEN 0 OR 1.
        " Starfield effect
        ls_macro-cmd = 'starfield'.
        ls_macro-params = '{"count":150,"speed":1.5}'.
        APPEND ls_macro TO rs_frame-macros.

      WHEN 2 OR 3.
        " Copper bars
        ls_macro-cmd = 'copper'.
        ls_macro-params = '{"bars":20,"speed":4}'.
        APPEND ls_macro TO rs_frame-macros.

      WHEN 4.
        " Plasma
        ls_macro-cmd = 'plasma'.
        ls_macro-params = '{"scale":16,"speed":1.2}'.
        APPEND ls_macro TO rs_frame-macros.

      WHEN 5.
        " Tunnel
        ls_macro-cmd = 'tunnel'.
        ls_macro-params = '{"speed":3,"twist":0.5,"rings":25}'.
        APPEND ls_macro TO rs_frame-macros.

      WHEN 6.
        " Joy Division
        ls_macro-cmd = 'joydiv'.
        ls_macro-params = '{"lines":25,"amplitude":80}'.
        APPEND ls_macro TO rs_frame-macros.

      WHEN 7.
        " Voxel landscape
        ls_macro-cmd = 'voxel'.
        ls_macro-params = '{"speed":3,"camHeight":200}'.
        APPEND ls_macro TO rs_frame-macros.

    ENDCASE.

    " Add scanlines overlay on top
    ls_macro-cmd = 'scanlines'.
    ls_macro-params = '{"spacing":3,"alpha":0.2}'.
    APPEND ls_macro TO rs_frame-macros.

    " Add text overlay from ABAP (hybrid: macro + primitives)
    DATA ls_text TYPE zif_o4d_effect~ty_text.
    ls_text-x = 320.
    ls_text-y = 380.
    ls_text-text = 'MACRO DEMO - Bar ' && lv_bar.
    ls_text-color = '#0f0'.
    ls_text-size = 16.
    ls_text-align = 'center'.
    APPEND ls_text TO rs_frame-texts.

    " Flash on bar
    IF is_ctx-gbi-on_bar = abap_true.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = '0.3'.
      rs_frame-flash-r = '1.0'.
      rs_frame-flash-g = '1.0'.
      rs_frame-flash-b = '1.0'.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
