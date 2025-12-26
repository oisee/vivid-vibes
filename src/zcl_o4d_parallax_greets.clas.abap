CLASS zcl_o4d_parallax_greets DEFINITION PUBLIC FINAL CREATE PUBLIC.
*======================================================================
* PARALLAX GREETINGS - 5 Layer Depth Scroller
*======================================================================
* Creates depth illusion with 5 text layers scrolling at different speeds
* Pattern: A-B-C-B-A (fast-mid-slow-mid-fast)
* Layer A: Fastest (far background) - ZX Spectrum scene
* Layer B: Medium speed - VRChat/Others
* Layer C: Slowest (foreground/center) - LinkedIn ABAP colleagues
* SMOOTH SCROLL: Uses frame number (integer) not time (float)
*======================================================================
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    CLASS-METHODS new
      IMPORTING iv_base_ppf TYPE i DEFAULT 2  " Base pixels per frame (layer C)
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_o4d_parallax_greets.

  PRIVATE SECTION.
    DATA mt_layer_a TYPE string_table.  " ZX Spectrum
    DATA mt_layer_b TYPE string_table.  " VRChat/Others
    DATA mt_layer_c TYPE string_table.  " LinkedIn ABAP
    DATA mv_base_ppf TYPE i.            " Base pixels per frame (integer!)

    METHODS init_greetings.
    METHODS get_layer_config
      IMPORTING iv_layer     TYPE i  " 0-4 (A-B-C-B-A)
      EXPORTING ev_ppf       TYPE i  " Pixels per frame (integer!)
                ev_y_pos     TYPE i  " Y position (integer!)
                ev_size      TYPE i
                ev_alpha     TYPE f
                ev_hue       TYPE i
                et_texts     TYPE string_table.
ENDCLASS.



CLASS ZCL_O4D_PARALLAX_GREETS IMPLEMENTATION.


  METHOD new.
    ro_obj = NEW #( ).
    ro_obj->mv_base_ppf = iv_base_ppf.
    ro_obj->init_greetings( ).
  ENDMETHOD.


  METHOD init_greetings.
    " Layer A - ZX Spectrum scene (fast, background)
    APPEND 'YERZMYEY' TO mt_layer_a.
    APPEND 'GASMAN' TO mt_layer_a.
    APPEND 'FACTOR6' TO mt_layer_a.
    APPEND 'BUSY' TO mt_layer_a.
    APPEND 'ALONE CODER' TO mt_layer_a.
    APPEND 'AY RIDERS' TO mt_layer_a.
    APPEND 'HOOY PROGRAM' TO mt_layer_a.
    APPEND 'TIBOH' TO mt_layer_a.
    APPEND 'MmcM' TO mt_layer_a.
    APPEND 'DEMARCHE' TO mt_layer_a.
    APPEND 'KYIV' TO mt_layer_a.
    APPEND 'SPECCY FOREVER' TO mt_layer_a.

    " Layer B - VRChat / Others (medium speed)
    APPEND 'VRCHAT CREW' TO mt_layer_b.
    APPEND 'DEMOSCENE.RU' TO mt_layer_b.
    APPEND 'POUET.NET' TO mt_layer_b.
    APPEND 'SCENE.ORG' TO mt_layer_b.
    APPEND 'ANTHROPIC' TO mt_layer_b.
    APPEND 'CLAUDE' TO mt_layer_b.
    APPEND 'AI CODERS' TO mt_layer_b.
    APPEND 'FUTURE CREW' TO mt_layer_b.
    APPEND 'REVISION' TO mt_layer_b.
    APPEND 'EVOKE' TO mt_layer_b.

    " Layer C - LinkedIn ABAP colleagues (slow, foreground)
    APPEND 'ABAP COMMUNITY' TO mt_layer_c.
    APPEND 'SAP DEVELOPERS' TO mt_layer_c.
    APPEND 'CLEAN ABAP' TO mt_layer_c.
    APPEND 'ABAP OPEN SOURCE' TO mt_layer_c.
    APPEND 'SITABAP' TO mt_layer_c.
    APPEND 'ABAPGIT TEAM' TO mt_layer_c.
    APPEND 'RAP DEVELOPERS' TO mt_layer_c.
    APPEND 'STEAMPUNK CREW' TO mt_layer_c.
    APPEND 'BTP PIONEERS' TO mt_layer_c.
    APPEND 'CDS MASTERS' TO mt_layer_c.
  ENDMETHOD.


  METHOD get_layer_config.
    " Layer pattern: 0=A, 1=B, 2=C, 3=B, 4=A (symmetric depth)
    DATA(lv_layer_type) = SWITCH i( iv_layer
      WHEN 0 THEN 0  " A (top, fast)
      WHEN 1 THEN 1  " B
      WHEN 2 THEN 2  " C (center, slow)
      WHEN 3 THEN 1  " B
      WHEN 4 THEN 0  " A (bottom, fast)
    ).

    " Y positions (symmetric around center 200) - INTEGER!
    ev_y_pos = SWITCH i( iv_layer
      WHEN 0 THEN 60   " Top A
      WHEN 1 THEN 120  " Top B
      WHEN 2 THEN 200  " Center C
      WHEN 3 THEN 280  " Bottom B
      WHEN 4 THEN 340  " Bottom A
    ).

    " Pixels per frame - INTEGER for smooth scroll!
    " Layer A = 3x base, Layer B = 2x base, Layer C = 1x base
    ev_ppf = SWITCH i( lv_layer_type
      WHEN 0 THEN mv_base_ppf * 3   " A - fastest (far)
      WHEN 1 THEN mv_base_ppf * 2   " B - medium
      WHEN 2 THEN mv_base_ppf       " C - slowest (near)
    ).

    " Text size
    ev_size = SWITCH i( lv_layer_type
      WHEN 0 THEN 80  " A - small (far)
      WHEN 1 THEN 80  " B - medium
      WHEN 2 THEN 80  " C - large (near)
    ).

    " Alpha/brightness (A=dim, C=bright)
    ev_alpha = SWITCH f( lv_layer_type
      WHEN 0 THEN '0.5'   " A - dim (far)
      WHEN 1 THEN '0.75'  " B - medium
      WHEN 2 THEN '1.0'   " C - bright (near)
    ).

    " Hue (depth color coding)
    ev_hue = SWITCH i( lv_layer_type
      WHEN 0 THEN 200  " A - blue (far/cold)
      WHEN 1 THEN 280  " B - purple
      WHEN 2 THEN 60   " C - yellow/gold (near/warm)
    ).

    " Return appropriate text list
    et_texts = SWITCH #( lv_layer_type
      WHEN 0 THEN mt_layer_a
      WHEN 1 THEN mt_layer_b
      WHEN 2 THEN mt_layer_c
    ).
  ENDMETHOD.


  METHOD zif_o4d_effect~get_name.
    rv_name = 'parallax_greets'.
  ENDMETHOD.


  METHOD zif_o4d_effect~get_params.
  ENDMETHOD.


  METHOD zif_o4d_effect~set_param.
  ENDMETHOD.


  METHOD zif_o4d_effect~render.
    rs_frame = zif_o4d_effect~render_frame( VALUE #(
      t = is_sync-time
      gt = is_sync-time
      gbi = VALUE #( bar = is_sync-bar bar_phase = is_sync-bar_phase pulse = is_sync-intensity )
    ) ).
  ENDMETHOD.


  METHOD zif_o4d_effect~render_frame.
    DATA(lv_w) = 640.
    DATA(lv_h) = 400.
    DATA(lv_frame) = is_ctx-gf.  " Global frame number (INTEGER!)

    DATA: lv_ppf       TYPE i,
          lv_y_pos     TYPE i,
          lv_size      TYPE i,
          lv_alpha     TYPE f,
          lv_hue       TYPE i,
          lt_texts     TYPE string_table.

    " Render 5 layers (A-B-C-B-A pattern)
    DO 5 TIMES.
      DATA(lv_layer) = sy-index - 1.

      get_layer_config(
        EXPORTING iv_layer = lv_layer
        IMPORTING ev_ppf   = lv_ppf
                  ev_y_pos = lv_y_pos
                  ev_size  = lv_size
                  ev_alpha = lv_alpha
                  ev_hue   = lv_hue
                  et_texts = lt_texts
      ).

      " Calculate total width of all texts in this layer (integer math)
      DATA(lv_total_width) = 0.
      DATA(lv_char_width) = lv_size * 6 / 10.  " ~0.6 ratio, integer
      DATA(lv_gap) = 80.

      LOOP AT lt_texts INTO DATA(lv_text).
        lv_total_width = lv_total_width + strlen( lv_text ) * lv_char_width + lv_gap.
      ENDLOOP.

      " Scroll position - ALL INTEGER MATH for smooth scroll!
      " scroll = frame * pixels_per_frame
      DATA(lv_scroll) = lv_frame * lv_ppf.
      DATA(lv_offset) = - ( lv_scroll MOD lv_total_width ).

      " Draw all texts in this layer
      DATA(lv_x) = lv_offset.
      LOOP AT lt_texts INTO lv_text.
        " Draw if visible (with margin)
        IF lv_x > -400 AND lv_x < lv_w + 100.
          DATA(lv_light) = 50 + CONV i( lv_alpha * 30 ).

          " Beat pulse
          IF is_ctx-gbi-pulse > '0.3'.
            DATA(lv_pulse_add) = CONV i( is_ctx-gbi-pulse * 20 ).
            IF lv_layer = 2. lv_pulse_add = lv_pulse_add + 5. ENDIF.
            lv_light = lv_light + lv_pulse_add.
          ENDIF.

          " Slight vertical wave (based on frame, not time)
          DATA(lv_wave) = 0. "( lv_frame / 8 + lv_x / 50 + lv_layer * 10 ) MOD 7 - 3.

          APPEND VALUE zif_o4d_effect=>ty_text(
            x = lv_x
            y = lv_y_pos + lv_wave
            text = lv_text
            color = |hsl({ lv_hue }, 70%, { lv_light }%)|
            size = lv_size
            align = 'left'
          ) TO rs_frame-texts.
        ENDIF.

        lv_x = lv_x + strlen( lv_text ) * lv_char_width + lv_gap.
      ENDLOOP.

      " Repeat texts to fill screen (seamless loop)
      lv_x = lv_offset + lv_total_width.
      LOOP AT lt_texts INTO lv_text.
        IF lv_x > -400 AND lv_x < lv_w + 100.
          DATA(lv_light2) = 50 + CONV i( lv_alpha * 30 ).
          DATA(lv_wave2) = ( lv_frame / 8 + lv_x / 50 + lv_layer * 10 ) MOD 7 - 3.

          APPEND VALUE zif_o4d_effect=>ty_text(
            x = lv_x
            y = lv_y_pos + lv_wave2
            text = lv_text
            color = |hsl({ lv_hue }, 70%, { lv_light2 }%)|
            size = lv_size
            align = 'left'
          ) TO rs_frame-texts.
        ENDIF.
        lv_x = lv_x + strlen( lv_text ) * lv_char_width + lv_gap.
      ENDLOOP.
    ENDDO.

    " Title
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_w / 2 y = 15
      text = 'GREETINGS TO:'
      color = '#fff' size = 12 align = 'center'
    ) TO rs_frame-texts.

    " Layer labels (subtle)
    APPEND VALUE zif_o4d_effect=>ty_text( x = 10 y = 60 text = 'ZX' color = '#4488ff40' size = 8 align = 'left' ) TO rs_frame-texts.
    APPEND VALUE zif_o4d_effect=>ty_text( x = 10 y = 120 text = 'VR' color = '#aa44ff40' size = 8 align = 'left' ) TO rs_frame-texts.
    APPEND VALUE zif_o4d_effect=>ty_text( x = 10 y = 200 text = 'ABAP' color = '#ffcc4460' size = 8 align = 'left' ) TO rs_frame-texts.
    APPEND VALUE zif_o4d_effect=>ty_text( x = 10 y = 280 text = 'VR' color = '#aa44ff40' size = 8 align = 'left' ) TO rs_frame-texts.
    APPEND VALUE zif_o4d_effect=>ty_text( x = 10 y = 340 text = 'ZX' color = '#4488ff40' size = 8 align = 'left' ) TO rs_frame-texts.

    " Beat flash
    IF is_ctx-gbi-on_bar = abap_true.
      rs_frame-flash-active = abap_true.
      rs_frame-flash-intensity = '0.15'.
      rs_frame-flash-r = 1. rs_frame-flash-g = 1. rs_frame-flash-b = '0.8'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
