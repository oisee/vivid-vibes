CLASS zcl_o4d_gallery DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    ALIASES render FOR zif_o4d_effect~render.

    CLASS-METHODS new
      IMPORTING iv_scroll_text TYPE string OPTIONAL
      RETURNING VALUE(ro_instance) TYPE REF TO zcl_o4d_gallery.

  PRIVATE SECTION.
    CONSTANTS c_pi TYPE f VALUE '3.14159265'.

    TYPES: BEGIN OF ty_slide,
             name      TYPE string,
             title     TYPE string,
             desc      TYPE string,
             classname TYPE string,
             long      TYPE abap_bool,  " 2x duration
           END OF ty_slide.

    DATA mt_slides TYPE STANDARD TABLE OF ty_slide.
    DATA mv_scroll_text TYPE string.

    METHODS init_slides.

    METHODS get_frame_3d
      IMPORTING iv_time      TYPE f
                iv_img_w     TYPE f
                iv_img_h     TYPE f
                iv_depth     TYPE f
      RETURNING VALUE(rt_lines) TYPE zif_o4d_effect=>tt_lines.

    METHODS get_scanlines
      IMPORTING iv_time   TYPE f
                iv_y_start TYPE i
                iv_y_end   TYPE i
      RETURNING VALUE(rt_lines) TYPE zif_o4d_effect=>tt_lines.

    METHODS int_to_hex
      IMPORTING iv_int TYPE i
      RETURNING VALUE(rv_hex) TYPE string.

ENDCLASS.



CLASS ZCL_O4D_GALLERY IMPLEMENTATION.


  METHOD new.
    ro_instance = NEW #( ).
    IF iv_scroll_text IS NOT INITIAL.
      ro_instance->mv_scroll_text = iv_scroll_text.
    ELSE.
      " Default scroll text
      ro_instance->mv_scroll_text =
        |  *  O4D GALLERY  *  ABAP DEMOSCENE EFFECTS  *  | &&
        |OLDSCHOOL: COPPER PLASMA ROTOZOOM AMIGABALL  *  | &&
        |3D/VECTOR: TESSERACT 24-CELL 120-CELL TORUS  *  | &&
        |FRACTALS: SIERPINSKI JULIA QUATERNION  *  | &&
        |OISEE + CLAUDE // 2025  *  |.
    ENDIF.
  ENDMETHOD.


  METHOD zif_o4d_effect~render.
    DATA: lv_cx       TYPE f,
          lv_cy       TYPE f,
          lv_time     TYPE f,
          lv_slide_idx TYPE i,
          lv_slide_t  TYPE f,
          lv_trans_t  TYPE f,
          lv_in_trans TYPE abap_bool,
          lv_img_w    TYPE f VALUE 345,
          lv_img_h    TYPE f VALUE 216,
          lv_frame_x  TYPE f VALUE 40,
          lv_frame_y  TYPE f VALUE 56,
          lv_info_x   TYPE f VALUE 440,
          lv_alpha    TYPE f,
          lv_hue      TYPE i,
          ls_slide    TYPE ty_slide.

    IF mt_slides IS INITIAL.
      init_slides( ).
    ENDIF.

    lv_cx = zif_o4d_effect=>c_width / 2.
    lv_cy = zif_o4d_effect=>c_height / 2.
    lv_time = is_sync-time. "/ '1.5' .

    DATA(lv_base_dur) = CONV f( '4.0' ).
    DATA(lv_trans_dur) = CONV f( '1.5' ).
    DATA(lv_num_slides) = lines( mt_slides ).

    " Calculate total cycle duration (sum of all slide durations)
    DATA(lv_cycle_dur) = CONV f( '0.0' ).
    LOOP AT mt_slides INTO DATA(ls_s).
      lv_cycle_dur = lv_cycle_dur + COND f( WHEN ls_s-long = abap_true THEN lv_base_dur * 2 ELSE lv_base_dur ).
    ENDLOOP.

    " Find current position in cycle
    DATA(lv_cycle_t) = lv_time - floor( lv_time / lv_cycle_dur ) * lv_cycle_dur.

    " Find which slide we're on
    DATA(lv_accum) = CONV f( '0.0' ).
    DATA(lv_slide_dur) = lv_base_dur.
    lv_slide_idx = 0.
    LOOP AT mt_slides INTO ls_slide.
      lv_slide_dur = COND f( WHEN ls_slide-long = abap_true THEN lv_base_dur * 2 ELSE lv_base_dur ).
      IF lv_cycle_t < lv_accum + lv_slide_dur.
        lv_slide_idx = sy-tabix - 1.
        lv_slide_t = lv_cycle_t - lv_accum.
        EXIT.
      ENDIF.
      lv_accum = lv_accum + lv_slide_dur.
    ENDLOOP.

    IF lv_slide_t > ( lv_slide_dur - lv_trans_dur ).
      lv_in_trans = abap_true.
      lv_trans_t = ( lv_slide_t - ( lv_slide_dur - lv_trans_dur ) ) / lv_trans_dur.
    ELSE.
      lv_in_trans = abap_false.
      lv_trans_t = 0.
    ENDIF.

    " === BACKGROUND ===
    APPEND VALUE zif_o4d_effect=>ty_rect(
      x = 0 y = 0 w = zif_o4d_effect=>c_width h = zif_o4d_effect=>c_height
      fill = '#050510'
    ) TO rs_frame-rects.

    " Grid lines
    DATA lv_grid_y TYPE i.
    DO 10 TIMES.
      lv_grid_y = sy-index * 40.
      APPEND VALUE zif_o4d_effect=>ty_line(
        x1 = 0 y1 = lv_grid_y x2 = zif_o4d_effect=>c_width y2 = lv_grid_y
        color = '#0a0a20' width = 1
      ) TO rs_frame-lines.
    ENDDO.

    " === IMAGE FRAME POSITION ===
    " lv_frame_x and lv_frame_y already set as defaults (20, 40)

    IF lv_in_trans = abap_true.
      DATA(lv_wobble) = sin( lv_trans_t * c_pi * 8 ) * 5 * ( 1 - lv_trans_t ).
      lv_frame_x = lv_frame_x + lv_wobble.
    ENDIF.

    " === 3D WIREFRAME ===
    DATA(lt_frame) = get_frame_3d(
      iv_time  = lv_time
      iv_img_w = lv_img_w
      iv_img_h = lv_img_h
      iv_depth = CONV f( '20' )
    ).

    LOOP AT lt_frame ASSIGNING FIELD-SYMBOL(<fs_line>).
      <fs_line>-x1 = <fs_line>-x1 + lv_frame_x.
      <fs_line>-y1 = <fs_line>-y1 + lv_frame_y.
      <fs_line>-x2 = <fs_line>-x2 + lv_frame_x.
      <fs_line>-y2 = <fs_line>-y2 + lv_frame_y.
      APPEND <fs_line> TO rs_frame-lines.
    ENDLOOP.

    " === IMAGE ===
    lv_alpha = COND #( WHEN lv_in_trans = abap_true
                       THEN 1 - lv_trans_t
                       ELSE nmin( val1 = CONV f( '1.0' ) val2 = lv_slide_t * 2 ) ).

    DATA(lv_alpha_i) = CONV i( lv_alpha * 255 ).
    DATA(lv_alpha_hex) = int_to_hex( lv_alpha_i ).

    " Background placeholder (shows through if image not loaded)
    APPEND VALUE zif_o4d_effect=>ty_rect(
      x = lv_frame_x + 5  y = lv_frame_y + 5
      w = lv_img_w - 10   h = lv_img_h - 10
      fill = |#101830{ lv_alpha_hex }|
    ) TO rs_frame-rects.

    " Check if music slide (no image, render note icon instead)
    IF ls_slide-name CS 'EARULEZ' OR ls_slide-name CS 'OLELUK'.
      " Vector music note - large centered ♫
      DATA(lv_note_x) = lv_frame_x + lv_img_w / 2.
      DATA(lv_note_y) = lv_frame_y + lv_img_h / 2 + 20.
      DATA(lv_note_hue) = CONV i( lv_time * 60 ) MOD 360.
      APPEND VALUE zif_o4d_effect=>ty_text(
        x = lv_note_x  y = lv_note_y  text = '♫'
        color = |hsl({ lv_note_hue }, 100%, 70%)|  size = 80  align = 'center'
      ) TO rs_frame-texts.
      " Smaller animated notes
      DATA(lv_float) = sin( lv_time * 3 ) * 15.
      APPEND VALUE zif_o4d_effect=>ty_text(
        x = lv_note_x - 60  y = lv_note_y - 30 + lv_float  text = '♪'
        color = |hsl({ lv_note_hue + 120 }, 100%, 60%)|  size = 32  align = 'center'
      ) TO rs_frame-texts.
      APPEND VALUE zif_o4d_effect=>ty_text(
        x = lv_note_x + 60  y = lv_note_y - 40 - lv_float  text = '♪'
        color = |hsl({ lv_note_hue + 240 }, 100%, 60%)|  size = 32  align = 'center'
      ) TO rs_frame-texts.
    ELSE.
      " Actual image reference with dynamic position
      APPEND VALUE zif_o4d_effect=>ty_image(
        name = ls_slide-name
        x = lv_frame_x + 5
        y = lv_frame_y + 5
        w = lv_img_w - 10
        h = lv_img_h - 10
      ) TO rs_frame-images.
    ENDIF.

    " === SCANLINES ===
    DATA(lt_scan) = get_scanlines(
      iv_time    = lv_time
      iv_y_start = CONV i( lv_frame_y )
      iv_y_end   = CONV i( lv_frame_y + lv_img_h )
    ).
    APPEND LINES OF lt_scan TO rs_frame-lines.

    " === CORNER BRACKETS ===
    DATA: lv_bx1 TYPE f, lv_by1 TYPE f,
          lv_bx2 TYPE f, lv_by2 TYPE f,
          lv_bracket_size TYPE f VALUE 20.

    lv_hue = CONV i( lv_time * 30 ) MOD 360.
    DATA(lv_bracket_color) = |hsl({ lv_hue }, 100%, 60%)|.

    lv_bx1 = lv_frame_x.
    lv_by1 = lv_frame_y.
    lv_bx2 = lv_frame_x + lv_img_w.
    lv_by2 = lv_frame_y + lv_img_h.

    " Top-left
    APPEND VALUE zif_o4d_effect=>ty_line( x1 = lv_bx1 y1 = lv_by1 + lv_bracket_size x2 = lv_bx1 y2 = lv_by1 color = lv_bracket_color width = 3 ) TO rs_frame-lines.
    APPEND VALUE zif_o4d_effect=>ty_line( x1 = lv_bx1 y1 = lv_by1 x2 = lv_bx1 + lv_bracket_size y2 = lv_by1 color = lv_bracket_color width = 3 ) TO rs_frame-lines.
    " Top-right
    APPEND VALUE zif_o4d_effect=>ty_line( x1 = lv_bx2 - lv_bracket_size y1 = lv_by1 x2 = lv_bx2 y2 = lv_by1 color = lv_bracket_color width = 3 ) TO rs_frame-lines.
    APPEND VALUE zif_o4d_effect=>ty_line( x1 = lv_bx2 y1 = lv_by1 x2 = lv_bx2 y2 = lv_by1 + lv_bracket_size color = lv_bracket_color width = 3 ) TO rs_frame-lines.
    " Bottom-left
    APPEND VALUE zif_o4d_effect=>ty_line( x1 = lv_bx1 y1 = lv_by2 - lv_bracket_size x2 = lv_bx1 y2 = lv_by2 color = lv_bracket_color width = 3 ) TO rs_frame-lines.
    APPEND VALUE zif_o4d_effect=>ty_line( x1 = lv_bx1 y1 = lv_by2 x2 = lv_bx1 + lv_bracket_size y2 = lv_by2 color = lv_bracket_color width = 3 ) TO rs_frame-lines.
    " Bottom-right
    APPEND VALUE zif_o4d_effect=>ty_line( x1 = lv_bx2 - lv_bracket_size y1 = lv_by2 x2 = lv_bx2 y2 = lv_by2 color = lv_bracket_color width = 3 ) TO rs_frame-lines.
    APPEND VALUE zif_o4d_effect=>ty_line( x1 = lv_bx2 y1 = lv_by2 x2 = lv_bx2 y2 = lv_by2 - lv_bracket_size color = lv_bracket_color width = 3 ) TO rs_frame-lines.

    " === RIGHT PANEL ===
    DATA(lv_panel_x) = lv_info_x.
    DATA(lv_panel_w) = 190.

    " Panel background
    APPEND VALUE zif_o4d_effect=>ty_rect(
      x = lv_panel_x - 10  y = lv_frame_y  w = lv_panel_w  h = lv_img_h
      fill = 'rgba(10,10,30,0.8)'
    ) TO rs_frame-rects.

    " Separator line
    APPEND VALUE zif_o4d_effect=>ty_line(
      x1 = lv_panel_x - 10 y1 = lv_frame_y x2 = lv_panel_x - 10 y2 = lv_frame_y + lv_img_h
      color = '#0088AA' width = 2
    ) TO rs_frame-lines.

    " Counter
    DATA(lv_counter) = |{ lv_slide_idx + 1 }/{ lv_num_slides }|.
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_panel_x + 85  y = lv_frame_y + 25  text = lv_counter
      color = '#00FFFF'  size = 18  align = 'center'
    ) TO rs_frame-texts.

    " Title
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_panel_x + 85  y = lv_frame_y + 70  text = ls_slide-title
      color = '#FFFFFF'  size = 14  align = 'center'
    ) TO rs_frame-texts.

    " Description
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_panel_x + 85  y = lv_frame_y + 100  text = ls_slide-desc
      color = '#88AAAA'  size = 10  align = 'center'
    ) TO rs_frame-texts.

    " Technical name
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_panel_x + 85  y = lv_frame_y + 130  text = ls_slide-name
      color = '#446688'  size = 9  align = 'center'
    ) TO rs_frame-texts.

    " Class name
    IF ls_slide-classname IS NOT INITIAL.
      APPEND VALUE zif_o4d_effect=>ty_text(
        x = lv_panel_x + 85  y = lv_frame_y + 150  text = ls_slide-classname
        color = '#FF8800'  size = 9  align = 'center'
      ) TO rs_frame-texts.
    ENDIF.

    " Progress bar
    DATA(lv_prog_w) = 160 * lv_slide_t / lv_slide_dur.
    APPEND VALUE zif_o4d_effect=>ty_rect(
      x = lv_panel_x + 5  y = lv_frame_y + lv_img_h - 25  w = 160  h = 8
      fill = '#222244'
    ) TO rs_frame-rects.
    APPEND VALUE zif_o4d_effect=>ty_rect(
      x = lv_panel_x + 5  y = lv_frame_y + lv_img_h - 25  w = lv_prog_w  h = 8
      fill = lv_bracket_color
    ) TO rs_frame-rects.

    " === BOTTOM SCROLLER ===
    DATA(lv_scroll_y) = 320.
    DATA(lv_scroll_speed) = 80.

    " Scroller background
    APPEND VALUE zif_o4d_effect=>ty_rect(
      x = 0  y = lv_scroll_y  w = zif_o4d_effect=>c_width  h = 80
      fill = 'rgba(0,0,20,0.9)'
    ) TO rs_frame-rects.

    " Top line
    APPEND VALUE zif_o4d_effect=>ty_line(
      x1 = 0 y1 = lv_scroll_y x2 = zif_o4d_effect=>c_width y2 = lv_scroll_y
      color = '#0088AA' width = 2
    ) TO rs_frame-lines.

    " Scrolling text - right to left, smooth looping
    DATA(lv_scroll_text) = mv_scroll_text.
    DATA(lv_text_width) = strlen( lv_scroll_text ) * 11.  " ~11px per char
    DATA(lv_wrap_dist) = lv_text_width + 640.  " Full scroll distance
    DATA(lv_scroll_offset) = CONV i( lv_time * lv_scroll_speed ) MOD lv_wrap_dist.
    DATA(lv_scroll_x) = 640 - lv_scroll_offset.  " Start off-right, scroll left

    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_scroll_x  y = lv_scroll_y + 50  text = lv_scroll_text
      color = '#00FFFF'  size = 18  align = 'left'
    ) TO rs_frame-texts.

    " === HEADER ===
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = 320  y = 25  text = '[ VIVID VIBES ]'
      color = '#FF00FF'  size = 14  align = 'center'
    ) TO rs_frame-texts.

  ENDMETHOD.


  METHOD zif_o4d_effect~render_frame.
    rs_frame = render( VALUE #(
      time = is_ctx-t
      bar = is_ctx-bi-bar
      beat = is_ctx-bi-beat
      bar_phase = is_ctx-bi-bar_phase
    ) ).
  ENDMETHOD.


  METHOD zif_o4d_effect~get_name.
    rv_name = 'GALLERY'.
  ENDMETHOD.


  METHOD zif_o4d_effect~get_required_media.
    " Return all gallery images for preloading
    rt_media = VALUE #(
      ( name = 'ZO4D_00_SALES'      type = 'img' )
      ( name = 'ZO4D_02_IGNITE'     type = 'img' )
      ( name = 'ZO4D_05_COPPER'     type = 'img' )
      ( name = 'ZO4D_06_PLASMA'     type = 'img' )
      ( name = 'ZO4D_07_TWISTER'    type = 'img' )
      ( name = 'ZO4D_08_MOUNTAINS'  type = 'img' )
      ( name = 'ZO4D_09_ROTOZOOM'   type = 'img' )
      ( name = 'ZO4D_10_VOXEL'      type = 'img' )
      ( name = 'ZO4D_11_ROTOPLASMA' type = 'img' )
      ( name = 'ZO4D_12_TESSERACT'  type = 'img' )
      ( name = 'ZO4D_13_CELL24'     type = 'img' )
      ( name = 'ZO4D_14_CELL16'     type = 'img' )
      ( name = 'ZO4D_15_CELL120'    type = 'img' )
      ( name = 'ZO4D_17_AMIGABALL'  type = 'img' )
      ( name = 'ZO4D_18_GLITCH'     type = 'img' )
      ( name = 'ZO4D_19_SIERPINSKI' type = 'img' )
      ( name = 'ZO4D_20_NEONCITY'   type = 'img' )
      ( name = 'ZO4D_21_JOYDIV'     type = 'img' )
      ( name = 'ZO4D_22_SIERPTET'   type = 'img' )
      ( name = 'ZO4D_23_QUATJULIA'  type = 'img' )
      ( name = 'ZO4D_24_METABALLS'  type = 'img' )
      ( name = 'ZO4D_25_TORUS'      type = 'img' )
      ( name = 'ZO4D_26_JULIAMORPH' type = 'img' )
      ( name = 'ZO4D_27_CONSTELL'   type = 'img' )
      ( name = 'ZO4D_99_QRCODE'    type = 'img' )
      " ZO4D_30_EARULEZ and ZO4D_31_OLELUK use vector notes, no image needed
    ).
  ENDMETHOD.


  METHOD zif_o4d_effect~get_params.
  ENDMETHOD.


  METHOD zif_o4d_effect~set_param.
  ENDMETHOD.


  METHOD zif_o4d_effect~is_loopable.
    rv_loopable = abap_true.  " Gallery is designed for infinite loop
  ENDMETHOD.


  METHOD int_to_hex.
    DATA lv_hex TYPE x LENGTH 1.
    lv_hex = iv_int.
    rv_hex = lv_hex.
  ENDMETHOD.


  METHOD init_slides.
    mt_slides = VALUE #(
      ( name = 'ZO4D_30_EARULEZ'    title = 'MAIN MUSIC'         desc = 'EA Rulez! by Oisee'           classname = 'AY-8910 1999 / rmx 2025' long = abap_true )
      ( name = 'ZO4D_31_OLELUK'     title = 'OUTRO MUSIC'        desc = 'Ole Lukøjle by Oisee'         classname = 'AY-8910 2001 / rmx 2025' long = abap_true )
      ( name = 'ZO4D_00_SALES'      title = 'SAP SALES CHARTS'   desc = 'Enterprise meets Demoscene'   classname = 'ZCL_O4D_SALES_DANCE' )
      ( name = 'ZO4D_02_IGNITE'     title = 'IGNITION'           desc = 'Particle explosion effect'    classname = 'ZCL_O4D_IGNITION' )
      ( name = 'ZO4D_05_COPPER'     title = 'COPPER BARS'        desc = 'Classic Amiga effect'         classname = 'ZCL_O4D_COPPERBARS' )
      ( name = 'ZO4D_06_PLASMA'     title = 'PLASMA'             desc = 'Oldschool color cycling'      classname = 'ZCL_O4D_PLASMA' )
      ( name = 'ZO4D_07_TWISTER'    title = 'TWIST ZOOMER'       desc = 'Rotating spiral tunnel'       classname = 'ZCL_O4D_TWISTZOOMER' )
      ( name = 'ZO4D_08_MOUNTAINS'  title = 'MOUNTAINS'          desc = 'Fractal landscape'            classname = 'ZCL_O4D_MOUNTAINS_OOPS' )
      ( name = 'ZO4D_09_ROTOZOOM'   title = 'ROTOZOOM'           desc = 'Texture rotation + zoom'      classname = 'ZCL_O4D_ROTOZOOM' )
      ( name = 'ZO4D_10_VOXEL'      title = 'VOXEL LANDSCAPE'    desc = 'Comanche-style terrain'       classname = 'ZCL_O4D_VOXEL' )
      ( name = 'ZO4D_11_ROTOPLASMA' title = 'ROTO + PLASMA'      desc = 'Combined effects'             classname = 'ZCL_O4D_ROTOZOOM_PLASMA' )
      ( name = 'ZO4D_12_TESSERACT'  title = 'TESSERACT'          desc = '4D Hypercube projection'      classname = 'ZCL_O4D_TESSERACT' )
      ( name = 'ZO4D_13_CELL24'     title = '24-CELL'            desc = '4D Polytope - 24 vertices'    classname = 'ZCL_O4D_CELL24' )
      ( name = 'ZO4D_14_CELL16'     title = '16-CELL'            desc = '4D Polytope - Hyperoctahedron' classname = 'ZCL_O4D_CELL16' )
      ( name = 'ZO4D_15_CELL120'    title = '120-CELL'           desc = '4D Polytope - 600 vertices'   classname = 'ZCL_O4D_CELL120' )
      ( name = 'ZO4D_17_AMIGABALL'  title = 'AMIGA BALL'         desc = 'The iconic bouncing ball'     classname = 'ZCL_O4D_AMIGABALL' )
      ( name = 'ZO4D_18_GLITCH'     title = 'GLITCH'             desc = 'Digital corruption art'       classname = 'ZCL_O4D_GLITCH' )
      ( name = 'ZO4D_19_SIERPINSKI' title = 'SIERPINSKI'         desc = 'Menger Sponge'                classname = 'ZCL_O4D_MENGER' )
      ( name = 'ZO4D_20_NEONCITY'   title = 'NEON CITY'          desc = 'Cyberpunk skyline'            classname = 'ZCL_O4D_NEON_CITY' )
      ( name = 'ZO4D_21_JOYDIV'     title = 'JOY DIVISION'       desc = 'Unknown Pleasures tribute'    classname = 'ZCL_O4D_JOYDIV' )
      ( name = 'ZO4D_22_SIERPTET'   title = 'SIERPINSKI 3D'      desc = 'Fractal tetrahedron'          classname = 'ZCL_O4D_SIERPINSKI_TET' )
      ( name = 'ZO4D_23_QUATJULIA'  title = 'QUATERNION JULIA'   desc = '4D fractal raymarched'        classname = 'ZCL_O4D_QUAT_JULIA' )
      ( name = 'ZO4D_24_METABALLS'  title = 'METABALLS'          desc = 'SDF blob fusion'              classname = 'ZCL_O4D_SDF_BLOBS' )
      ( name = 'ZO4D_25_TORUS'      title = '3D TORUS'           desc = 'Raymarched donut'             classname = 'ZCL_O4D_TORUS_3D' )
      ( name = 'ZO4D_26_JULIAMORPH' title = 'JULIA MORPH'        desc = 'Animated fractal'             classname = 'ZCL_O4D_JULIA_MORPH' )
      ( name = 'ZO4D_27_CONSTELL'   title = 'CONSTELLATION'      desc = 'Connected star field'         classname = 'ZCL_O4D_CONSTELLATION' )
      ( name = 'ZO4D_99_QRCODE'     title = 'VIBING STEAMPUNK'   desc = 'github.com/oisee'             classname = 'Developed With Claude' long = abap_true )
    ).
  ENDMETHOD.


  METHOD get_frame_3d.
    DATA: lv_depth_x TYPE f,
          lv_depth_y TYPE f,
          lv_rot     TYPE f,
          lv_wobble  TYPE f,
          lv_bx1 TYPE f, lv_by1 TYPE f,
          lv_bx2 TYPE f, lv_by2 TYPE f.

    lv_rot = iv_time * CONV f( '0.3' ).
    lv_wobble = sin( iv_time * 2 ) * 5.
    lv_depth_x = cos( lv_rot ) * iv_depth + lv_wobble.
    lv_depth_y = sin( lv_rot ) * iv_depth * CONV f( '0.3' ).

    lv_bx1 = lv_depth_x.
    lv_by1 = lv_depth_y.
    lv_bx2 = iv_img_w + lv_depth_x.
    lv_by2 = iv_img_h + lv_depth_y.

    DATA(lv_frame_color) = '#0088AA'.
    DATA(lv_back_color) = '#004466'.

    " Depth lines
    APPEND VALUE zif_o4d_effect=>ty_line( x1 = 0 y1 = 0 x2 = lv_bx1 y2 = lv_by1 color = lv_frame_color width = 1 ) TO rt_lines.
    APPEND VALUE zif_o4d_effect=>ty_line( x1 = iv_img_w y1 = 0 x2 = lv_bx2 y2 = lv_by1 color = lv_frame_color width = 1 ) TO rt_lines.
    APPEND VALUE zif_o4d_effect=>ty_line( x1 = 0 y1 = iv_img_h x2 = lv_bx1 y2 = lv_by2 color = lv_frame_color width = 1 ) TO rt_lines.
    APPEND VALUE zif_o4d_effect=>ty_line( x1 = iv_img_w y1 = iv_img_h x2 = lv_bx2 y2 = lv_by2 color = lv_frame_color width = 1 ) TO rt_lines.

    " Back face
    APPEND VALUE zif_o4d_effect=>ty_line( x1 = lv_bx1 y1 = lv_by1 x2 = lv_bx2 y2 = lv_by1 color = lv_back_color width = 1 ) TO rt_lines.
    APPEND VALUE zif_o4d_effect=>ty_line( x1 = lv_bx2 y1 = lv_by1 x2 = lv_bx2 y2 = lv_by2 color = lv_back_color width = 1 ) TO rt_lines.
    APPEND VALUE zif_o4d_effect=>ty_line( x1 = lv_bx2 y1 = lv_by2 x2 = lv_bx1 y2 = lv_by2 color = lv_back_color width = 1 ) TO rt_lines.
    APPEND VALUE zif_o4d_effect=>ty_line( x1 = lv_bx1 y1 = lv_by2 x2 = lv_bx1 y2 = lv_by1 color = lv_back_color width = 1 ) TO rt_lines.
  ENDMETHOD.


  METHOD get_scanlines.
    DATA: lv_y      TYPE i,
          lv_offset TYPE f,
          lv_alpha  TYPE i.

    lv_offset = iv_time * 50.
    lv_y = iv_y_start.

    WHILE lv_y < iv_y_end.
      IF ( lv_y + CONV i( lv_offset ) ) MOD 4 = 0.
        lv_alpha = 15 + CONV i( sin( CONV f( lv_y ) / 10 + iv_time * 3 ) * 10 ).
        APPEND VALUE zif_o4d_effect=>ty_line(
          x1 = 40  y1 = lv_y  x2 = 385  y2 = lv_y
          color = |rgba(0, 255, 255, { CONV f( lv_alpha ) / 100 })|
          width = 1
        ) TO rt_lines.
      ENDIF.
      lv_y = lv_y + 2.
    ENDWHILE.
  ENDMETHOD.
ENDCLASS.
