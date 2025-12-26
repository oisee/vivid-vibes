CLASS zcl_o4d_apc_handler DEFINITION
  PUBLIC INHERITING FROM cl_apc_wsp_ext_stateful_base FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS if_apc_wsp_extension~on_accept REDEFINITION.
    METHODS if_apc_wsp_extension~on_start  REDEFINITION.
    METHODS if_apc_wsp_extension~on_message REDEFINITION.
    METHODS if_apc_wsp_extension~on_close  REDEFINITION.
    METHODS if_apc_wsp_extension~on_error  REDEFINITION.


  PRIVATE SECTION.
    CONSTANTS: c_ticks_per_bar TYPE i VALUE 64,
               c_mode_viewer   TYPE string VALUE 'viewer',
               c_mode_dev      TYPE string VALUE 'dev'.

    DATA: mo_demo             TYPE REF TO zcl_o4d_demo,
          mv_mode             TYPE string VALUE 'viewer',
          mv_running          TYPE abap_bool,
          mv_frame_num        TYPE i,
          mv_last_effect      TYPE string,
          mv_effect_start_bar TYPE i,
          ms_flash_pending    TYPE zif_o4d_effect=>ty_flash.  " Pending flash from client

    " Effect instances (shared across demos)
    DATA: mo_starfield     TYPE REF TO zcl_o4d_starfield, mo_copperbars TYPE REF TO zcl_o4d_copperbars,
          mo_joydiv        TYPE REF TO zcl_o4d_joydivision,  mo_plasma TYPE REF TO zcl_o4d_plasma,
          mo_tunnel        TYPE REF TO zcl_o4d_tunnel,       mo_elite3d TYPE REF TO zcl_o4d_elite3d,
          mo_metaballs     TYPE REF TO zcl_o4d_metaballs, mo_twister TYPE REF TO zcl_o4d_twister,
          mo_constellation TYPE REF TO zcl_o4d_constellation, mo_sales_quarter TYPE REF TO zcl_o4d_sales_quarter,
          mo_starburst     TYPE REF TO zcl_o4d_starburst, mo_voxel TYPE REF TO zcl_o4d_voxel_landscape,
          mo_kaleidoscope  TYPE REF TO zcl_o4d_kaleidoscope,
          mo_ignition      TYPE REF TO zcl_o4d_ignition, mo_amigaball TYPE REF TO zcl_o4d_amigaball,
          mo_amigaball2    TYPE REF TO zcl_o4d_amigaball2,
          mo_glitch        TYPE REF TO zcl_o4d_glitch, mo_firegreetings TYPE REF TO zcl_o4d_firegreetings,
          mo_mountains     TYPE REF TO zcl_o4d_mountains, mo_swirl TYPE REF TO zcl_o4d_swirl,
          mo_greetings     TYPE REF TO zcl_o4d_greetings,
          mo_sierpinski    TYPE REF TO zcl_o4d_sierpinski,
          mo_lowpoly       TYPE REF TO zcl_o4d_lowpoly,
          mo_rotozoom      TYPE REF TO zcl_o4d_rotozoom,
          mo_rotozoom_fire TYPE REF TO zcl_o4d_rotozoom_fire,
          mo_tornado       TYPE REF TO zcl_o4d_tornado_greetings,
          mo_flash_greet   TYPE REF TO zcl_o4d_flash_greetings,
          mo_joy_division  TYPE REF TO zcl_o4d_joy_division,
          mo_joy_div_v2    TYPE REF TO zcl_o4d_joy_div,
          mo_voxel_land    TYPE REF TO zcl_o4d_voxel_land,
          mo_tunnel_v2     TYPE REF TO zcl_o4d_tunnel,
          mo_sales_dance   TYPE REF TO zcl_o4d_sales_dance,
          mo_ignite_emit   TYPE REF TO zcl_o4d_ignite_emit,
          mo_twistzoomer   TYPE REF TO zcl_o4d_twistzoomer,
          mo_rotozoom_sine TYPE REF TO zcl_o4d_rotozoom_sine,
          mo_mnt_oops      TYPE REF TO zcl_o4d_mountains_oops.

    METHODS: init_all_demos,
      create_effects,
      build_demo_main,
      build_demo_outro,
      load_demo IMPORTING iv_demo_id TYPE string,
      calc_beat_info IMPORTING iv_time TYPE f RETURNING VALUE(rs_bi) TYPE zif_o4d_effect=>ty_beat_info,
      build_render_ctx IMPORTING iv_global_time TYPE f RETURNING VALUE(rs_ctx) TYPE zif_o4d_effect=>ty_render_ctx,
      send_text IMPORTING i_message_manager TYPE REF TO if_apc_wsp_message_manager iv_text TYPE string,
      send_frame IMPORTING i_message_manager TYPE REF TO if_apc_wsp_message_manager iv_global_time TYPE f,
      send_config IMPORTING i_message_manager TYPE REF TO if_apc_wsp_message_manager,
      send_megademo IMPORTING i_message_manager TYPE REF TO if_apc_wsp_message_manager,
      frame_to_json IMPORTING is_frame TYPE zif_o4d_effect=>ty_frame is_ctx TYPE zif_o4d_effect=>ty_render_ctx
                              iv_effect_name TYPE string OPTIONAL
                    RETURNING VALUE(rv_json) TYPE string,
      handle_json_cmd IMPORTING i_message_manager TYPE REF TO if_apc_wsp_message_manager iv_json TYPE string,
      send_scenario IMPORTING i_message_manager TYPE REF TO if_apc_wsp_message_manager,
      seek_to_bar IMPORTING iv_bar TYPE f,
      seek_to_frame IMPORTING iv_frame TYPE i,
      send_preload_frame IMPORTING i_message_manager TYPE REF TO if_apc_wsp_message_manager iv_frame TYPE i,
      "! Build render context statelessly (for preload - no session state dependency)
      build_preload_ctx IMPORTING iv_global_time TYPE f iv_frame TYPE i
                        RETURNING VALUE(rs_ctx) TYPE zif_o4d_effect=>ty_render_ctx.
ENDCLASS.



CLASS ZCL_O4D_APC_HANDLER IMPLEMENTATION.


  METHOD if_apc_wsp_extension~on_accept. e_connect_mode = co_connect_mode_accept. ENDMETHOD.


  METHOD if_apc_wsp_extension~on_start.
    init_all_demos( ).
    load_demo( 'main' ).
    mv_running = abap_true. mv_frame_num = 0. mv_last_effect = ''. mv_effect_start_bar = 0.
    mv_mode = c_mode_viewer.
    send_config( i_message_manager ).
  ENDMETHOD.


  METHOD init_all_demos.
    " Create all effects once
    create_effects( ).
    " Build and register all demos
    build_demo_main( ).
    build_demo_outro( ).
  ENDMETHOD.


  METHOD create_effects.
    mo_starfield = NEW #( ).
    mo_copperbars = NEW #( iv_speed = '1' iv_bar_height = 48 ).
    mo_joydiv = NEW #( ).
    mo_plasma = NEW #( ).
    mo_tunnel = NEW #( ).
    mo_elite3d = NEW #( ).
    mo_metaballs = NEW #( ).
    mo_twister = NEW #( ).
    mo_constellation = NEW #( ).
    mo_sales_quarter = NEW #( ).
    mo_starburst = NEW #( ).
    mo_voxel = NEW #( ).
    mo_kaleidoscope = NEW #( ).
    mo_rotozoom = NEW #( ).
    mo_ignition = NEW #( ).
    mo_amigaball = NEW #( ).
    mo_amigaball2 = NEW #( iv_cycle_bars = 4 ).
    mo_glitch = NEW #( ).
    mo_firegreetings = NEW #( ).
    mo_mountains = NEW #( ).
    mo_swirl = NEW #( ).
    mo_greetings = NEW #( ).
    mo_sierpinski = NEW #( ).
    mo_lowpoly = NEW #( ).
    "mo_tornado = NEW #( ).
    mo_flash_greet = NEW #( ).
    mo_joy_division = NEW #( ).
    mo_joy_div_v2 = NEW #( ).
    mo_voxel_land = NEW #( ).
    mo_tunnel_v2 = NEW #( ).
    mo_sales_dance = NEW #( ).
    mo_ignite_emit = NEW #( ).
    mo_twistzoomer = NEW #( ).
    mo_rotozoom_sine = NEW #( ).
    mo_rotozoom_fire = NEW #( ).
    mo_mnt_oops      = NEW #( ).


    mo_starfield->zif_o4d_effect~set_param( iv_name = 'num_stars' iv_value = '200' ).
    mo_tunnel->zif_o4d_effect~set_param( iv_name = 'num_rings' iv_value = '25' ).
  ENDMETHOD.


  METHOD build_demo_main.
    DATA(lo_demo) = zcl_o4d_demo=>new(
      iv_id    = 'main'
      iv_name  = 'Vivid-Vibes'
      iv_bpm   = 152
      iv_fpt   = 1 " 2 frames per tick → ~40 fps
      iv_audio = 'ZOISEE-EAR-02.MP3'
    ).

    " 4D Hypercube
    DATA(lo_tess)        = zcl_o4d_tesseract=>new( iv_size = 120 iv_line_width = 4 ).
    DATA(lo_pentachoron) = zcl_o4d_pentachoron=>new( iv_line_width = 4 ).
    DATA(lo_cell24)      = zcl_o4d_cell24=>new( iv_line_width = 4 ).
    DATA(lo_cell16)      = zcl_o4d_cell16=>new( iv_line_width = 4 ).
    DATA(lo_cell120)     = zcl_o4d_cell120=>new( iv_line_width = 4 ).
    DATA(lo_simplex5)    = zcl_o4d_simplex5=>new( iv_line_width = 4 ).
    DATA(lo_stet)        = zcl_o4d_sierpinski_tet=>new(
*                             iv_depth      = 4
*                             iv_size       = 180
*                             iv_line_width = 2
                           ).

    " Fractal Tree (grows over 8 bars)
    DATA(lo_tree)  = zcl_o4d_lsystem=>new( iv_angle = 25 iv_depth = 6 ).
    " Letter morphing
*  DATA(lo_sap)   = zcl_o4d_scanmorph=>new( iv_text = 'SAP' iv_bars = 3 ).
    DATA(lo_moire) = zcl_o4d_moire=>new( iv_lines = 40 iv_grids = 5 ).

*--------------------------------------------------------------------*
    DATA lt_scenes TYPE zcl_o4d_demo=>tt_scene_defs.
    "    APPEND VALUE #( len = 4  effect = lo_sap  )   TO lt_scenes.
*    APPEND VALUE #( len = 4  effect = lo_abap )   TO lt_scenes.
*    APPEND VALUE #( len = 4  effect = lo_tess )   TO lt_scenes.
*    APPEND VALUE #( len = 4  effect = lo_tree )   TO lt_scenes.
*    APPEND VALUE #( len = 4  effect = lo_moire  )   TO lt_scenes.
*   01-08 no beat
    APPEND VALUE #( len = 8  effect = mo_sales_dance )   TO lt_scenes.
*   08-16 first beats (+flash)
    APPEND VALUE #( len = 1  effect = mo_ignition    )   TO lt_scenes.
    APPEND VALUE #( len = 1  effect = mo_ignite_emit )   TO lt_scenes.
    APPEND VALUE #( len = 1  effect = mo_ignition    )   TO lt_scenes.
    APPEND VALUE #( len = 1  effect = mo_ignite_emit )   TO lt_scenes.
    APPEND VALUE #( len = 4  effect = mo_copperbars )    TO lt_scenes.

*   16-24 beats & move (+flash)
    APPEND VALUE #( len = 4  effect = mo_plasma )        TO lt_scenes.
    APPEND VALUE #( len = 4  effect = mo_twistzoomer )   TO lt_scenes.

*   24-32 pre-bulding tension
*   APPEND VALUE #( len = 4  effect = mo_constellation ) TO lt_scenes.
    APPEND VALUE #( len = 8  effect = NEW zcl_o4d_mountains_oops( )  )      TO lt_scenes.
    "APPEND VALUE #( len = 4  effect = mo_voxel )         TO lt_scenes.
*    APPEND VALUE #( len = 2  effect = new zcl_o4d_mountains_round( ) )      TO lt_scenes.
*    APPEND VALUE #( len = 2  effect = new zcl_o4d_mountains_sharp( ) )      TO lt_scenes.

*   32-40 Tension Resolution
    "APPEND VALUE #( len = 4  effect = mo_metaballs )    TO lt_scenes.
    APPEND VALUE #( len = 8  effect = mo_rotozoom )      TO lt_scenes.

*   40-48 relax
    "    APPEND VALUE #( len = 8  effect = mo_voxel )         TO lt_scenes.
    APPEND VALUE #( len = 4  effect = mo_voxel )         TO lt_scenes.
    "APPEND VALUE #( len = 4  effect = mo_rotozoom_sine ) TO lt_scenes.
    APPEND VALUE #( len = 4 effect = zcl_o4d_rotozoom_plasma=>new(
                    iv_layers = 2
                    ) ) TO lt_scenes.

*   48-56 relax + melody
    APPEND VALUE #( len = 2  effect = lo_tess )   TO lt_scenes.
    APPEND VALUE #( len = 2  effect = lo_cell24      )   TO lt_scenes.
    APPEND VALUE #( len = 1  effect = lo_cell16      )   TO lt_scenes.
    APPEND VALUE #( len = 3  effect = lo_cell120     )   TO lt_scenes.

*  DATA(lo_cell24)      = zcl_o4d_cell24=>new( ).
*  DATA(lo_cell16)      = zcl_o4d_cell16=>new( ).
*  DATA(lo_cell120)     = zcl_o4d_cell120=>new( ).
*  DATA(lo_simplex5)    = zcl_o4d_simplex5=>new( ).


*   56-64 melody-move
    APPEND VALUE #( len = 4  effect = mo_amigaball ) TO lt_scenes.
    APPEND VALUE #( len = 3  effect = mo_amigaball2 ) TO lt_scenes.
    APPEND VALUE #( len = 1  effect = mo_glitch )        TO lt_scenes.

*   64-72 melody-move
    APPEND VALUE #( len = 8  effect = mo_sierpinski ) TO lt_scenes.
*   72-80 arpeggio
    APPEND VALUE #( len = 4  effect = zcl_o4d_neon_city=>new( 32 ) ) TO lt_scenes.
    APPEND VALUE #( len = 4  effect = mo_joydiv )        TO lt_scenes.
    APPEND VALUE #( len = 8  effect = lo_stet ) TO lt_scenes.
*--------------------------------------------------------------------*
*   80-88 summit
    "APPEND VALUE #( len = 4  effect = zcl_o4d_mountains_sunrise=>new( ) )     TO lt_scenes.
*    APPEND VALUE #( len = 4 effect = zcl_o4d_cave_tunnel=>new( iv_layers = 1 ) ) TO lt_scenes.
    "APPEND VALUE #( len = 4  effect = mo_elite3d )       TO lt_scenes.
    APPEND VALUE #( len = 8 effect = zcl_o4d_quat_julia=>new(
                    iv_resolution = 24
                    iv_max_iter   = 8
                    iv_threshold  = 2
                    iv_size       = 150
                    ) ) TO lt_scenes.

    APPEND VALUE #( len = 4 effect = zcl_o4d_sdf_blobs=>new(
                    iv_num_blobs  = 7
                    iv_resolution = 40
                    iv_smoothness = 30
                    ) ) TO lt_scenes.



    APPEND VALUE #( len = 4 effect = zcl_o4d_torus_3d=>new(
*                                       iv_resolution   = 80
*                                       iv_major_radius = 70
*                                       iv_minor_radius = 28
                    ) ) TO lt_scenes.
*    APPEND VALUE #( len = 4 effect = zcl_o4d_fire_twirl=>new(
**                                       iv_resolution = 64
**                                       iv_zoom_speed = '0.5'
**                                       iv_twist      = 3
*                    ) ) TO lt_scenes.

*    APPEND VALUE #( len = 4 effect = zcl_o4d_fractal_fly=>new(
**                                       iv_resolution = 64
**                                       iv_max_iter   = 32
*                    ) ) TO lt_scenes.



    APPEND VALUE #( len = 8 effect = zcl_o4d_julia_morph=>new(
                    iv_resolution = 128
                    iv_max_iter   = 16
                    ) ) TO lt_scenes.

    APPEND VALUE #( len = 8  effect = mo_constellation )   TO lt_scenes.

*    APPEND VALUE #( len = 8 effect = zcl_o4d_torus_knot=>new(
*                    iv_p          = 2
*                    iv_q          = 3
**                                        iv_segments   = 300
**                                        iv_tube_segs  = 8
**                                        iv_radius     = 100
**                                        iv_tube_r     = 25
*                    iv_line_width = 2
*                    ) ) TO lt_scenes.

*   88-96 ?
*    APPEND VALUE #( len = 8 effect = zcl_o4d_parallax_greets=>new(
**                                        iv_speed_base = 80
*                    ) ) TO lt_scenes.
*    APPEND VALUE #( len = 8  effect = mo_firegreetings ) TO lt_scenes.
*    "   APPEND VALUE #( len = 2  effect = mo_lowpoly )       TO lt_scenes.
**   APPEND VALUE #( len = 2  effect = mo_glitch )        TO lt_scenes.
**   APPEND VALUE #( len = 6  effect = mo_sierpinski ) TO lt_scenes.
*
**   88-96 ?
*    APPEND VALUE #( len = 4  effect = mo_metaballs ) TO lt_scenes.
*    APPEND VALUE #( len = 8  effect = mo_twister )   TO lt_scenes.
*    APPEND VALUE #( len = 4  effect = mo_greetings ) TO lt_scenes.

    lo_demo->add_scenes( lt_scenes ).
  ENDMETHOD.


  METHOD build_demo_outro.
    DATA(lo_demo) = zcl_o4d_demo=>new(
      iv_id    = 'outro'
      iv_name  = 'EAR ASSAULT II - CREDITS'
      iv_bpm   = 94
      iv_fpt   = 3  " 2 frames per tick → ~50 fps
      iv_audio = 'ZOISEE-OUTRO.MP3'
      iv_loop  = abap_true
    ).

    DATA(lo_twister) = NEW zcl_o4d_twister(
      iv_speed = '-3'
    ).
    DATA(lo_twister3) = NEW zcl_o4d_twister(
      iv_speed = '3'
    ).

    DATA lt_scenes TYPE zcl_o4d_demo=>tt_scene_defs.
*   APPEND VALUE #( len = 6  effect = mo_starfield ) TO lt_scenes.
*   APPEND VALUE #( len = 2 effect = lo_twister   ) TO lt_scenes.
    "APPEND VALUE #( len = 4 effect = new zcl_o4d_voxel_land( )   ) TO lt_scenes.
    APPEND VALUE #( len = 44 effect = zcl_o4d_gallery=>new(
      iv_scroll_text = `                                                                                                                      ` &&
      `                                                                                                                                       ` &&
      `                                                                                                                                       ` &&
      `                                                                                                                                       ` &&
      `                                                                                                                                       ` &&
      | 8-bit beam of greetings goes to:| &&
      | Scott Hanselman, Paul Modderman, Jelena Perfiljeva, Fred Huet, Holger Bruchelt, Dr. Philip Herzig, Level 9, Infocom, Amit Lal, Prof. Dr. Alexander Zeier, Marian Zeis,| &&
      | Lars Hvam Petersen, Anthropic, Volker Buzek, Camunda, Filipp Gnilyak, Claude, Parazite, Bizhuka, Sq, Kq, Thomas Jung, Enno Wulff, HallycinoJen, KiM BBS, Marcello Urbani, Martin Pankraz, Emma Qian,| &&
      | Florian Farr, S. Novikov, Megus, SAP, Devraj Bardhan, IBM, Random/CC, Nora von Thenen, TSL, Ivan Pirog, Nik-O, G_D, JtN, CyberJack, 4D, Triebkraft, Stardust, Gasman, BaZe,| &&
      | Nova, Aki, Arwel Owen, Edgar Martinez, Dirk Roeckmann, Robin van het Hof, Yurii Sychov, Aλex Nihirash, and you! Thank you for your time! Happy new year! Alice V.| &&
      | This Oldschool ABAP demo "Vibing Vibes" has been build with "Vibing-Steampunk" and Claude Code CLI|
    ) )  TO lt_scenes.
*
*
*    APPEND VALUE #( len = 4 effect = NEW zcl_o4d_voxel_landscape( )   ) TO lt_scenes.
*    APPEND VALUE #( len        = 4 effect = NEW zcl_o4d_moire(
*                                          iv_lines = 27
*                                          iv_grids = 7
*                    ) ) TO lt_scenes.
**    APPEND VALUE #( len = 8 effect = zcl_o4d_scanmorph=>new(
**                                       iv_text      = 'SAP'
**                                       iv_bars      = 4
**                                       iv_scanlines = 64
**                                     )  ) TO lt_scenes.
*    APPEND VALUE #( len = 4 effect = NEW zcl_o4d_waveform( )  ) TO lt_scenes.
*    APPEND VALUE #( len        = 4 effect = NEW zcl_o4d_flash_greetings( ) )  TO lt_scenes.
*    APPEND VALUE #( len = 4  effect = mo_starfield ) TO lt_scenes.
    lo_demo->add_scenes( lt_scenes ).
  ENDMETHOD.


  METHOD load_demo.
    mo_demo = zcl_o4d_demo=>get( iv_demo_id ).
    IF mo_demo IS NOT BOUND.
      mo_demo = zcl_o4d_demo=>get( 'main' ).
    ENDIF.
    mv_frame_num = 0. mv_last_effect = ''. mv_effect_start_bar = 0.
  ENDMETHOD.


  METHOD send_config.
    DATA(lv_cfg) = |\{"type":"config","fps":{ mo_demo->get_fps( ) },"fpt":{ mo_demo->get_fpt( ) },"bpm":{ mo_demo->get_bpm( ) },| &&
                   |"bar_sec":{ mo_demo->get_bar_sec( ) },"beat_sec":{ mo_demo->get_beat_sec( ) },| &&
                   |"total_bars":{ mo_demo->get_total_bars( ) },"audio":"{ mo_demo->get_audio( ) }",| &&
                   |"width":640,"height":400,"name":"{ mo_demo->get_name( ) }","demo":"{ mo_demo->get_id( ) }"\}|.
    send_text( i_message_manager = i_message_manager iv_text = lv_cfg ).
  ENDMETHOD.


  METHOD send_megademo.
    " Generate megademo list dynamically from registry
    DATA: lv_parts TYPE string.
    DATA(lt_ids) = zcl_o4d_demo=>get_all_ids( ).

    lv_parts = |[|.
    LOOP AT lt_ids INTO DATA(lv_id).
      DATA(lo_demo) = zcl_o4d_demo=>get( lv_id ).
      IF lo_demo IS BOUND.
        IF sy-tabix > 1. lv_parts = lv_parts && |,|. ENDIF.
        lv_parts = lv_parts &&
          |\{"id":"{ lo_demo->get_id( ) }","name":"{ lo_demo->get_name( ) }",| &&
          |"bpm":{ lo_demo->get_bpm( ) },"bars":{ lo_demo->get_total_bars( ) },| &&
          |"audio":"{ lo_demo->get_audio( ) }"\}|.
      ENDIF.
    ENDLOOP.
    lv_parts = lv_parts && |]|.

    DATA(lv_json) = |\{"type":"megademo","name":"EAR ASSAULT II - MEGADEMO","parts":{ lv_parts }\}|.
    send_text( i_message_manager = i_message_manager iv_text = lv_json ).
  ENDMETHOD.


  METHOD if_apc_wsp_extension~on_message.
    TRY.
        DATA(lv_cmd) = i_message->get_text( ).
        IF strlen( lv_cmd ) > 0 AND lv_cmd+0(1) = '{'.
          handle_json_cmd( i_message_manager = i_message_manager iv_json = lv_cmd ).
          RETURN.
        ENDIF.
        CASE lv_cmd.
          WHEN 'start'. mv_running = abap_true.
          WHEN 'stop'. mv_running = abap_false.
          WHEN 'frame'.
            IF mv_running = abap_true.
              DATA(lv_gt) = CONV f( mv_frame_num ) / mo_demo->get_fps( ).
              send_frame( i_message_manager = i_message_manager iv_global_time = lv_gt ).
              mv_frame_num = mv_frame_num + 1.
            ENDIF.
          WHEN 'reset'. mv_frame_num = 0. mv_last_effect = ''. mv_effect_start_bar = 0.
          WHEN 'scenario'. send_scenario( i_message_manager ).
        ENDCASE.
      CATCH cx_apc_error.
    ENDTRY.
  ENDMETHOD.


  METHOD handle_json_cmd.
    DATA: lv_cmd   TYPE string, lv_mode TYPE string, lv_bar TYPE f, lv_frame TYPE i,
          lv_pos   TYPE i, lv_end TYPE i, lv_val TYPE string, lv_len TYPE i.

    lv_pos = find( val = iv_json sub = '"cmd":"' ).
    IF lv_pos >= 0.
      lv_pos = lv_pos + 7.
      lv_end = find( val = iv_json off = lv_pos sub = '"' ).
      IF lv_end > lv_pos. lv_len = lv_end - lv_pos. lv_cmd = iv_json+lv_pos(lv_len). ENDIF.
    ENDIF.

    CASE lv_cmd.
      WHEN 'set_mode'.
        lv_pos = find( val = iv_json sub = '"mode":"' ).
        IF lv_pos >= 0.
          lv_pos = lv_pos + 8.
          lv_end = find( val = iv_json off = lv_pos sub = '"' ).
          IF lv_end > lv_pos. lv_len = lv_end - lv_pos. lv_mode = iv_json+lv_pos(lv_len). ENDIF.
        ENDIF.
        IF lv_mode = 'dev' OR lv_mode = 'viewer'.
          mv_mode = lv_mode.
          send_text( i_message_manager = i_message_manager iv_text = |\{"type":"mode","mode":"{ mv_mode }"\}| ).
        ENDIF.

      WHEN 'get_scenario'.
        send_scenario( i_message_manager ).

      WHEN 'seek'.
        lv_pos = find( val = iv_json sub = '"bar":' ).
        IF lv_pos >= 0.
          lv_pos = lv_pos + 6. lv_end = lv_pos.
          WHILE lv_end < strlen( iv_json ) AND iv_json+lv_end(1) CO '0123456789.'. lv_end = lv_end + 1. ENDWHILE.
          IF lv_end > lv_pos. lv_len = lv_end - lv_pos. lv_val = iv_json+lv_pos(lv_len). lv_bar = lv_val. seek_to_bar( lv_bar ). ENDIF.
        ELSE.
          lv_pos = find( val = iv_json sub = '"frame":' ).
          IF lv_pos >= 0.
            lv_pos = lv_pos + 8. lv_end = lv_pos.
            WHILE lv_end < strlen( iv_json ) AND iv_json+lv_end(1) CO '0123456789'. lv_end = lv_end + 1. ENDWHILE.
            IF lv_end > lv_pos. lv_len = lv_end - lv_pos. lv_val = iv_json+lv_pos(lv_len). lv_frame = lv_val. seek_to_frame( lv_frame ). ENDIF.
          ENDIF.
        ENDIF.
        send_text( i_message_manager = i_message_manager iv_text = |\{"type":"seeked","frame":{ mv_frame_num }\}| ).

      WHEN 'frame'.
        " Tick-based timing: {"cmd":"frame","tick":5,"sub":0}
        DATA: lv_tick TYPE i, lv_sub TYPE i.
        lv_pos = find( val = iv_json sub = '"tick":' ).
        IF lv_pos >= 0.
          lv_pos = lv_pos + 7. lv_end = lv_pos.
          WHILE lv_end < strlen( iv_json ) AND iv_json+lv_end(1) CO '0123456789'. lv_end = lv_end + 1. ENDWHILE.
          IF lv_end > lv_pos. lv_len = lv_end - lv_pos. lv_tick = iv_json+lv_pos(lv_len). ENDIF.
        ENDIF.
        lv_pos = find( val = iv_json sub = '"sub":' ).
        IF lv_pos >= 0.
          lv_pos = lv_pos + 6. lv_end = lv_pos.
          WHILE lv_end < strlen( iv_json ) AND iv_json+lv_end(1) CO '0123456789'. lv_end = lv_end + 1. ENDWHILE.
          IF lv_end > lv_pos. lv_len = lv_end - lv_pos. lv_sub = iv_json+lv_pos(lv_len). ENDIF.
        ENDIF.
        " time = (tick + sub/fpt) * sec_per_tick
        DATA(lv_fpt) = mo_demo->get_fpt( ).
        DATA(lv_gt2) = ( CONV f( lv_tick ) + CONV f( lv_sub ) / lv_fpt ) * mo_demo->get_sec_per_tick( ).
        mv_frame_num = lv_tick * lv_fpt + lv_sub.
        send_frame( i_message_manager = i_message_manager iv_global_time = lv_gt2 ).

      WHEN 'preload'.
        lv_pos = find( val = iv_json sub = '"frame":' ).
        IF lv_pos >= 0.
          lv_pos = lv_pos + 8. lv_end = lv_pos.
          WHILE lv_end < strlen( iv_json ) AND iv_json+lv_end(1) CO '0123456789'. lv_end = lv_end + 1. ENDWHILE.
          IF lv_end > lv_pos.
            lv_len = lv_end - lv_pos. lv_val = iv_json+lv_pos(lv_len). lv_frame = lv_val.
            send_preload_frame( i_message_manager = i_message_manager iv_frame = lv_frame ).
          ENDIF.
        ENDIF.

      WHEN 'get_megademo'.
        send_megademo( i_message_manager ).

      WHEN 'load_demo'.
        DATA lv_demo_id TYPE string.
        lv_pos = find( val = iv_json sub = '"demo":"' ).
        IF lv_pos >= 0.
          lv_pos = lv_pos + 8. lv_end = find( val = iv_json off = lv_pos sub = '"' ).
          IF lv_end > lv_pos.
            lv_len = lv_end - lv_pos. lv_demo_id = iv_json+lv_pos(lv_len).
            load_demo( lv_demo_id ).
            send_config( i_message_manager ).
          ENDIF.
        ENDIF.

      WHEN 'flash'.
        " Client triggered flash: {"cmd":"flash","intensity":0.8,"r":1,"g":1,"b":1}
        DATA: lv_intensity TYPE f VALUE '1.0', lv_r TYPE f VALUE '1.0',
              lv_g         TYPE f VALUE '1.0', lv_b TYPE f VALUE '1.0'.
        lv_pos = find( val = iv_json sub = '"intensity":' ).
        IF lv_pos >= 0.
          lv_pos = lv_pos + 12. lv_end = lv_pos.
          WHILE lv_end < strlen( iv_json ) AND iv_json+lv_end(1) CO '0123456789.'. lv_end = lv_end + 1. ENDWHILE.
          IF lv_end > lv_pos. lv_len = lv_end - lv_pos. lv_intensity = iv_json+lv_pos(lv_len). ENDIF.
        ENDIF.
        ms_flash_pending = VALUE #( active = abap_true intensity = lv_intensity r = lv_r g = lv_g b = lv_b ).
        send_text( i_message_manager = i_message_manager
                   iv_text           = |\{"type":"flash_ack","intensity":{ lv_intensity }\}| ).
    ENDCASE.
  ENDMETHOD.


  METHOD send_scenario.
    send_text( i_message_manager = i_message_manager iv_text = mo_demo->get_scenario( ) ).
  ENDMETHOD.


  METHOD seek_to_bar.
    mv_frame_num = floor( iv_bar * mo_demo->get_bar_sec( ) * mo_demo->get_fps( ) ).
    mv_last_effect = ''. mv_effect_start_bar = 0.
  ENDMETHOD.


  METHOD seek_to_frame.
    mv_frame_num = iv_frame. mv_last_effect = ''. mv_effect_start_bar = 0.
  ENDMETHOD.


  METHOD send_preload_frame.
    DATA: lo_effect TYPE REF TO zif_o4d_effect, ls_frame TYPE zif_o4d_effect=>ty_frame, lv_effect_name TYPE string.
    DATA(lv_gt) = CONV f( iv_frame ) / mo_demo->get_fps( ).
    " Use stateless context builder for preload (no session state dependency)
    DATA(ls_ctx) = build_preload_ctx( iv_global_time = lv_gt iv_frame = iv_frame ).
    lo_effect = mo_demo->get_effect_at_bar( ls_ctx-gbi-bar ).
    IF lo_effect IS BOUND.
      lv_effect_name = lo_effect->get_name( ).
      ls_frame = lo_effect->render_frame( ls_ctx ).
    ENDIF.
    DATA(lv_json) = frame_to_json( is_frame = ls_frame is_ctx = ls_ctx iv_effect_name = lv_effect_name ).
    DATA(lv_out) = |\{"type":"preload","pf":{ iv_frame },| && lv_json+1.
    send_text( i_message_manager = i_message_manager iv_text = lv_out ).
  ENDMETHOD.


  METHOD build_preload_ctx.
    " Stateless context builder - calculates everything from frame number and scenario
    " No dependency on mv_effect_start_bar or mv_last_effect session state

    " Global timing
    rs_ctx-gt = iv_global_time.
    rs_ctx-gbi = calc_beat_info( iv_global_time ).
    rs_ctx-gtick = floor( iv_global_time / mo_demo->get_sec_per_tick( ) ).
    rs_ctx-gf = iv_frame.

    " Get scene info directly from demo (stateless lookup)
    DATA(ls_scene) = mo_demo->get_scene_at_bar( rs_ctx-gbi-bar ).

    " Calculate local time from scene start bar (stateless!)
    DATA(lv_scene_start_sec) = CONV f( ls_scene-bar_start ) * mo_demo->get_bar_sec( ).
    rs_ctx-t = iv_global_time - lv_scene_start_sec.
    IF rs_ctx-t < 0. rs_ctx-t = 0. ENDIF.

    " Local beat info
    rs_ctx-bi = calc_beat_info( rs_ctx-t ).

    " Local tick and frame
    rs_ctx-tick = floor( rs_ctx-t / mo_demo->get_sec_per_tick( ) ).
    DATA(lv_fpt) = mo_demo->get_fpt( ).
    DATA(lv_scene_start_frame) = ls_scene-bar_start * c_ticks_per_bar * lv_fpt.
    rs_ctx-f = iv_frame - lv_scene_start_frame.
    IF rs_ctx-f < 0. rs_ctx-f = 0. ENDIF.

    " Transition detection (stateless - based on scene boundary)
    rs_ctx-transition = xsdbool( rs_ctx-bi-bar = 0 AND rs_ctx-bi-bar_phase < CONV f( '0.02' ) ).
    rs_ctx-prev_effect = ''.  " Not tracked in stateless mode
  ENDMETHOD.


  METHOD if_apc_wsp_extension~on_close.
    mv_running = abap_false.
    zcl_o4d_demo=>clear_registry( ).
    FREE: mo_demo, mo_starfield, mo_copperbars, mo_joydiv, mo_plasma, mo_tunnel, mo_elite3d,
          mo_metaballs, mo_twister, mo_constellation, mo_sales_quarter, mo_starburst, mo_voxel,
          mo_kaleidoscope, mo_ignition, mo_amigaball, mo_amigaball2, mo_glitch, mo_firegreetings,
          mo_mountains, mo_swirl, mo_greetings, mo_sierpinski, mo_lowpoly, mo_rotozoom, mo_tornado, mo_flash_greet,
          mo_joy_division, mo_joy_div_v2, mo_voxel_land, mo_tunnel_v2,
          mo_sales_dance, mo_ignite_emit, mo_twistzoomer, mo_rotozoom_sine.
  ENDMETHOD.


  METHOD if_apc_wsp_extension~on_error. mv_running = abap_false. ENDMETHOD.


  METHOD calc_beat_info.
    DATA: lv_bar_sec    TYPE f, lv_beat_sec TYPE f, lv_sec_per_8 TYPE f, lv_sec_per_16 TYPE f.
    lv_bar_sec = mo_demo->get_bar_sec( ). lv_beat_sec = mo_demo->get_beat_sec( ).

    rs_bi-time = iv_time. rs_bi-bar = floor( iv_time / lv_bar_sec ).
    rs_bi-bar_phase = frac( iv_time / lv_bar_sec ).
    rs_bi-beat = floor( rs_bi-bar_phase * 4 ).
    rs_bi-beat_phase = frac( rs_bi-bar_phase * 4 ).
    rs_bi-pos_4 = rs_bi-bar * 4 + rs_bi-beat.
    lv_sec_per_8 = lv_beat_sec / 2. lv_sec_per_16 = lv_beat_sec / 4.
    rs_bi-pos_8 = floor( iv_time / lv_sec_per_8 ).
    rs_bi-pos_16 = floor( iv_time / lv_sec_per_16 ).
    " Pulse: log-scale decay, only on pos_16 MOD 16 = 8 (once per bar, middle of bar)
    IF rs_bi-pos_16 MOD 16 = 8.
      " Log-scale: 1/(1+phase*k) gives nice 1->0 curve
      DATA(lv_phase16) = frac( iv_time / lv_sec_per_16 ).
      rs_bi-pulse = 1 / ( 1 + lv_phase16 * 12 ).
    ELSE.
      rs_bi-pulse = 0.
    ENDIF.
    rs_bi-i1 = rs_bi-bar. rs_bi-i2 = floor( iv_time / ( lv_bar_sec / 2 ) ).
    rs_bi-i4 = rs_bi-pos_4. rs_bi-i8 = rs_bi-pos_8. rs_bi-i16 = rs_bi-pos_16.
    DATA(lv_phase_8) = frac( iv_time / lv_sec_per_8 ).
    DATA(lv_phase_16) = frac( iv_time / lv_sec_per_16 ).
    rs_bi-on_bar = xsdbool( rs_bi-bar_phase < CONV f( '0.02' ) ).
    rs_bi-on_beat = xsdbool( rs_bi-beat_phase < CONV f( '0.05' ) ).
    rs_bi-on_4th = rs_bi-on_beat.
    rs_bi-on_8th = xsdbool( lv_phase_8 < CONV f( '0.08' ) ).
    rs_bi-on_16th = xsdbool( lv_phase_16 < CONV f( '0.15' ) ).
    rs_bi-f1 = COND f( WHEN rs_bi-on_bar = abap_true THEN 1 ELSE 0 ).
    rs_bi-f4 = COND f( WHEN rs_bi-on_4th = abap_true THEN 1 ELSE 0 ).
    rs_bi-f8 = COND f( WHEN rs_bi-on_8th = abap_true THEN 1 ELSE 0 ).
    rs_bi-f16 = COND f( WHEN rs_bi-on_16th = abap_true THEN 1 ELSE 0 ).
  ENDMETHOD.


  METHOD build_render_ctx.
    DATA: lo_effect      TYPE REF TO zif_o4d_effect, lv_effect_name TYPE string.
    rs_ctx-gt = iv_global_time. rs_ctx-gbi = calc_beat_info( iv_global_time ).

    " Global integer counters (pace-related)
    rs_ctx-gtick = floor( iv_global_time / mo_demo->get_sec_per_tick( ) ).
    rs_ctx-gf = mv_frame_num.

    lo_effect = mo_demo->get_effect_at_bar( rs_ctx-gbi-bar ).
    IF lo_effect IS BOUND. lv_effect_name = lo_effect->get_name( ). ENDIF.
    IF lv_effect_name <> mv_last_effect.
      rs_ctx-transition = abap_true. rs_ctx-prev_effect = mv_last_effect.
      mv_last_effect = lv_effect_name. mv_effect_start_bar = rs_ctx-gbi-bar.
    ELSE.
      rs_ctx-transition = abap_false.
    ENDIF.
    DATA(lv_est) = CONV f( mv_effect_start_bar ) * mo_demo->get_bar_sec( ).
    rs_ctx-t = iv_global_time - lv_est. rs_ctx-bi = calc_beat_info( rs_ctx-t ).

    " Local integer counters (relative to scene start)
    rs_ctx-tick = floor( rs_ctx-t / mo_demo->get_sec_per_tick( ) ).
    DATA(lv_fpt) = mo_demo->get_fpt( ).
    DATA(lv_scene_start_frame) = mv_effect_start_bar * c_ticks_per_bar * lv_fpt.
    rs_ctx-f = mv_frame_num - lv_scene_start_frame.
    IF rs_ctx-f < 0. rs_ctx-f = 0. ENDIF.
  ENDMETHOD.


  METHOD send_text.
    TRY.
        DATA(lo_msg) = i_message_manager->create_message( ).
        lo_msg->set_text( iv_text ).
        i_message_manager->send( lo_msg ).
      CATCH cx_apc_error.
    ENDTRY.
  ENDMETHOD.


  METHOD send_frame.
    DATA: lo_effect TYPE REF TO zif_o4d_effect, ls_frame TYPE zif_o4d_effect=>ty_frame.
    DATA(ls_ctx) = build_render_ctx( iv_global_time ).
    lo_effect = mo_demo->get_effect_at_bar( ls_ctx-gbi-bar ).
    IF lo_effect IS BOUND. ls_frame = lo_effect->render_frame( ls_ctx ). ENDIF.
    DATA(lv_json) = frame_to_json( is_frame = ls_frame is_ctx = ls_ctx ).
    send_text( i_message_manager = i_message_manager iv_text = lv_json ).
  ENDMETHOD.


  METHOD frame_to_json.
    DATA: lv_lines TYPE string, lv_texts TYPE string, lv_tris TYPE string, lv_rects TYPE string, lv_debug TYPE string,
          lv_effect TYPE string.
    lv_effect = COND #( WHEN iv_effect_name IS NOT INITIAL THEN iv_effect_name ELSE mv_last_effect ).

    lv_lines = |[|.
    LOOP AT is_frame-lines INTO DATA(ls_l).
      IF sy-tabix > 1. lv_lines = lv_lines && |,|. ENDIF.
      " Only output "w" if width > 1 (otherwise use frame state default)
      IF ls_l-width > 1.
        lv_lines = lv_lines && |\{"x1":{ ls_l-x1 },"y1":{ ls_l-y1 },"x2":{ ls_l-x2 },"y2":{ ls_l-y2 },"c":"{ ls_l-color }","w":{ ls_l-width }\}|.
      ELSE.
        lv_lines = lv_lines && |\{"x1":{ ls_l-x1 },"y1":{ ls_l-y1 },"x2":{ ls_l-x2 },"y2":{ ls_l-y2 },"c":"{ ls_l-color }"\}|.
      ENDIF.
    ENDLOOP.
    lv_lines = lv_lines && |]|.

    lv_texts = |[|.
    LOOP AT is_frame-texts INTO DATA(ls_t).
      IF sy-tabix > 1. lv_texts = lv_texts && |,|. ENDIF.
      lv_texts = lv_texts && |\{"x":{ ls_t-x },"y":{ ls_t-y },"t":"{ ls_t-text }","c":"{ ls_t-color }","s":{ ls_t-size }\}|.
    ENDLOOP.
    lv_texts = lv_texts && |]|.

    lv_tris = |[|.
    LOOP AT is_frame-triangles INTO DATA(ls_tri).
      IF sy-tabix > 1. lv_tris = lv_tris && |,|. ENDIF.
      lv_tris = lv_tris && |\{"x1":{ ls_tri-x1 },"y1":{ ls_tri-y1 },"x2":{ ls_tri-x2 },"y2":{ ls_tri-y2 },"x3":{ ls_tri-x3 },"y3":{ ls_tri-y3 },"f":"{ ls_tri-fill }"\}|.
    ENDLOOP.
    lv_tris = lv_tris && |]|.

    " Background rects (rendered before circles)
    DATA lv_rects_back TYPE string.
    lv_rects_back = |[|.
    LOOP AT is_frame-rects_back INTO DATA(ls_rb).
      IF sy-tabix > 1. lv_rects_back = lv_rects_back && |,|. ENDIF.
      lv_rects_back = lv_rects_back && |\{"x":{ ls_rb-x },"y":{ ls_rb-y },"w":{ ls_rb-w },"h":{ ls_rb-h },"f":"{ ls_rb-fill }"\}|.
    ENDLOOP.
    lv_rects_back = lv_rects_back && |]|.

    " Foreground rects (rendered after circles)
    lv_rects = |[|.
    LOOP AT is_frame-rects INTO DATA(ls_r).
      IF sy-tabix > 1. lv_rects = lv_rects && |,|. ENDIF.
      lv_rects = lv_rects && |\{"x":{ ls_r-x },"y":{ ls_r-y },"w":{ ls_r-w },"h":{ ls_r-h },"f":"{ ls_r-fill }"\}|.
    ENDLOOP.
    lv_rects = lv_rects && |]|.

    DATA lv_circles TYPE string.
    lv_circles = |[|.
    LOOP AT is_frame-circles INTO DATA(ls_c).
      IF sy-tabix > 1. lv_circles = lv_circles && |,|. ENDIF.
      lv_circles = lv_circles && |\{"x":{ ls_c-x },"y":{ ls_c-y },"r":{ ls_c-radius },"f":"{ ls_c-fill }"\}|.
    ENDLOOP.
    lv_circles = lv_circles && |]|.

    " Images (rendered from preloaded cache by JS)
    DATA lv_images TYPE string.
    lv_images = |[|.
    LOOP AT is_frame-images INTO DATA(ls_img).
      IF sy-tabix > 1. lv_images = lv_images && |,|. ENDIF.
      lv_images = lv_images && |\{"n":"{ ls_img-name }","x":{ ls_img-x },"y":{ ls_img-y },"w":{ ls_img-w },"h":{ ls_img-h }\}|.
    ENDLOOP.
    lv_images = lv_images && |]|.

    DATA(lv_tr) = COND string( WHEN is_ctx-transition = abap_true THEN 'true' ELSE 'false' ).
    DATA(lv_lc) = lines( is_frame-lines ).
    DATA(lv_tc) = lines( is_frame-texts ).
    DATA(lv_tric) = lines( is_frame-triangles ).
    DATA(lv_rbc) = lines( is_frame-rects_back ).
    DATA(lv_rc) = lines( is_frame-rects ).
    DATA(lv_cc) = lines( is_frame-circles ).
    DATA(lv_ic) = lines( is_frame-images ).

    IF mv_mode = c_mode_dev.
      DATA(lv_scene_bars) = COND i( WHEN is_ctx-gbi-bar >= mv_effect_start_bar THEN is_ctx-gbi-bar - mv_effect_start_bar ELSE 0 ).
      DATA(lv_scene_progress) = COND f( WHEN lv_scene_bars > 0 THEN lv_scene_bars / 4 ELSE 0 ).
      DATA(lv_vars) = is_frame-debug-vars.
      IF lv_vars IS INITIAL. lv_vars = '{}'. ENDIF.
      lv_debug = |,"debug":\{"gf":{ is_ctx-gf },"f":{ is_ctx-f },| &&
                 |"gtick":{ is_ctx-gtick },"tick":{ is_ctx-tick },| &&
                 |"bar":{ is_ctx-gbi-bar },"beat":{ is_ctx-gbi-beat },| &&
                 |"bar_phase":{ is_ctx-gbi-bar_phase },"beat_phase":{ is_ctx-gbi-beat_phase },| &&
                 |"scene":"{ lv_effect }","scene_bar":{ lv_scene_bars },| &&
                 |"scene_progress":{ lv_scene_progress },"pulse":{ is_ctx-bi-pulse },| &&
                 |"on_beat":{ COND string( WHEN is_ctx-gbi-on_beat = abap_true THEN 'true' ELSE 'false' ) },| &&
                 |"on_bar":{ COND string( WHEN is_ctx-gbi-on_bar = abap_true THEN 'true' ELSE 'false' ) },| &&
                 |"vars":{ lv_vars }\}|.
    ENDIF.

    " Flash overlay (from effect or pending from client)
    DATA lv_flash TYPE string.
    DATA(ls_flash) = is_frame-flash.
    IF ms_flash_pending-active = abap_true.
      ls_flash = ms_flash_pending.
      CLEAR ms_flash_pending.  " Consume pending flash
    ENDIF.
    IF ls_flash-active = abap_true.
      lv_flash = |,"flash":\{"intensity":{ ls_flash-intensity },"color":[{ ls_flash-r },{ ls_flash-g },{ ls_flash-b }]\}|.
    ENDIF.

    " Render state (frame-level defaults for shapes)
    DATA lv_state TYPE string.
    DATA(ls_state) = is_frame-state.
    IF ls_state-line_width > 0 OR ls_state-line_color IS NOT INITIAL OR
       ls_state-line_cap IS NOT INITIAL OR ls_state-line_join IS NOT INITIAL OR
       ls_state-alpha > 0 OR ls_state-blend IS NOT INITIAL.
      lv_state = |,"state":\{|.
      DATA(lv_first) = abap_true.
      IF ls_state-line_width > 0.
        lv_state = lv_state && |"lw":{ ls_state-line_width }|. lv_first = abap_false.
      ENDIF.
      IF ls_state-line_color IS NOT INITIAL.
        IF lv_first = abap_false. lv_state = lv_state && |,|. ENDIF.
        lv_state = lv_state && |"lc":"{ ls_state-line_color }"|. lv_first = abap_false.
      ENDIF.
      IF ls_state-line_cap IS NOT INITIAL.
        IF lv_first = abap_false. lv_state = lv_state && |,|. ENDIF.
        lv_state = lv_state && |"cap":"{ ls_state-line_cap }"|. lv_first = abap_false.
      ENDIF.
      IF ls_state-line_join IS NOT INITIAL.
        IF lv_first = abap_false. lv_state = lv_state && |,|. ENDIF.
        lv_state = lv_state && |"join":"{ ls_state-line_join }"|. lv_first = abap_false.
      ENDIF.
      IF ls_state-alpha > 0.
        IF lv_first = abap_false. lv_state = lv_state && |,|. ENDIF.
        lv_state = lv_state && |"alpha":{ ls_state-alpha }|. lv_first = abap_false.
      ENDIF.
      IF ls_state-blend IS NOT INITIAL.
        IF lv_first = abap_false. lv_state = lv_state && |,|. ENDIF.
        lv_state = lv_state && |"blend":"{ ls_state-blend }"|.
      ENDIF.
      lv_state = lv_state && |\}|.
    ENDIF.

    rv_json = |\{"t":{ is_ctx-t },"gt":{ is_ctx-gt },"b":{ is_ctx-bi-bar },"gb":{ is_ctx-gbi-bar },| &&
              |"bp":{ is_ctx-bi-bar_phase },"p":{ is_ctx-bi-pulse },"tr":{ lv_tr },"e":"{ lv_effect }"{ lv_state },| &&
              |"lc":{ lv_lc },"tc":{ lv_tc },"tric":{ lv_tric },"rbc":{ lv_rbc },"rc":{ lv_rc },"cc":{ lv_cc },"ic":{ lv_ic },| &&
              |"l":{ lv_lines },"tx":{ lv_texts },"tri":{ lv_tris },"rb":{ lv_rects_back },"r":{ lv_rects },"c":{ lv_circles },"img":{ lv_images }{ lv_flash }{ lv_debug }\}|.
  ENDMETHOD.
ENDCLASS.
