CLASS zcl_o4d_demo DEFINITION
  PUBLIC FINAL CREATE PRIVATE.

  PUBLIC SECTION.
    " Transition types: shrink, fade, flash, wipe-left, wipe-right, iris, diamond, pixelate, glitch, crossfade
    " Transition modes: pre (before scene ends), post (after scene starts), cross (spans boundary)
    TYPES:
      BEGIN OF ty_transition,
        type  TYPE string,   " shrink, fade, iris, etc.
        mode  TYPE string,   " pre, post, cross
        beats TYPE i,        " duration in beats (default 2)
      END OF ty_transition,

      BEGIN OF ty_scene,
        bar_start  TYPE i,
        bar_end    TYPE i,
        effect     TYPE REF TO zif_o4d_effect,
        transition TYPE ty_transition,
      END OF ty_scene,
      tt_scenes TYPE STANDARD TABLE OF ty_scene WITH EMPTY KEY,

      BEGIN OF ty_scene_def,
        len        TYPE i,
        effect     TYPE REF TO zif_o4d_effect,
        transition TYPE ty_transition,  " optional transition config
      END OF ty_scene_def,
      tt_scene_defs TYPE STANDARD TABLE OF ty_scene_def WITH EMPTY KEY.

    " Factory + Registry
    CLASS-METHODS:
      new
        IMPORTING iv_id    TYPE string
                  iv_name  TYPE string DEFAULT ''
                  iv_bpm   TYPE f DEFAULT '76.0'
                  iv_fpt   TYPE i DEFAULT 1  " frames per tick (1=~20fps, 2=~40fps, 3=~60fps)
                  iv_audio TYPE string DEFAULT ''
                  iv_loop  TYPE abap_bool DEFAULT abap_false
        RETURNING VALUE(ro_demo) TYPE REF TO zcl_o4d_demo,

      get
        IMPORTING iv_id TYPE string
        RETURNING VALUE(ro_demo) TYPE REF TO zcl_o4d_demo,

      get_all_ids
        RETURNING VALUE(rt_ids) TYPE string_table,

      exists
        IMPORTING iv_id TYPE string
        RETURNING VALUE(rv_exists) TYPE abap_bool,

      clear_registry.

    " Getters
    METHODS get_id RETURNING VALUE(rv_id) TYPE string.
    METHODS get_name RETURNING VALUE(rv_name) TYPE string.
    METHODS get_bpm RETURNING VALUE(rv_bpm) TYPE f.
    METHODS get_fps RETURNING VALUE(rv_fps) TYPE f.
    METHODS get_bar_sec RETURNING VALUE(rv_bar_sec) TYPE f.
    METHODS get_beat_sec RETURNING VALUE(rv_beat_sec) TYPE f.
    METHODS get_sec_per_tick RETURNING VALUE(rv_sec) TYPE f.
    METHODS get_audio RETURNING VALUE(rv_audio) TYPE string.
    METHODS get_fpt RETURNING VALUE(rv_fpt) TYPE i.
    METHODS get_total_bars RETURNING VALUE(rv_bars) TYPE i.
    METHODS is_loop RETURNING VALUE(rv_loop) TYPE abap_bool.

    " Scene management
    METHODS add_scene
      IMPORTING iv_bar_start  TYPE i
                iv_bar_end    TYPE i
                io_effect     TYPE REF TO zif_o4d_effect
                is_transition TYPE ty_transition OPTIONAL.

    METHODS add_scenes
      IMPORTING it_scenes TYPE tt_scene_defs.

    METHODS get_scenes
      RETURNING VALUE(rt_scenes) TYPE tt_scenes.

    METHODS get_effect_at_bar
      IMPORTING iv_bar TYPE i
      RETURNING VALUE(ro_effect) TYPE REF TO zif_o4d_effect.

    "! Get full scene info at bar (for stateless time calculation)
    METHODS get_scene_at_bar
      IMPORTING iv_bar TYPE i
      RETURNING VALUE(rs_scene) TYPE ty_scene.

    METHODS get_scenario
      RETURNING VALUE(rv_json) TYPE string.

  PRIVATE SECTION.
    CLASS-DATA: gt_registry TYPE HASHED TABLE OF REF TO zcl_o4d_demo
                WITH UNIQUE KEY table_line.

    DATA: mv_id           TYPE string,
          mv_name         TYPE string,
          mv_bpm          TYPE f,
          mv_fpt          TYPE i,
          mv_fps          TYPE f,
          mv_bar_sec      TYPE f,
          mv_beat_sec     TYPE f,
          mv_sec_per_tick TYPE f,
          mv_audio        TYPE string,
          mv_loop         TYPE abap_bool,
          mt_scenes       TYPE tt_scenes.

    METHODS calculate_timing.

ENDCLASS.

CLASS zcl_o4d_demo IMPLEMENTATION.

  METHOD new.
    " Create and register demo
    ro_demo = NEW #( ).
    ro_demo->mv_id = iv_id.
    ro_demo->mv_name = COND #( WHEN iv_name IS INITIAL THEN iv_id ELSE iv_name ).
    ro_demo->mv_bpm = iv_bpm.
    ro_demo->mv_fpt = COND #( WHEN iv_fpt < 1 THEN 1 ELSE iv_fpt ).
    ro_demo->mv_audio = iv_audio.
    ro_demo->mv_loop = iv_loop.
    ro_demo->calculate_timing( ).

    " Auto-register
    INSERT ro_demo INTO TABLE gt_registry.
  ENDMETHOD.

  METHOD get.
    " Lookup by ID
    LOOP AT gt_registry INTO DATA(lo_demo).
      IF lo_demo->mv_id = iv_id.
        ro_demo = lo_demo.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_all_ids.
    LOOP AT gt_registry INTO DATA(lo_demo).
      APPEND lo_demo->mv_id TO rt_ids.
    ENDLOOP.
  ENDMETHOD.

  METHOD exists.
    rv_exists = xsdbool( get( iv_id ) IS BOUND ).
  ENDMETHOD.

  METHOD clear_registry.
    CLEAR gt_registry.
  ENDMETHOD.

  METHOD calculate_timing.
    mv_bar_sec = 240 / mv_bpm.
    mv_beat_sec = 60 / mv_bpm.
    mv_sec_per_tick = mv_bar_sec / 64.
    mv_fps = ( mv_bpm / '3.75' ) * mv_fpt.  " fpt=1→~20fps, fpt=2→~40fps, fpt=3→~60fps
  ENDMETHOD.

  METHOD get_id. rv_id = mv_id. ENDMETHOD.
  METHOD get_name. rv_name = mv_name. ENDMETHOD.
  METHOD get_bpm. rv_bpm = mv_bpm. ENDMETHOD.
  METHOD get_fps. rv_fps = mv_fps. ENDMETHOD.
  METHOD get_fpt. rv_fpt = mv_fpt. ENDMETHOD.
  METHOD get_bar_sec. rv_bar_sec = mv_bar_sec. ENDMETHOD.
  METHOD get_beat_sec. rv_beat_sec = mv_beat_sec. ENDMETHOD.
  METHOD get_sec_per_tick. rv_sec = mv_sec_per_tick. ENDMETHOD.
  METHOD get_audio. rv_audio = mv_audio. ENDMETHOD.
  METHOD is_loop. rv_loop = mv_loop. ENDMETHOD.

  METHOD get_total_bars.
    LOOP AT mt_scenes INTO DATA(ls_scene).
      IF ls_scene-bar_end > rv_bars.
        rv_bars = ls_scene-bar_end.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_scene.
    APPEND VALUE #(
      bar_start  = iv_bar_start
      bar_end    = iv_bar_end
      effect     = io_effect
      transition = is_transition
    ) TO mt_scenes.
  ENDMETHOD.

  METHOD add_scenes.
    DATA: lv_pos TYPE i VALUE 0.
    LOOP AT it_scenes INTO DATA(ls_scene).
      add_scene(
        iv_bar_start  = lv_pos
        iv_bar_end    = lv_pos + ls_scene-len
        io_effect     = ls_scene-effect
        is_transition = ls_scene-transition
      ).
      lv_pos = lv_pos + ls_scene-len.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_scenes.
    rt_scenes = mt_scenes.
  ENDMETHOD.

  METHOD get_effect_at_bar.
    LOOP AT mt_scenes INTO DATA(ls_scene).
      IF iv_bar >= ls_scene-bar_start AND iv_bar < ls_scene-bar_end.
        ro_effect = ls_scene-effect.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_scene_at_bar.
    LOOP AT mt_scenes INTO DATA(ls_scene).
      IF iv_bar >= ls_scene-bar_start AND iv_bar < ls_scene-bar_end.
        rs_scene = ls_scene.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_scenario.
    DATA: lv_scenes TYPE string, lv_name TYPE string, lv_id TYPE string, lv_trans TYPE string.

    lv_scenes = |[|.
    LOOP AT mt_scenes INTO DATA(ls_scene).
      IF ls_scene-effect IS BOUND.
        lv_name = ls_scene-effect->get_name( ).
        lv_id = to_lower( lv_name ).
        REPLACE ALL OCCURRENCES OF ` ` IN lv_id WITH `_`.
      ELSE.
        lv_name = 'Unknown'. lv_id = 'unknown'.
      ENDIF.

      " Build transition JSON if defined
      CLEAR lv_trans.
      IF ls_scene-transition-type IS NOT INITIAL.
        DATA(lv_beats) = COND i( WHEN ls_scene-transition-beats > 0 THEN ls_scene-transition-beats ELSE 2 ).
        DATA(lv_mode) = COND string( WHEN ls_scene-transition-mode IS NOT INITIAL THEN ls_scene-transition-mode ELSE 'pre' ).
        lv_trans = |,"transition":\{"type":"{ ls_scene-transition-type }","mode":"{ lv_mode }","beats":{ lv_beats }\}|.
      ENDIF.

      IF sy-tabix > 1. lv_scenes = lv_scenes && |,|. ENDIF.
      lv_scenes = lv_scenes &&
        |\{"id":"{ lv_id }","name":"{ lv_name }",| &&
        |"start_bar":{ ls_scene-bar_start },"end_bar":{ ls_scene-bar_end }{ lv_trans }\}|.
    ENDLOOP.
    lv_scenes = lv_scenes && |]|.

    " Collect media from all effects
    DATA: lv_media TYPE string, lt_media TYPE zif_o4d_effect=>tt_media.
    lv_media = |[|.
    DATA(lv_first_media) = abap_true.
    LOOP AT mt_scenes INTO DATA(ls_scn).
      IF ls_scn-effect IS BOUND.
        TRY.
            lt_media = ls_scn-effect->get_required_media( ).
            LOOP AT lt_media INTO DATA(ls_m).
              IF lv_first_media = abap_false. lv_media = lv_media && |,|. ENDIF.
              lv_media = lv_media && |\{"name":"{ ls_m-name }","type":"{ ls_m-type }"\}|.
              lv_first_media = abap_false.
            ENDLOOP.
          CATCH cx_sy_dyn_call_illegal_method.
            " Method not implemented - skip
        ENDTRY.
      ENDIF.
    ENDLOOP.
    lv_media = lv_media && |]|.

    rv_json = |\{"type":"scenario","id":"{ mv_id }","name":"{ mv_name }","bpm":{ mv_bpm },"fps":{ mv_fps },"fpt":{ mv_fpt },| &&
              |"bar_sec":{ mv_bar_sec },"total_bars":{ get_total_bars( ) },| &&
              |"audio":"{ mv_audio }","loop":{ COND string( WHEN mv_loop = abap_true THEN 'true' ELSE 'false' ) },| &&
              |"media":{ lv_media },| &&
              |"scenes":{ lv_scenes }\}|.
  ENDMETHOD.

ENDCLASS.
