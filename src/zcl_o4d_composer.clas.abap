"! Demo Composer - Orchestrates scenes and transitions
"!
"! Usage:
"!   composer->add_scene( id = 'intro' start_bar = 0 end_bar = 8 effect = starfield )
"!   composer->add_scene( id = 'main'  start_bar = 8 end_bar = 32 effect = plasma )
"!   composer->add_transition( from = 'intro' to = 'main' type = 'crossfade' bars = 1 )
"!   frame = composer->render( time = 5.0 dev_mode = abap_true )
CLASS zcl_o4d_composer DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES:
      " Scene definition
      BEGIN OF ty_scene,
        id        TYPE string,
        start_bar TYPE f,
        end_bar   TYPE f,
        effect    TYPE REF TO zif_o4d_effect,
        recording TYPE string,
      END OF ty_scene,
      tt_scenes TYPE STANDARD TABLE OF ty_scene WITH KEY id,

      " Transition definition
      BEGIN OF ty_transition,
        from_scene TYPE string,
        to_scene   TYPE string,
        type       TYPE string,  " crossfade, glitch, wipe
        bars       TYPE f,
      END OF ty_transition,
      tt_transitions TYPE STANDARD TABLE OF ty_transition WITH EMPTY KEY.

    METHODS:
      constructor
        IMPORTING
          iv_bpm TYPE f DEFAULT '76',

      add_scene
        IMPORTING
          iv_id        TYPE string
          iv_start_bar TYPE f
          iv_end_bar   TYPE f
          io_effect    TYPE REF TO zif_o4d_effect
          iv_recording TYPE string OPTIONAL,

      add_transition
        IMPORTING
          iv_from TYPE string
          iv_to   TYPE string
          iv_type TYPE string DEFAULT 'crossfade'
          iv_bars TYPE f DEFAULT '1.0',

      set_overlay
        IMPORTING
          io_effect TYPE REF TO zif_o4d_effect,

      render
        IMPORTING
          iv_time     TYPE f
          iv_dev_mode TYPE abap_bool DEFAULT abap_false
        RETURNING
          VALUE(rs_frame) TYPE zif_o4d_effect=>ty_frame,

      get_current_scene
        RETURNING VALUE(rv_id) TYPE string,

      get_scene_count
        RETURNING VALUE(rv_count) TYPE i,

      set_recording_loader
        IMPORTING
          io_loader TYPE REF TO zcl_o4d_recording_loader.

  PRIVATE SECTION.
    DATA:
      mv_bpm        TYPE f,
      mv_bar_dur    TYPE f,         " seconds per bar
      mt_scenes     TYPE tt_scenes,
      mt_transitions TYPE tt_transitions,
      mo_overlay    TYPE REF TO zif_o4d_effect,
      mo_recording  TYPE REF TO zcl_o4d_recording_loader,
      mv_cur_scene  TYPE string,
      mv_frame      TYPE i.

    METHODS:
      time_to_bar
        IMPORTING iv_time TYPE f
        RETURNING VALUE(rv_bar) TYPE f,

      bar_to_time
        IMPORTING iv_bar TYPE f
        RETURNING VALUE(rv_time) TYPE f,

      get_active_scenes
        IMPORTING iv_bar TYPE f
        RETURNING VALUE(rt_scenes) TYPE tt_scenes,

      get_transition
        IMPORTING iv_from TYPE string
                  iv_to   TYPE string
        RETURNING VALUE(rs_trans) TYPE ty_transition,

      build_context
        IMPORTING
          iv_time     TYPE f
          iv_bar      TYPE f
          is_scene    TYPE ty_scene
          iv_dev_mode TYPE abap_bool
        RETURNING
          VALUE(rs_ctx) TYPE zif_o4d_effect=>ty_render_ctx,

      build_beat_info
        IMPORTING iv_time TYPE f
                  iv_offset TYPE f DEFAULT 0
        RETURNING VALUE(rs_bi) TYPE zif_o4d_effect=>ty_beat_info,

      merge_frames
        IMPORTING
          is_frame1 TYPE zif_o4d_effect=>ty_frame
          is_frame2 TYPE zif_o4d_effect=>ty_frame
          iv_alpha  TYPE f
        RETURNING
          VALUE(rs_frame) TYPE zif_o4d_effect=>ty_frame.
ENDCLASS.

CLASS zcl_o4d_composer IMPLEMENTATION.
  METHOD constructor.
    mv_bpm = iv_bpm.
    mv_bar_dur = 60 / mv_bpm * 4.  " 4 beats per bar
    mv_frame = 0.
  ENDMETHOD.

  METHOD set_recording_loader.
    mo_recording = io_loader.
  ENDMETHOD.

  METHOD add_scene.
    APPEND VALUE #(
      id = iv_id
      start_bar = iv_start_bar
      end_bar = iv_end_bar
      effect = io_effect
      recording = iv_recording
    ) TO mt_scenes.
  ENDMETHOD.

  METHOD add_transition.
    APPEND VALUE #(
      from_scene = iv_from
      to_scene = iv_to
      type = iv_type
      bars = iv_bars
    ) TO mt_transitions.
  ENDMETHOD.

  METHOD set_overlay.
    mo_overlay = io_effect.
  ENDMETHOD.

  METHOD render.
    DATA(lv_bar) = time_to_bar( iv_time ).
    DATA(lt_active) = get_active_scenes( lv_bar ).

    mv_frame = mv_frame + 1.

    " Single scene - simple render
    IF lines( lt_active ) = 1.
      DATA(ls_scene) = lt_active[ 1 ].
      mv_cur_scene = ls_scene-id.
      DATA(ls_ctx) = build_context(
        iv_time = iv_time
        iv_bar = lv_bar
        is_scene = ls_scene
        iv_dev_mode = iv_dev_mode
      ).
      rs_frame = ls_scene-effect->render_frame( ls_ctx ).

    " Multiple overlapping scenes - check for transition
    ELSEIF lines( lt_active ) > 1.
      DATA(ls_scene1) = lt_active[ 1 ].
      DATA(ls_scene2) = lt_active[ 2 ].
      DATA(ls_trans) = get_transition( iv_from = ls_scene1-id iv_to = ls_scene2-id ).

      IF ls_trans IS NOT INITIAL.
        " Calculate transition progress
        DATA(lv_trans_start) = ls_scene2-start_bar.
        DATA(lv_trans_end) = lv_trans_start + ls_trans-bars.
        DATA(lv_alpha) = ( lv_bar - lv_trans_start ) / ls_trans-bars.
        lv_alpha = nmax( val1 = 0 val2 = nmin( val1 = 1 val2 = lv_alpha ) ).

        " Render both and merge
        DATA(ls_ctx1) = build_context( iv_time = iv_time iv_bar = lv_bar
                                        is_scene = ls_scene1 iv_dev_mode = iv_dev_mode ).
        DATA(ls_ctx2) = build_context( iv_time = iv_time iv_bar = lv_bar
                                        is_scene = ls_scene2 iv_dev_mode = iv_dev_mode ).
        ls_ctx1-transition = abap_true.
        ls_ctx2-transition = abap_true.

        DATA(ls_frame1) = ls_scene1-effect->render_frame( ls_ctx1 ).
        DATA(ls_frame2) = ls_scene2-effect->render_frame( ls_ctx2 ).

        rs_frame = merge_frames( is_frame1 = ls_frame1 is_frame2 = ls_frame2 iv_alpha = lv_alpha ).
        mv_cur_scene = ls_scene2-id.
      ELSE.
        " No transition defined - just use second scene
        mv_cur_scene = ls_scene2-id.
        DATA(ls_ctx3) = build_context( iv_time = iv_time iv_bar = lv_bar
                                        is_scene = ls_scene2 iv_dev_mode = iv_dev_mode ).
        rs_frame = ls_scene2-effect->render_frame( ls_ctx3 ).
      ENDIF.
    ENDIF.

    " Render overlay on top
    IF mo_overlay IS BOUND.
      DATA(ls_ovl_ctx) = VALUE zif_o4d_effect=>ty_render_ctx(
        t = iv_time gt = iv_time
        frame = mv_frame
        dev_mode = iv_dev_mode
        gbi = build_beat_info( iv_time )
      ).
      DATA(ls_ovl_frame) = mo_overlay->render_frame( ls_ovl_ctx ).
      APPEND LINES OF ls_ovl_frame-lines TO rs_frame-lines.
      APPEND LINES OF ls_ovl_frame-texts TO rs_frame-texts.
      APPEND LINES OF ls_ovl_frame-rects TO rs_frame-rects.
      APPEND LINES OF ls_ovl_frame-triangles TO rs_frame-triangles.
    ENDIF.
  ENDMETHOD.

  METHOD time_to_bar.
    rv_bar = iv_time / mv_bar_dur.
  ENDMETHOD.

  METHOD bar_to_time.
    rv_time = iv_bar * mv_bar_dur.
  ENDMETHOD.

  METHOD get_active_scenes.
    LOOP AT mt_scenes INTO DATA(ls_scene)
         WHERE start_bar <= iv_bar AND end_bar > iv_bar.
      APPEND ls_scene TO rt_scenes.
    ENDLOOP.
    SORT rt_scenes BY start_bar ASCENDING.
  ENDMETHOD.

  METHOD get_transition.
    READ TABLE mt_transitions INTO rs_trans
         WITH KEY from_scene = iv_from to_scene = iv_to.
  ENDMETHOD.

  METHOD build_context.
    DATA(lv_scene_start) = bar_to_time( is_scene-start_bar ).
    DATA(lv_local_time) = iv_time - lv_scene_start.
    DATA(lv_scene_dur) = bar_to_time( is_scene-end_bar - is_scene-start_bar ).
    DATA(lv_tick) = CONV i( lv_local_time * 30 ).  " Assuming 30fps

    rs_ctx = VALUE #(
      t = lv_local_time
      gt = iv_time
      frame = mv_frame
      tick = lv_tick
      scene = is_scene-id
      bi = build_beat_info( iv_time = lv_local_time )
      gbi = build_beat_info( iv_time = iv_time )
      dev_mode = iv_dev_mode
    ).

    " Get input from recording loader if available
    IF mo_recording IS BOUND.
      rs_ctx-input = mo_recording->get_frame( iv_time ).
    ENDIF.

    " Populate debug info
    rs_ctx-input-frame = mv_frame.
    rs_ctx-input-tick = lv_tick.
  ENDMETHOD.

  METHOD build_beat_info.
    DATA(lv_time) = iv_time - iv_offset.
    DATA(lv_beat_dur) = 60 / mv_bpm.

    rs_bi-time = lv_time.
    rs_bi-bar = CONV i( lv_time / mv_bar_dur ).
    rs_bi-beat = CONV i( lv_time / lv_beat_dur ) MOD 4.
    rs_bi-pos_4 = CONV i( lv_time / lv_beat_dur ).
    rs_bi-pos_8 = CONV i( lv_time / ( lv_beat_dur / 2 ) ).
    rs_bi-pos_16 = CONV i( lv_time / ( lv_beat_dur / 4 ) ).
    rs_bi-bar_phase = frac( lv_time / mv_bar_dur ).
    rs_bi-beat_phase = frac( lv_time / lv_beat_dur ).

    " Pulse decay: 1.0 at beat start, fades to 0
    rs_bi-pulse = nmax( val1 = 0 val2 = CONV f( 1 ) - rs_bi-beat_phase * 2 ).
    rs_bi-on_beat = xsdbool( rs_bi-beat_phase < CONV f( '0.05' ) ).
    rs_bi-on_bar = xsdbool( rs_bi-bar_phase < CONV f( '0.02' ) ).
  ENDMETHOD.

  METHOD merge_frames.
    " Simple crossfade: include all from both, alpha controls opacity hint
    " (actual alpha blending happens client-side based on color values)
    rs_frame-lines = is_frame1-lines.
    APPEND LINES OF is_frame2-lines TO rs_frame-lines.
    rs_frame-texts = is_frame1-texts.
    APPEND LINES OF is_frame2-texts TO rs_frame-texts.
    rs_frame-rects = is_frame1-rects.
    APPEND LINES OF is_frame2-rects TO rs_frame-rects.
    rs_frame-triangles = is_frame1-triangles.
    APPEND LINES OF is_frame2-triangles TO rs_frame-triangles.
  ENDMETHOD.

  METHOD get_current_scene.
    rv_id = mv_cur_scene.
  ENDMETHOD.

  METHOD get_scene_count.
    rv_count = lines( mt_scenes ).
  ENDMETHOD.
ENDCLASS.
