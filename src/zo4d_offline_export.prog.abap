*&---------------------------------------------------------------------*
*& Report ZO4D_OFFLINE_EXPORT
*&---------------------------------------------------------------------*
*& Generates offline demo package to CLIENT:
*& - demo_frames.json (pre-rendered frames)
*& - demo_player.html (standalone player)
*&---------------------------------------------------------------------*
REPORT zo4d_offline_export.

CONSTANTS: c_bpm      TYPE f VALUE '76.0',
           c_fps      TYPE i VALUE 30,
           c_bar_sec  TYPE f VALUE '3.157894736',
           c_beat_sec TYPE f VALUE '0.789473684'.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_dur   TYPE i DEFAULT 210,
              p_fps   TYPE i DEFAULT 30.
SELECTION-SCREEN END OF BLOCK b1.

CLASS lcl_offline_export DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor,
             export_demo.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_frame_data,
             frame_num TYPE i,
             time      TYPE f,
             json      TYPE string,
           END OF ty_frame_data.

    DATA: mo_demo        TYPE REF TO zcl_o4d_demo,
          mt_frames      TYPE STANDARD TABLE OF ty_frame_data,
          mv_last_effect TYPE string,
          mv_effect_start_bar TYPE i,
          mv_client_path TYPE string.

    DATA: mo_starfield TYPE REF TO zcl_o4d_starfield, mo_copperbars TYPE REF TO zcl_o4d_copperbars,
          mo_joydiv TYPE REF TO zcl_o4d_joydivision, mo_plasma TYPE REF TO zcl_o4d_plasma,
          mo_tunnel TYPE REF TO zcl_o4d_tunnel, mo_elite3d TYPE REF TO zcl_o4d_elite3d,
          mo_metaballs TYPE REF TO zcl_o4d_metaballs, mo_twister TYPE REF TO zcl_o4d_twister,
          mo_constellation TYPE REF TO zcl_o4d_constellation, mo_sales_quarter TYPE REF TO zcl_o4d_sales_quarter,
          mo_starburst TYPE REF TO zcl_o4d_starburst, mo_voxel TYPE REF TO zcl_o4d_voxel_landscape,
          mo_kaleidoscope TYPE REF TO zcl_o4d_kaleidoscope,
          mo_ignition TYPE REF TO zcl_o4d_ignition, mo_amigaball TYPE REF TO zcl_o4d_amigaball,
          mo_amigaball2 TYPE REF TO zcl_o4d_amigaball2,
          mo_glitch TYPE REF TO zcl_o4d_glitch, mo_firegreetings TYPE REF TO zcl_o4d_firegreetings,
          mo_mountains TYPE REF TO zcl_o4d_mountains, mo_swirl TYPE REF TO zcl_o4d_swirl,
          mo_greetings TYPE REF TO zcl_o4d_greetings, mo_sierpinski TYPE REF TO zcl_o4d_sierpinski,
          mo_lowpoly TYPE REF TO zcl_o4d_lowpoly, mo_rotozoom TYPE REF TO zcl_o4d_rotozoom.

    METHODS: init_demo,
             ask_client_folder RETURNING VALUE(rv_ok) TYPE abap_bool,
             render_all_frames,
             calc_beat_info IMPORTING iv_time TYPE f RETURNING VALUE(rs_bi) TYPE zif_o4d_effect=>ty_beat_info,
             build_render_ctx IMPORTING iv_global_time TYPE f RETURNING VALUE(rs_ctx) TYPE zif_o4d_effect=>ty_render_ctx,
             frame_to_json IMPORTING is_frame TYPE zif_o4d_effect=>ty_frame is_ctx TYPE zif_o4d_effect=>ty_render_ctx
                           RETURNING VALUE(rv_json) TYPE string,
             save_to_client IMPORTING iv_filename TYPE string iv_content TYPE string,
             generate_html_player RETURNING VALUE(rv_html) TYPE string.
ENDCLASS.

CLASS lcl_offline_export IMPLEMENTATION.
  METHOD constructor.
    init_demo( ).
  ENDMETHOD.

  METHOD export_demo.
    DATA: lt_json TYPE STANDARD TABLE OF string,
          lv_json_size TYPE i,
          lv_mp3_xdata TYPE xstring,
          lt_mp3_bin TYPE STANDARD TABLE OF x255.

    IF ask_client_folder( ) = abap_false.
      WRITE: / 'Export cancelled.'.
      RETURN.
    ENDIF.

    WRITE: / '╔══════════════════════════════════════════════════════╗'.
    WRITE: / '║  BYTEBEAT-ABAP OFFLINE EXPORT                        ║'.
    WRITE: / '╚══════════════════════════════════════════════════════╝'.
    WRITE: / ''.
    WRITE: / |Duration: { p_dur } seconds @ { p_fps } FPS|.
    WRITE: / |Total frames: { p_dur * p_fps }|.
    WRITE: / |Output: { mv_client_path }|.
    WRITE: / ''.

    " Render frames directly to JSON table (no intermediate storage!)
    WRITE: / ''.
    WRITE: / 'Rendering frames to JSON...'.
    APPEND |[| TO lt_json.
    DATA(lv_total) = p_dur * p_fps.
    DATA(lv_last_pct) = -1.
    mv_last_effect = ''.
    mv_effect_start_bar = 0.

    DO lv_total TIMES.
      DATA(lv_frame_num) = sy-index - 1.
      DATA(lv_time) = CONV f( lv_frame_num ) / p_fps.
      DATA(ls_ctx) = build_render_ctx( lv_time ).

      DATA(lo_effect) = mo_demo->get_effect_at_bar( ls_ctx-gbi-bar ).
      DATA(ls_frame) = COND zif_o4d_effect=>ty_frame( WHEN lo_effect IS BOUND
                                                       THEN lo_effect->render_frame( ls_ctx ) ).

      DATA(lv_json) = frame_to_json( is_frame = ls_frame is_ctx = ls_ctx ).
      DATA(lv_line) = COND string( WHEN lv_frame_num = 0 THEN lv_json ELSE |,| && lv_json ).
      APPEND lv_line TO lt_json.
      lv_json_size = lv_json_size + strlen( lv_line ).

      DATA(lv_pct) = lv_frame_num * 100 / lv_total.
      IF lv_pct <> lv_last_pct AND lv_pct MOD 10 = 0.
        WRITE: / |  { lv_pct }% ({ lv_frame_num }/{ lv_total }) { mv_last_effect }|.
        lv_last_pct = lv_pct.
      ENDIF.
    ENDDO.

    APPEND |]| TO lt_json.
    WRITE: / |  100% - { lv_total } frames rendered|.

    " Write JSON to client
    WRITE: / 'Saving JSON...'.
    DATA(lv_json_path) = mv_client_path && 'demo_frames.json'.
    cl_gui_frontend_services=>gui_download(
      EXPORTING filename = lv_json_path filetype = 'ASC'
      CHANGING data_tab = lt_json
      EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc = 0.
      WRITE: / |  Saved: { lv_json_path } ({ lv_json_size / 1024 } KB)|.
    ELSE.
      WRITE: / |  ERROR saving JSON|.
    ENDIF.

    " Save HTML player
    WRITE: / 'Saving HTML player...'.
    save_to_client( iv_filename = 'bytebeat_demo.html' iv_content = generate_html_player( ) ).

    " Export MP3 from MIME repository
    WRITE: / 'Exporting MP3...'.
    cl_mime_repository_api=>get_api( )->get(
      EXPORTING i_url = '/SAP/PUBLIC/ZO4D/oisee-ear-02.mp3'
      IMPORTING e_content = lv_mp3_xdata
      EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc = 0 AND lv_mp3_xdata IS NOT INITIAL.
      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING buffer = lv_mp3_xdata
        TABLES binary_tab = lt_mp3_bin.
      DATA(lv_mp3_path) = mv_client_path && 'oisee-ear-02.mp3'.
      DATA(lv_mp3_len) = xstrlen( lv_mp3_xdata ).
      cl_gui_frontend_services=>gui_download(
        EXPORTING filename = lv_mp3_path filetype = 'BIN' bin_filesize = lv_mp3_len
        CHANGING data_tab = lt_mp3_bin
        EXCEPTIONS OTHERS = 1 ).
      IF sy-subrc = 0.
        WRITE: / |  Saved: { lv_mp3_path } ({ lv_mp3_len / 1024 } KB)|.
      ELSE.
        WRITE: / '  ERROR saving MP3'.
      ENDIF.
    ELSE.
      WRITE: / '  MP3 not in MIME repo - copy manually'.
    ENDIF.

    WRITE: / ''.
    WRITE: / '╔══════════════════════════════════════════════════════╗'.
    WRITE: / '║  EXPORT COMPLETE!                                    ║'.
    WRITE: / '╠══════════════════════════════════════════════════════╣'.
    WRITE: / |║  Files: bytebeat_demo.html + demo_frames.json + mp3  ║|.
    WRITE: / |║  Frames: { lv_total WIDTH = 5 }                                    ║|.
    WRITE: / |║  JSON: { lv_json_size / 1024 WIDTH = 7 DECIMALS = 1 } KB                                   ║|.
    WRITE: / '╠══════════════════════════════════════════════════════╣'.
    WRITE: / '║  USAGE: Open bytebeat_demo.html, load files, PLAY    ║'.
    WRITE: / '║  Works from file:// - no server needed!              ║'.
    WRITE: / '╚══════════════════════════════════════════════════════╝'.
  ENDMETHOD.

  METHOD ask_client_folder.
    DATA: lv_folder TYPE string,
          lv_len TYPE i,
          lv_last_char TYPE string.

    cl_gui_frontend_services=>directory_browse(
      EXPORTING
        window_title = 'Select folder for demo export'
        initial_folder = 'C:\'
      CHANGING
        selected_folder = lv_folder
      EXCEPTIONS
        OTHERS = 1 ).

    IF sy-subrc <> 0 OR lv_folder IS INITIAL.
      rv_ok = abap_false.
      RETURN.
    ENDIF.

    mv_client_path = lv_folder.
    lv_len = strlen( mv_client_path ).
    IF lv_len > 0.
      lv_last_char = substring( val = mv_client_path off = lv_len - 1 len = 1 ).
      IF lv_last_char <> '\' AND lv_last_char <> '/'.
        mv_client_path = mv_client_path && '\'.
      ENDIF.
    ENDIF.

    rv_ok = abap_true.
  ENDMETHOD.

  METHOD save_to_client.
    DATA: lt_data TYPE STANDARD TABLE OF string,
          lv_fullpath TYPE string.

    APPEND iv_content TO lt_data.
    lv_fullpath = mv_client_path && iv_filename.

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        filename = lv_fullpath
        filetype = 'ASC'
      CHANGING
        data_tab = lt_data
      EXCEPTIONS
        OTHERS = 1 ).

    IF sy-subrc = 0.
      WRITE: / |  Saved: { lv_fullpath }|.
    ELSE.
      WRITE: / |  ERROR saving: { lv_fullpath }|.
    ENDIF.
  ENDMETHOD.

  METHOD init_demo.
    mo_demo = NEW zcl_o4d_demo( ).

    mo_starfield = NEW #( ). mo_copperbars = NEW #( ). mo_joydiv = NEW #( ). mo_plasma = NEW #( ).
    mo_tunnel = NEW #( ). mo_elite3d = NEW #( ). mo_metaballs = NEW #( ). mo_twister = NEW #( ).
    mo_constellation = NEW #( ). mo_sales_quarter = NEW #( ). mo_starburst = NEW #( ).
    mo_voxel = NEW #( ). mo_kaleidoscope = NEW #( ). mo_rotozoom = NEW #( ).
    mo_ignition = NEW #( ). mo_amigaball = NEW #( ). mo_amigaball2 = NEW #( ). mo_glitch = NEW #( ).
    mo_firegreetings = NEW #( ). mo_mountains = NEW #( ). mo_swirl = NEW #( ).
    mo_greetings = NEW #( ). mo_sierpinski = NEW #( ). mo_lowpoly = NEW #( ).

    mo_starfield->zif_o4d_effect~set_param( iv_name = 'num_stars' iv_value = '200' ).
    mo_tunnel->zif_o4d_effect~set_param( iv_name = 'num_rings' iv_value = '25' ).

    mo_demo->add_scene( iv_bar_start = 0  iv_bar_end = 4  io_effect = mo_ignition ).
    mo_demo->add_scene( iv_bar_start = 4  iv_bar_end = 8  io_effect = mo_starfield ).
    mo_demo->add_scene( iv_bar_start = 8  iv_bar_end = 12 io_effect = mo_sales_quarter ).
    mo_demo->add_scene( iv_bar_start = 12 iv_bar_end = 16 io_effect = mo_constellation ).
    mo_demo->add_scene( iv_bar_start = 16 iv_bar_end = 20 io_effect = mo_copperbars ).
    mo_demo->add_scene( iv_bar_start = 20 iv_bar_end = 24 io_effect = mo_joydiv ).
    mo_demo->add_scene( iv_bar_start = 24 iv_bar_end = 28 io_effect = mo_voxel ).
    mo_demo->add_scene( iv_bar_start = 28 iv_bar_end = 32 io_effect = mo_plasma ).
    mo_demo->add_scene( iv_bar_start = 32 iv_bar_end = 36 io_effect = mo_rotozoom ).
    mo_demo->add_scene( iv_bar_start = 36 iv_bar_end = 40 io_effect = mo_amigaball ).
    mo_demo->add_scene( iv_bar_start = 40 iv_bar_end = 44 io_effect = mo_elite3d ).
    mo_demo->add_scene( iv_bar_start = 44 iv_bar_end = 48 io_effect = mo_kaleidoscope ).
    mo_demo->add_scene( iv_bar_start = 48 iv_bar_end = 52 io_effect = mo_mountains ).
    mo_demo->add_scene( iv_bar_start = 52 iv_bar_end = 56 io_effect = mo_lowpoly ).
    mo_demo->add_scene( iv_bar_start = 56 iv_bar_end = 58 io_effect = mo_glitch ).
    mo_demo->add_scene( iv_bar_start = 58 iv_bar_end = 62 io_effect = mo_metaballs ).
    mo_demo->add_scene( iv_bar_start = 62 iv_bar_end = 66 io_effect = mo_tunnel ).
    mo_demo->add_scene( iv_bar_start = 66 iv_bar_end = 72 io_effect = mo_sierpinski ).
    mo_demo->add_scene( iv_bar_start = 72 iv_bar_end = 76 io_effect = mo_starburst ).
    mo_demo->add_scene( iv_bar_start = 76 iv_bar_end = 80 io_effect = mo_swirl ).
    mo_demo->add_scene( iv_bar_start = 80 iv_bar_end = 84 io_effect = mo_amigaball2 ).
    mo_demo->add_scene( iv_bar_start = 84 iv_bar_end = 88 io_effect = mo_plasma ).
    mo_demo->add_scene( iv_bar_start = 88 iv_bar_end = 92 io_effect = mo_elite3d ).
    mo_demo->add_scene( iv_bar_start = 92 iv_bar_end = 96 io_effect = mo_starfield ).
    mo_demo->add_scene( iv_bar_start = 96 iv_bar_end = 100 io_effect = mo_firegreetings ).
    mo_demo->add_scene( iv_bar_start = 100 iv_bar_end = 108 io_effect = mo_twister ).
    mo_demo->add_scene( iv_bar_start = 108 iv_bar_end = 116 io_effect = mo_greetings ).
    mo_demo->add_scene( iv_bar_start = 116 iv_bar_end = 999 io_effect = mo_starfield ).
  ENDMETHOD.

  METHOD render_all_frames.
    DATA: lo_effect TYPE REF TO zif_o4d_effect,
          ls_frame  TYPE zif_o4d_effect=>ty_frame,
          lv_total  TYPE i,
          lv_pct    TYPE i,
          lv_last_pct TYPE i VALUE -1.

    lv_total = p_dur * p_fps.
    CLEAR mt_frames.
    mv_last_effect = ''.
    mv_effect_start_bar = 0.

    DO lv_total TIMES.
      DATA(lv_frame_num) = sy-index - 1.
      DATA(lv_time) = CONV f( lv_frame_num ) / p_fps.
      DATA(ls_ctx) = build_render_ctx( lv_time ).

      lo_effect = mo_demo->get_effect_at_bar( ls_ctx-gbi-bar ).
      IF lo_effect IS BOUND.
        ls_frame = lo_effect->render_frame( ls_ctx ).
      ELSE.
        CLEAR ls_frame.
      ENDIF.

      APPEND VALUE ty_frame_data(
        frame_num = lv_frame_num
        time = lv_time
        json = frame_to_json( is_frame = ls_frame is_ctx = ls_ctx )
      ) TO mt_frames.

      lv_pct = lv_frame_num * 100 / lv_total.
      IF lv_pct <> lv_last_pct AND lv_pct MOD 5 = 0.
        WRITE: / |Progress: { lv_pct }% ({ lv_frame_num }/{ lv_total }) - { mv_last_effect }|.
        lv_last_pct = lv_pct.
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD calc_beat_info.
    rs_bi-time = iv_time.
    rs_bi-bar = floor( iv_time / c_bar_sec ).
    rs_bi-bar_phase = frac( iv_time / c_bar_sec ).
    rs_bi-beat = floor( rs_bi-bar_phase * 4 ).
    rs_bi-beat_phase = frac( rs_bi-bar_phase * 4 ).
    rs_bi-pos_4 = rs_bi-bar * 4 + rs_bi-beat.
    rs_bi-pos_8 = floor( iv_time / ( c_beat_sec / 2 ) ).
    rs_bi-pos_16 = floor( iv_time / ( c_beat_sec / 4 ) ).
    rs_bi-pulse = exp( -8 * rs_bi-beat_phase ).
    rs_bi-on_beat = xsdbool( rs_bi-beat_phase < CONV f( '0.05' ) ).
    rs_bi-on_bar = xsdbool( rs_bi-bar_phase < CONV f( '0.02' ) ).
    rs_bi-tick = floor( iv_time * p_fps ).
  ENDMETHOD.

  METHOD build_render_ctx.
    DATA: lo_effect TYPE REF TO zif_o4d_effect, lv_effect_name TYPE string.
    rs_ctx-gt = iv_global_time.
    rs_ctx-gbi = calc_beat_info( iv_global_time ).

    lo_effect = mo_demo->get_effect_at_bar( rs_ctx-gbi-bar ).
    IF lo_effect IS BOUND. lv_effect_name = lo_effect->get_name( ). ENDIF.

    IF lv_effect_name <> mv_last_effect.
      rs_ctx-transition = abap_true. rs_ctx-prev_effect = mv_last_effect.
      mv_last_effect = lv_effect_name. mv_effect_start_bar = rs_ctx-gbi-bar.
    ELSE. rs_ctx-transition = abap_false. ENDIF.

    DATA(lv_est) = CONV f( mv_effect_start_bar ) * c_bar_sec.
    rs_ctx-t = iv_global_time - lv_est.
    rs_ctx-bi = calc_beat_info( rs_ctx-t ).
  ENDMETHOD.

  METHOD frame_to_json.
    DATA: lv_lines TYPE string, lv_texts TYPE string, lv_tris TYPE string, lv_rects TYPE string.

    lv_lines = |[|.
    LOOP AT is_frame-lines INTO DATA(ls_l).
      IF sy-tabix > 1. lv_lines = lv_lines && |,|. ENDIF.
      lv_lines = lv_lines && |[{ ls_l-x1 },{ ls_l-y1 },{ ls_l-x2 },{ ls_l-y2 },"{ ls_l-color }",{ ls_l-width }]|.
    ENDLOOP.
    lv_lines = lv_lines && |]|.

    lv_texts = |[|.
    LOOP AT is_frame-texts INTO DATA(ls_t).
      IF sy-tabix > 1. lv_texts = lv_texts && |,|. ENDIF.
      lv_texts = lv_texts && |[{ ls_t-x },{ ls_t-y },"{ ls_t-text }","{ ls_t-color }",{ ls_t-size }]|.
    ENDLOOP.
    lv_texts = lv_texts && |]|.

    lv_tris = |[|.
    LOOP AT is_frame-triangles INTO DATA(ls_tri).
      IF sy-tabix > 1. lv_tris = lv_tris && |,|. ENDIF.
      lv_tris = lv_tris && |[{ ls_tri-x1 },{ ls_tri-y1 },{ ls_tri-x2 },{ ls_tri-y2 },| &&
                           |{ ls_tri-x3 },{ ls_tri-y3 },"{ ls_tri-fill }"]|.
    ENDLOOP.
    lv_tris = lv_tris && |]|.

    lv_rects = |[|.
    LOOP AT is_frame-rects INTO DATA(ls_r).
      IF sy-tabix > 1. lv_rects = lv_rects && |,|. ENDIF.
      lv_rects = lv_rects && |[{ ls_r-x },{ ls_r-y },{ ls_r-w },{ ls_r-h },"{ ls_r-fill }"]|.
    ENDLOOP.
    lv_rects = lv_rects && |]|.

    rv_json = |[{ is_ctx-gt },{ is_ctx-bi-pulse },"{ mv_last_effect }",{ lv_lines },{ lv_texts },{ lv_tris },{ lv_rects }]|.
  ENDMETHOD.

  METHOD generate_html_player.
    DATA(nl) = cl_abap_char_utilities=>newline.

    " HTML player with file inputs (works from file:// without CORS)
    rv_html =
|<!DOCTYPE html>| && nl &&
|<html lang="en"><head><meta charset="UTF-8">| && nl &&
|<title>BYTEBEAT-ABAP // OFFLINE DEMO</title>| && nl &&
|<style>| && nl &&
|*\{margin:0;padding:0;box-sizing:border-box\}| && nl &&
|body\{background:#000;color:#0f0;font-family:monospace;display:flex;flex-direction:column;align-items:center;padding:20px\}| && nl &&
|h1\{color:#0ff;margin-bottom:5px;text-shadow:0 0 10px #0ff\}| && nl &&
|.sub\{color:#080;margin-bottom:15px\}| && nl &&
|#canvas\{border:2px solid #0f0;image-rendering:pixelated;cursor:pointer\}| && nl &&
|#controls\{margin-top:15px;display:flex;gap:10px;align-items:center;flex-wrap:wrap;justify-content:center\}| && nl &&
|button\{background:#0f0;color:#000;border:none;padding:10px 25px;font-family:monospace;cursor:pointer\}| && nl &&
|button:hover\{background:#0c0\}button:disabled\{background:#030;color:#060\}| && nl &&
|.ldr\{padding:8px 12px;background:#111;border:1px solid #ff0;color:#ff0;cursor:pointer;font-size:12px\}| && nl &&
|.ldr:hover\{background:#020\}.ldr input\{display:none\}.ldr.ok\{border-color:#0f0;color:#0f0\}| && nl &&
|#status\{margin-top:10px;color:#ff0\}#info\{margin-top:8px;color:#080;font-size:11px\}| && nl &&
|</style></head>| && nl &&
|<body><h1>BYTEBEAT-ABAP</h1>| && nl &&
|<p class="sub">OFFLINE DEMO // Load files below</p>| && nl &&
|<canvas id="canvas" width="640" height="400"></canvas>| && nl &&
|<div id="controls">| && nl &&
|<label class="ldr" id="jldr"><span>1.Load JSON</span><input type="file" id="jsonF" accept=".json"></label>| && nl &&
|<label class="ldr" id="mldr"><span>2.Load MP3</span><input type="file" id="mp3F" accept=".mp3,audio/*"></label>| && nl &&
|<button id="playBtn" disabled>PLAY</button>| && nl &&
|<span>VOL:</span><input type="range" id="vol" min="0" max="100" value="50"><span id="vv">50%</span>| && nl &&
|</div>| && nl &&
|<div id="status">Load demo_frames.json and oisee-ear-02.mp3</div>| && nl &&
|<div id="info">640x400 @ { p_fps }fps // 76 BPM // F=fullscreen SPACE=play</div>| && nl &&
|<audio id="audio"></audio>| && nl &&
|<script>| && nl &&
|const C=document.getElementById('canvas'),X=C.getContext('2d'),A=document.getElementById('audio'),| && nl &&
|P=document.getElementById('playBtn'),V=document.getElementById('vol'),VV=document.getElementById('vv'),| && nl &&
|S=document.getElementById('status'),JL=document.getElementById('jldr'),ML=document.getElementById('mldr'),| && nl &&
|JF=document.getElementById('jsonF'),MF=document.getElementById('mp3F');| && nl &&
|let frames=[],playing=0,anim=0,hasJson=0,hasAudio=0;const FPS={ p_fps };| && nl &&
|function chk()\{if(hasJson&&hasAudio)\{P.disabled=0;S.textContent='Ready! Press PLAY or SPACE';\}\}| && nl &&
|JF.onchange=e=>\{const f=e.target.files[0];if(!f)return;S.textContent='Loading JSON...';| && nl &&
|const r=new FileReader();r.onload=ev=>\{try\{frames=JSON.parse(ev.target.result);hasJson=1;| && nl &&
|JL.classList.add('ok');JL.querySelector('span').textContent='JSON OK ('+frames.length+')';| && nl &&
|if(frames.length)R(frames[0]);chk();\}catch(e)\{S.textContent='JSON error: '+e.message;\}\};r.readAsText(f);\};| && nl &&
|MF.onchange=e=>\{const f=e.target.files[0];if(!f)return;A.src=URL.createObjectURL(f);hasAudio=1;| && nl &&
|ML.classList.add('ok');ML.querySelector('span').textContent='MP3 OK';chk();\};| && nl &&
|function R(f)\{X.fillStyle='#000';X.fillRect(0,0,640,400);| && nl &&
|for(const t of f[5])\{X.beginPath();X.moveTo(t[0],t[1]);X.lineTo(t[2],t[3]);X.lineTo(t[4],t[5]);X.closePath();X.fillStyle=t[6];X.fill();\}| && nl &&
|for(const r of f[6])\{X.fillStyle=r[4];X.fillRect(r[0],r[1],r[2],r[3]);\}| && nl &&
|for(const l of f[3])\{X.beginPath();X.moveTo(l[0],l[1]);X.lineTo(l[2],l[3]);X.strokeStyle=l[4];X.lineWidth=l[5];X.stroke();\}| && nl &&
|for(const t of f[4])\{X.fillStyle=t[3];X.font=t[4]+'px monospace';X.textAlign='center';X.fillText(t[2],t[0],t[1]);\}| && nl &&
|X.fillStyle='#0f04';X.font='10px monospace';X.textAlign='left';X.fillText(f[2]+' t='+f[0].toFixed(1),5,395);\}| && nl &&
|function loop()\{if(!playing)return;const i=Math.floor(A.currentTime*FPS);| && nl &&
|if(i>=0&&i<frames.length)\{R(frames[i]);S.textContent='Frame '+i+'/'+frames.length+' - '+frames[i][2];\}| && nl &&
|anim=requestAnimationFrame(loop);\}| && nl &&
|P.onclick=()=>\{if(!hasJson\|\|!hasAudio)return;if(!playing)\{A.play();playing=1;P.textContent='PAUSE';loop();\}| && nl &&
|else\{A.pause();playing=0;P.textContent='PLAY';cancelAnimationFrame(anim);\}\};| && nl &&
|V.oninput=()=>\{A.volume=V.value/100;VV.textContent=V.value+'%';\};A.volume=0.5;| && nl &&
|document.onkeydown=e=>\{if(e.key===' ')\{e.preventDefault();P.click();\}if(e.key==='f')C.requestFullscreen?.();\};| && nl &&
|C.onclick=()=>C.requestFullscreen?.();| && nl &&
|X.fillStyle='#111';X.fillRect(0,0,640,400);X.fillStyle='#0f0';X.font='16px monospace';X.textAlign='center';| && nl &&
|X.fillText('BYTEBEAT-ABAP OFFLINE',320,180);X.fillText('Load JSON + MP3 to start',320,210);| && nl &&
|</script></body></html>|.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA(lo_export) = NEW lcl_offline_export( ).
  lo_export->export_demo( ).
