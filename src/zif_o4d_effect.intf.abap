INTERFACE zif_o4d_effect
  PUBLIC.

  "======================================================================
  " TYPES - Frame data (vector primitives)
  "======================================================================
  TYPES:
    BEGIN OF ty_line,
      x1    TYPE f,
      y1    TYPE f,
      x2    TYPE f,
      y2    TYPE f,
      color TYPE string,
      width TYPE f,
    END OF ty_line,
    tt_lines TYPE STANDARD TABLE OF ty_line WITH EMPTY KEY,

    BEGIN OF ty_text,
      x      TYPE f,
      y      TYPE f,
      text   TYPE string,
      color  TYPE string,
      size   TYPE f,
      align  TYPE string,
    END OF ty_text,
    tt_texts TYPE STANDARD TABLE OF ty_text WITH EMPTY KEY,

    BEGIN OF ty_triangle,
      x1   TYPE f,
      y1   TYPE f,
      x2   TYPE f,
      y2   TYPE f,
      x3   TYPE f,
      y3   TYPE f,
      fill TYPE string,
    END OF ty_triangle,
    tt_triangles TYPE STANDARD TABLE OF ty_triangle WITH EMPTY KEY,

    BEGIN OF ty_rect,
      x    TYPE f,
      y    TYPE f,
      w    TYPE f,
      h    TYPE f,
      fill TYPE string,
    END OF ty_rect,
    tt_rects TYPE STANDARD TABLE OF ty_rect WITH EMPTY KEY,

    BEGIN OF ty_circle,
      x      TYPE f,
      y      TYPE f,
      radius TYPE f,
      fill   TYPE string,
    END OF ty_circle,
    tt_circles TYPE STANDARD TABLE OF ty_circle WITH EMPTY KEY,

    " Image reference (rendered by JS from preloaded cache)
    BEGIN OF ty_image,
      name TYPE string,       " Image name (e.g., ZO4D_00_SALES)
      x    TYPE f,            " X position
      y    TYPE f,            " Y position
      w    TYPE f,            " Width (0 = natural)
      h    TYPE f,            " Height (0 = natural)
    END OF ty_image,
    tt_images TYPE STANDARD TABLE OF ty_image WITH EMPTY KEY,

    " Macro command (JS-rendered effect)
    BEGIN OF ty_macro_param,
      name  TYPE string,
      value TYPE string,
    END OF ty_macro_param,
    tt_macro_params TYPE STANDARD TABLE OF ty_macro_param WITH EMPTY KEY,

    BEGIN OF ty_macro,
      cmd    TYPE string,       " Effect name: starfield, plasma, copper, tunnel, etc.
      params TYPE string,       " JSON params: {"count":100,"speed":2}
    END OF ty_macro,
    tt_macros TYPE STANDARD TABLE OF ty_macro WITH EMPTY KEY,

    " Flash overlay (screen flash effect)
    BEGIN OF ty_flash,
      active    TYPE abap_bool,  " true = render flash
      intensity TYPE f,          " 0.0-1.0 alpha
      r         TYPE f,          " Red 0.0-1.0
      g         TYPE f,          " Green 0.0-1.0
      b         TYPE f,          " Blue 0.0-1.0
    END OF ty_flash,

    " Render state (frame-level defaults for shapes)
    BEGIN OF ty_render_state,
      line_width TYPE f,         " Default line width (0 = use 1)
      line_color TYPE string,    " Default line/stroke color
      line_cap   TYPE string,    " lineCap: butt, round, square
      line_join  TYPE string,    " lineJoin: miter, round, bevel
      alpha      TYPE f,         " globalAlpha (0 = use 1.0)
      blend      TYPE string,    " compositeOperation: source-over, lighter, etc.
    END OF ty_render_state.

  "======================================================================
  " TYPES - Debug (Dev Mode payload)
  "======================================================================
  TYPES:
    BEGIN OF ty_debug_input,
      x        TYPE i,
      y        TYPE i,
      x_norm   TYPE f,
      y_norm   TYPE f,
      pressed  TYPE abap_bool,
      triggers TYPE i,
    END OF ty_debug_input,

    BEGIN OF ty_debug,
      frame          TYPE i,      " Global frame number
      tick           TYPE i,      " Local frame in scene
      bar            TYPE i,
      beat           TYPE i,
      bar_phase      TYPE f,
      scene          TYPE string,
      scene_progress TYPE f,
      input          TYPE ty_debug_input,
      vars           TYPE string, " JSON blob for scene-specific vars
    END OF ty_debug.

  "======================================================================
  " TYPES - Output Frame
  "======================================================================
  TYPES:
    BEGIN OF ty_frame,
      lines      TYPE tt_lines,
      texts      TYPE tt_texts,
      triangles  TYPE tt_triangles,
      rects_back TYPE tt_rects,    " Background rects (rendered before circles)
      rects      TYPE tt_rects,    " Foreground rects (rendered after circles)
      circles    TYPE tt_circles,
      images     TYPE tt_images,   " Optional: image references with position
      macros     TYPE tt_macros,   " Optional: JS-rendered macro effects
      flash      TYPE ty_flash,    " Optional: screen flash overlay
      state      TYPE ty_render_state, " Optional: frame-level render defaults
      debug      TYPE ty_debug,    " Optional: only in dev mode
    END OF ty_frame.

  "======================================================================
  " TYPES - Beat Info (time/beat synchronization)
  "======================================================================
  TYPES:
    BEGIN OF ty_beat_info,
      time       TYPE f,         " time in seconds
      bar        TYPE i,         " bar number (4 beats)
      beat       TYPE i,         " beat number within bar (0-3)
      pos_4      TYPE i,         " position in 1/4 notes
      pos_8      TYPE i,         " position in 1/8 notes
      pos_16     TYPE i,         " position in 1/16 notes
      bar_phase  TYPE f,         " 0.0-1.0 phase within bar
      beat_phase TYPE f,         " 0.0-1.0 phase within beat
      pulse      TYPE f,         " beat pulse intensity
      on_beat    TYPE abap_bool, " true on beat start
      on_bar     TYPE abap_bool, " true on bar start
      on_4th     TYPE abap_bool, " true on 1/4 note start
      on_8th     TYPE abap_bool, " true on 1/8 note start
      on_16th    TYPE abap_bool, " true on 1/16 note start
      f1         TYPE f,         " pulse on bar (1.0 on bar, else 0.0)
      f4         TYPE f,         " pulse on 1/4 note
      f8         TYPE f,         " pulse on 1/8 note
      f16        TYPE f,         " pulse on 1/16 note
      i1         TYPE i,         " bar position (same as bar)
      i2         TYPE i,         " position in 1/2 notes
      i4         TYPE i,         " position in 1/4 notes
      i8         TYPE i,         " position in 1/8 notes
      i16        TYPE i,         " position in 1/16 notes
    END OF ty_beat_info.

  "======================================================================
  " TYPES - Input Frame (Pattern Mapper recording data)
  "======================================================================
  TYPES:
    BEGIN OF ty_recorded_point,
      t       TYPE f,
      x       TYPE f,
      y       TYPE f,
      pressed TYPE abap_bool,
    END OF ty_recorded_point,
    tt_recorded_points TYPE STANDARD TABLE OF ty_recorded_point WITH EMPTY KEY,

    BEGIN OF ty_recorded_trigger,
      t   TYPE f,
      key TYPE string,
      bit TYPE i,
    END OF ty_recorded_trigger,
    tt_recorded_triggers TYPE STANDARD TABLE OF ty_recorded_trigger WITH EMPTY KEY,

    BEGIN OF ty_input_frame,
      x        TYPE i,
      y        TYPE i,
      x_norm   TYPE f,
      y_norm   TYPE f,
      pressed  TYPE abap_bool,
      triggers TYPE i,
      t_global TYPE f,
      t_local  TYPE f,
      frame    TYPE i,           " Global frame number
      tick     TYPE i,           " Local frame in scene
      global_rec TYPE tt_recorded_points,
      local_rec  TYPE tt_recorded_points,
      trigger_events TYPE tt_recorded_triggers,
    END OF ty_input_frame.

  "======================================================================
  " TYPES - Render Context
  "======================================================================
  TYPES:
    BEGIN OF ty_render_ctx,
      t           TYPE f,            " relative time from scene start (seconds)
      gt          TYPE f,            " global demo time (seconds)
      f           TYPE i,            " local frame within scene (integer)
      gf          TYPE i,            " global frame number (integer)
      tick        TYPE i,            " local tick within scene (integer, 64 per bar)
      gtick       TYPE i,            " global tick number (integer, 64 per bar)
      bi          TYPE ty_beat_info, " beat info relative to scene
      gbi         TYPE ty_beat_info, " global beat info
      scene       TYPE string,       " current scene id
      transition  TYPE abap_bool,    " true during transition
      prev_effect TYPE string,       " previous effect name
      dev_mode    TYPE abap_bool,    " true = include debug in output
      input       TYPE ty_input_frame,
    END OF ty_render_ctx,

    " Legacy sync (for compatibility)
    BEGIN OF ty_sync,
      time      TYPE f,
      bar       TYPE i,
      beat      TYPE i,
      bar_phase TYPE f,
      intensity TYPE f,
    END OF ty_sync.

  "======================================================================
  " TYPES - Effect parameters
  "======================================================================
  TYPES:
    BEGIN OF ty_param,
      name  TYPE string,
      type  TYPE string,
      value TYPE string,
      min   TYPE string,
      max   TYPE string,
      descr TYPE string,
    END OF ty_param,
    tt_params TYPE STANDARD TABLE OF ty_param WITH EMPTY KEY.

  "======================================================================
  " CONSTANTS
  "======================================================================
  CONSTANTS:
    c_width  TYPE i VALUE 640,
    c_height TYPE i VALUE 400,
    c_pi     TYPE f VALUE '3.14159265358979'.

  " Trigger bits (Pattern Mapper)
  CONSTANTS:
    c_trigger_flash      TYPE i VALUE 0,
    c_trigger_spawn      TYPE i VALUE 1,
    c_trigger_transition TYPE i VALUE 2,
    c_trigger_text       TYPE i VALUE 3,
    c_trigger_color      TYPE i VALUE 4,
    c_trigger_speed      TYPE i VALUE 5,
    c_trigger_reverse    TYPE i VALUE 6,
    c_trigger_special    TYPE i VALUE 7.

  "======================================================================
  " TYPES - Media (for preloading)
  "======================================================================
  TYPES:
    BEGIN OF ty_media,
      name TYPE string,       " File name without extension (e.g., ZO4D_00_SALES)
      type TYPE string,       " Type: img, audio
    END OF ty_media,
    tt_media TYPE STANDARD TABLE OF ty_media WITH EMPTY KEY.

  "======================================================================
  " METHODS
  "======================================================================
  METHODS:
    get_name
      RETURNING VALUE(rv_name) TYPE string,

    get_required_media
      RETURNING VALUE(rt_media) TYPE tt_media,

    get_params
      RETURNING VALUE(rt_params) TYPE tt_params,

    set_param
      IMPORTING
        iv_name  TYPE string
        iv_value TYPE string,

    render
      IMPORTING is_sync TYPE ty_sync
      RETURNING VALUE(rs_frame) TYPE ty_frame,

    render_frame
      IMPORTING is_ctx TYPE ty_render_ctx
      RETURNING VALUE(rs_frame) TYPE ty_frame,

    " Returns true if effect is designed for infinite looping
    is_loopable
      RETURNING VALUE(rv_loopable) TYPE abap_bool.

ENDINTERFACE.
