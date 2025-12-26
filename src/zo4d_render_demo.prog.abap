*&---------------------------------------------------------------------*
*& Report ZO4D_RENDER_DEMO
*& Render vector demo and output JSON for browser player
*&---------------------------------------------------------------------*
REPORT zo4d_render_demo.

PARAMETERS:
  p_dur TYPE p DECIMALS 1 DEFAULT '30.0',
  p_dl  TYPE abap_bool AS CHECKBOX DEFAULT abap_true.

START-OF-SELECTION.

  DATA: lo_demo      TYPE REF TO zcl_o4d_demo,
        lo_starfield TYPE REF TO zcl_o4d_starfield,
        lo_tunnel    TYPE REF TO zcl_o4d_tunnel,
        lt_frames    TYPE zcl_o4d_demo=>tt_output_frames,
        lv_json      TYPE string,
        lv_duration  TYPE f.

  lv_duration = p_dur.

  " Create demo
  lo_demo = NEW zcl_o4d_demo( ).

  " Create effects
  lo_starfield = NEW zcl_o4d_starfield( ).
  lo_tunnel = NEW zcl_o4d_tunnel( ).

  " Configure starfield
  lo_starfield->zif_o4d_effect~set_param( iv_name = 'num_stars' iv_value = '200' ).
  lo_starfield->zif_o4d_effect~set_param( iv_name = 'speed' iv_value = '1.5' ).

  " Configure tunnel
  lo_tunnel->zif_o4d_effect~set_param( iv_name = 'num_rings' iv_value = '25' ).

  " Build timeline
  lo_demo->add_scene(
    iv_bar_start = 0
    iv_bar_end   = 4
    io_effect    = lo_starfield
  ).

  lo_demo->add_scene(
    iv_bar_start = 4
    iv_bar_end   = 8
    io_effect    = lo_tunnel
  ).

  lo_demo->add_scene(
    iv_bar_start = 8
    iv_bar_end   = 12
    io_effect    = lo_starfield
  ).

  lo_demo->add_scene(
    iv_bar_start = 12
    iv_bar_end   = 20
    io_effect    = lo_tunnel
  ).

  " Render
  WRITE: / 'Rendering demo...'.
  lt_frames = lo_demo->render( lv_duration ).
  WRITE: / |Rendered { lines( lt_frames ) } frames|.

  " Convert to JSON
  lv_json = lo_demo->to_json(
    it_frames = lt_frames
    iv_name   = 'ABAP VECTOR DEMO v12'
  ).

  WRITE: / |JSON size: { strlen( lv_json ) } bytes|.

  " Output or download
  IF p_dl = abap_true.
    " Download as file
    DATA: lt_data TYPE STANDARD TABLE OF string.
    APPEND lv_json TO lt_data.

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        filename = 'C:\temp\demo_abap.json'
        filetype = 'ASC'
      CHANGING
        data_tab = lt_data
      EXCEPTIONS
        OTHERS   = 1
    ).

    IF sy-subrc = 0.
      WRITE: / 'Downloaded to C:\temp\demo_abap.json'.
    ELSE.
      WRITE: / 'Download failed - showing JSON below:'.
      WRITE: / lv_json.
    ENDIF.
  ELSE.
    " Just display
    WRITE: / lv_json.
  ENDIF.
