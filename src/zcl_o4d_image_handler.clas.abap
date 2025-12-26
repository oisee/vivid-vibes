CLASS zcl_o4d_image_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_http_extension.

  PRIVATE SECTION.
    METHODS get_image_from_smw0
      IMPORTING iv_name TYPE clike
      EXPORTING ev_data TYPE xstring
                ev_mime TYPE string
      RETURNING VALUE(rv_found) TYPE abap_bool.

ENDCLASS.

CLASS zcl_o4d_image_handler IMPLEMENTATION.

  METHOD if_http_extension~handle_request.
    DATA: lv_path  TYPE string,
          lv_name  TYPE string,
          lv_data  TYPE xstring,
          lv_mime  TYPE string,
          lv_found TYPE abap_bool.

    lv_path = server->request->get_header_field( '~path_info' ).

    IF lv_path IS NOT INITIAL.
      lv_name = lv_path.
      SHIFT lv_name LEFT DELETING LEADING '/'.
    ENDIF.

    IF lv_name IS INITIAL.
      server->response->set_header_field( name = 'Content-Type' value = 'application/json' ).
      server->response->set_cdata( '{"status":"ok","info":"O4D Image Handler"}' ).
      server->response->set_status( code = 200 reason = 'OK' ).
      RETURN.
    ENDIF.

    DATA(lv_lookup) = to_upper( lv_name ).

    lv_found = get_image_from_smw0(
      EXPORTING iv_name = lv_lookup
      IMPORTING ev_data = lv_data
                ev_mime = lv_mime
    ).

    IF lv_found = abap_true.
      server->response->set_header_field( name = 'Access-Control-Allow-Origin' value = '*' ).
      server->response->set_header_field( name = 'Cache-Control' value = 'public, max-age=3600' ).
      server->response->set_header_field( name = 'Content-Type' value = lv_mime ).
      server->response->set_data( lv_data ).
      server->response->set_status( code = 200 reason = 'OK' ).
    ELSE.
      server->response->set_header_field( name = 'Content-Type' value = 'text/plain' ).
      server->response->set_cdata( 'Image not found' ).
      server->response->set_status( code = 404 reason = 'Not Found' ).
    ENDIF.
  ENDMETHOD.


  METHOD get_image_from_smw0.
    DATA: lt_mime TYPE STANDARD TABLE OF w3mime,
          ls_key  TYPE wwwdatatab.

    rv_found = abap_false.
    CLEAR: ev_data, ev_mime.

    ls_key-relid = 'MI'.
    ls_key-objid = iv_name.

    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key               = ls_key
      TABLES
        mime              = lt_mime
      EXCEPTIONS
        wrong_object_type = 1
        import_error      = 2
        OTHERS            = 3.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_mime ASSIGNING FIELD-SYMBOL(<m>).
      CONCATENATE ev_data <m>-line INTO ev_data IN BYTE MODE.
    ENDLOOP.

    IF ev_data IS NOT INITIAL.
      rv_found = abap_true.
      ev_mime = 'image/png'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
