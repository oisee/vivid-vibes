"! @title STARFIELD Effect - J-Drive hyperspace
"!
"! ╔══════════════════════════════════════════════════════════════════╗
"! ║                         TWEAK GUIDE                              ║
"! ╠══════════════════════════════════════════════════════════════════╣
"! ║  PARAMETER      │ LOCATION           │ EFFECT                   ║
"! ╠══════════════════════════════════════════════════════════════════╣
"! ║  mv_num_stars   │ DATA/150           │ Number of stars (50-300) ║
"! ║  mv_speed       │ DATA/1.0           │ Global speed multiplier  ║
"! ║  mv_trail_len   │ DATA/5.0           │ Star trail length        ║
"! ║  lv_seed        │ render_frame/*9973 │ Star distribution seed   ║
"! ║  lv_angle       │ frac(seed/1000)    │ Angular spread           ║
"! ║  lv_spd base    │ 0.5 + frac*5       │ Individual star speed    ║
"! ║  lv_z MOD       │ MOD 500            │ Depth cycling (Z-buffer) ║
"! ║  line width     │ lv_scale * 2       │ Star thickness at close  ║
"! ║  lv_bright      │ lv_scale * 20      │ Brightness vs distance   ║
"! ╠══════════════════════════════════════════════════════════════════╣
"! ║  TIP: Increase mv_num_stars for denser field during hyperspace  ║
"! ║  TIP: Increase mv_trail_len for more streaky warp effect        ║
"! ╚══════════════════════════════════════════════════════════════════╝
CLASS zcl_o4d_starfield DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.

  PRIVATE SECTION.
    "--- TWEAK: Core parameters ---
    DATA: mv_num_stars TYPE i VALUE 150,   " Star count (50-300 recommended)
          mv_speed     TYPE f VALUE '1.0', " Speed multiplier
          mv_trail_len TYPE f VALUE '5.0'. " Trail length
    CLASS-METHODS int_to_hex IMPORTING iv_int TYPE i RETURNING VALUE(rv_hex) TYPE string.
ENDCLASS.

CLASS zcl_o4d_starfield IMPLEMENTATION.
  METHOD zif_o4d_effect~get_name. rv_name = 'starfield'. ENDMETHOD.
  METHOD zif_o4d_effect~get_params. ENDMETHOD.
  METHOD zif_o4d_effect~set_param.
    CASE iv_name.
      WHEN 'num_stars'. mv_num_stars = CONV i( iv_value ).
      WHEN 'speed'. mv_speed = CONV f( iv_value ).
      WHEN 'trail_len'. mv_trail_len = CONV f( iv_value ).
    ENDCASE.
  ENDMETHOD.

  METHOD zif_o4d_effect~render.
    DATA(ls_ctx) = VALUE zif_o4d_effect=>ty_render_ctx(
      t = is_sync-time gt = is_sync-time
      bi = VALUE #( pulse = is_sync-intensity )
    ).
    rs_frame = zif_o4d_effect~render_frame( ls_ctx ).
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    DATA(lv_t) = is_ctx-t * mv_speed.
    DATA(lv_cx) = CONV f( zif_o4d_effect=>c_width ) / 2.
    DATA(lv_cy) = CONV f( zif_o4d_effect=>c_height ) / 2.

    DO mv_num_stars TIMES.
      "--- TWEAK: Star distribution ---
      DATA(lv_seed) = CONV f( sy-index * 9973 ).       " Pseudo-random seed
      DATA(lv_angle) = frac( lv_seed / 1000 ) * zif_o4d_effect=>c_pi * 2.  " Full circle
      DATA(lv_spd) = CONV f( '0.5' ) + frac( lv_seed / 500 ) * 5.  " Speed variation 0.5-5.5

      "--- TWEAK: Depth cycling ---
      DATA(lv_z) = CONV i( lv_t * lv_spd * 50 + lv_seed ) MOD 500.  " Z depth 0-499
      DATA(lv_scale) = 500 / ( CONV f( lv_z ) + 1 ).  " Perspective scale

      "--- TWEAK: Star position from center ---
      DATA(lv_x1) = lv_cx + cos( lv_angle ) * lv_scale * 100.
      DATA(lv_y1) = lv_cy + sin( lv_angle ) * lv_scale * 100.
      DATA(lv_x2) = lv_cx + cos( lv_angle ) * lv_scale * ( 100 - mv_trail_len ).
      DATA(lv_y2) = lv_cy + sin( lv_angle ) * lv_scale * ( 100 - mv_trail_len ).

      IF lv_x1 > 0 AND lv_x1 < zif_o4d_effect=>c_width AND lv_y1 > 0 AND lv_y1 < zif_o4d_effect=>c_height.
        "--- TWEAK: Brightness calculation ---
        DATA(lv_bright) = nmin( val1 = 255 val2 = CONV i( lv_scale * 20 ) ).
        DATA(lv_hex) = int_to_hex( lv_bright ).
        APPEND VALUE zif_o4d_effect=>ty_line(
          x1 = lv_x1 y1 = lv_y1 x2 = lv_x2 y2 = lv_y2
          color = |#{ lv_hex }{ lv_hex }{ lv_hex }|
          width = nmax( val1 = CONV f( 1 ) val2 = lv_scale * 2 )
        ) TO rs_frame-lines.
      ENDIF.
    ENDDO.

    " Title
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = 320 y = 30 text = 'HYPERSPACE' color = '#4488FF' size = 14 align = 'center'
    ) TO rs_frame-texts.

    " Debug vars
    rs_frame-debug-vars = |\{"num_stars":{ mv_num_stars },"speed":{ mv_speed },| &&
                          |"trail_len":{ mv_trail_len },"t":{ lv_t }\}|.
  ENDMETHOD.

  METHOD int_to_hex.
    DATA(lv_val) = nmax( val1 = 0 val2 = nmin( val1 = 255 val2 = iv_int ) ).
    rv_hex = |{ lv_val ALIGN = RIGHT WIDTH = 2 PAD = '0' }|.
    IF lv_val < 16. rv_hex = |0{ CONV string( CONV xstring( lv_val ) ) }|.
    ELSE. rv_hex = CONV string( CONV xstring( lv_val ) ). ENDIF.
    TRANSLATE rv_hex TO LOWER CASE.
  ENDMETHOD.
ENDCLASS.
