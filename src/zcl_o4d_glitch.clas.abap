"! @title GLITCH - Digital noise transition effect
"!
"! ╔══════════════════════════════════════════════════════════════════╗
"! ║                         TWEAK GUIDE                              ║
"! ╠══════════════════════════════════════════════════════════════════╣
"! ║  PARAMETER      │ LOCATION           │ EFFECT                   ║
"! ╠══════════════════════════════════════════════════════════════════╣
"! ║  lv_seed        │ lv_t * 20          │ Random seed speed        ║
"! ║  block count    │ DO 20 TIMES        │ Number of random blocks  ║
"! ║  block x/y      │ seed*i*17/23 MOD   │ Block position           ║
"! ║  block w/h      │ 10+seed*7 MOD 50   │ Block size (10-60 x 5-25)║
"! ║  scanline %     │ rand < 30          │ % of displaced scanlines ║
"! ║  scanline step  │ lv_sy + 8          │ Scanline spacing         ║
"! ║  shift range    │ (rand%30) - 15     │ Horizontal shift pixels  ║
"! ╠══════════════════════════════════════════════════════════════════╣
"! ║  TEXT OVERLAYS (customize for your transition):                 ║
"! ║  Line 1: "SYSTEM UPGRADE" - red, y=85                           ║
"! ║  Line 2: "S/4 HANA" - green, y=110                              ║
"! ║  Line 3: ">>> WebGL <<<" - cyan, y=140                          ║
"! ╠══════════════════════════════════════════════════════════════════╣
"! ║  TIP: Increase block count (20→40) for more chaotic look        ║
"! ║  TIP: Change seed multiplier (*20→*50) for faster animation     ║
"! ║  TIP: Modify text for custom transition messages                ║
"! ╚══════════════════════════════════════════════════════════════════╝
CLASS zcl_o4d_glitch DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
  PRIVATE SECTION.
    CLASS-METHODS int_to_hex
      IMPORTING iv_int TYPE i
      RETURNING VALUE(rv_hex) TYPE string.
ENDCLASS.

CLASS zcl_o4d_glitch IMPLEMENTATION.
  METHOD zif_o4d_effect~get_name. rv_name = 'glitch'. ENDMETHOD.
  METHOD zif_o4d_effect~get_params. ENDMETHOD.
  METHOD zif_o4d_effect~set_param. ENDMETHOD.

  METHOD zif_o4d_effect~render.
    DATA(ls_ctx) = VALUE zif_o4d_effect=>ty_render_ctx(
      t = is_sync-time gt = is_sync-time bi = VALUE #( pulse = is_sync-intensity )
    ).
    rs_frame = zif_o4d_effect~render_frame( ls_ctx ).
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    DATA(lv_t) = is_ctx-t.
    DATA(lv_w) = zif_o4d_effect=>c_width.
    DATA(lv_h) = zif_o4d_effect=>c_height.

    "--- TWEAK: Random seed (multiply for faster animation) ---
    DATA(lv_seed) = CONV i( lv_t * 20 ).

    "--- TWEAK: Random colored blocks ---
    DO 20 TIMES.  " Number of blocks
      DATA(lv_i) = sy-index.

      " Pseudo-random position using prime multipliers
      DATA(lv_x) = ( lv_seed * lv_i * 17 ) MOD lv_w.
      DATA(lv_y) = ( lv_seed * lv_i * 23 ) MOD lv_h.

      "--- TWEAK: Block dimensions ---
      DATA(lv_bw) = 10 + ( lv_seed * lv_i * 7 ) MOD 50.   " Width 10-60
      DATA(lv_bh) = 5 + ( lv_seed * lv_i * 11 ) MOD 20.   " Height 5-25

      " Random RGB color
      DATA(lv_r) = ( lv_seed * lv_i * 3 ) MOD 256.
      DATA(lv_g) = ( lv_seed * lv_i * 5 ) MOD 256.
      DATA(lv_b) = ( lv_seed * lv_i * 7 ) MOD 256.

      DATA(lv_color) = |#{ int_to_hex( lv_r ) }{ int_to_hex( lv_g ) }{ int_to_hex( lv_b ) }|.

      " Draw block as horizontal lines (scanline style)
      DATA lv_dy TYPE i.
      lv_dy = 0.
      WHILE lv_dy < lv_bh.
        APPEND VALUE zif_o4d_effect=>ty_line(
          x1 = lv_x y1 = lv_y + lv_dy x2 = lv_x + lv_bw y2 = lv_y + lv_dy
          width = 1 color = lv_color
        ) TO rs_frame-lines.
        lv_dy = lv_dy + 2.  "--- TWEAK: Line spacing within block
      ENDWHILE.
    ENDDO.

    "--- TWEAK: Scanline displacement effect ---
    DATA lv_sy TYPE i.
    lv_sy = 0.
    WHILE lv_sy < lv_h.
      DATA(lv_rand) = ( lv_seed + lv_sy * 13 ) MOD 100.

      "--- TWEAK: Percentage of displaced lines (30%) ---
      IF lv_rand < 30.
        DATA(lv_shift) = ( lv_rand MOD 30 ) - 15.  " Shift -15 to +15 pixels

        " Random scanline color
        DATA(lv_sr) = ( lv_seed + lv_sy * 3 ) MOD 256.
        DATA(lv_sg) = ( lv_seed + lv_sy * 5 ) MOD 256.
        DATA(lv_sb) = ( lv_seed + lv_sy * 7 ) MOD 256.

        APPEND VALUE zif_o4d_effect=>ty_line(
          x1 = lv_shift y1 = lv_sy x2 = lv_w + lv_shift y2 = lv_sy
          width = 4 color = |#{ int_to_hex( lv_sr ) }{ int_to_hex( lv_sg ) }{ int_to_hex( lv_sb ) }|
        ) TO rs_frame-lines.
      ENDIF.

      lv_sy = lv_sy + 8.  "--- TWEAK: Scanline spacing
    ENDWHILE.

    "--- TWEAK: Text overlays (customize for your transition) ---
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = CONV f( lv_w ) / 2 y = 85 text = 'SYSTEM UPGRADE'
      color = '#ff0000' size = 16 align = 'center'
    ) TO rs_frame-texts.

    APPEND VALUE zif_o4d_effect=>ty_text(
      x = CONV f( lv_w ) / 2 y = 110 text = 'S/4 HANA'
      color = '#00ff00' size = 16 align = 'center'
    ) TO rs_frame-texts.

    APPEND VALUE zif_o4d_effect=>ty_text(
      x = CONV f( lv_w ) / 2 y = 140 text = '>>> WebGL <<<'
      color = '#00ffff' size = 20 align = 'center'
    ) TO rs_frame-texts.
  ENDMETHOD.

  METHOD int_to_hex.
    DATA lv_hex TYPE x LENGTH 1.
    DATA(lv_val) = nmax( val1 = 0 val2 = nmin( val1 = 255 val2 = iv_int ) ).
    lv_hex = lv_val.
    rv_hex = |{ lv_hex }|.
    TRANSLATE rv_hex TO LOWER CASE.
  ENDMETHOD.
ENDCLASS.
