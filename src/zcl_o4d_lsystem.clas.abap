CLASS zcl_o4d_lsystem DEFINITION PUBLIC FINAL CREATE PUBLIC.
*======================================================================
* L-SYSTEM TREE - Fractal tree generator with turtle graphics
*======================================================================
* Grows progressively with music - each bar adds more branches
* Uses stochastic rules for natural variation
*======================================================================
  PUBLIC SECTION.
    INTERFACES zif_o4d_effect.
    CLASS-METHODS new
      IMPORTING iv_angle TYPE f DEFAULT 25
                iv_len   TYPE f DEFAULT 60
                iv_depth TYPE i DEFAULT 6
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_o4d_lsystem.
    METHODS constructor IMPORTING iv_angle TYPE f iv_len TYPE f iv_depth TYPE i.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_branch, x1 TYPE f, y1 TYPE f, x2 TYPE f, y2 TYPE f, d TYPE i, END OF ty_branch.
    DATA mv_angle TYPE f.
    DATA mv_len TYPE f.
    DATA mv_depth TYPE i.
    DATA mt_branches TYPE STANDARD TABLE OF ty_branch WITH EMPTY KEY.
    DATA mv_seed TYPE f.
    METHODS generate IMPORTING iv_x TYPE f iv_y TYPE f iv_a TYPE f iv_l TYPE f iv_d TYPE i.
    METHODS rand RETURNING VALUE(rv) TYPE f.
ENDCLASS.

CLASS zcl_o4d_lsystem IMPLEMENTATION.
  METHOD new. ro_obj = NEW #( iv_angle = iv_angle iv_len = iv_len iv_depth = iv_depth ). ENDMETHOD.
  METHOD constructor. mv_angle = iv_angle. mv_len = iv_len. mv_depth = iv_depth. ENDMETHOD.

  METHOD rand.
    mv_seed = frac( sin( mv_seed * 12345 + '0.731' ) * 43758 ).
    rv = mv_seed.
  ENDMETHOD.

  METHOD generate.
    IF iv_d <= 0 OR iv_l < 2. RETURN. ENDIF.
    " Calculate end point
    DATA(lv_rad) = iv_a * '3.14159' / 180.
    DATA(lv_x2) = iv_x + sin( lv_rad ) * iv_l.
    DATA(lv_y2) = iv_y - cos( lv_rad ) * iv_l.  " Y grows down
    APPEND VALUE ty_branch( x1 = iv_x y1 = iv_y x2 = lv_x2 y2 = lv_y2 d = iv_d ) TO mt_branches.
    " Recursion with stochastic variation
    DATA(lv_var) = ( rand( ) - '0.5' ) * 10.  " Random angle variation
    DATA(lv_lvar) = '0.6' + rand( ) * '0.15'.  " Length reduction factor
    " Left branch
    generate( iv_x = lv_x2 iv_y = lv_y2 iv_a = iv_a - mv_angle + lv_var iv_l = iv_l * lv_lvar iv_d = iv_d - 1 ).
    " Right branch
    generate( iv_x = lv_x2 iv_y = lv_y2 iv_a = iv_a + mv_angle + lv_var iv_l = iv_l * lv_lvar iv_d = iv_d - 1 ).
    " Sometimes add middle branch
    IF rand( ) > '0.7'.
      generate( iv_x = lv_x2 iv_y = lv_y2 iv_a = iv_a + lv_var iv_l = iv_l * '0.5' iv_d = iv_d - 1 ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_o4d_effect~get_name. rv_name = 'lsystem'. ENDMETHOD.
  METHOD zif_o4d_effect~get_params. ENDMETHOD.
  METHOD zif_o4d_effect~set_param. ENDMETHOD.
  METHOD zif_o4d_effect~render.
    rs_frame = zif_o4d_effect~render_frame( VALUE #( t = is_sync-time gt = is_sync-time
      gbi = VALUE #( bar = is_sync-bar bar_phase = is_sync-bar_phase pulse = is_sync-intensity ) ) ).
  ENDMETHOD.

  METHOD zif_o4d_effect~render_frame.
    DATA(lv_w) = CONV f( zif_o4d_effect=>c_width ).
    DATA(lv_h) = CONV f( zif_o4d_effect=>c_height ).
    DATA(lv_cx) = lv_w / 2.
    DATA(lv_bar) = is_ctx-gbi-bar.
    DATA(lv_bp) = is_ctx-gbi-bar_phase.

    " Progressive growth: depth increases with bars (max 8 bars to full)
    DATA(lv_grow) = nmin( val1 = mv_depth val2 = ( lv_bar MOD 8 ) + 1 ).
    " Interpolate for smooth growth within bar
    DATA(lv_grow_frac) = CONV f( lv_grow ) + lv_bp * '0.3'.

    " Reset and regenerate tree (deterministic with seed reset)
    CLEAR mt_branches.
    mv_seed = '0.42'.  " Fixed seed for reproducibility
    generate( iv_x = lv_cx iv_y = lv_h - 20 iv_a = 0 iv_l = mv_len iv_d = CONV i( lv_grow_frac ) ).

    " Sky gradient
    DATA lv_yi TYPE i. lv_yi = 0.
    WHILE lv_yi < lv_h.
      DATA(lv_sky) = 20 + CONV i( CONV f( lv_yi ) / lv_h * 40 ).
      APPEND VALUE zif_o4d_effect=>ty_line( x1 = 0 y1 = lv_yi x2 = lv_w y2 = lv_yi width = 1
        color = |rgb({ lv_sky / 2 },{ lv_sky },{ lv_sky + 30 })| ) TO rs_frame-lines.
      lv_yi = lv_yi + 4.
    ENDWHILE.

    " Draw branches - deeper branches are thinner and greener
    LOOP AT mt_branches INTO DATA(ls_b).
      DATA(lv_thick) = nmax( val1 = 1 val2 = ls_b-d ).
      " Color: brown trunk → green leaves
      DATA(lv_green) = CONV i( 80 + ( mv_depth - ls_b-d ) * 25 ).
      DATA(lv_brown) = CONV i( 60 + ls_b-d * 15 ).
      DATA(lv_color) = COND string(
        WHEN ls_b-d > 3 THEN |rgb({ lv_brown + 40 },{ lv_brown },{ lv_brown / 2 })|
        ELSE |rgb({ 50 },{ lv_green },{ 30 })| ).
      APPEND VALUE zif_o4d_effect=>ty_line(
        x1 = ls_b-x1 y1 = ls_b-y1 x2 = ls_b-x2 y2 = ls_b-y2 width = lv_thick color = lv_color
      ) TO rs_frame-lines.
    ENDLOOP.

    " Ground
    APPEND VALUE zif_o4d_effect=>ty_rect( x = 0 y = lv_h - 25 w = lv_w h = 25 fill = '#3d2817' ) TO rs_frame-rects.

    " Info
    APPEND VALUE zif_o4d_effect=>ty_text(
      x = lv_cx y = 20 text = |L-SYSTEM DEPTH { CONV i( lv_grow_frac ) }| color = '#ffffff' size = 14 align = 'center'
    ) TO rs_frame-texts.
  ENDMETHOD.
ENDCLASS.
