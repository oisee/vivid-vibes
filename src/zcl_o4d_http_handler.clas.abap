CLASS zcl_o4d_http_handler DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_http_extension.
  PRIVATE SECTION.
    CONSTANTS: c_bpm TYPE f VALUE '152.0', c_fps TYPE i VALUE 30,
               c_bar_sec TYPE f VALUE '1.578947368'.
    METHODS get_html IMPORTING iv_demo TYPE string DEFAULT 'main' RETURNING VALUE(rv_html) TYPE string.
    METHODS get_dev_html IMPORTING iv_demo TYPE string DEFAULT 'main' RETURNING VALUE(rv_html) TYPE string.
    METHODS get_megademo_html RETURNING VALUE(rv_html) TYPE string.
    METHODS get_scenario_json RETURNING VALUE(rv_json) TYPE string.
    METHODS get_audio_from_smw0 IMPORTING iv_name TYPE string
      EXPORTING ev_data TYPE xstring ev_size TYPE i.
    METHODS get_image_from_smw0 IMPORTING iv_name TYPE string
      EXPORTING ev_data TYPE xstring ev_mime TYPE string.
ENDCLASS.

CLASS zcl_o4d_http_handler IMPLEMENTATION.

  METHOD if_http_extension~handle_request.
    DATA(lv_query) = server->request->get_header_field( '~query_string' ).
    DATA: lv_demo TYPE string VALUE 'main'.

    " Parse demo parameter from query string
    IF lv_query CP '*demo=*'.
      DATA(lv_pos) = find( val = lv_query sub = 'demo=' ).
      IF lv_pos >= 0.
        lv_pos = lv_pos + 5.
        DATA(lv_end) = find( val = lv_query off = lv_pos sub = '&' ).
        IF lv_end < 0. lv_end = strlen( lv_query ). ENDIF.
        DATA(lv_len) = lv_end - lv_pos.
        lv_demo = lv_query+lv_pos(lv_len).
      ENDIF.
    ENDIF.

    " Route: ?player=megademo - Full megademo sequencer
    IF lv_query CP '*player=megademo*'.
      server->response->set_header_field( name = 'Content-Type' value = 'text/html; charset=utf-8' ).
      server->response->set_cdata( get_megademo_html( ) ).
      RETURN.
    ENDIF.

    " Route: ?player=dev - Dev player with HUD
    IF lv_query CP '*player=dev*'.
      server->response->set_header_field( name = 'Content-Type' value = 'text/html; charset=utf-8' ).
      server->response->set_cdata( get_dev_html( iv_demo = lv_demo ) ).
      RETURN.
    ENDIF.

    " Route: ?scenario - Get scenario JSON
    IF lv_query = 'scenario' OR lv_query CP 'scenario&*'.
      server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=utf-8' ).
      server->response->set_header_field( name = 'Access-Control-Allow-Origin' value = '*' ).
      server->response->set_cdata( get_scenario_json( ) ).
      RETURN.
    ENDIF.

    " Route: ?audio=<file> - Serve audio from SMW0
    IF lv_query CP 'audio=*'.
      DATA(lv_file) = lv_query+6. TRANSLATE lv_file TO UPPER CASE.
      DATA: lv_audio TYPE xstring, lv_size TYPE i.
      get_audio_from_smw0( EXPORTING iv_name = lv_file IMPORTING ev_data = lv_audio ev_size = lv_size ).
      IF lv_audio IS NOT INITIAL.
        server->response->set_header_field( name = 'Content-Type' value = 'audio/mpeg' ).
        server->response->set_header_field( name = 'Content-Length' value = CONV string( lv_size ) ).
        server->response->set_data( lv_audio ).
      ELSE. server->response->set_status( code = 404 reason = 'Not Found' ). ENDIF.
      RETURN.
    ENDIF.

    " Route: ?img=<name> - Serve image from SMW0
    IF lv_query CP 'img=*'.
      DATA(lv_img_name) = lv_query+4. TRANSLATE lv_img_name TO UPPER CASE.
      DATA: lv_img_data TYPE xstring, lv_img_mime TYPE string.
      get_image_from_smw0( EXPORTING iv_name = lv_img_name IMPORTING ev_data = lv_img_data ev_mime = lv_img_mime ).
      IF lv_img_data IS NOT INITIAL.
        DATA(lv_img_size) = xstrlen( lv_img_data ).
        server->response->set_header_field( name = 'Content-Type' value = lv_img_mime ).
        server->response->set_header_field( name = 'Content-Length' value = CONV string( lv_img_size ) ).
        server->response->set_header_field( name = 'Access-Control-Allow-Origin' value = '*' ).
        server->response->set_header_field( name = 'Cache-Control' value = 'public, max-age=3600' ).
        server->response->set_data( lv_img_data ).
      ELSE.
        server->response->set_status( code = 404 reason = 'Image Not Found' ).
        server->response->set_cdata( |Image not found: { lv_img_name }| ).
      ENDIF.
      RETURN.
    ENDIF.

    " Default: viewer HTML
    server->response->set_header_field( name = 'Content-Type' value = 'text/html; charset=utf-8' ).
    server->response->set_cdata( get_html( ) ).
  ENDMETHOD.

  METHOD get_audio_from_smw0.
    DATA: lt_mime TYPE w3mimetabtype, ls_key TYPE wwwdatatab,
          lt_params TYPE STANDARD TABLE OF wwwparams, ls_param TYPE wwwparams.
    ls_key-relid = 'MI'. ls_key-objid = iv_name.
    SELECT * FROM wwwparams INTO TABLE lt_params WHERE relid = ls_key-relid AND objid = ls_key-objid.
    IF sy-subrc <> 0. RETURN. ENDIF.
    READ TABLE lt_params INTO ls_param WITH KEY name = 'filesize'.
    IF sy-subrc = 0. ev_size = ls_param-value. ENDIF.
    CALL FUNCTION 'WWWDATA_IMPORT' EXPORTING key = ls_key TABLES mime = lt_mime EXCEPTIONS OTHERS = 1.
    IF sy-subrc <> 0. RETURN. ENDIF.
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING' EXPORTING input_length = ev_size IMPORTING buffer = ev_data TABLES binary_tab = lt_mime.
  ENDMETHOD.

  METHOD get_image_from_smw0.
    DATA: lt_mime TYPE w3mimetabtype, ls_key TYPE wwwdatatab,
          lt_params TYPE STANDARD TABLE OF wwwparams, ls_param TYPE wwwparams,
          lv_size TYPE i.
    ls_key-relid = 'MI'. ls_key-objid = iv_name.
    SELECT * FROM wwwparams INTO TABLE lt_params WHERE relid = ls_key-relid AND objid = ls_key-objid.
    IF sy-subrc <> 0. RETURN. ENDIF.
    READ TABLE lt_params INTO ls_param WITH KEY name = 'filesize'.
    IF sy-subrc = 0. lv_size = ls_param-value. ENDIF.
    CALL FUNCTION 'WWWDATA_IMPORT' EXPORTING key = ls_key TABLES mime = lt_mime EXCEPTIONS OTHERS = 1.
    IF sy-subrc <> 0. RETURN. ENDIF.
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING' EXPORTING input_length = lv_size IMPORTING buffer = ev_data TABLES binary_tab = lt_mime.
    IF ev_data IS NOT INITIAL. ev_mime = 'image/png'. ENDIF.
  ENDMETHOD.

  METHOD get_megademo_html.
    " Megademo player - sequences through all parts automatically
    rv_html =
`<!DOCTYPE html><html><head><meta charset="UTF-8"><title>MEGADEMO - EAR ASSAULT II</title>` &&
`<style>*{margin:0;padding:0;box-sizing:border-box}body{background:#000;font-family:monospace;color:#0f0;` &&
`display:flex;flex-direction:column;align-items:center;justify-content:center;min-height:100vh}` &&
`#container{position:relative;border:2px solid #0f0}canvas{display:block}` &&
`#info{margin-top:10px;font-size:14px}#part-info{color:#f0f;font-size:18px;margin:10px 0}` &&
`#progress{width:640px;height:8px;background:#222;margin-top:10px}` &&
`#pbar{height:100%;background:#0f0;width:0%;transition:width 0.1s}` &&
`button{background:#0f0;color:#000;border:none;padding:10px 20px;font-family:monospace;font-size:16px;cursor:pointer;margin:10px}` &&
`button:hover{background:#0c0}.fullscreen{position:fixed;top:0;left:0;right:0;bottom:0;z-index:1000}` &&
`</style></head><body>` &&
`<h1 style="text-shadow:0 0 20px #0f0;margin-bottom:10px">EAR ASSAULT II - MEGADEMO</h1>` &&
`<div id="part-info">Loading...</div>` &&
`<div id="container"><canvas id="glc" width="640" height="400"></canvas>` &&
`<canvas id="txc" width="640" height="400" style="position:absolute;top:0;left:0;pointer-events:none"></canvas></div>` &&
`<div id="progress"><div id="pbar"></div></div>` &&
`<div id="info">Click START to begin</div>` &&
`<button id="btn-start">▶ START MEGADEMO</button>` &&
`<audio id="au" preload="auto"></audio>`.

    rv_html = rv_html &&
`<script id="vs" type="x-shader/x-vertex">attribute vec2 aPos;attribute vec3 aCol;varying vec3 vCol;` &&
`void main(){gl_Position=vec4(aPos.x/320.0-1.0,1.0-aPos.y/200.0,0.0,1.0);vCol=aCol;}</script>` &&
`<script id="fs" type="x-shader/x-fragment">precision mediump float;varying vec3 vCol;void main(){gl_FragColor=vec4(vCol,1.0);}</script>` &&
`<script>` &&
`var gl,tx,au,ws,playing=0,frame=0,megademo=null,currentPart=0,partStartTime=0;` &&
`var BPM=152,FPS=30,BAR_SEC=1.5789,SEC_PER_TICK=BAR_SEC/64,TOTAL_BARS=116;` &&
`var glc=document.getElementById('glc'),txc=document.getElementById('txc');` &&
`gl=glc.getContext('webgl');tx=txc.getContext('2d');au=document.getElementById('au');` &&
`function compileShader(t,s){var sh=gl.createShader(t);gl.shaderSource(sh,s);gl.compileShader(sh);return sh;}` &&
`var vs=compileShader(gl.VERTEX_SHADER,document.getElementById('vs').textContent);` &&
`var fs=compileShader(gl.FRAGMENT_SHADER,document.getElementById('fs').textContent);` &&
`var prog=gl.createProgram();gl.attachShader(prog,vs);gl.attachShader(prog,fs);gl.linkProgram(prog);gl.useProgram(prog);` &&
`var triB=gl.createBuffer(),lineB=gl.createBuffer();gl.enable(gl.BLEND);gl.blendFunc(gl.SRC_ALPHA,gl.ONE_MINUS_SRC_ALPHA);` &&
`function parseCol(c){if(!c)return[1,1,1];if(c[0]==='#'){var h=c.slice(1);if(h.length===3)h=h[0]+h[0]+h[1]+h[1]+h[2]+h[2];` &&
`return[parseInt(h.slice(0,2),16)/255,parseInt(h.slice(2,4),16)/255,parseInt(h.slice(4,6),16)/255];}` &&
`if(c.startsWith('hsl')){var m=c.match(/\d+/g);if(m){var h=parseInt(m[0])/360,s=parseInt(m[1])/100,l=parseInt(m[2])/100;` &&
`if(s===0)return[l,l,l];var q=l<0.5?l*(1+s):l+s-l*s,p=2*l-q;return[hue2rgb(p,q,h+1/3),hue2rgb(p,q,h),hue2rgb(p,q,h-1/3)];}}` &&
`if(c.startsWith('rgb')){var m=c.match(/\d+/g);if(m)return[m[0]/255,m[1]/255,m[2]/255];}return[1,1,1];}` &&
`function hue2rgb(p,q,t){if(t<0)t+=1;if(t>1)t-=1;if(t<1/6)return p+(q-p)*6*t;if(t<1/2)return q;if(t<2/3)return p+(q-p)*(2/3-t)*6;return p;}`.

    rv_html = rv_html &&
`function rf(d){gl.clearColor(0,0,0,1);gl.clear(gl.COLOR_BUFFER_BIT);tx.clearRect(0,0,640,400);` &&
`var aPos=gl.getAttribLocation(prog,'aPos'),aCol=gl.getAttribLocation(prog,'aCol'),v=[];` &&
`if(d.tri)for(var i=0;i<d.tri.length;i++){var t=d.tri[i],c=parseCol(t.f);` &&
`v.push(t.x1,t.y1,c[0],c[1],c[2],t.x2,t.y2,c[0],c[1],c[2],t.x3,t.y3,c[0],c[1],c[2]);}` &&
`if(d.rb)for(var i=0;i<d.rb.length;i++){var r=d.rb[i],c=parseCol(r.f),x=r.x,y=r.y,w=r.w,h=r.h;` &&
`v.push(x,y,c[0],c[1],c[2],x+w,y,c[0],c[1],c[2],x+w,y+h,c[0],c[1],c[2]);` &&
`v.push(x,y,c[0],c[1],c[2],x+w,y+h,c[0],c[1],c[2],x,y+h,c[0],c[1],c[2]);}` &&
`if(d.c)for(var i=0;i<d.c.length;i++){var ci=d.c[i],c=parseCol(ci.f),cx=ci.x,cy=ci.y,cr=ci.r,seg=16;` &&
`for(var s=0;s<seg;s++){var a1=s/seg*Math.PI*2,a2=(s+1)/seg*Math.PI*2;` &&
`v.push(cx,cy,c[0],c[1],c[2],cx+Math.cos(a1)*cr,cy+Math.sin(a1)*cr,c[0],c[1],c[2],cx+Math.cos(a2)*cr,cy+Math.sin(a2)*cr,c[0],c[1],c[2]);}}` &&
`if(d.r)for(var i=0;i<d.r.length;i++){var r=d.r[i],c=parseCol(r.f),x=r.x,y=r.y,w=r.w,h=r.h;` &&
`v.push(x,y,c[0],c[1],c[2],x+w,y,c[0],c[1],c[2],x+w,y+h,c[0],c[1],c[2]);` &&
`v.push(x,y,c[0],c[1],c[2],x+w,y+h,c[0],c[1],c[2],x,y+h,c[0],c[1],c[2]);}` &&
`if(v.length){gl.bindBuffer(gl.ARRAY_BUFFER,triB);gl.bufferData(gl.ARRAY_BUFFER,new Float32Array(v),gl.DYNAMIC_DRAW);` &&
`gl.enableVertexAttribArray(aPos);gl.enableVertexAttribArray(aCol);` &&
`gl.vertexAttribPointer(aPos,2,gl.FLOAT,false,20,0);gl.vertexAttribPointer(aCol,3,gl.FLOAT,false,20,8);` &&
`gl.drawArrays(gl.TRIANGLES,0,v.length/5);}` &&
`if(d.l&&d.l.length){var lv=[];for(var i=0;i<d.l.length;i++){var l=d.l[i],c=parseCol(l.c);` &&
`lv.push(l.x1,l.y1,c[0],c[1],c[2],l.x2,l.y2,c[0],c[1],c[2]);}` &&
`gl.bindBuffer(gl.ARRAY_BUFFER,lineB);gl.bufferData(gl.ARRAY_BUFFER,new Float32Array(lv),gl.DYNAMIC_DRAW);` &&
`gl.enableVertexAttribArray(aPos);gl.enableVertexAttribArray(aCol);` &&
`gl.vertexAttribPointer(aPos,2,gl.FLOAT,false,20,0);gl.vertexAttribPointer(aCol,3,gl.FLOAT,false,20,8);` &&
`gl.drawArrays(gl.LINES,0,d.l.length*2);}` &&
`if(d.tx)for(var j=0;j<d.tx.length;j++){var t=d.tx[j];tx.fillStyle=t.c;tx.font=(t.s||16)+'px monospace';` &&
`tx.textAlign=t.align||'center';tx.fillText(t.t,t.x,t.y);}` &&
`updateProgress();}`.

    rv_html = rv_html &&
`function updateProgress(){if(!megademo)return;var part=megademo.parts[currentPart];if(!part)return;` &&
`var elapsed=au.currentTime;var duration=part.bars*BAR_SEC;var pct=Math.min(100,elapsed/duration*100);` &&
`document.getElementById('pbar').style.width=pct+'%';` &&
`document.getElementById('info').textContent='Part '+(currentPart+1)+'/'+megademo.parts.length+` &&
`' | Bar: '+Math.floor(elapsed/BAR_SEC)+'/'+part.bars+' | '+part.name;` &&
`if(elapsed>=duration-0.1){nextPart();}}` &&
`function nextPart(){currentPart++;if(currentPart>=megademo.parts.length){` &&
`document.getElementById('part-info').textContent='MEGADEMO COMPLETE!';` &&
`document.getElementById('info').textContent='Thanks for watching!';playing=0;return;}` &&
`loadPart(megademo.parts[currentPart]);}` &&
`function loadPart(part){document.getElementById('part-info').textContent='► '+part.name+' ('+part.bpm+' BPM, '+part.bars+' bars)';` &&
`BPM=part.bpm;BAR_SEC=240/BPM;SEC_PER_TICK=BAR_SEC/64;sI=SEC_PER_TICK*1000/FPT;FPS=BPM/3.75;cT=0;cS=0;lT=0;` &&
`ws.send(JSON.stringify({cmd:'load_demo',demo:part.id}));` &&
`au.src='?audio='+part.audio;au.load();` &&
`au.oncanplaythrough=function(){au.oncanplaythrough=null;au.play();partStartTime=Date.now();rq();};}` &&
`var FPT=1,sI=SEC_PER_TICK*1000/FPT,cT=0,cS=0,lT=0;` &&
`function rq(){if(!playing)return;var n=performance.now();` &&
`if(n-lT>=sI){lT=n;if(ws&&ws.readyState===1)ws.send(JSON.stringify({cmd:'frame',tick:cT,sub:cS}));` &&
`cS++;if(cS>=FPT){cS=0;cT++;}}requestAnimationFrame(rq);}`.

    rv_html = rv_html &&
`function connect(){document.getElementById('info').textContent='Connecting...';` &&
`ws=new WebSocket((location.protocol==='https:'?'wss:':'ws:')+'//'+location.host+'/sap/bc/apc/sap/zo4d_demo');` &&
`ws.onopen=function(){document.getElementById('info').textContent='Connected! Getting playlist...';` &&
`ws.send(JSON.stringify({cmd:'get_megademo'}));};` &&
`ws.onclose=function(){document.getElementById('info').textContent='Disconnected';playing=0;};` &&
`ws.onmessage=function(e){try{var d=JSON.parse(e.data);` &&
`if(d.type==='megademo'){megademo=d;document.getElementById('part-info').textContent=d.name;` &&
`document.getElementById('info').textContent='Ready! '+d.parts.length+' parts. Click START.';}` &&
`else if(d.type==='config'){BPM=d.bpm;BAR_SEC=d.bar_sec;FPS=d.fps;FPT=d.fpt||1;TOTAL_BARS=d.total_bars;sI=SEC_PER_TICK*1000/FPT;}` &&
`else if(!d.type){rf(d);}}catch(x){}};}` &&
`function startMegademo(){if(!megademo||!megademo.parts.length)return;` &&
`document.getElementById('btn-start').style.display='none';` &&
`currentPart=0;playing=1;loadPart(megademo.parts[0]);}` &&
`document.getElementById('btn-start').onclick=startMegademo;` &&
`document.onkeydown=function(e){if(e.key===' '){e.preventDefault();if(!playing&&megademo)startMegademo();}` &&
`if(e.key==='f'){glc.parentElement.classList.toggle('fullscreen');}};` &&
`tx.fillStyle='#0f0';tx.font='24px monospace';tx.textAlign='center';tx.fillText('MEGADEMO',320,200);connect();` &&
`</script></body></html>`.
  ENDMETHOD.


  METHOD get_scenario_json.
    " Return scenario manifest matching APC handler timeline
    DATA: lv_scenes TYPE string.
    lv_scenes = |[| &&
      |\{"id":"ignition","name":"Ignition","start_bar":0,"end_bar":4\},| &&
      |\{"id":"starfield","name":"Starfield","start_bar":4,"end_bar":8\},| &&
      |\{"id":"sales","name":"SAP Charts","start_bar":8,"end_bar":12\},| &&
      |\{"id":"constellation","name":"Constellation","start_bar":12,"end_bar":16\},| &&
      |\{"id":"copper","name":"Copper Bars","start_bar":16,"end_bar":20\},| &&
      |\{"id":"joydiv","name":"Joy Division","start_bar":20,"end_bar":24\},| &&
      |\{"id":"voxel","name":"Voxel Terrain","start_bar":24,"end_bar":28\},| &&
      |\{"id":"plasma","name":"Plasma","start_bar":28,"end_bar":32\},| &&
      |\{"id":"rotozoom","name":"Rotozoom","start_bar":32,"end_bar":36\},| &&
      |\{"id":"amigaball","name":"Amiga Ball","start_bar":36,"end_bar":40\},| &&
      |\{"id":"elite3d","name":"Elite 3D","start_bar":40,"end_bar":44\},| &&
      |\{"id":"kaleidoscope","name":"Kaleidoscope","start_bar":44,"end_bar":48\},| &&
      |\{"id":"mountains","name":"Mountains","start_bar":48,"end_bar":52\},| &&
      |\{"id":"lowpoly","name":"LowPoly","start_bar":52,"end_bar":56\},| &&
      |\{"id":"glitch","name":"Glitch","start_bar":56,"end_bar":58\},| &&
      |\{"id":"metaballs","name":"Metaballs","start_bar":58,"end_bar":62\},| &&
      |\{"id":"tunnel","name":"Tunnel","start_bar":62,"end_bar":66\},| &&
      |\{"id":"sierpinski","name":"Sierpinski","start_bar":66,"end_bar":72\},| &&
      |\{"id":"starburst","name":"Starburst","start_bar":72,"end_bar":76\},| &&
      |\{"id":"swirl","name":"Swirl","start_bar":76,"end_bar":80\},| &&
      |\{"id":"amigaball2","name":"Amiga Ball 2","start_bar":80,"end_bar":84\},| &&
      |\{"id":"plasma2","name":"Plasma Encore","start_bar":84,"end_bar":88\},| &&
      |\{"id":"elite3d2","name":"Elite Encore","start_bar":88,"end_bar":92\},| &&
      |\{"id":"starfield2","name":"Hyperdrive","start_bar":92,"end_bar":96\},| &&
      |\{"id":"firegreetings","name":"Fire Greetings","start_bar":96,"end_bar":100\},| &&
      |\{"id":"twister","name":"Twister Credits","start_bar":100,"end_bar":108\},| &&
      |\{"id":"greetings","name":"Greetings","start_bar":108,"end_bar":116\}]|.

    " Media preload list (from GALLERY effect)
    DATA: lv_media TYPE string.
    lv_media = |[| &&
      |\{"name":"ZO4D_00_SALES","type":"img"\},| &&
      |\{"name":"ZO4D_02_IGNITE","type":"img"\},| &&
      |\{"name":"ZO4D_05_COPPER","type":"img"\},| &&
      |\{"name":"ZO4D_06_PLASMA","type":"img"\},| &&
      |\{"name":"ZO4D_07_TWISTER","type":"img"\},| &&
      |\{"name":"ZO4D_08_MOUNTAINS","type":"img"\},| &&
      |\{"name":"ZO4D_09_ROTOZOOM","type":"img"\},| &&
      |\{"name":"ZO4D_10_VOXEL","type":"img"\},| &&
      |\{"name":"ZO4D_11_ROTOPLASMA","type":"img"\},| &&
      |\{"name":"ZO4D_12_TESSERACT","type":"img"\},| &&
      |\{"name":"ZO4D_13_CELL24","type":"img"\},| &&
      |\{"name":"ZO4D_14_CELL16","type":"img"\},| &&
      |\{"name":"ZO4D_15_CELL120","type":"img"\},| &&
      |\{"name":"ZO4D_17_AMIGABALL","type":"img"\},| &&
      |\{"name":"ZO4D_18_GLITCH","type":"img"\},| &&
      |\{"name":"ZO4D_19_SIERPINSKI","type":"img"\},| &&
      |\{"name":"ZO4D_20_NEONCITY","type":"img"\},| &&
      |\{"name":"ZO4D_21_JOYDIV","type":"img"\},| &&
      |\{"name":"ZO4D_22_SIERPTET","type":"img"\},| &&
      |\{"name":"ZO4D_23_QUATJULIA","type":"img"\},| &&
      |\{"name":"ZO4D_24_METABALLS","type":"img"\},| &&
      |\{"name":"ZO4D_25_TORUS","type":"img"\},| &&
      |\{"name":"ZO4D_26_JULIAMORPH","type":"img"\},| &&
      |\{"name":"ZO4D_27_CONSTELL","type":"img"\}]|.

    rv_json = |\{"name":"EAR ASSAULT II","version":"1.0","bpm":152,"fps":30,| &&
              |"bar_sec":1.578947368,"total_bars":116,"audio":"?audio=ZOISEE-EAR-02.MP3",| &&
              |"media":{ lv_media },| &&
              |"scenes":{ lv_scenes }\}|.
  ENDMETHOD.

  METHOD get_dev_html.
    " Dev player with debug HUD and navigation
    rv_html =
`<!DOCTYPE html><html><head><meta charset="UTF-8"><title>O4D DEV PLAYER</title>` &&
`<style>*{margin:0;padding:0;box-sizing:border-box}body{background:#000;font-family:monospace;color:#0f0;overflow-x:hidden}` &&
`#main{display:flex;flex-direction:column;align-items:center;padding:10px}` &&
`#container{position:relative;background:#111;border:2px solid #333}` &&
`canvas{display:block}` &&
`.transport{display:flex;gap:5px;margin:10px 0;align-items:center;flex-wrap:wrap;justify-content:center}` &&
`button{background:#222;color:#0f0;border:1px solid #0f0;padding:6px 12px;font-family:monospace;cursor:pointer}` &&
`button:hover{background:#0f0;color:#000}button:disabled{opacity:0.3;cursor:default}` &&
`#timeline{width:90vw;max-width:900px;height:50px;background:#111;border:1px solid #333;margin:10px 0;position:relative}` &&
`#tl-scenes{position:absolute;top:0;left:0;right:0;height:25px}` &&
`.tl-scene{position:absolute;height:100%;border-right:1px solid #333;font-size:9px;padding:2px;overflow:hidden;cursor:pointer}` &&
`#tl-grid{position:absolute;bottom:0;left:0;right:0;height:25px}` &&
`.tl-bar{position:absolute;bottom:0;height:100%;border-left:1px solid #333;font-size:8px;color:#666}` &&
`#playhead{position:absolute;top:0;bottom:0;width:2px;background:#f00;pointer-events:none}` &&
`#info{font-size:11px;color:#888}input[type=number]{width:50px;background:#111;color:#0f0;border:1px solid #0f0;padding:3px}` &&
`#hud-panel{display:flex;gap:10px;margin:10px 0;width:90vw;max-width:900px}` &&
`#hud{flex:0 0 200px;background:#111;border:1px solid #0f0;padding:10px;font-size:11px}` &&
`#hud .row{margin:4px 0}#hud .lbl{color:#888}#hud .val{color:#0ff}#hud .scene{color:#f80}` &&
`#hud-panel{display:flex;gap:10px;margin:10px 0;width:90vw;max-width:900px;min-height:250px}` &&
`.debug-box{flex:1;background:#111;border:1px solid #333;padding:10px;font-size:10px;min-height:200px;max-height:500px;overflow:auto;resize:vertical}` &&
`.debug-box .title{color:#f0f;margin-bottom:5px;font-weight:bold}.debug-box pre{color:#0f0;white-space:pre-wrap;word-break:break-all;margin:0}` &&
`#debug-global{border-color:#0f0}#debug-scene{border-color:#0ff}` &&
`</style></head><body><div id="main">` &&
`<h2 style="margin-bottom:5px">O4D DEV PLAYER</h2>` &&
`<div id="container">` &&
`<canvas id="glc" width="640" height="400"></canvas>` &&
`<canvas id="txc" width="640" height="400" style="position:absolute;top:0;left:0;pointer-events:none"></canvas>` &&
`</div>` &&
`<div class="transport">` &&
`<button id="b-play">▶ PLAY</button><button id="b-playscn" style="color:#0ff">▶ SCN</button><button id="b-loop" style="color:#ff0">⟳ LOOP</button>` &&
`<span style="color:#666">|</span>` &&
`<button id="b-fb">⏪</button><button id="b-sb">◀</button><span style="color:#888">1/64</span>` &&
`<button id="b-sf">▶</button><button id="b-ff">⏩</button>` &&
`<span style="color:#666">|</span>` &&
`<button id="b-pbar">⏮</button><span style="color:#888">BAR</span><button id="b-nbar">⏭</button>` &&
`<span style="color:#666">|</span>` &&
`<button id="b-pscn">⏮</button><span style="color:#888">SCN</span><button id="b-nscn">⏭</button>` &&
`<span style="color:#666">|</span>` &&
`<span style="color:#888">GOTO:</span><input type="number" id="i-bar" value="0" min="0">` &&
`<button id="b-go">GO</button>` &&
`<span style="color:#666">|</span>` &&
`<button id="b-dbg">DEBUG</button><button id="b-mode" style="color:#f0f">DEV</button>` &&
`</div>` &&
`<div class="transport">` &&
`<span style="color:#888">ws</span>` &&
`<select id="s-streams" style="background:#111;color:#ff0;border:1px solid #ff0;padding:5px">` &&
`<option value="1">1</option><option value="2">2</option><option value="4" selected>4</option>` &&
`<option value="6">6</option><option value="8">8</option><option value="10">10</option>` &&
`<option value="12">12</option><option value="16">16</option></select>` &&
`<span style="color:#666">×</span>` &&
`<span style="color:#888">s</span>` &&
`<select id="s-pipeline" style="background:#111;color:#ff0;border:1px solid #ff0;padding:5px">` &&
`<option value="1">1</option><option value="2">2</option><option value="3">3</option>` &&
`<option value="4" selected>4</option><option value="5">5</option><option value="6">6</option>` &&
`<option value="8">8</option><option value="10">10</option><option value="12">12</option>` &&
`<option value="16">16</option></select>` &&
`<span style="color:#666">|</span>` &&
`<button id="b-preload" style="color:#ff0">PRELOAD ALL</button>` &&
`<span id="preload-speed" style="color:#888;font-size:10px"></span>` &&
`<span style="color:#666">|</span>` &&
`<button id="b-playcache" style="color:#0f0">▶ CACHED</button>` &&
`<button id="b-export" style="color:#f80">⏺ EXPORT</button>` &&
`<button id="b-probes" style="color:#f0f">📷 PROBES</button>` &&
`<button id="b-trace" style="color:#0ff">📊 TRACE</button>` &&
`<span id="export-status" style="color:#888;font-size:10px"></span>` &&
`<span style="color:#666">|</span>` &&
`<button id="b-reload" style="color:#0ff">RELOAD SCN</button>` &&
`<button id="b-profile" style="color:#f0f">PROFILE</button>` &&
`<button id="b-interleave" style="color:#0f0">L:H 1:3</button>` &&
`</div>` &&
`<div id="timeline"><div id="tl-scenes"></div><div id="tl-grid"></div><div id="playhead" style="left:0"></div></div>` &&
`<div id="hud-panel">` &&
`<div id="hud">` &&
`<div class="row"><span class="lbl">TICK</span> <span class="val" id="h-tick">0</span></div>` &&
`<div class="row"><span class="lbl">S.TICK</span> <span class="val" id="h-stick">0</span></div>` &&
`<div class="row"><span class="lbl">FRAME</span> <span class="val" id="h-frame">0</span></div>` &&
`<div class="row"><span class="lbl">S.FRM</span> <span class="val" id="h-sframe">0</span></div>` &&
`<div class="row"><span class="lbl">BAR</span> <span class="val" id="h-bar">0</span>.<span class="val" id="h-beat">0</span></div>` &&
`<div class="row"><span class="lbl">PHASE</span> <span class="val" id="h-phase">0.00</span></div>` &&
`<div class="row"><span class="lbl">SCENE</span> <span class="scene" id="h-scene">-</span></div>` &&
`<div class="row"><span class="lbl">PROGRESS</span> <span class="val" id="h-sprog">0</span>%</div>` &&
`<div class="row"><span class="lbl">SHAPES</span> <span class="val" id="h-shapes">0</span></div>` &&
`</div>` &&
`<div id="debug-global" class="debug-box">` &&
`<div class="title">GLOBAL DEBUG</div>` &&
`<pre id="debug-json">waiting...</pre>` &&
`</div>` &&
`<div id="debug-scene" class="debug-box">` &&
`<div class="title">SCENE DEBUG</div>` &&
`<pre id="scene-json">waiting...</pre>` &&
`</div>` &&
`</div>` &&
`<div id="info">Keys: Space=Play ←→=1/64 Shift=1/16 Ctrl=Beat []=Bar PgUp/Dn=Scene</div>` &&
`<audio id="au" preload="auto"><source src="?audio=ZOISEE-EAR-02.MP3" type="audio/mpeg"></audio>` &&
`</div>`.

    " WebGL shaders
    rv_html = rv_html &&
`<script id="vs" type="x-shader/x-vertex">attribute vec2 aPos;attribute vec3 aCol;varying vec3 vCol;` &&
`void main(){gl_Position=vec4(aPos.x/320.0-1.0,1.0-aPos.y/200.0,0.0,1.0);vCol=aCol;}</script>` &&
`<script id="fs" type="x-shader/x-fragment">precision mediump float;varying vec3 vCol;void main(){gl_FragColor=vec4(vCol,1.0);}</script>` &&
`<script>` &&
|var gl,tx,au,ws,playing=0,frame=0,tick=0,scenario=null,mode='dev',hudVisible=1,DEMO_ID='{ iv_demo }';| &&
`var frameCache={},preloading=0,preloadQueue=[],preloadStreams=2,preloadProgress={total:0,loaded:0},preloadStartTime=0;` &&
`var loopScene=null;` &&
`var preloadSockets=[],preloadActive=[],preloadPipelineDepth=4;` &&
`var sceneStats={},lastSceneFrame={},trState=null;` &&
`var glc=document.getElementById('glc'),txc=document.getElementById('txc');` &&
`gl=glc.getContext('webgl');tx=txc.getContext('2d');au=document.getElementById('au');` &&
`var BPM=152,FPS=30,BAR_SEC=60/BPM*4,TOTAL_BARS=116;` &&
`var TICKS_PER_BEAT=16,TICKS_PER_BAR=64,SEC_PER_TICK=60/(BPM*TICKS_PER_BEAT);` &&
`var imgCache={};function loadGalleryImg(n){if(!n||imgCache[n])return;imgCache[n]='loading';` &&
`var img=new Image();img.onload=(function(k,i){return function(){imgCache[k]=i;};})(n,img);` &&
`img.onerror=(function(k){return function(){imgCache[k]='error'};})(n);` &&
`img.src='?img='+n+'.PNG';}` &&
`function compileShader(t,s){var sh=gl.createShader(t);gl.shaderSource(sh,s);gl.compileShader(sh);return sh;}` &&
`var vs=compileShader(gl.VERTEX_SHADER,document.getElementById('vs').textContent);` &&
`var fs=compileShader(gl.FRAGMENT_SHADER,document.getElementById('fs').textContent);` &&
`var prog=gl.createProgram();gl.attachShader(prog,vs);gl.attachShader(prog,fs);gl.linkProgram(prog);gl.useProgram(prog);` &&
`var triB=gl.createBuffer(),lineB=gl.createBuffer();gl.enable(gl.BLEND);gl.blendFunc(gl.SRC_ALPHA,gl.ONE_MINUS_SRC_ALPHA);` &&
`function parseCol(c){if(!c)return[1,1,1];if(c[0]==='#'){var h=c.slice(1);if(h.length===3)h=h[0]+h[0]+h[1]+h[1]+h[2]+h[2];` &&
`return[parseInt(h.slice(0,2),16)/255,parseInt(h.slice(2,4),16)/255,parseInt(h.slice(4,6),16)/255];}` &&
`if(c.startsWith('hsl')){var m=c.match(/\d+/g);if(m){var h=parseInt(m[0])/360,s=parseInt(m[1])/100,l=parseInt(m[2])/100;` &&
`if(s===0)return[l,l,l];var q=l<0.5?l*(1+s):l+s-l*s,p=2*l-q;return[hue2rgb(p,q,h+1/3),hue2rgb(p,q,h),hue2rgb(p,q,h-1/3)];}}` &&
`if(c.startsWith('rgb')){var m=c.match(/\d+/g);if(m)return[m[0]/255,m[1]/255,m[2]/255];}return[1,1,1];}` &&
`function hue2rgb(p,q,t){if(t<0)t+=1;if(t>1)t-=1;if(t<1/6)return p+(q-p)*6*t;if(t<1/2)return q;if(t<2/3)return p+(q-p)*(2/3-t)*6;return p;}`.

    " Render frame
    rv_html = rv_html &&
`function rf(d){gl.clearColor(0,0,0,1);gl.clear(gl.COLOR_BUFFER_BIT);tx.clearRect(0,0,640,400);` &&
`trState=TM.getState(d.gb||0,d.bp||0);TM.applyPre(tx,trState);` &&
`var aPos=gl.getAttribLocation(prog,'aPos'),aCol=gl.getAttribLocation(prog,'aCol'),v=[];` &&
`if(d.tri)for(var i=0;i<d.tri.length;i++){var t=d.tri[i],c=parseCol(t.f);` &&
`v.push(t.x1,t.y1,c[0],c[1],c[2],t.x2,t.y2,c[0],c[1],c[2],t.x3,t.y3,c[0],c[1],c[2]);}` &&
`if(d.rb)for(var i=0;i<d.rb.length;i++){var r=d.rb[i],c=parseCol(r.f),x=r.x,y=r.y,w=r.w,h=r.h;` &&
`v.push(x,y,c[0],c[1],c[2],x+w,y,c[0],c[1],c[2],x+w,y+h,c[0],c[1],c[2]);` &&
`v.push(x,y,c[0],c[1],c[2],x+w,y+h,c[0],c[1],c[2],x,y+h,c[0],c[1],c[2]);}` &&
`if(d.c)for(var i=0;i<d.c.length;i++){var ci=d.c[i],c=parseCol(ci.f),cx=ci.x,cy=ci.y,cr=ci.r,seg=16;` &&
`for(var s=0;s<seg;s++){var a1=s/seg*Math.PI*2,a2=(s+1)/seg*Math.PI*2;` &&
`v.push(cx,cy,c[0],c[1],c[2],cx+Math.cos(a1)*cr,cy+Math.sin(a1)*cr,c[0],c[1],c[2],cx+Math.cos(a2)*cr,cy+Math.sin(a2)*cr,c[0],c[1],c[2]);}}` &&
`if(d.r)for(var i=0;i<d.r.length;i++){var r=d.r[i],c=parseCol(r.f),x=r.x,y=r.y,w=r.w,h=r.h;` &&
`v.push(x,y,c[0],c[1],c[2],x+w,y,c[0],c[1],c[2],x+w,y+h,c[0],c[1],c[2]);` &&
`v.push(x,y,c[0],c[1],c[2],x+w,y+h,c[0],c[1],c[2],x,y+h,c[0],c[1],c[2]);}` &&
`if(v.length){gl.bindBuffer(gl.ARRAY_BUFFER,triB);gl.bufferData(gl.ARRAY_BUFFER,new Float32Array(v),gl.DYNAMIC_DRAW);` &&
`gl.enableVertexAttribArray(aPos);gl.enableVertexAttribArray(aCol);` &&
`gl.vertexAttribPointer(aPos,2,gl.FLOAT,false,20,0);gl.vertexAttribPointer(aCol,3,gl.FLOAT,false,20,8);` &&
`gl.drawArrays(gl.TRIANGLES,0,v.length/5);}` &&
`if(d.img&&d.img.length){for(var k=0;k<d.img.length;k++){var imgDef=d.img[k];loadGalleryImg(imgDef.n);` &&
`var im=imgCache[imgDef.n];if(im&&im.width){tx.drawImage(im,imgDef.x,imgDef.y,imgDef.w||im.width,imgDef.h||im.height);` &&
`for(var sy=0;sy<imgDef.h;sy+=4){tx.strokeStyle='rgba(0,255,255,'+(0.08+Math.sin(sy/10+d.t*3)*0.04)+')';` &&
`tx.beginPath();tx.moveTo(imgDef.x,imgDef.y+sy);tx.lineTo(imgDef.x+imgDef.w,imgDef.y+sy);tx.stroke();}}}}` &&
`if(d.l&&d.l.length){var lv=[];for(var i=0;i<d.l.length;i++){var l=d.l[i],c=parseCol(l.c);` &&
`lv.push(l.x1,l.y1,c[0],c[1],c[2],l.x2,l.y2,c[0],c[1],c[2]);}` &&
`gl.bindBuffer(gl.ARRAY_BUFFER,lineB);gl.bufferData(gl.ARRAY_BUFFER,new Float32Array(lv),gl.DYNAMIC_DRAW);` &&
`gl.enableVertexAttribArray(aPos);gl.enableVertexAttribArray(aCol);` &&
`gl.vertexAttribPointer(aPos,2,gl.FLOAT,false,20,0);gl.vertexAttribPointer(aCol,3,gl.FLOAT,false,20,8);` &&
`gl.drawArrays(gl.LINES,0,d.l.length*2);}` &&
`if(d.tx)for(var j=0;j<d.tx.length;j++){var t=d.tx[j];tx.fillStyle=t.c;tx.font=(t.s||16)+'px monospace';` &&
`tx.textAlign=t.align||'center';tx.fillText(t.t,t.x,t.y);}` &&
`if(d.flash){var fl=d.flash,r=Math.round((fl.color?fl.color[0]:1)*255),g=Math.round((fl.color?fl.color[1]:1)*255),b=Math.round((fl.color?fl.color[2]:1)*255);` &&
`tx.fillStyle='rgba('+r+','+g+','+b+','+fl.intensity+')';tx.fillRect(0,0,640,400);}` &&
`TM.applyPost(tx,trState);updateHUD(d);updatePlayhead(d.gb||0);}`.

    " HUD update + timeline + debug panel
    rv_html = rv_html &&
`var debugVisible=1,lastFrameData=null;` &&
`var TM={scene:null,trans:null,getState:function(bar,bp){` &&
`if(!scenario||!scenario.scenes)return null;var s=scenario.scenes.find(function(sc){return bar>=sc.start_bar&&bar<sc.end_bar;});` &&
`if(!s||!s.transition)return null;var tr=s.transition,len=s.end_bar-s.start_bar,barInScene=bar-s.start_bar,beatLen=tr.beats||2,beatBars=beatLen/4;` &&
`if(tr.mode==='pre'){var preStart=len-beatBars;if(barInScene>=preStart){var p=(barInScene-preStart+bp)/beatBars;return{type:tr.type,progress:Math.min(1,p),phase:'out'};}}` &&
`if(tr.mode==='post'){if(barInScene<beatBars){var p=(barInScene+bp)/beatBars;return{type:tr.type,progress:Math.min(1,p),phase:'in'};}}return null;},` &&
`applyPre:function(ctx,st){if(!st)return;var p=st.progress,cx=320,cy=200;ctx.save();` &&
`if(st.type==='shrink'){var sc=st.phase==='out'?1-p*0.3:0.7+p*0.3;ctx.translate(cx,cy);ctx.scale(sc,sc);ctx.translate(-cx,-cy);}` &&
`if(st.type==='iris'){var r=st.phase==='out'?400*(1-p):400*p;ctx.beginPath();ctx.arc(cx,cy,Math.max(1,r),0,Math.PI*2);ctx.clip();}` &&
`if(st.type==='diamond'){var sz=st.phase==='out'?400*(1-p):400*p;ctx.beginPath();ctx.moveTo(cx,cy-sz);ctx.lineTo(cx+sz,cy);ctx.lineTo(cx,cy+sz);ctx.lineTo(cx-sz,cy);ctx.closePath();ctx.clip();}` &&
`if(st.type==='wipe-left'){var w=st.phase==='out'?640*(1-p):640*p;ctx.beginPath();ctx.rect(0,0,w,400);ctx.clip();}` &&
`if(st.type==='wipe-right'){var w=st.phase==='out'?640*(1-p):640*p;ctx.beginPath();ctx.rect(640-w,0,w,400);ctx.clip();}},` &&
`applyPost:function(ctx,st){if(!st)return;ctx.restore();var p=st.progress;` &&
`if(st.type==='fade'){var a=st.phase==='out'?p:1-p;ctx.fillStyle='rgba(0,0,0,'+a+')';ctx.fillRect(0,0,640,400);}` &&
`if(st.type==='flash'){var a=st.phase==='out'?1-p:p;a=a*0.8;ctx.fillStyle='rgba(255,255,255,'+a+')';ctx.fillRect(0,0,640,400);}` &&
`if(st.type==='pixelate'&&p>0.1){var ps=Math.floor(2+p*20);ctx.imageSmoothingEnabled=false;` &&
`var tmp=document.createElement('canvas');tmp.width=640/ps;tmp.height=400/ps;var tc=tmp.getContext('2d');` &&
`tc.drawImage(ctx.canvas,0,0,tmp.width,tmp.height);ctx.clearRect(0,0,640,400);ctx.drawImage(tmp,0,0,640,400);}}};` &&
`function updateHUD(d){lastFrameData=d;var db=d.debug||{};` &&
`tick=Math.round(au.currentTime/SEC_PER_TICK);var gf=Math.round(tick*SEC_PER_TICK*FPS);` &&
`document.getElementById('h-tick').textContent=tick;document.getElementById('h-frame').textContent=gf;` &&
`var st=0,sf=0;if(scenario){var gb=d.gb||0;var scn=scenario.scenes.find(function(s){return gb>=s.start_bar&&gb<s.end_bar;});` &&
`if(scn){var scnStartTick=scn.start_bar*TICKS_PER_BAR;st=tick-scnStartTick;sf=Math.round(st*SEC_PER_TICK*FPS);}}` &&
`document.getElementById('h-stick').textContent=st;document.getElementById('h-sframe').textContent=sf;` &&
`document.getElementById('h-bar').textContent=d.gb||0;` &&
`document.getElementById('h-beat').textContent=db.beat||0;` &&
`document.getElementById('h-phase').textContent=(db.bar_phase||0).toFixed(2);` &&
`document.getElementById('h-scene').textContent=d.e||'-';` &&
`document.getElementById('h-sprog').textContent=Math.round((db.scene_progress||0)*100);` &&
`var shapes=(d.lc||0)+(d.tric||0)+(d.rc||0)+(d.cc||0)+(d.tc||0);document.getElementById('h-shapes').textContent=shapes;` &&
`if(trState){document.getElementById('h-scene').textContent=(d.e||'-')+' ['+trState.type+' '+Math.round(trState.progress*100)+'%]';}else{document.getElementById('h-scene').textContent=d.e||'-';}` &&
`if(debugVisible){var global={timing:{t:d.t,gt:d.gt,bar:d.gb,bar_phase:d.bp,pulse:d.p},` &&
`frames:{gf:db.gf,f:db.f,gtick:db.gtick,tick:db.tick},` &&
`beat:{bar:db.bar,beat:db.beat,bar_phase:db.bar_phase,beat_phase:db.beat_phase},` &&
`counts:{lines:d.lc,tris:d.tric,rects:d.rc,circles:d.cc,texts:d.tc},effect:d.e,transition:d.tr};` &&
`document.getElementById('debug-json').textContent=JSON.stringify(global,null,2);` &&
`var scene={effect:d.e,scene_bar:db.scene_bar,progress:db.scene_progress,` &&
`on_beat:db.on_beat,on_bar:db.on_bar,pulse:db.pulse};` &&
`if(db.vars)try{scene.vars=JSON.parse(db.vars);}catch(e){scene.vars=db.vars;}` &&
`document.getElementById('scene-json').textContent=JSON.stringify(scene,null,2);}}` &&
`function updatePlayhead(bar){document.getElementById('playhead').style.left=(bar/TOTAL_BARS*100)+'%';}` &&
`function buildTimeline(){if(!scenario)return;var sc=document.getElementById('tl-scenes');sc.innerHTML='';` &&
`var colors=['#f80','#0f0','#0ff','#f0f','#ff0','#08f'];` &&
`scenario.scenes.forEach(function(s,i){var d=document.createElement('div');d.className='tl-scene';` &&
`var st=(s.start_bar/TOTAL_BARS)*100,w=((s.end_bar-s.start_bar)/TOTAL_BARS)*100;` &&
`d.style.left=st+'%';d.style.width=w+'%';d.style.background=colors[i%colors.length]+'44';` &&
`d.style.borderColor=colors[i%colors.length];d.textContent=s.name;` &&
`d.onclick=function(){seekToBar(s.start_bar);};sc.appendChild(d);});` &&
`var gr=document.getElementById('tl-grid');gr.innerHTML='';` &&
`for(var b=0;b<TOTAL_BARS;b+=8){var d=document.createElement('div');d.className='tl-bar';` &&
`d.style.left=(b/TOTAL_BARS*100)+'%';d.textContent=b;gr.appendChild(d);}}`.

    " WebSocket + controls
    rv_html = rv_html &&
`function cn(){document.getElementById('info').textContent='CONNECTING...';` &&
`ws=new WebSocket((location.protocol==='https:'?'wss:':'ws:')+'//'+location.host+'/sap/bc/apc/sap/zo4d_demo');` &&
`ws.onopen=function(){document.getElementById('info').textContent='CONNECTED [DEV:'+DEMO_ID+'] '+BPM+' BPM / '+Math.round(FPS)+' FPS';` &&
`if(DEMO_ID!=='main')ws.send(JSON.stringify({cmd:'load_demo',demo:DEMO_ID}));` &&
`ws.send(JSON.stringify({cmd:'set_mode',mode:mode}));ws.send(JSON.stringify({cmd:'get_scenario'}));};` &&
`ws.onclose=function(){document.getElementById('info').textContent='DISCONNECTED';};` &&
`ws.onmessage=function(e){try{var d=JSON.parse(e.data);` &&
`if(d.type==='scenario'){scenario=d;TOTAL_BARS=d.total_bars||116;BPM=d.bpm||152;FPT=d.fpt||1;BAR_SEC=240/BPM;` &&
`SEC_PER_TICK=BAR_SEC/64;sI=SEC_PER_TICK*1000/FPT;FPS=d.fps||(1000/sI);` &&
`if(d.audio)au.src='?audio='+d.audio;` &&
`if(d.media){d.media.forEach(function(m){if(m.type==='img')loadGalleryImg(m.name);});}` &&
`buildTimeline();updateInfoLine();}` &&
`else if(d.type==='mode'){mode=d.mode;document.getElementById('b-mode').textContent=mode.toUpperCase();}` &&
`else if(d.type==='preload'){handlePreloadFrame(d);}` &&
`else if(!d.type){rf(d);if(d.debug&&d.debug.frame!==undefined)frame=d.debug.frame;else if(playing)frame++;}}catch(x){}};}` &&
`function play(){if(!ws||ws.readyState!==1)return;cT=0;cS=0;lT=0;au.play();ws.send('start');playing=1;` &&
`document.getElementById('b-play').textContent='⏸ PAUSE';rq();}` &&
`function pause(){if(ws&&ws.readyState===1)ws.send('stop');au.pause();playing=0;` &&
`document.getElementById('b-play').textContent='▶ PLAY';}` &&
`function toggle(){playing?pause():play();}` &&
`function playFromScene(){if(!scenario)return;var cb=Math.floor(frame/(FPS*BAR_SEC));` &&
`var scn=scenario.scenes.find(function(s){return cb>=s.start_bar&&cb<s.end_bar;});` &&
`if(scn){seekToBar(scn.start_bar);setTimeout(play,100);}}` &&
`function toggleLoop(){if(!scenario)return;var cb=Math.floor(au.currentTime/BAR_SEC);` &&
`var scn=scenario.scenes.find(function(s){return cb>=s.start_bar&&cb<s.end_bar;});` &&
`if(!scn)return;if(loopScene){loopScene=null;document.getElementById('b-loop').style.background='';` &&
`document.getElementById('b-loop').textContent='⟳ LOOP';}else{loopScene=scn;` &&
`document.getElementById('b-loop').style.background='#ff0';document.getElementById('b-loop').style.color='#000';` &&
`document.getElementById('b-loop').textContent='⟳ '+scn.name;seekToBar(scn.start_bar);play();}}` &&
`function checkLoop(){if(!loopScene||!playing)return;var ct=au.currentTime;var endTime=loopScene.end_bar*BAR_SEC;` &&
`if(ct>=endTime){au.currentTime=loopScene.start_bar*BAR_SEC;}}` &&
`var FPT=1,sI=SEC_PER_TICK*1000/FPT,cT=0,cS=0,lT=0;` &&
`function rq(){if(!playing)return;checkLoop();var n=performance.now();if(n-lT>=sI){lT=n;` &&
`var tt=au.currentTime/SEC_PER_TICK;cT=Math.floor(tt);cS=Math.floor((tt-cT)*FPT);` &&
`if(ws&&ws.readyState===1)ws.send(JSON.stringify({cmd:'frame',tick:cT,sub:cS}));}requestAnimationFrame(rq);}` &&
`function seekToBar(b,skipSync){if(!ws||ws.readyState!==1)return;cT=Math.floor(b*64);cS=0;lT=0;ws.send(JSON.stringify({cmd:'seek',bar:b}));` &&
`au.currentTime=b*BAR_SEC;frame=Math.round(b*BAR_SEC*FPS);document.getElementById('i-bar').value=b;updatePlayhead(b);if(!skipSync)syncSceneIdx();}` &&
`function seekToTick(t){if(!ws||ws.readyState!==1)return;cT=t;cS=0;lT=0;var time=t*SEC_PER_TICK;var bar=t/TICKS_PER_BAR;` &&
`ws.send(JSON.stringify({cmd:'seek',bar:bar}));au.currentTime=time;tick=t;frame=Math.round(t*SEC_PER_TICK*FPS);` &&
`updatePlayhead(bar);syncSceneIdx();}` &&
`function reqFrame(){if(ws&&ws.readyState===1)ws.send(JSON.stringify({cmd:'frame'}));}` &&
`function stepTick(d){var nt=tick+d;if(nt<0)nt=0;seekToTick(nt);if(!playing)setTimeout(reqFrame,50);}` &&
`function stepBar(d){var cb=Math.floor((au.currentTime+0.05)/BAR_SEC),nb=cb+d;if(nb<0)nb=0;if(nb>=TOTAL_BARS)nb=TOTAL_BARS-1;` &&
`seekToBar(nb);if(!playing)setTimeout(reqFrame,50);}` &&
`var curSceneIdx=0;` &&
`function stepScene(d){if(!scenario||!scenario.scenes.length)return;` &&
`curSceneIdx+=d;if(curSceneIdx<0)curSceneIdx=0;if(curSceneIdx>=scenario.scenes.length)curSceneIdx=scenario.scenes.length-1;` &&
`seekToBar(scenario.scenes[curSceneIdx].start_bar,true);if(!playing)setTimeout(reqFrame,50);}` &&
`function syncSceneIdx(){if(!scenario)return;var cb=au.currentTime/BAR_SEC;` &&
`var idx=scenario.scenes.findIndex(function(s){return cb>=s.start_bar&&cb<s.end_bar;});` &&
`if(idx>=0)curSceneIdx=idx;}` &&
`var preloadQueue=[],preloadQueueLight=[],preloadQueueHeavy=[],useSplit=true;` &&
`var heavyScenes=['starfield','plasma','sierpinski','elite3d'];` &&
`function buildSplitQueues(){if(!scenario)return;preloadQueueLight=[];preloadQueueHeavy=[];` &&
`var totalFrames=Math.round(TOTAL_BARS*BAR_SEC*FPS);` &&
`scenario.scenes.forEach(function(s){var sf=Math.round(s.start_bar*BAR_SEC*FPS),ef=Math.round(s.end_bar*BAR_SEC*FPS);` &&
`var isHeavy=heavyScenes.indexOf(s.id)>=0;` &&
`for(var f=sf;f<ef&&f<totalFrames;f++){if(isHeavy)preloadQueueHeavy.push(f);else preloadQueueLight.push(f);}});}` &&
`function startPreloadAll(){if(!scenario||preloading)return;preloading=1;frameCache={};sceneStats={};` &&
`preloadStreams=parseInt(document.getElementById('s-streams').value)||4;` &&
`preloadPipelineDepth=parseInt(document.getElementById('s-pipeline').value)||4;` &&
`var totalFrames=Math.round(TOTAL_BARS*BAR_SEC*FPS);` &&
`if(useSplit){buildSplitQueues();preloadQueue=[];}else{preloadQueueLight=[];preloadQueueHeavy=[];` &&
`preloadQueue=[];for(var i=0;i<totalFrames;i++)preloadQueue.push(i);}` &&
`preloadProgress={total:totalFrames,loaded:0};preloadStartTime=Date.now();` &&
`document.getElementById('b-preload').textContent='⏹ STOP';document.getElementById('b-preload').style.color='#f00';` &&
`document.getElementById('preload-speed').textContent=' opening '+preloadStreams+' ws (shared queue)...';` &&
`closePreloadSockets();preloadActive=[];` &&
`for(var s=0;s<preloadStreams;s++)openPreloadSocket(s);}` &&
`function openPreloadSocket(idx){var url=(location.protocol==='https:'?'wss:':'ws:')+'//'+location.host+'/sap/bc/apc/sap/zo4d_demo';` &&
`var sock=new WebSocket(url);preloadSockets[idx]=sock;preloadActive[idx]=0;` &&
`sock.onopen=function(){sock.send(JSON.stringify({cmd:'set_mode',mode:'dev'}));for(var p=0;p<preloadPipelineDepth;p++)preloadNextFor(idx);};` &&
`sock.onmessage=function(e){try{var d=JSON.parse(e.data);if(d.type==='preload'){handlePreloadFrame(d,idx);}}catch(x){}};` &&
`sock.onerror=function(){preloadActive[idx]=0;};sock.onclose=function(){preloadActive[idx]=0;};}` &&
`function closePreloadSockets(){for(var i=0;i<preloadSockets.length;i++){if(preloadSockets[i]&&preloadSockets[i].readyState<2)preloadSockets[i].close();}preloadSockets=[];}` &&
`function preloadNextFor(idx){if(!preloading){checkPreloadDone();return;}` &&
`var f,lightWS=Math.max(1,Math.floor(preloadStreams/4));` &&
`if(useSplit){if(idx<lightWS){if(preloadQueueLight.length)f=preloadQueueLight.shift();else if(preloadQueueHeavy.length)f=preloadQueueHeavy.shift();}` &&
`else{if(preloadQueueHeavy.length)f=preloadQueueHeavy.shift();else if(preloadQueueLight.length)f=preloadQueueLight.shift();}}` &&
`else{if(preloadQueue.length)f=preloadQueue.shift();}` &&
`if(f===undefined){checkPreloadDone();return;}` &&
`preloadActive[idx]=(preloadActive[idx]||0)+1;preloadSockets[idx].send(JSON.stringify({cmd:'preload',frame:f}));}` &&
`function handlePreloadFrame(d,idx){var f=d.pf,scene=d.e||'unknown',now=Date.now();frameCache[f]=d;preloadProgress.loaded++;` &&
`preloadActive[idx]=Math.max(0,(preloadActive[idx]||1)-1);` &&
`if(!sceneStats[scene])sceneStats[scene]={frames:0,start:now,end:now};` &&
`sceneStats[scene].frames++;sceneStats[scene].end=now;` &&
`var pct=Math.round(preloadProgress.loaded/preloadProgress.total*100);` &&
`var elapsed=(Date.now()-preloadStartTime)/1000;var fps=elapsed>0?Math.round(preloadProgress.loaded/elapsed):0;` &&
`document.getElementById('preload-speed').textContent=' '+pct+'% '+fps+' f/s ['+scene+']';preloadNextFor(idx);}` &&
`function stopPreload(){if(!preloading)return;preloading=0;preloadQueue=[];preloadQueueLight=[];preloadQueueHeavy=[];closePreloadSockets();` &&
`var elapsed=(Date.now()-preloadStartTime)/1000;var fps=elapsed>0?Math.round(preloadProgress.loaded/elapsed):0;` &&
`document.getElementById('b-preload').textContent='PRELOAD ALL';document.getElementById('b-preload').style.color='#ff0';` &&
`document.getElementById('preload-speed').textContent=' STOPPED: '+preloadProgress.loaded+'/'+preloadProgress.total+' ('+fps+' f/s, '+preloadStreams+'×'+preloadPipelineDepth+')';}` &&
`function clearCache(){frameCache={};preloadProgress={total:0,loaded:0};` &&
`document.getElementById('preload-speed').textContent=' cache cleared';}` &&
`function showProfile(){var stats=[];for(var s in sceneStats){var st=sceneStats[s];` &&
`var dur=(st.end-st.start)/1000;var fps=dur>0?Math.round(st.frames/dur):0;` &&
`stats.push({scene:s,frames:st.frames,dur:dur.toFixed(1),fps:fps});}` &&
`stats.sort(function(a,b){return a.fps-b.fps;});` &&
`var txt='SCENE PROFILE (sorted by fps, slowest first):\\n';` &&
`stats.forEach(function(s){txt+=s.scene.padEnd(15)+' '+s.frames+' frames, '+s.dur+'s, '+s.fps+' f/s\\n';});` &&
`alert(txt);console.log(stats);}` &&
`function checkPreloadDone(){var allIdle=preloadActive.every(function(a){return !a||a===0;});` &&
`var allEmpty=!preloadQueue.length&&!preloadQueueLight.length&&!preloadQueueHeavy.length;` &&
`if(preloadProgress.loaded>=preloadProgress.total||(allEmpty&&allIdle)){preloading=0;closePreloadSockets();` &&
`var elapsed=(Date.now()-preloadStartTime)/1000;var fps=elapsed>0?Math.round(preloadProgress.loaded/elapsed):0;` &&
`document.getElementById('b-preload').textContent='CACHED '+Object.keys(frameCache).length;document.getElementById('b-preload').style.color='#0f0';` &&
`document.getElementById('preload-speed').textContent=' '+fps+' f/s ('+elapsed.toFixed(1)+'s, '+preloadStreams+'×'+preloadPipelineDepth+')';}}` &&
`function reloadCurrentScene(){if(!scenario)return;var cb=Math.floor(au.currentTime/BAR_SEC);` &&
`var scn=scenario.scenes.find(function(s){return cb>=s.start_bar&&cb<s.end_bar;});if(!scn)return;` &&
`var sf=Math.round(scn.start_bar*BAR_SEC*FPS),ef=Math.round(scn.end_bar*BAR_SEC*FPS);` &&
`for(var i=sf;i<ef;i++)delete frameCache[i];` &&
`document.getElementById('b-reload').textContent='RELOAD...';` &&
`preloadQueue=[];for(var i=sf;i<ef;i++)preloadQueue.push(i);` &&
`preloadProgress={total:ef-sf,loaded:0};preloading=1;` &&
`for(var s=0;s<preloadStreams;s++)preloadNext();}` &&
`var cachedMode=0,exportMode=0,mediaRecorder=null,recordedChunks=[];` &&
`function playCached(){if(!cachedMode)return;var cached=frameCache[frame];` &&
`if(cached){rf(cached);frame++;updatePlayhead(cached.gb||0);` &&
`var pct=Math.round(frame/preloadProgress.total*100);document.getElementById('export-status').textContent=' '+pct+'%';` &&
`if(frame<preloadProgress.total){requestAnimationFrame(playCached);}else{stopCached();}}}` &&
`function startCached(){if(Object.keys(frameCache).length<10){alert('Preload frames first!');return;}` &&
`pause();frame=0;cachedMode=1;au.currentTime=0;au.play();` &&
`document.getElementById('b-playcache').textContent='⏹ STOP';document.getElementById('b-playcache').style.color='#f00';playCached();}` &&
`function stopCached(){cachedMode=0;au.pause();document.getElementById('b-playcache').textContent='▶ CACHED';` &&
`document.getElementById('b-playcache').style.color='#0f0';document.getElementById('export-status').textContent=' done';}` &&
`function startExport(){if(Object.keys(frameCache).length<10){alert('Preload frames first!');return;}` &&
`pause();frame=0;exportMode=1;recordedChunks=[];` &&
`var combined=document.createElement('canvas');combined.width=640;combined.height=400;var cctx=combined.getContext('2d');` &&
`var stream=combined.captureStream(FPS);var audioCtx=new AudioContext();` &&
`var audioSrc=audioCtx.createMediaElementSource(au);var audioDest=audioCtx.createMediaStreamDestination();audioSrc.connect(audioDest);audioSrc.connect(audioCtx.destination);` &&
`stream.addTrack(audioDest.stream.getAudioTracks()[0]);` &&
`mediaRecorder=new MediaRecorder(stream,{mimeType:'video/webm;codecs=vp9',videoBitsPerSecond:8000000});` &&
`mediaRecorder.ondataavailable=function(e){if(e.data.size>0)recordedChunks.push(e.data);};` &&
`mediaRecorder.onstop=function(){var blob=new Blob(recordedChunks,{type:'video/webm'});var url=URL.createObjectURL(blob);` &&
`var a=document.createElement('a');a.href=url;a.download=DEMO_ID+'_export.webm';a.click();URL.revokeObjectURL(url);` &&
`document.getElementById('b-export').textContent='⏺ EXPORT';document.getElementById('b-export').style.color='#f80';` &&
`document.getElementById('export-status').textContent=' saved!';exportMode=0;};` &&
`mediaRecorder.start();au.currentTime=0;au.play();` &&
`document.getElementById('b-export').textContent='⏹ STOP';document.getElementById('b-export').style.color='#f00';` &&
`function renderExport(){if(!exportMode)return;var cached=frameCache[frame];` &&
`if(cached){rf(cached);cctx.drawImage(glc,0,0);cctx.drawImage(txc,0,0);frame++;` &&
`var pct=Math.round(frame/preloadProgress.total*100);document.getElementById('export-status').textContent=' REC '+pct+'%';` &&
`updatePlayhead(cached.gb||0);if(frame<preloadProgress.total){requestAnimationFrame(renderExport);}else{stopExport();}}}renderExport();}` &&
`function stopExport(){exportMode=0;au.pause();if(mediaRecorder&&mediaRecorder.state!=='inactive')mediaRecorder.stop();}` &&
`var probesPerScene=3;` &&
`function saveProbes(){if(Object.keys(frameCache).length<10){alert('Preload frames first!');return;}` &&
`if(!scenario||!scenario.scenes){alert('No scenario!');return;}` &&
`var combined=document.createElement('canvas');combined.width=640;combined.height=400;var cctx=combined.getContext('2d');` &&
`var probes=[],saved=0;` &&
`scenario.scenes.forEach(function(s,idx){var sf=Math.round(s.start_bar*BAR_SEC*FPS),ef=Math.round(s.end_bar*BAR_SEC*FPS);` &&
`var len=ef-sf;if(len<3)return;` &&
`for(var p=0;p<probesPerScene;p++){var offset=Math.floor((p+0.5)/probesPerScene*len);var f=sf+offset;` &&
`probes.push({frame:f,scene:s.name||s.id,idx:idx,probe:p});}});` &&
`document.getElementById('export-status').textContent=' saving '+probes.length+' probes...';` &&
`function saveNext(){if(saved>=probes.length){document.getElementById('export-status').textContent=' saved '+saved+' PNGs!';return;}` &&
`var pr=probes[saved],cached=frameCache[pr.frame];` &&
`if(cached){rf(cached);cctx.drawImage(glc,0,0);cctx.drawImage(txc,0,0);` &&
`combined.toBlob(function(blob){var url=URL.createObjectURL(blob);var a=document.createElement('a');` &&
`a.href=url;a.download=DEMO_ID+'_'+String(pr.idx).padStart(2,'0')+'_'+pr.scene.replace(/\\s+/g,'_')+'_'+pr.probe+'.png';` &&
`a.click();URL.revokeObjectURL(url);saved++;document.getElementById('export-status').textContent=' '+saved+'/'+probes.length;` &&
`setTimeout(saveNext,200);},'image/png');}else{saved++;saveNext();}}saveNext();}` &&
`var traceData=[],traceMode=0,traceScene=null;` &&
`function startTrace(){if(Object.keys(frameCache).length<10){alert('Preload first!');return;}` &&
`var sceneName=prompt('Scene to trace (or * for all):','rotozoom');if(!sceneName)return;` &&
`traceScene=sceneName==='*'?null:sceneName.toLowerCase();traceData=[];traceMode=1;frame=0;` &&
`document.getElementById('b-trace').textContent='⏹ STOP';document.getElementById('b-trace').style.color='#f00';` &&
`document.getElementById('export-status').textContent=' tracing...';traceLoop();}` &&
`function traceLoop(){if(!traceMode){finishTrace();return;}` &&
`var cached=frameCache[frame];if(!cached){frame++;if(frame<preloadProgress.total){traceLoop();}else{finishTrace();}return;}` &&
`var dominated=traceScene&&cached.e&&cached.e.toLowerCase().indexOf(traceScene)<0;` &&
`if(!dominated&&cached.debug){var vars={};try{vars=JSON.parse(cached.debug.vars||'{}');}catch(e){}` &&
`traceData.push({f:frame,t:cached.t,gt:cached.gt,gb:cached.gb,bp:cached.bp,e:cached.e,` &&
`zoom:vars.zoom,angle:vars.angle_deg,boost:vars.boost,pulse:cached.debug.pulse});}` &&
`frame++;var pct=Math.round(frame/preloadProgress.total*100);` &&
`document.getElementById('export-status').textContent=' trace '+pct+'% ('+traceData.length+' pts)';` &&
`if(frame<preloadProgress.total){setTimeout(traceLoop,1);}else{finishTrace();}}` &&
`function finishTrace(){traceMode=0;document.getElementById('b-trace').textContent='📊 TRACE';` &&
`document.getElementById('b-trace').style.color='#0ff';` &&
`if(traceData.length===0){document.getElementById('export-status').textContent=' no data';return;}` &&
`console.log('TRACE DATA:',traceData);` &&
`var csv='frame,t,gt,bar,bp,effect,zoom,angle,boost,pulse\\n';` &&
`traceData.forEach(function(r){csv+=r.f+','+r.t.toFixed(4)+','+r.gt.toFixed(4)+','+r.gb+','+r.bp.toFixed(4)+','+` &&
`r.e+','+(r.zoom||'')+','+(r.angle||'')+','+(r.boost||'')+','+(r.pulse||'')+'\\n';});` &&
`var blob=new Blob([csv],{type:'text/csv'});var url=URL.createObjectURL(blob);` &&
`var a=document.createElement('a');a.href=url;a.download=DEMO_ID+'_trace_'+(traceScene||'all')+'.csv';a.click();` &&
`document.getElementById('export-status').textContent=' saved '+traceData.length+' points';` &&
`analyzeTrace();}` &&
`function analyzeTrace(){if(traceData.length<2)return;var anomalies=[];` &&
`for(var i=1;i<traceData.length;i++){var prev=traceData[i-1],cur=traceData[i];` &&
`if(prev.zoom&&cur.zoom){var dz=Math.abs(cur.zoom-prev.zoom);if(dz>2)anomalies.push({f:cur.f,t:cur.t,type:'zoom_jump',delta:dz});}` &&
`if(prev.angle!==undefined&&cur.angle!==undefined){var da=cur.angle-prev.angle;` &&
`if(da<-180)da+=360;if(da>180)da-=360;if(Math.abs(da)>30)anomalies.push({f:cur.f,t:cur.t,type:'angle_jump',delta:da});}}` &&
`if(anomalies.length){console.log('ANOMALIES DETECTED:',anomalies);` &&
`alert('Found '+anomalies.length+' anomalies! Check console.');}}`.

    " Event handlers
    rv_html = rv_html &&
`document.getElementById('b-play').onclick=toggle;document.getElementById('b-playscn').onclick=playFromScene;document.getElementById('b-loop').onclick=toggleLoop;` &&
`document.getElementById('b-sf').onclick=function(){stepTick(1);};` &&
`document.getElementById('b-sb').onclick=function(){stepTick(-1);};` &&
`document.getElementById('b-ff').onclick=function(){stepTick(16);};` &&
`document.getElementById('b-fb').onclick=function(){stepTick(-16);};` &&
`document.getElementById('b-nbar').onclick=function(){stepBar(1);};` &&
`document.getElementById('b-pbar').onclick=function(){stepBar(-1);};` &&
`document.getElementById('b-nscn').onclick=function(){stepScene(1);};` &&
`document.getElementById('b-pscn').onclick=function(){stepScene(-1);};` &&
`document.getElementById('b-go').onclick=function(){seekToBar(parseInt(document.getElementById('i-bar').value)||0);if(!playing)setTimeout(reqFrame,50);};` &&
`document.getElementById('i-bar').onkeydown=function(e){if(e.key==='Enter'){e.preventDefault();document.getElementById('b-go').click();}};` &&
`document.getElementById('b-dbg').onclick=function(){debugVisible=!debugVisible;` &&
`document.getElementById('debug-global').style.display=debugVisible?'block':'none';` &&
`document.getElementById('debug-scene').style.display=debugVisible?'block':'none';};` &&
`document.getElementById('b-preload').onclick=function(){preloading?stopPreload():startPreloadAll();};` &&
`document.getElementById('b-playcache').onclick=function(){cachedMode?stopCached():startCached();};` &&
`document.getElementById('b-export').onclick=function(){exportMode?stopExport():startExport();};` &&
`document.getElementById('b-probes').onclick=saveProbes;` &&
`document.getElementById('b-trace').onclick=function(){traceMode?finishTrace():startTrace();};` &&
`document.getElementById('b-reload').onclick=reloadCurrentScene;` &&
`document.getElementById('b-profile').onclick=showProfile;` &&
`document.getElementById('b-interleave').onclick=function(){useSplit=!useSplit;` &&
`this.textContent=useSplit?'L:H 1:3':'SHARED';this.style.color=useSplit?'#0f0':'#888';};` &&
`document.getElementById('b-mode').onclick=function(){mode=mode==='dev'?'viewer':'dev';` &&
`if(ws&&ws.readyState===1)ws.send(JSON.stringify({cmd:'set_mode',mode:mode}));};` &&
`document.getElementById('timeline').onclick=function(e){var r=this.getBoundingClientRect();` &&
`var pct=(e.clientX-r.left)/r.width;var b=Math.floor(pct*TOTAL_BARS);seekToBar(b);if(!playing)setTimeout(reqFrame,50);};` &&
`document.onkeydown=function(e){if(e.target.tagName==='INPUT')return;` &&
`if(e.key===' '){e.preventDefault();playing?pause():play();}` &&
`if(e.key==='ArrowRight'){var st=e.ctrlKey?16:e.shiftKey?4:1;stepTick(st);}` &&
`if(e.key==='ArrowLeft'){var st=e.ctrlKey?16:e.shiftKey?4:1;stepTick(-st);}` &&
`if(e.key===']')stepTick(64);if(e.key==='[')stepTick(-64);` &&
`if(e.key==='}')stepScene(1);if(e.key==='{')stepScene(-1);` &&
`if(e.key==='d')document.getElementById('b-hud').click();` &&
`if(e.key==='Home'){seekToBar(0);if(!playing)setTimeout(reqFrame,50);}` &&
`if(e.key==='End'){seekToBar(TOTAL_BARS-1);if(!playing)setTimeout(reqFrame,50);}` &&
`if(e.key==='PageDown'){e.preventDefault();stepScene(1);}if(e.key==='PageUp'){e.preventDefault();stepScene(-1);}};` &&
`tx.fillStyle='#0f0';tx.font='20px monospace';tx.textAlign='center';tx.fillText('O4D DEV PLAYER',320,200);` &&
`function updateInfoLine(){var txt=BPM+' BPM | '+Math.round(FPS)+' FPS ('+sI.toFixed(1)+'ms)';` &&
`if(au.duration){var dur=au.duration,bars=dur/BAR_SEC;txt+=' | Audio: '+dur.toFixed(1)+'s / '+bars.toFixed(1)+' bars';}` &&
`document.getElementById('info').textContent=txt;}` &&
`au.onloadedmetadata=updateInfoLine;cn();` &&
`</script></body></html>`.
  ENDMETHOD.

  METHOD get_html.
    " HTML + CSS
    rv_html =
`<!DOCTYPE html><html><head><meta charset="UTF-8"><title>O4D ABAP DEMO - WebGL</title>` &&
`<style>*{margin:0;padding:0}body{background:#000;display:flex;flex-direction:column;` &&
`align-items:center;justify-content:center;min-height:100vh;font-family:monospace;color:#0f0}` &&
`h1{text-shadow:0 0 10px #0f0;margin-bottom:10px}#container{position:relative;width:644px;height:404px}` &&
`#glc{border:2px solid #0f0;cursor:pointer}` &&
`#txc{position:absolute;top:2px;left:2px;pointer-events:none}` &&
`.controls{margin-top:15px;display:flex;gap:10px;align-items:center}` &&
`button{background:#0f0;color:#000;border:none;padding:8px 16px;font-family:monospace;cursor:pointer}` &&
`button:hover{background:#0c0}button:disabled{background:#333;color:#666}` &&
`#info{margin-top:10px;font-size:12px;color:#0a0}` &&
`#progress{width:300px;height:6px;background:#222;margin-top:10px}` &&
`#pbar{height:100%;background:#0f0;width:0%}input[type=range]{width:80px}` &&
`</style></head><body>` &&
`<h1>O4D - OLDSCHOOL 4EVER DEMO</h1>` &&
`<p style="color:#888;margin-bottom:15px">ABAP + WebGL // 152 BPM</p>` &&
`<div id="container">` &&
`<canvas id="glc" width="640" height="400"></canvas>` &&
`<canvas id="txc" width="640" height="400"></canvas>` &&
`</div>` &&
`<div id="progress"><div id="pbar"></div></div>` &&
`<div class="controls">` &&
`<button id="run">RUN</button><button id="brk" disabled>BREAK</button>` &&
`<button id="rst">RESET</button>` &&
`<span style="color:#888">VOL:</span><input type="range" id="vol" min="0" max="100" value="50">` &&
`<a href="?player=dev" style="color:#f0f;margin-left:20px">DEV MODE</a>` &&
`</div><div id="info">WebGL Renderer - Click RUN</div>` &&
`<audio id="au" preload="auto"><source src="?audio=ZOISEE-EAR-02.MP3" type="audio/mpeg"></audio>`.

    " WebGL Shaders
    rv_html = rv_html &&
`<script id="vs" type="x-shader/x-vertex">` &&
`attribute vec2 aPos;attribute vec3 aCol;varying vec3 vCol;` &&
`void main(){gl_Position=vec4(aPos.x/320.0-1.0,1.0-aPos.y/200.0,0.0,1.0);vCol=aCol;}` &&
`</script>` &&
`<script id="fs" type="x-shader/x-fragment">` &&
`precision mediump float;varying vec3 vCol;` &&
`void main(){gl_FragColor=vec4(vCol,1.0);}` &&
`</script>`.

    " JavaScript - WebGL init
    rv_html = rv_html &&
`<script>` &&
`var glc=document.getElementById('glc'),txc=document.getElementById('txc'),` &&
`gl=glc.getContext('webgl'),tx=txc.getContext('2d'),` &&
`au=document.getElementById('au'),info=document.getElementById('info'),pbar=document.getElementById('pbar'),` &&
`run=document.getElementById('run'),brk=document.getElementById('brk'),rst=document.getElementById('rst'),` &&
`ws,playing=0,fc=0,lt=Date.now(),fps=0,spb=60/152,prog,triB,lineB;` &&
`function compileShader(t,s){var sh=gl.createShader(t);gl.shaderSource(sh,s);gl.compileShader(sh);return sh;}` &&
`function initGL(){` &&
`var vs=compileShader(gl.VERTEX_SHADER,document.getElementById('vs').textContent);` &&
`var fs=compileShader(gl.FRAGMENT_SHADER,document.getElementById('fs').textContent);` &&
`prog=gl.createProgram();gl.attachShader(prog,vs);gl.attachShader(prog,fs);gl.linkProgram(prog);gl.useProgram(prog);` &&
`triB=gl.createBuffer();lineB=gl.createBuffer();` &&
`gl.enable(gl.BLEND);gl.blendFunc(gl.SRC_ALPHA,gl.ONE_MINUS_SRC_ALPHA);}` &&
`initGL();`.

    " Color parser + render function
    rv_html = rv_html &&
`function parseCol(c){if(!c)return[1,1,1];if(c[0]==='#'){var h=c.slice(1);if(h.length===3)h=h[0]+h[0]+h[1]+h[1]+h[2]+h[2];` &&
`return[parseInt(h.slice(0,2),16)/255,parseInt(h.slice(2,4),16)/255,parseInt(h.slice(4,6),16)/255];}` &&
`if(c.startsWith('hsl')){var m=c.match(/\d+/g);if(m){var h=parseInt(m[0])/360,s=parseInt(m[1])/100,l=parseInt(m[2])/100;` &&
`var r,g,b;if(s===0){r=g=b=l;}else{var q=l<0.5?l*(1+s):l+s-l*s,p=2*l-q;` &&
`r=hue2rgb(p,q,h+1/3);g=hue2rgb(p,q,h);b=hue2rgb(p,q,h-1/3);}return[r,g,b];}}` &&
`if(c.startsWith('rgb')){var m=c.match(/\d+/g);if(m)return[parseInt(m[0])/255,parseInt(m[1])/255,parseInt(m[2])/255];}` &&
`return[1,1,1];}` &&
`function hue2rgb(p,q,t){if(t<0)t+=1;if(t>1)t-=1;if(t<1/6)return p+(q-p)*6*t;if(t<1/2)return q;if(t<2/3)return p+(q-p)*(2/3-t)*6;return p;}`.

    " Main render function - triangles + rectangles (as 2 tris each)
    rv_html = rv_html &&
`function rf(d){gl.clearColor(0,0,0,1);gl.clear(gl.COLOR_BUFFER_BIT);tx.clearRect(0,0,640,400);var aPos=gl.getAttribLocation(prog,'aPos');` &&
`var aCol=gl.getAttribLocation(prog,'aCol');var v=[];` &&
`if(d.tri&&d.tri.length){for(var i=0;i<d.tri.length;i++){var t=d.tri[i],c=parseCol(t.f);` &&
`v.push(t.x1,t.y1,c[0],c[1],c[2],t.x2,t.y2,c[0],c[1],c[2],t.x3,t.y3,c[0],c[1],c[2]);}}` &&
`if(d.r&&d.r.length){for(var i=0;i<d.r.length;i++){var r=d.r[i],c=parseCol(r.f);` &&
`var x=r.x,y=r.y,w=r.w,h=r.h;` &&
`v.push(x,y,c[0],c[1],c[2],x+w,y,c[0],c[1],c[2],x+w,y+h,c[0],c[1],c[2]);` &&
`v.push(x,y,c[0],c[1],c[2],x+w,y+h,c[0],c[1],c[2],x,y+h,c[0],c[1],c[2]);}}` &&
`if(v.length>0){gl.bindBuffer(gl.ARRAY_BUFFER,triB);gl.bufferData(gl.ARRAY_BUFFER,new Float32Array(v),gl.DYNAMIC_DRAW);` &&
`gl.enableVertexAttribArray(aPos);gl.enableVertexAttribArray(aCol);` &&
`gl.vertexAttribPointer(aPos,2,gl.FLOAT,false,20,0);gl.vertexAttribPointer(aCol,3,gl.FLOAT,false,20,8);` &&
`gl.drawArrays(gl.TRIANGLES,0,v.length/5);}`.

    " Lines rendering
    rv_html = rv_html &&
`if(d.l&&d.l.length){var lv=[];for(var i=0;i<d.l.length;i++){var l=d.l[i],c=parseCol(l.c);` &&
`lv.push(l.x1,l.y1,c[0],c[1],c[2],l.x2,l.y2,c[0],c[1],c[2]);}` &&
`gl.bindBuffer(gl.ARRAY_BUFFER,lineB);gl.bufferData(gl.ARRAY_BUFFER,new Float32Array(lv),gl.DYNAMIC_DRAW);` &&
`gl.enableVertexAttribArray(aPos);gl.enableVertexAttribArray(aCol);` &&
`gl.vertexAttribPointer(aPos,2,gl.FLOAT,false,20,0);gl.vertexAttribPointer(aCol,3,gl.FLOAT,false,20,8);` &&
`gl.drawArrays(gl.LINES,0,d.l.length*2);}`.

    " Gallery image + Text overlay (Canvas2D) + beat flash
    rv_html = rv_html &&
`if(d.img&&d.img.length){for(var k=0;k<d.img.length;k++){var imgDef=d.img[k];loadGalleryImg(imgDef.n);` &&
`var im=imgCache[imgDef.n];if(im&&im.width){tx.drawImage(im,imgDef.x,imgDef.y,imgDef.w||im.width,imgDef.h||im.height);` &&
`for(var sy=0;sy<imgDef.h;sy+=4){tx.strokeStyle='rgba(0,255,255,'+(0.08+Math.sin(sy/10+d.t*3)*0.04)+')';` &&
`tx.beginPath();tx.moveTo(imgDef.x,imgDef.y+sy);tx.lineTo(imgDef.x+imgDef.w,imgDef.y+sy);tx.stroke();}}}}` &&
`if(d.tx){for(var j=0;j<d.tx.length;j++){var t=d.tx[j];tx.fillStyle=t.c;tx.font=(t.s||16)+'px monospace';` &&
`tx.textAlign=t.align||'center';tx.fillText(t.t,t.x,t.y);}}` &&
`if(d.flash){var fl=d.flash,r=Math.round((fl.color?fl.color[0]:1)*255),g=Math.round((fl.color?fl.color[1]:1)*255),b=Math.round((fl.color?fl.color[2]:1)*255);` &&
`tx.fillStyle='rgba('+r+','+g+','+b+','+fl.intensity+')';tx.fillRect(0,0,640,400);}` &&
`var bt=(au.currentTime%spb)/spb;if(bt<0.1){tx.fillStyle='rgba(255,255,255,'+((0.1-bt)*0.3)+')';tx.fillRect(0,0,640,400);}` &&
`fc++;var n=Date.now();if(n-lt>=1000){fps=fc;fc=0;lt=n;}` &&
`pbar.style.width=(au.duration?au.currentTime/au.duration*100:0)+'%';` &&
`var eff=d.e||'?',lc=d.lc||0,tc=d.tc||0,tric=d.tric||0,rc=d.rc||0;` &&
`info.textContent='['+eff.toUpperCase()+'] Bar:'+(d.gb||0)+' L:'+lc+' Tri:'+tric+' Rect:'+rc+' FPS:'+fps+' [WebGL]';}`.

    " WebSocket + controls
    rv_html = rv_html &&
`function gu(){return(location.protocol==='https:'?'wss:':'ws:')+'//'+location.host+'/sap/bc/apc/sap/zo4d_demo';}` &&
`function cn(){info.textContent='CONNECTING...';ws=new WebSocket(gu());` &&
`ws.onopen=function(){info.textContent='CONNECTED [WebGL] - Press RUN';run.disabled=false;};` &&
`ws.onclose=function(){info.textContent='DISCONNECTED';run.disabled=true;brk.disabled=true;stop();};` &&
`ws.onerror=function(){info.textContent='CONNECTION ERROR';};` &&
`ws.onmessage=function(e){try{var d=JSON.parse(e.data);if(!d.type)rf(d);}catch(x){}};}` &&
`var FPT=2,sI=24.6,cT=0,cS=0,lT=0;function rq(){if(!playing)return;var n=performance.now();` &&
`if(n-lT>=sI){lT=n;if(ws&&ws.readyState===1)ws.send(JSON.stringify({cmd:'frame',tick:cT,sub:cS}));` &&
`cS++;if(cS>=FPT){cS=0;cT++;}}requestAnimationFrame(rq);}` &&
`function start(){if(!ws||ws.readyState!==1)return;cT=0;cS=0;lT=0;au.play();ws.send('start');playing=1;run.disabled=true;brk.disabled=false;rq();}` &&
`function stop(){if(ws&&ws.readyState===1)ws.send('stop');au.pause();playing=0;run.disabled=false;brk.disabled=true;}` &&
`function reset(){if(ws&&ws.readyState===1)ws.send('reset');au.currentTime=0;au.pause();playing=0;run.disabled=false;brk.disabled=true;` &&
`gl.clearColor(0,0,0,1);gl.clear(gl.COLOR_BUFFER_BIT);tx.clearRect(0,0,640,400);` &&
`tx.fillStyle='#0f0';tx.font='24px monospace';tx.textAlign='center';tx.fillText('O4D WebGL',320,200);` &&
`info.textContent='RESET - Press RUN';}` &&
`run.onclick=start;brk.onclick=stop;rst.onclick=reset;` &&
`document.getElementById('vol').oninput=function(e){au.volume=e.target.value/100;};au.volume=0.5;` &&
`document.onkeydown=function(e){if(e.key===' '){e.preventDefault();playing?stop():start();}if(e.key==='r')reset();` &&
`if(e.key==='f'){glc.requestFullscreen&&glc.requestFullscreen();}};` &&
`glc.onclick=function(){glc.requestFullscreen&&glc.requestFullscreen();};` &&
`tx.fillStyle='#0f0';tx.font='24px monospace';tx.textAlign='center';tx.fillText('O4D WebGL',320,200);cn();` &&
`</script></body></html>`.
  ENDMETHOD.

ENDCLASS.
