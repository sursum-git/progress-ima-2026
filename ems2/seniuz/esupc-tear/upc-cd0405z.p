DEF VAR wh-pesquisa AS WIDGET-HANDLE.
DEF NEW GLOBAL SHARED VAR l-implanta AS LOGICAL INIT NO.
DEF NEW GLOBAL SHARED VAR wh-doca AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-window  AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR adm-broker-hdl AS HANDLE NO-UNDO.

{include/zoomvar.i &prog-zoom=eszoom/z01es087.w
                   &proghandle=wh-window
                   &campohandle=wh-doca
                   &campozoom=cod-localiz}
