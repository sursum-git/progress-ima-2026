DEF NEW GLOBAL SHARED VAR wh-btn-confirma AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-btCompletOrder AS WIDGET-HANDLE NO-UNDO.

IF wh-btn-confirma:IMAGE = "image/im-uchk4.bmp" THEN
   APPLY 'MOUSE-SELECT-CLICK' TO h-btCompletOrder.
