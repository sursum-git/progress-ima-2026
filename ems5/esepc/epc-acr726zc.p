DEF INPUT PARAM p-ind-event      AS CHAR          NO-UNDO.
DEF INPUT PARAM p-ind-object     AS CHAR          NO-UNDO.
DEF INPUT PARAM p-wgh-object     AS HANDLE        NO-UNDO.
DEF INPUT PARAM p-wgh-frame      AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-cod-table      AS CHAR          NO-UNDO.
DEF INPUT PARAM p-recid-table    AS RECID         NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE wh-narrativa AS WIDGET-HANDLE.
DEFINE NEW GLOBAL SHARED VARIABLE gc-narrativa AS CHARACTER.

IF p-ind-event = "DISPLAY" THEN DO:
   CREATE EDITOR wh-narrativa
   ASSIGN FRAME     = p-wgh-frame 
           WIDTH     = 35
           HEIGHT    = 3
           ROW       = 5.7
           COL       = 32
           SCROLLBAR-VERTICAL = YES
           VISIBLE   = YES
           SENSITIVE = YES.

   ASSIGN wh-narrativa:SCREEN-VALUE = gc-narrativa.
END.

IF p-ind-event = "ASSIGN" THEN 
   ASSIGN gc-narrativa = wh-narrativa:SCREEN-VALUE.
