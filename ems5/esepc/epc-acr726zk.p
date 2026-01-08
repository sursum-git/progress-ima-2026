DEF INPUT PARAM p-ind-event      AS CHAR          NO-UNDO.
DEF INPUT PARAM p-ind-object     AS CHAR          NO-UNDO.
DEF INPUT PARAM p-wgh-object     AS HANDLE        NO-UNDO.
DEF INPUT PARAM p-wgh-frame      AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-cod-table      AS CHAR          NO-UNDO.
DEF INPUT PARAM p-recid-table    AS RECID         NO-UNDO.

DEF NEW GLOBAL SHARED VARIABLE wh-bt-datamedia    AS WIDGET-HANDLE.

DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.

OUTPUT TO p:\ponto-acr726sk.txt.
   PUT p-ind-event SKIP.
OUTPUT CLOSE.

IF p-ind-event = "DISPLAY" THEN DO:
   CREATE BUTTON wh-bt-datamedia
   ASSIGN FRAME     = p-wgh-frame 
           WIDTH     = 3
           HEIGHT    = 1
           ROW       = 14.5
           COL       = 85.5
           VISIBLE   = YES
           SENSITIVE = YES
           TOOLTIP   = "Data M‚dia"
           TRIGGERS:
               ON CHOOSE PERSISTENT RUN esepc/epc-acr726zk-a.p (INPUT p-recid-table).
           END TRIGGERS.

    wh-bt-datamedia:LOAD-IMAGE("image/im-orcto.gif").
END.

IF p-ind-event = "AFTER-ENABLE" THEN DO:
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
    
   DO WHILE VALID-HANDLE(h-objeto):
       IF h-objeto:NAME = "bt_avdeb_img" THEN 
          ASSIGN h-objeto:SENSITIVE = YES.
    
       ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.
END.

