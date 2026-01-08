DEF INPUT PARAM p-ind-event      AS CHAR          NO-UNDO.
DEF INPUT PARAM p-ind-object     AS CHAR          NO-UNDO.
DEF INPUT PARAM p-wgh-object     AS HANDLE        NO-UNDO.
DEF INPUT PARAM p-wgh-frame      AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-cod-table      AS CHAR          NO-UNDO.
DEF INPUT PARAM p-recid-table    AS RECID         NO-UNDO.

DEF NEW GLOBAL SHARED VARIABLE wh-bt-datamedia       AS WIDGET-HANDLE.

DEF VAR h-objeto   AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-browse   AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-calc-col AS WIDGET-HANDLE.
DEF VAR h-query    AS HANDLE.
DEF VAR h-col      AS HANDLE.
DEF VAR i-col      AS INTEGER.

IF p-ind-event = "DISPLAY" THEN DO:
   CREATE BUTTON wh-bt-datamedia
   ASSIGN FRAME     = p-wgh-frame 
           WIDTH     = 4.50
           HEIGHT    = 1.22
           ROW       = 7.3
           COL       = 83.2
           VISIBLE   = YES
           SENSITIVE = YES
           TOOLTIP   = "Data M‚dia"
           TRIGGERS:
               ON CHOOSE PERSISTENT RUN esepc/epc-acr726zk-a.p (INPUT p-recid-table).
           END TRIGGERS.

    wh-bt-datamedia:LOAD-IMAGE("image/im-orcto.gif").

    ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
    ASSIGN h-objeto = h-objeto:FIRST-CHILD.
    DO WHILE VALID-HANDLE(h-objeto):
       IF h-objeto:NAME = "br_cheq_acr_autom_agrup" THEN DO.
          h-calc-col = h-objeto:ADD-CALC-COLUMN("date", "99/99/9999", "", "Dt Prev Apres", 7).
          h-calc-col:LABEL-BGCOLOR = ?.
          ASSIGN h-browse = h-objeto
                 h-query = h-browse:QUERY.

         DO i-col = 1 TO h-browse:NUM-COLUMNS.
            ASSIGN h-col = h-browse:GET-BROWSE-COLUMN(i-col).

            IF h-col:NAME = "cod_indic_econ" THEN
               ASSIGN h-col:VISIBLE = NO.
         END.

       END.
       ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
    END.

    ON 'row-display':U OF h-browse PERSISTENT RUN esepc/epc-acr726zh-r1.p (INPUT h-query, INPUT h-calc-col). 
END.

