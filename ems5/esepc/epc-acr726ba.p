DEF INPUT PARAM p-ind-event      AS CHAR          NO-UNDO.
DEF INPUT PARAM p-ind-object     AS CHAR          NO-UNDO.
DEF INPUT PARAM p-wgh-object     AS HANDLE        NO-UNDO.
DEF INPUT PARAM p-wgh-frame      AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-cod-table      AS CHAR          NO-UNDO.
DEF INPUT PARAM p-recid-table    AS RECID         NO-UNDO.

DEF NEW GLOBAL SHARED VARIABLE wh-bt-datamedia       AS WIDGET-HANDLE.
DEF NEW GLOBAL SHARED VARIABLE v-rec-lote            AS RECID.
DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.

DEF VAR h-cod-estab AS HANDLE.
DEF VAR h-cod-refer AS HANDLE.

IF p-ind-event = "DISPLAY" THEN DO:
   ASSIGN v-rec-lote = p-recid-table.

   IF v-rec-lote = ? THEN DO.
      ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
      ASSIGN h-objeto = h-objeto:FIRST-CHILD.
      DO WHILE VALID-HANDLE(h-objeto):
         IF h-objeto:NAME = 'cod_estab_refer' THEN
            ASSIGN h-cod-estab = h-objeto.
    
         IF h-objeto:NAME = 'cod_refer' THEN 
            ASSIGN h-cod-refer = h-objeto.
    
         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
    
      FIND lote_liquidac_acr WHERE
           lote_liquidac_acr.cod_estab_refer = h-cod-estab:SCREEN-VALUE AND
           lote_liquidac_acr.cod_refer = h-cod-refer:SCREEN-VALUE
           NO-LOCK NO-ERROR.
      IF AVAIL lote_liquidac_acr THEN
         ASSIGN v-rec-lote = RECID(lote_liquidac_acr).
   END.

   CREATE BUTTON wh-bt-datamedia
   ASSIGN FRAME     = p-wgh-frame 
           WIDTH     = 10
           HEIGHT    = 1
           ROW       = 13.4
           COL       = 74
           LABEL     = "Data M‚dia"
           VISIBLE   = YES
           SENSITIVE = YES
           TOOLTIP   = "Data M‚dia"
           TRIGGERS:
               ON CHOOSE PERSISTENT RUN esepc/epc-acr726zk-a.p (INPUT v-rec-lote).
           END TRIGGERS.
END.
