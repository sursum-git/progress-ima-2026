/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

DEF NEW GLOBAL SHARED VAR h-bt-imprime AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-browse AS WIDGET-HANDLE NO-UNDO.

DEF VAR h-frame  AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.

/* Main Block ***************************************************************/

IF p-ind-event = 'BEFORE-INITIALIZE' THEN DO.
   CREATE BUTTON h-bt-imprime
          ASSIGN FRAME = p-wgh-frame
                 WIDTH        = 13
                 ROW          = 13.97
                 COL          = 60
                 LABEL        = "Imprimir / Email" 
                 VISIBLE      = YES
                 SENSITIVE    = NO
                 TOOLTIP      = "Imprime Carta de Corre‡Æo ESPECÖFICO "
                 TRIGGERS:
                     ON "CHOOSE":U PERSISTENT RUN esepc/epc-ft0909f2.p.
                 END TRIGGERS.

   ASSIGN h-frame = p-wgh-frame:FIRST-CHILD
          h-objeto = h-frame:FIRST-CHILD.
   DO WHILE h-objeto <> ?: /*Passando campo a campo da tela para efetuar altera‡äes*/
      IF h-objeto:TYPE <> "field-group" THEN DO:
         
         IF h-objeto:NAME = 'brCartaCorrecao' THEN
            ASSIGN h-browse = h-objeto.
         
         IF h-objeto:NAME = 'btAtualiza' THEN
            ON 'ENTRY' OF h-objeto PERSISTENT RUN esepc/epc-ft0909f1.p (INPUT p-wgh-frame).
           
         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
END.

