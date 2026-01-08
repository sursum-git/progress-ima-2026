/* Programa: upc-re1001.p
** Objetivo: Criar bot∆o que executa um progama (upc-re1001c1.w), onde ser†
**           informado quais etiquetas est∆o sendo devolvidas.
**           Executar programa que valida se a metragem das etiquetas devolvidas
**           Ç igual Ö metragem informada na Nota Fiscal de devoluá∆o.
** Autor...: Prodb - Toninho  Abril/2006
*/

/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Global Variable Definitions **********************************************/
DEF NEW GLOBAL SHARED VAR wh-btn-etq AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-row-in090 AS ROWID.
DEF NEW GLOBAL SHARED VAR gr-row-in176 AS ROWID.

/* Variable Definitions *****************************************************/
DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.

/* Variavies criadas na viewer dinamicamente*********************************/

/* Inicializaá∆o */

/* Main Block ***************************************************************/

IF p-ind-event = "AFTER-DISPLAY" AND
   p-row-table <> ? THEN DO.

   FIND docum-est WHERE
        ROWID(docum-est) = p-row-table NO-LOCK NO-ERROR.

   ASSIGN wh-btn-etq:SENSITIVE = NO.
   IF docum-est.esp-docto = 20 THEN
      ASSIGN wh-btn-etq:SENSITIVE = YES.

   ASSIGN gr-row-in090 = p-row-table.
END.

IF p-ind-event = "AFTER-VALUE-CHANGED" AND
   p-row-table <> ? THEN 
   ASSIGN gr-row-in176 = p-row-table.

IF p-ind-event = "BEFORE-INITIALIZE" THEN DO:
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
       
      IF h-objeto:TYPE <> "field-group" THEN DO:
         IF h-objeto:NAME = "btConf" THEN 
            ON 'mouse-select-click':U OF h-objeto PERSISTENT RUN esupc/upc-re1001m1.p.

      IF h-objeto:NAME = "fpage1" THEN DO.
         CREATE BUTTON wh-btn-etq
                ASSIGN FRAME = h-objeto
                       WIDTH        = 10
                       ROW          = 8.29
                       COL          = 12
                       LABEL        = "Etiquetas" 
                       VISIBLE      = YES
                       SENSITIVE    = NO
                       TOOLTIP      = "Informa Etiquetas devolvidas do Item Selecionado..."
                       TRIGGERS:
                           ON "CHOOSE":U PERSISTENT RUN esupc/upc-re1001c2.p.
                      END TRIGGERS.
         END.
         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
END.
  
