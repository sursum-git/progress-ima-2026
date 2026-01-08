/* Programa: epc-cc0300.p
** Objetivo: Criar bot∆o que executa um progama (epc-cc0300c1.w), onde ser†
**           informado qual o codigo do Container, gerando as OC's com os Itens
**           do Container....
** Autor...: SeniuZ - Toninho  Abril/2013
*/

/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Global Variable Definitions **********************************************/
DEF NEW GLOBAL SHARED VAR bh-btn-container AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-row-in295 AS ROWID.

DEF NEW GLOBAL SHARED VAR h-btPrev AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-btNext AS WIDGET-HANDLE NO-UNDO.

/* Variable Definitions *****************************************************/

/* Variavies criadas na viewer dinamicamente*********************************/
DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-objeto2 AS WIDGET-HANDLE NO-UNDO.

/* Inicializaá∆o */
/* Main Block ***************************************************************/
IF p-ind-event = "BEFORE-INITIALIZE" THEN DO:
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.

   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:NAME = "btNext" THEN 
         ASSIGN h-btNext = h-objeto.

      IF h-objeto:NAME = "btPrev" THEN 
         ASSIGN h-btPrev = h-objeto.


      IF h-objeto:NAME = "fpage1" THEN DO.
         CREATE BUTTON bh-btn-container
                ASSIGN FRAME = h-objeto
                       WIDTH        = 28
                       ROW          = 6.7
                       COL          = 56
                       LABEL        = "Gerar OC's com Itens do Container" 
                       VISIBLE      = YES
                       SENSITIVE    = NO
                       TOOLTIP      = "Importa Itens do Container e Gera as Ordens de Compra..."
                       TRIGGERS:
                           ON "CHOOSE":U PERSISTENT RUN esepc/epc-cc0300c1.p.
                       END TRIGGERS.
         
      END.
      ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.
END.
  
IF p-ind-event = "AFTER-DISPLAY" AND
   p-row-table <> ? THEN DO.

   FIND pedido-compr WHERE
        ROWID(pedido-compr) = p-row-table NO-LOCK NO-ERROR.

   FIND processo-imp OF pedido-compr NO-LOCK NO-ERROR.
   ASSIGN bh-btn-container:SENSITIVE = AVAIL processo-imp.

   ASSIGN gr-row-in295 = p-row-table.
END.
