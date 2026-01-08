/* Programa: epc-re1001.p
** Objetivo: Criar nota-fiscal de Venda para Dep¢sito Fechado
** Autor...: PGS - Toninho  Mar‡o/2022
*/

/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Global Variable Definitions **********************************************/
DEF NEW GLOBAL SHARED VAR gr-row-in090 AS ROWID.

/* Variable Definitions *****************************************************/
DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.

/* Main Block ***************************************************************/

IF p-ind-event = "AFTER-DISPLAY" AND
   p-row-table <> ? THEN DO.

   FIND docum-est WHERE
        ROWID(docum-est) = p-row-table NO-LOCK NO-ERROR.

   ASSIGN gr-row-in090 = p-row-table.
END.

IF p-ind-event = "BEFORE-INITIALIZE" THEN DO:
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
       
      IF h-objeto:TYPE <> "field-group" THEN DO:
         IF h-objeto:NAME = "btConf" THEN 
            ON 'mouse-select-click':U OF h-objeto PERSISTENT RUN esepc/epc-re1001m1.p.

         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
END.
  
