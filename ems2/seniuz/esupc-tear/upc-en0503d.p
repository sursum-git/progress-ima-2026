/* Programa: upc-en0503d.p
** Objetivo: Zerar o as Unidades uma vez que o EMS sugere sempre 100,
**           Verificar se o tempo homem foi informado
** Autor...: Prodb - Toninho  Mar‡o/2004
*/

/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Global Variable Definitions **********************************************/

/* Variable Definitions *****************************************************/
DEF VAR h-objeto AS HANDLE.
DEF VAR c-objeto AS CHAR.

DEF NEW GLOBAL SHARED VAR h-op-codigo AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-tempo-homem AS HANDLE.

/* Main Block ***************************************************************/
ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

/*
MESSAGE p-ind-event
        c-objeto
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/    


IF p-ind-event = 'INITIALIZE' AND
   c-objeto = "v06in253.w" THEN DO.
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto).
      IF h-objeto:NAME = 'op-altern' THEN
         ASSIGN h-op-codigo = h-objeto.
      ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.
END.


IF p-ind-event = 'INITIALIZE' AND
   c-objeto = "v05in253.w" THEN DO.
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto).

      IF h-objeto:NAME = "v-unidades" THEN 
         ON 'entry':U OF h-op-codigo PERSISTENT RUN esupc/upc-en0507b-e1.p (INPUT h-objeto). 

      IF h-objeto:NAME = "tempo-homem" THEN 
         ASSIGN h-tempo-homem = h-objeto.

      ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.
END.

IF p-ind-event = 'BEFORE-ASSIGN' AND
   c-objeto = "v05in253.w" THEN DO.

   IF int(h-tempo-homem:SCREEN-VALUE) = 0 THEN DO.
      MESSAGE  "Tempo Homem deve ser Informado..."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO h-tempo-homem.
      RETURN 'NOK'.
   END.
END.

