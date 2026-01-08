/* Programa: 
** Objetivo: 
**           
** Autor...: 
*/

/* Parameter Definitions ****************************************************/
define input parameter p-ind-event  as character.
define input parameter p-ind-object as character.
define input parameter p-wgh-object as handle.
define input parameter p-wgh-frame  as widget-handle.
define input parameter p-cod-table  as character.
define input parameter p-row-table  as rowid.

/* Global Variable Definitions **********************************************/

/* Variable Definitions *****************************************************/
DEF VAR h-objeto AS HANDLE.
DEF VAR c-objeto AS CHAR.

/* Variavies criadas na viewer dinamicamente*********************************/
ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

/* Main Block ***************************************************************/

IF p-ind-event = "INITIALIZE" AND
   c-objeto = "en0105h.w" THEN DO.
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto).
      IF h-objeto:NAME = "br-table" THEN DO.
         ON 'ANY-KEY':U OF h-objeto PERSISTENT RUN esupc/upc-en0105hk1.p.
         ON 'VALUE-CHANGED':U OF h-objeto PERSISTENT RUN esupc/upc-en0105hv1.p.
      END.
      ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.
END.
