/* Programa: upc-cd0402.p
*/

/* Parameter Definitions ****************************************************/
define input parameter p-ind-event  as character.
define input parameter p-ind-object as character.
define input parameter p-wgh-object as handle.
define input parameter p-wgh-frame  as widget-handle.
define input parameter p-cod-table  as character.
define input parameter p-row-table  as rowid.

/* Global Variable Definitions **********************************************/

/* Variavies criadas na viewer dinamicamente*********************************/

/* Global Variable Definitions for CEPONLINE *******************************/
DEFINE NEW GLOBAL SHARED VAR h-cep          AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-endereco     AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-bairro       AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-cidade       AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-estado       AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-pais         AS HANDLE.

/* Variable Definitions *****************************************************/
DEF VAR c-objeto AS CHAR NO-UNDO.
DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

/* Main Block ***************************************************************/

IF p-ind-event = "BEFORE-INITIALIZE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v06ad268.w" THEN DO:
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
       IF h-objeto:TYPE <> "field-group" THEN DO:
          IF h-objeto:NAME = "endereco" THEN DO.
             ASSIGN h-endereco = h-objeto.
             ON 'entry':U OF h-objeto PERSISTENT RUN CepOnline/upc/upc-cd0401b.p.
          END.

          IF h-objeto:NAME = "bairro" THEN
             ASSIGN h-bairro = h-objeto.

          IF h-objeto:NAME = "cidade" THEN
             ASSIGN h-cidade = h-objeto.

          IF h-objeto:NAME = "estado" THEN
             ASSIGN h-estado = h-objeto.

          IF h-objeto:NAME = "pais" THEN
             ASSIGN h-pais = h-objeto.

          IF h-objeto:NAME = "cep" THEN DO.
             ASSIGN h-cep = h-objeto.
             ON 'leave':U OF h-objeto PERSISTENT RUN CepOnline/upc/upc-cd0401a.p.
          END.
          ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
       END.
       ELSE 
          ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
END.


