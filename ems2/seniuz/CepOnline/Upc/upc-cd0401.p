/****************************************************************************
** Programa: upc-cd0401.p 
** Objetivo: Preencher Automaticamente os Campos:
**           (ENDERECO, ENDERE€O2, BAIRRO, CIDADE, CEP, UF) do folder 
**           endere‡o, apos o cursor estiver no campo CEP, e for prescionado
**           a tecla F10.
**
*****************************************************************************/
/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Global Variable Definitions **********************************************/
DEFINE NEW GLOBAL SHARED VAR h-cep          AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-endereco     AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-bairro       AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-cidade       AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-estado       AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-pais         AS HANDLE.


/* Variable Definitions *****************************************************/
DEFINE VAR i-ct           AS INT.
DEFINE VAR c-folder       AS CHARACTER NO-UNDO.
DEFINE VAR c-objects      AS CHARACTER NO-UNDO.
DEFINE VAR h-object       AS HANDLE    NO-UNDO.
DEFINE VAR i-objects      AS INTEGER   NO-UNDO.
DEFINE VAR l-record       AS LOGICAL   NO-UNDO INITIAL NO.
DEFINE VAR l-group-assign AS LOGICAL   NO-UNDO INITIAL NO.

DEF VAR h-objeto          AS WIDGET-HANDLE NO-UNDO.
DEF VAR c-objeto          AS CHAR NO-UNDO.


/* Variavies criadas na viewer dinamicamente*********************************/
ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

/* Main Block ***************************************************************/

IF p-ind-event = "INITIALIZE" AND
   p-ind-object = 'VIEWER' AND
   c-objeto = "'advwr\v05ad098.w'" THEN DO.
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

         IF h-objeto:NAME = "cidade" THEN DO.
            ASSIGN h-cidade = h-objeto.
            ON 'leave':U OF h-objeto PERSISTENT RUN CepOnline/upc/upc-cd0401c.p.  
         END.

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
