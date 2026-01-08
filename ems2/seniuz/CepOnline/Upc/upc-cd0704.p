/****************************************************************************
** Programa: upc-cd0704.p 
*****************************************************************************/
/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Global Variable Definitions **********************************************/

/* Variaveis para o CEPONLINE */
DEFINE NEW GLOBAL SHARED VAR h-cep          AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-endereco     AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-bairro       AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-cidade       AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-estado       AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-pais         AS HANDLE.

DEFINE NEW GLOBAL SHARED VAR h-cep-cob      AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-endereco-cob AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-bairro-cob   AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-cidade-cob   AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-estado-cob   AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-pais-cob     AS HANDLE.

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

IF p-ind-event = "BEFORE-INITIALIZE" AND
   c-objeto = "'advwr\v28ad098.w'" THEN DO: /* Localiza endere‡o-cob e chama procedimento */
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:TYPE <> "field-group" THEN DO:
         CASE h-objeto:NAME.
             WHEN "endereco" THEN DO.
                  ASSIGN h-endereco = h-objeto.
                  ON 'entry':U OF h-objeto PERSISTENT RUN CepOnline/upc/upc-cd0704e1.p.
             END.
             WHEN "bairro"   THEN ASSIGN h-bairro = h-objeto.
             WHEN "cidade"   THEN ASSIGN h-cidade = h-objeto.
             WHEN "estado"   THEN ASSIGN h-estado = h-objeto.
             WHEN "pais"     THEN ASSIGN h-pais = h-objeto.
             WHEN "cep" THEN DO.
                ASSIGN h-cep = h-objeto.
                ON 'leave':U OF h-objeto PERSISTENT RUN CepOnline/upc/upc-cd0704a.p (INPUT h-cep,
                                                                                     INPUT h-endereco,
                                                                                     INPUT h-bairro,
                                                                                     INPUT h-cidade,
                                                                                     INPUT h-estado,
                                                                                     INPUT h-pais).  
             END.
             WHEN "endereco-cob" THEN DO:
                ASSIGN h-endereco-cob = h-objeto.
                ON 'entry':U OF h-objeto PERSISTENT RUN CepOnline/upc/upc-cd0704e2.p.
             END.
             WHEN "bairro-cob"   THEN ASSIGN h-bairro-cob = h-objeto.
             WHEN "cidade-cob"   THEN ASSIGN h-cidade-cob = h-objeto.
             WHEN "estado-cob"   THEN ASSIGN h-estado-cob = h-objeto.
             WHEN "pais-cob"     THEN ASSIGN h-pais-cob = h-objeto.
             WHEN "cep-cob" THEN DO.
                ASSIGN h-cep-cob = h-objeto.
                ON 'leave':U OF h-objeto PERSISTENT RUN CepOnline/upc/upc-cd0704a.p (INPUT h-cep-cob,
                                                                                     INPUT h-endereco-cob,
                                                                                     INPUT h-bairro-cob,
                                                                                     INPUT h-cidade-cob,
                                                                                     INPUT h-estado-cob,
                                                                                     INPUT h-pais-cob).  
             END.
         END CASE.

         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
END.

