DEFINE VARIABLE cErro    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE pPagina  AS INTEGER     NO-UNDO.
{esapi/analisarJsonObject2.i}

DEFINE TEMP-TABLE tt LIKE ttJson.
ASSIGN pPagina = 0.
REPEAT:
    ASSIGN pPagina = pPagina + 1.
    RUN lisa/consultarPedVendaGeradoLisa.p('gerados_' + STRING(TIME) + "-" + STRING(pPagina),
                                        pPagina,
                                       OUTPUT cErro,
                                        OUTPUT TABLE ttJson).
   IF cErro <> '' THEN
      MESSAGE cErro
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

   //implementar condi‡Æo para sair do repeat a partir da tag hasNext



END.


