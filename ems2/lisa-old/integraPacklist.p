DEFINE INPUT  PARAMETER pRowid AS ROWID       NO-UNDO.
DEFINE OUTPUT PARAMETER cErro  AS CHARACTER   NO-UNDO.

DEFINE VARIABLE h-acomp   AS HANDLE      NO-UNDO.
{utp/ut-glob.i}
RUN utp/ut-acomp PERSIST SET h-acomp.
{utp/ut-liter.i Integra‡Æo Packing-List *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).


FIND lisa-integra NO-LOCK
    WHERE ROWID(lisa-integra) = pRowid NO-ERROR.


RUN pi-acompanhar IN h-acomp (INPUT "Enviando Packing-List: " + lisa-integra.chave ).
FIND pp-container WHERE
    pp-container.nr-container = INTEGER(lisa-integra.chave)
    NO-LOCK NO-ERROR.

FIND nota-fiscal WHERE
    nota-fiscal.cod-estabel = pp-container.cod-estabel AND
    nota-fiscal.nro-proc-entrada = pp-container.nr-container AND
    nota-fiscal.dt-cancela = ? NO-LOCK NO-ERROR.
IF NOT AVAIL nota-fiscal THEN NEXT.

RUN esapi/envia-pckl-lisa.p (INPUT ROWID(nota-fiscal)).
IF RETURN-VALUE <> 'ADM-OK' THEN DO:
   ASSIGN cErro = "Erro no envio do Packlist".
   RUN pi-finalizar IN h-acomp.
   RETURN 'adm-error'.



END.
RUN pi-acompanhar IN h-acomp (INPUT "Mudando Etiquetas para Estoque. Container: " + lisa-integra.chave ).
RUN mudarEtqParaEstoque(INTEGER(lisa-integra.chave)).

FIND CURRENT lisa-integra EXCLUSIVE-LOCK NO-ERROR.
ASSIGN lisa-integra.acao         = ''
      lisa-integra.ind-situacao = 2 .
RELEASE lisa-integra.



PROCEDURE mudarEtqParaEstoque:

    DEFINE INPUT  PARAMETER pNrContainer AS INTEGER     NO-UNDO.

    FOR EACH ob-etiqueta EXCLUSIVE-LOCK
        WHERE ob-etiqueta.situacao  = 2
        AND   ob-etiqueta.nr-container = pNrContainer.
        ASSIGN ob-etiqueta.situacao = 3 .
    END.


END PROCEDURE.
