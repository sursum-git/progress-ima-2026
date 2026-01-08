DEF INPUT  PARAMETER pCodControlePpreco AS INTEGER.
DEF OUTPUT PARAMETER pVlReal            AS DEC.
DEF OUTPUT PARAMETER pVlDolar           AS DEC.
DEF OUTPUT PARAMETER pTipoPreco         AS CHAR.

DEF VAR h-boPrecosItemRef AS HANDLE NO-UNDO.
DEF VAR iTipoPreco        AS INT.

RUN esbo/boPrecosItemRef.p PERSISTENT SET h-boPrecosItemRef.

RUN iniciarBos      IN h-boPrecosItemRef.
RUN limparTTPreco   IN h-boPrecosItemRef.
RUN limparTTMsg     IN h-boPrecosItemRef.
RUN buscarPrecos    IN h-boPrecosItemRef.

RUN getPrecoPrazoPorId IN h-boPrecosItemRef (INPUT pCodControlePpreco,
                                             OUTPUT pVlReal,
                                             OUTPUT pvlDolar,
                                             OUTPUT iTipoPreco).

CASE iTipoPreco.
    WHEN 1 THEN ASSIGN pTipoPreco = 'PE'.
    WHEN 2 THEN ASSIGN pTipoPreco = 'PI'.
    WHEN 3 THEN ASSIGN pTipoPreco = 'OutLet'.
END CASE.

RUN finalizarBos IN h-boPrecosItemRef.
IF VALID-HANDLE(h-boPrecosItemRef) THEN
   DELETE PROCEDURE h-boPrecosItemRef.

