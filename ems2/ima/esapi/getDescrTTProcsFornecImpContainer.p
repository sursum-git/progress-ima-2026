/******************************************************************************
**  Programa: esapi/getDescrTTProcsFornecImpContainer.p
**  Objetivo: Retornar uma string com os codigos de processo de fornecedor
    referentes a determinado container, separados por virgula.
**  Data: 08/2025
**  Autor:Tadeu silva
******************************************************************************/
{esapi/getTTProcsFornecImpContainer.i}
{esp/util.i}
DEFINE INPUT  PARAMETER pNrContainer AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER cprocs AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cParte AS CHARACTER   NO-UNDO.
RUN esapi/getTTProcsFornecImpContainer.p(pNrContainer,OUTPUT TABLE ttProc).

FOR EACH ttProc:
    ASSIGN cParte = ENTRY(1,ttProc.procFornec,"(").
    RUN incrValor(INPUT-OUTPUT cProcs, cParte,",").
END.




