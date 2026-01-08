/******************************************************************************
Programa: esbo/getDadosFaturPorData.p
objetivo: Buscar os dados de faturamento da BO esbo_fatur por faixa de data.
autor: Tadeu Silva
data: 06/2024
*****************************************************************************/
{esbo/esbo_fatur.i}
DEFINE INPUT PARAMETER  dtInicial           AS DATE        NO-UNDO.
DEFINE INPUT PARAMETER  dtFinal             AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER logRetirarAcomp     AS LOGICAL     NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt-fatur .


DEFINE VARIABLE HBoFatur            AS HANDLE      NO-UNDO.

{esp/util.i}
{utp/ut-glob.i}
//{esapi/extrairDadosRomaneioItem.i} 
{esp/paramsLog.i}

RUN esbo/esbo_fatur   PERSIST SET hBoFatur.
RUN iniciarBOs              IN  hBoFatur.
RUN retirarAcomp            IN hBoFatur(logRetirarAComp).
RUN limparTtFatur           IN hBoFatur.
RUN iniciarBos              IN hBoFatur.
RUN setInterValDtEmisNota   IN hBoFatur(dtInicial,DtFinal).
RUN buscarFaturados         IN hBoFatur.
RUN buscarDevolucao         IN hBoFatur.
RUN buscarMetasRepres       IN hBoFatur.
RUN retornarTtFatur         IN hBoFatur(OUTPUT  TABLE tt-Fatur).
IF getLogPrograma(program-name(1)) THEN
   RUN exportarTtFatur     IN hBoFatur('c:\temp\fatur.txt').

IF VALID-HANDLE(hBoFatur) THEN DO:
   RUN finalizarBos IN hBoFatur.
   DELETE PROCEDURE hBoFatur.
END.





