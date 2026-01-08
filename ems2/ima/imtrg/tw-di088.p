
/****************************************************************************
** Programa: Twdi088 - Trigger de write na Tabela Item Nota Fiscal  
** Data    : Maio de 2024
** Objetivo: Trigger de Write para a tabela Item Nota Fiscal 
** Empresa: IMA 
** Autor: tadeu Silva
*****************************************************************************/
DEFINE PARAMETER BUFFER b-it-nota-fisc     FOR it-nota-fisc. /** atual **/
DEFINE PARAMETER BUFFER b-it-nota-fisc-old FOR it-nota-fisc. /** antes **/   
DEFINE VARIABLE hBoFats99 AS HANDLE      NO-UNDO.
{include/i-prgvrs.i tw-di088 2.04.00.001}

RUN esbo/boFats99.p PERSIST SET hBoFats99.
RUN iniciar         IN hBoFats99.
RUN setData         IN hBoFats99(b-it-nota-fisc.dt-emis-nota).
RUN setProgOrigem   IN hBoFats99('tw-di088').
RUN setTipoRegistro IN hBoFats99('faturamento').
RUN inserir         IN hBoFats99.
RUN finalizar       IN hBoFats99.


RETURN 'OK':u.
