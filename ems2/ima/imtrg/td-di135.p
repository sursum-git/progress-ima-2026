/****************************************************************************
** Programa: Td-di135 - Trigger de delete  da Tabela Nota Fiscal  
** Data    : 05/2024
** Objetivo: Trigger de delete para a tabela Nota Fiscal 
** Empresa: IMA E INTERMALHAS 
** Vers∆o:  2.04.001
*****************************************************************************/
DEFINE PARAMETER BUFFER b-nota-fiscal FOR nota-fiscal. /** atual **/

{include/i-prgvrs.i td-di135 2.06.00.001}
 
DEFINE VARIABLE hBoFats99 AS HANDLE      NO-UNDO.

//tsp01
RUN esbo/boFats99.p PERSIST SET hBoFats99.
RUN iniciar         IN hBoFats99.
RUN setData         IN hBoFats99(b-nota-fiscal.dt-emis-nota).
RUN setProgOrigem   IN hBoFats99('td-di135').
RUN setTipoRegistro IN hBoFats99('faturamento').
RUN inserir         IN hBoFats99.
RUN finalizar       IN hBoFats99.





RETURN 'OK'.
