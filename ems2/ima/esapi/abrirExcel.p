/*
IMPORTANTE: ‚ preciso que o arquivo excel seja um modelo
para evitar que o arquivo original seja aberto e fique travado
com o usuario corrente.
*/
{esp/util.i}


DEFINE INPUT  PARAMETER cExcel AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hParam              AS HANDLE      NO-UNDO.
DEFINE VARIABLE cExcelEncontrado    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDirERP             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMapeamento         AS CHARACTER   NO-UNDO.
RUN esbo/boConsParam.p PERSIST SET hParam.

RUN getDirERP IN hParam(OUTPUT cDirERP).
RUN getMapeamentoERP IN hParam(OUTPUT cMapeamento).


IF VALID-HANDLE(hParam) THEN DO:
   DELETE PROCEDURE hParam.      
END.
ASSIGN cExcelEncontrado = SEARCH(cExcel).
IF cExcelEncontrado <> ? THEN DO:
   ASSIGN cExcelEncontrado = REPLACE(cExcelEncontrado,cDirERP,cMapeamento + ':').
  /* MESSAGE cExcelEncontrado
       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
   OS-COMMAND SILENT VALUE('start ' + cExcelEncontrado) NO-ERROR. 
END.

ELSE 
    MESSAGE "Arquivo Excel" cExcel " NÆo Encontrado no Propath"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
