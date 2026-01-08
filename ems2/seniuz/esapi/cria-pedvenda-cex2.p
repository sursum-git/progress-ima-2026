DEF INPUT PARAMETER p-nr-pedcli LIKE ped-venda.nr-pedcli.
DEF INPUT PARAMETER p-nome-abrev LIKE ped-venda.nome-abrev.

DEF VAR h-bodi159cex AS HANDLE.
DEF VAR r-rowid AS ROWID.

DEF TEMP-TABLE tt-ped-venda-cex LIKE ped-venda-cex
     FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE RowErrors NO-UNDO
       FIELD ErrorSequence    AS INTEGER
       FIELD ErrorNumber      AS INTEGER
       FIELD ErrorDescription AS CHARACTER
       FIELD ErrorParameters  AS CHARACTER
       FIELD ErrorType        AS CHARACTER
       FIELD ErrorHelp        AS CHARACTER
       FIELD ErrorSubType     AS CHARACTER.


DEFINE TEMP-TABLE tt-erros
    FIELD cod-erro  AS INTEGER
    FIELD desc-erro AS CHARACTER FORMAT "x(120)"
    FIELD desc-arq  AS CHARACTER.

DEF OUTPUT PARAMETER TABLE FOR tt-erros.

IF NOT VALID-HANDLE(h-bodi159cex) OR
   h-bodi159cex:TYPE      <> "PROCEDURE":U OR
   h-bodi159cex:FILE-NAME <> "dibo/bodi159cex.p":U THEN
   RUN dibo/bodi159cex.p PERSISTENT SET h-bodi159cex.

CREATE tt-ped-venda-cex.
ASSIGN tt-ped-venda-cex.nr-pedido = INT(p-nr-pedcli)
       tt-ped-venda-cex.nome-abrev = p-nome-abrev
       tt-ped-venda-cex.nr-pedcli = p-nr-pedcli.

RUN emptyRowErrors IN h-bodi159cex.
RUN inputExportation IN h-bodi159cex (INPUT TABLE tt-ped-venda-cex).
RUN SetDefaultExportation IN h-bodi159cex.
RUN OutputExportation IN h-bodi159cex (OUTPUT TABLE tt-ped-venda-cex).

RUN CreateExportation IN h-bodi159cex (INPUT TABLE tt-ped-venda-cex).

IF VALID-HANDLE(h-bodi159cex) THEN
   DELETE PROCEDURE h-bodi159cex.


