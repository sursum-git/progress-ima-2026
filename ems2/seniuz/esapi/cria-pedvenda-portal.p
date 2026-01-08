DEF TEMP-TABLE tt-ped-venda NO-UNDO LIKE ped-venda
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-ped-repre NO-UNDO LIKE ped-repre
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-cond-ped NO-UNDO LIKE cond-ped
    FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE RowErrors NO-UNDO
       FIELD ErrorSequence    AS INTEGER
       FIELD ErrorNumber      AS INTEGER
       FIELD ErrorDescription AS CHARACTER
       FIELD ErrorParameters  AS CHARACTER
       FIELD ErrorType        AS CHARACTER
       FIELD ErrorHelp        AS CHARACTER
       FIELD ErrorSubType     AS CHARACTER.

DEF VAR h-bodi159 AS HANDLE.

DEF INPUT  PARAMETER TABLE FOR tt-ped-venda.
DEF OUTPUT PARAMETER c-erro-pedvenda AS CHAR.

FIND FIRST tt-ped-venda NO-ERROR.

IF NOT VALID-HANDLE(h-bodi159) OR
   h-bodi159:TYPE      <> "PROCEDURE":U OR
   h-bodi159:FILE-NAME <> "dibo/bodi159.p":U THEN
   RUN dibo/bodi159.p PERSISTENT SET h-bodi159.

/* Cria o Pedido */
FIND ped-venda WHERE
     ped-venda.nome-abrev = tt-ped-venda.nome-abrev AND
     ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli NO-LOCK NO-ERROR.

IF NOT AVAIL ped-venda THEN DO.
   RUN openQueryStatic IN h-bodi159(INPUT "Main":U).
   RUN emptyRowErrors IN h-bodi159.
   RUN setRecord IN h-bodi159(INPUT TABLE tt-ped-venda).
   RUN createRecord IN h-bodi159.
   RUN getRowErrors IN h-bodi159(OUTPUT TABLE RowErrors).
   IF CAN-FIND(FIRST RowErrors 
               WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
      FOR EACH rowerrors WHERE
               RowErrors.ErrorSubType = "ERROR":U:
      /*    MESSAGE "Erro ao Gravar no Pedido" SKIP
                   rowerrors.errordescription 
                  VIEW-AS ALERT-BOX. */
          ASSIGN c-erro-pedvenda = "Pedido: " + tt-ped-venda.nr-pedcli + " COM O ERRO " + rowerrors.errordescription.
      END. 
      RETURN 'ADM-ERROR'.
   END.

END.

FIND ped-venda WHERE
     ped-venda.nome-abrev = tt-ped-venda.nome-abrev AND
     ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli SHARE-LOCK NO-ERROR.

FIND emitente WHERE
     emitente.nome-abrev = ped-venda.nome-abrev NO-LOCK NO-ERROR.

/* Avalia‡Æo de Cr‚dito Autom tico */
IF emitente.ind-cre-cli = 2 THEN
   ASSIGN ped-venda.cod-sit-aval = 3.

FIND CURRENT ped-venda NO-LOCK NO-ERROR.

IF VALID-HANDLE(h-bodi159) THEN
   DELETE PROCEDURE h-bodi159.

