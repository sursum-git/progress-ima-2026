DEF TEMP-TABLE tt-ped-venda LIKE ped-venda
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-ped-repre LIKE ped-repre
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-cond-ped LIKE cond-ped
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
DEF VAR h-bodi159sdf AS HANDLE.

DEF INPUT PARAMETER TABLE FOR tt-ped-venda.

FIND FIRST tt-ped-venda NO-ERROR.

IF NOT VALID-HANDLE(h-bodi159) OR
   h-bodi159:TYPE      <> "PROCEDURE":U OR
   h-bodi159:FILE-NAME <> "dibo/bodi159.p":U THEN
   RUN dibo/bodi159.p PERSISTENT SET h-bodi159.

IF NOT VALID-HANDLE(h-bodi159sdf) OR
   h-bodi159sdf:TYPE      <> "PROCEDURE":U OR
   h-bodi159sdf:FILE-NAME <> "dibo/bodi159.p":U THEN
   RUN dibo/bodi159sdf.p PERSISTENT SET h-bodi159sdf.

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
      FOR EACH rowerrors NO-LOCK:
          MESSAGE "Erro ao Gravar no Pedido" SKIP
                   rowerrors.errordescription SKIP
                   "Controle TI: (esapi/cria-pedvenda.p)"
                  VIEW-AS ALERT-BOX.
      END.
      RETURN 'ADM-ERROR'.
   END.

END.

FIND ped-venda WHERE
     ped-venda.nome-abrev = tt-ped-venda.nome-abrev AND
     ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli SHARE-LOCK NO-ERROR.

FIND repres WHERE
     repres.nome-abrev = ped-venda.no-ab-reppri NO-LOCK NO-ERROR.

FIND emitente WHERE
     emitente.nome-abrev = ped-venda.nome-abrev SHARE-LOCK NO-ERROR.

IF emitente.cod-rep = 1 THEN
   ASSIGN emitente.cod-rep = repres.cod-rep.

/* Avalia‡Æo de Cr‚dito Autom tico */
IF emitente.ind-cre-cli = 2 THEN
   ASSIGN ped-venda.cod-sit-aval = 3.

IF VALID-HANDLE(h-bodi159) THEN
   DELETE PROCEDURE h-bodi159.

IF VALID-HANDLE(h-bodi159sdf) THEN
   DELETE PROCEDURE h-bodi159sdf.
