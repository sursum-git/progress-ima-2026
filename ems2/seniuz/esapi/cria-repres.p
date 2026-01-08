DEF INPUT PARAMETER p-nr-pedido LIKE ped-repre.nr-pedido.
DEF INPUT PARAMETER p-nome-ab-rep LIKE ped-repre.nome-ab-rep.
DEF INPUT PARAMETER p-perc-comis LIKE ped-repre.perc-comis.

DEF TEMP-TABLE tt-ped-repre LIKE ped-repre 
    FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE RowErrors NO-UNDO
       FIELD ErrorSequence    AS INTEGER
       FIELD ErrorNumber      AS INTEGER
       FIELD ErrorDescription AS CHARACTER
       FIELD ErrorParameters  AS CHARACTER
       FIELD ErrorType        AS CHARACTER
       FIELD ErrorHelp        AS CHARACTER
       FIELD ErrorSubType     AS CHARACTER.

DEF VAR h-bodi157    AS HANDLE.

IF NOT VALID-HANDLE(h-bodi157) or
   h-bodi157:TYPE      <> "PROCEDURE":U OR
   h-bodi157:FILE-NAME <> "dibo/bodi157.p":U THEN
   RUN dibo/bodi157.p PERSISTENT SET h-bodi157.
    
FIND ped-venda WHERE
     ped-venda.nr-pedcli = STRING(p-nr-pedido) NO-LOCK NO-ERROR.

FIND repres WHERE
     repres.nome-abrev = p-nome-ab-rep NO-LOCK NO-ERROR.

CREATE tt-ped-repre.
ASSIGN tt-ped-repre.nr-pedido = p-nr-pedido
       tt-ped-repre.nome-ab-rep = p-nome-ab-rep
       tt-ped-repre.perc-comis = p-perc-comis
       tt-ped-repre.comis-emis = repres.comis-emis
       tt-ped-repre.ind-repbase = YES.

RUN openQueryStatic IN h-bodi157 (INPUT "Main":U).
RUN emptyRowErrors IN h-bodi157.
RUN setRecord IN h-bodi157 (INPUT TABLE tt-ped-repre).
RUN createRecord IN h-bodi157.
RUN getRowErrors IN h-bodi157 (OUTPUT TABLE RowErrors).
IF CAN-FIND(FIRST RowErrors 
            WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
   FOR EACH rowerrors WHERE
            RowErrors.ErrorSubType = "ERROR":U:
       MESSAGE "Erro ao Criar Representantes do Pedido" SKIP
               rowerrors.errordescription 
               VIEW-AS ALERT-BOX.
   END.
   UNDO.
   RETURN 'ADM-ERROR'.
END.

IF VALID-HANDLE(h-bodi157) THEN
   DELETE PROCEDURE h-bodi157.







