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

FIND ped-repre WHERE
     ped-repre.nr-pedido = p-nr-pedido AND
     ped-repre.nome-ab-rep = p-nome-ab-rep
     NO-LOCK NO-ERROR.

CREATE tt-ped-repre.
BUFFER-COPY ped-repre TO tt-ped-repre
     ASSIGN tt-ped-repre.perc-comis = p-perc-comis.

RUN setConstraintRowid IN h-bodi157 (INPUT ROWID(ped-repre)).
RUN openQueryStatic IN h-bodi157 (INPUT "rowid":U).        
RUN emptyRowErrors IN h-bodi157.
RUN setRecord IN h-bodi157 (INPUT TABLE tt-ped-repre).
RUN UpdateRecord IN h-bodi157.
RUN getRowErrors IN h-bodi157 (OUTPUT TABLE RowErrors).
    
IF CAN-FIND(FIRST RowErrors 
            WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
   FOR EACH rowerrors WHERE
            RowErrors.ErrorSubType = "ERROR":U:

       MESSAGE "Erro ao Modificar o Representes do Pedido" SKIP
                rowerrors.errornumber " - " 
                rowerrors.errordescription  SKIP
                "Controle CPD: esapi/altera-repres.p"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
   RETURN 'ADM-ERROR'.
END.

IF VALID-HANDLE(h-bodi157) THEN
   DELETE PROCEDURE h-bodi157.


