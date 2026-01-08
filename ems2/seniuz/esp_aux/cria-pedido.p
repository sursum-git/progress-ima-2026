DEF TEMP-TABLE tt-ped-venda LIKE ped-venda
     FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-ped-item LIKE ped-item 
    FIELD r-rowid AS ROWID.

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

DEF VAR h-bodi154 AS HANDLE.
DEF VAR h-bodi157 AS HANDLE.
DEF VAR h-bodi159 AS HANDLE.
DEF VAR h-bodi159com AS HANDLE.

IF NOT VALID-HANDLE(h-bodi154) or
   h-bodi154:TYPE      <> "PROCEDURE":U OR
   h-bodi154:FILE-NAME <> "dibo/bodi154.p":U THEN
   RUN dibo/bodi154.p PERSISTENT SET h-bodi154.

IF NOT VALID-HANDLE(h-bodi157) or
   h-bodi157:TYPE      <> "PROCEDURE":U OR
   h-bodi157:FILE-NAME <> "dibo/bodi157.p":U THEN
   RUN dibo/bodi157.p PERSISTENT SET h-bodi157.

IF NOT VALID-HANDLE(h-bodi159com) OR
   h-bodi159com:TYPE      <> "PROCEDURE":U OR
   h-bodi159com:FILE-NAME <> "dibo/bodi159com.p":U THEN
   RUN dibo/bodi159com.p PERSISTENT SET h-bodi159com.

IF NOT VALID-HANDLE(h-bodi159) OR
   h-bodi159:TYPE      <> "PROCEDURE":U OR
   h-bodi159:FILE-NAME <> "dibo/bodi159.p":U THEN
   RUN dibo/bodi159.p PERSISTENT SET h-bodi159.

FIND emitente WHERE
     emitente.cod-emit = 8 NO-LOCK NO-ERROR.

FIND tab-finan WHERE
     tab-finan.dt-ini-val <= TODAY AND
     tab-finan.dt-fim-val >= TODAY NO-LOCK NO-ERROR.

FIND repres WHERE
     repres.cod-rep = emitente.cod-rep NO-LOCK NO-ERROR.

CREATE tt-ped-venda.
ASSIGN tt-ped-venda.nome-abrev = emitente.nome-abrev
       tt-ped-venda.nr-pedcli = STRING(NEXT-VALUE(seq-nr-pedido))
       tt-ped-venda.nr-pedido = int(tt-ped-venda.nr-pedcli)
       tt-ped-venda.cod-estabel = '2'
       tt-ped-venda.cod-portador = emitente.portador
       tt-ped-venda.modalidade = emitente.modalidade
       tt-ped-venda.nat-operacao = emitente.nat-operacao
       tt-ped-venda.cod-entrega = emitente.cod-entrega
       tt-ped-venda.nr-tab-finan = tab-finan.nr-tab-finan
       tt-ped-venda.nr-ind-finan = tab-finan.tab-ind-fin[1]
       tt-ped-venda.no-ab-rep = repres.nome-abrev.

CREATE tt-ped-repre.
ASSIGN tt-ped-repre.nr-pedido = tt-ped-venda.nr-pedido.
       tt-ped-repre.nome-ab-rep = tt-ped-venda.no-ab-rep.

CREATE tt-ped-item.
ASSIGN tt-ped-item.nome-abrev = tt-ped-venda.nome-abrev
       tt-ped-item.nr-pedcli = tt-ped-venda.nr-pedcli
       tt-ped-item.nr-sequencia = 10
       tt-ped-item.it-codigo = '502971'
       tt-ped-item.cod-refer = '0106190'
       tt-ped-item.qt-pedida = 100
       tt-ped-item.vl-liq-abe = 0.01
       tt-ped-item.vl-preori = 0.01
       tt-ped-item.nat-operacao = tt-ped-venda.nat-operacao
       tt-ped-item.cod-entrega = "Padr∆o"
       tt-ped-item.dt-entrega = TODAY.


/* Cria o Pedido */
RUN openQueryStatic IN h-bodi159(INPUT "Main":U).
RUN emptyRowErrors IN h-bodi159.
RUN setRecord IN h-bodi159(INPUT TABLE tt-ped-venda).
RUN createRecord IN h-bodi159.
RUN getRowErrors IN h-bodi159(OUTPUT TABLE RowErrors).
IF CAN-FIND(FIRST RowErrors 
           WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
   FOR EACH rowerrors WHERE
            RowErrors.ErrorSubType = "ERROR":U:
       MESSAGE "Erro ao Gravar no Pedido" SKIP
                rowerrors.errordescription 
               VIEW-AS ALERT-BOX.
  END.
END.


/* Cria Representantes do Pedido */
RUN openQueryStatic IN h-bodi157 (INPUT "Main":U).
RUN emptyRowErrors IN h-bodi157.
RUN setRecord IN h-bodi157 (INPUT TABLE tt-ped-repre).
RUN createRecord IN h-bodi157.
RUN getRowErrors IN h-bodi157 (OUTPUT TABLE RowErrors).
IF CAN-FIND(FIRST RowErrors 
          WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
   FOR EACH rowerrors WHERE
            RowErrors.ErrorSubType = "ERROR":U:
       MESSAGE "Erro ao Gravar no Pedido" SKIP
               rowerrors.errordescription 
               VIEW-AS ALERT-BOX.
   END.
END.


/* Cria os Itens do Pedido */
FOR EACH tt-ped-item.
    RUN openQueryStatic IN h-bodi154 (INPUT "Main":U).
    RUN emptyRowErrors IN h-bodi154.
    RUN setRecord IN h-bodi154 (INPUT TABLE tt-ped-item).
    RUN createRecord IN h-bodi154.
    RUN getRowErrors IN h-bodi154 (OUTPUT TABLE RowErrors).
    IF CAN-FIND(FIRST RowErrors 
               WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
       FOR EACH rowerrors WHERE
                RowErrors.ErrorSubType = "ERROR":U:
           MESSAGE "Erro ao Gravar os Itens do Pedido" SKIP
                    rowerrors.errordescription 
                   VIEW-AS ALERT-BOX.
      END.
    END.
END.


/*Completa o Pedido */
FIND ped-venda WHERE
     ped-venda.nome-abrev = tt-ped-venda.nome-abrev AND
     ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli NO-LOCK NO-ERROR.

RUN emptyRowErrors IN h-bodi159com.
RUN completeOrder IN h-bodi159com (INPUT ROWID(ped-venda),
                                   OUTPUT TABLE Rowerrors).
RUN getRowErrors IN h-bodi159com (OUTPUT TABLE RowErrors).

FOR EACH rowerrors WHERE
         RowErrors.ErrorSubType = "ERROR":U:
    MESSAGE "Erro ao Completar o Pedido" SKIP
            "Erro:" rowerrors.errornumber " - "
            rowerrors.errordescription
           VIEW-AS ALERT-BOX.
            
END.

MESSAGE tt-ped-venda.nr-pedcli
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

IF VALID-HANDLE(h-bodi154) THEN
   DELETE PROCEDURE h-bodi154.

IF VALID-HANDLE(h-bodi157) THEN
   DELETE PROCEDURE h-bodi157.

IF VALID-HANDLE(h-bodi159) THEN
   DELETE PROCEDURE h-bodi159.

IF VALID-HANDLE(h-bodi159com) THEN
   DELETE PROCEDURE h-bodi159com.

