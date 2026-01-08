DEF TEMP-TABLE tt-ped-item NO-UNDO LIKE ped-item
    FIELD r-rowid AS ROWID.

DEF VAR c-desc-log AS CHAR.

DEF INPUT PARAMETER p-nr-embarque LIKE pre-fatur.nr-embarque.

FIND pre-fatur WHERE
     pre-fatur.nr-embarque = p-nr-embarque NO-LOCK NO-ERROR.

FOR EACH it-pre-fat OF pre-fatur NO-LOCK.
    FIND ped-item OF it-pre-fat NO-LOCK.

    IF it-pre-fat.qt-alocada <> ped-item.qt-pedida THEN DO.
       EMPTY TEMP-TABLE tt-ped-item.
       CREATE tt-ped-item.
       BUFFER-COPY ped-item TO tt-ped-item
                   ASSIGN tt-ped-item.qt-pedida = it-pre-fat.qt-alocada.

       RUN esapi/altera-peditem-v2.p (INPUT TABLE tt-ped-item).

       ASSIGN c-desc-log = " Ajustada a Quantidade, Para: " + TRIM(STRING(it-pre-fat.qt-alocada,">>>,>>9.99")).

       RUN esapi/cria-log-pedvenda.p (INPUT ped-item.nr-pedcli,
                                      INPUT ped-item.nome-abrev,
                                      INPUT c-desc-log,
                                      INPUT YES).
    END.
END.

FIND ped-venda OF pre-fatur NO-LOCK NO-ERROR.
IF NOT ped-venda.completo THEN
   RUN esapi/completa-pedvenda.p (INPUT pre-fatur.nr-pedcli).


