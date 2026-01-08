DEF TEMP-TABLE tt-ped-item NO-UNDO LIKE ped-item
    FIELD r-rowid AS ROWID.

FIND ped-venda 114543.

FIND pre-fatur OF ped-venda.

FOR EACH it-pre-fat OF pre-fatur NO-LOCK.
    FIND ped-item OF it-pre-fat NO-LOCK.

    IF it-pre-fat.qt-alocada <> ped-item.qt-pedida THEN DO.
       EMPTY TEMP-TABLE tt-ped-item.
       CREATE tt-ped-item.
       BUFFER-COPY ped-item TO tt-ped-item
                   ASSIGN tt-ped-item.qt-pedida = it-pre-fat.qt-alocada.

       RUN esapi/altera-peditem-v2.p (INPUT TABLE tt-ped-item).
    END.
END.

