DEF TEMP-TABLE tt-ped-item NO-UNDO LIKE ped-item
     FIELD r-rowid AS ROWID.

DEF VAR c-nr-pedcli AS CHAR.

ASSIGN c-nr-pedcli = '212761'.

FIND ped-venda WHERE
     ped-venda.nr-pedcli = c-nr-pedcli NO-LOCK.

FOR EACH ped-item WHERE OF ped-venda.
    FIND ped-item-res OF ped-item.

    IF ped-item-res.qt-pedida = 0 THEN NEXT.

    IF ped-item-res.qt-pedida <> ped-item.qt-pedida THEN DO.
       EMPTY TEMP-TABLE tt-ped-item.
       CREATE tt-ped-item.
       BUFFER-COPY ped-item TO tt-ped-item
                   ASSIGN tt-ped-item.qt-pedida = ped-item-res.qt-pedida.

       RUN esapi/altera-peditem-v2.p (INPUT TABLE tt-ped-item).

       RUN esapi/completa-pedvenda.p (INPUT tt-ped-item.nr-pedcli).
    END.
END.

