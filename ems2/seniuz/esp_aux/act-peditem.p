DEF TEMP-TABLE tt-ped-item LIKE ped-item 
    FIELD r-rowid AS ROWID.

FIND ped-venda WHERE
     ped-venda.nr-pedcli = '140679'.

FIND ped-item OF ped-venda WHERE
     ped-item.nr-sequencia = 20.

    FIND ped-item-res OF ped-item NO-ERROR.
    IF NOT AVAIL ped-item-res THEN NEXT.

    IF ped-item.qt-pedida <> ped-item-res.qt-pedida THEN DO.
       FOR EACH tt-ped-item.
           DELETE tt-ped-item.
       END.
       
       CREATE tt-ped-item.
       BUFFER-COPY ped-item TO tt-ped-item
           ASSIGN tt-ped-item.qt-pedida = ped-item-res.qt-pedida.

       RUN esapi/altera-peditem-v2.p (INPUT TABLE tt-ped-item).

    END.

