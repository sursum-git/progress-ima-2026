DEF VAR de-qtd AS DEC.
OUTPUT TO PRINTER.
FOR EACH ob-etiqueta WHERE
         ob-etiqueta.situacao = 4 AND
         ob-etiqueta.dt-emiss <= 06.24.2007.

    IF NOT ob-etiqueta.nr-lote BEGINS 'r' THEN NEXT.

    FIND inv-acab WHERE
         inv-acab.data-inven = 06.24.2007 AND
         inv-acab.num-etiqueta = ob-etiqueta.num-etiqueta
         NO-LOCK NO-ERROR.

    IF NOT AVAIL inv-acab THEN DO.
       FIND ped-item-rom WHERE
            ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta
            NO-LOCK NO-ERROR.
           
       FIND ped-item-res WHERE
            ped-item-res.nr-pedcli    = ped-item-rom.nr-pedcli  AND
            ped-item-res.nome-abrev   = ped-item-rom.nome-abrev AND
            ped-item-res.nr-sequencia = ped-item-rom.nr-sequencia
            NO-LOCK NO-ERROR.

       DISP ob-etiqueta.num-etiqueta
            ped-item-res.nr-pedcli
            ped-item-res.dt-trans 
            ob-etiqueta.quantidade.
    END.
END.

