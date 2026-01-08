OUTPUT TO PRINTER.

FOR EACH nota-fiscal WHERE
         nota-fiscal.dt-emis >= 12.01.2010 NO-LOCK.

    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK,
        FIRST fat-ser-lote OF it-nota-fisc NO-LOCK.

        FIND ped-item WHERE
             ped-item.nr-pedcli = nota-fiscal.nr-pedcli AND
             ped-item.nome-abrev = nota-fiscal.nome-ab-cli AND
             ped-item.nr-sequencia = it-nota-fisc.nr-seq-ped
             NO-LOCK NO-ERROR.

        IF NOT AVAIL ped-item THEN NEXT.

        FIND ped-item-ext OF ped-item NO-LOCK.

        IF ped-item-ext.lote BEGINS 'sc' THEN NEXT.

        IF fat-ser-lote.nr-serlote = '' THEN NEXT. 

        IF fat-ser-lote.nr-serlote <> ped-item-ext.lote THEN
           DISP nota-fiscal.cod-estab
                nota-fiscal.nr-nota-fis
                ped-item-ext.lote  COLUMN-LABEL "Lote PED"
                fat-ser-lote.nr-serlote COLUMN-LABEL "Lote NF" 
                it-nota-fisc.qt-faturada[1].
    END.
END.
