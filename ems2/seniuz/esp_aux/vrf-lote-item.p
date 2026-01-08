FOR EACH nota-fiscal WHERE
         nota-fiscal.cod-estab = '2' AND
         nota-fiscal.serie = '3' AND
         nota-fiscal.dt-emis >= 07.01.2010.

    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.
        FIND ped-item WHERE
             ped-item.nr-pedcli = nota-fiscal.nr-pedcli AND
             ped-item.nome-abrev = nota-fiscal.nome-ab-cli AND
             ped-item.nr-sequencia = it-nota-fisc.nr-seq-ped
             NO-LOCK NO-ERROR.

        IF NOT AVAIL ped-item THEN NEXT.

        FIND ped-item-res OF ped-item NO-LOCK NO-ERROR.

        IF NOT AVAIL ped-item-res THEN NEXT.

        IF ped-item-res.lote BEGINS 'sc' THEN NEXT.

        FOR EACH fat-ser-lote OF it-nota-fisc NO-LOCK.
            IF fat-ser-lote.nr-serlote <> ped-item-res.lote THEN
               DISP it-nota-fisc.it-codigo
                    ped-item-res.lote.
        END.
    END.
END.


