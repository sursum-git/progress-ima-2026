DEF VAR de-tot-rom AS DEC.

OUTPUT TO p:\daniel\ax\nf-dif.txt.

FOR EACH nota-fiscal WHERE
         nota-fiscal.dt-emis >= 04.04.2013 NO-LOCK.

    FIND ped-venda WHERE 
         ped-venda.nome-abrev = nota-fiscal.nome-ab-cli AND
         ped-venda.nr-pedcli = nota-fiscal.nr-pedcli  NO-LOCK NO-ERROR.

    IF NOT AVAIL ped-venda THEN NEXT.

    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.

        FIND ped-item OF ped-venda WHERE
             ped-item.nr-sequencia = it-nota-fisc.nr-seq-ped NO-LOCK NO-ERROR.

        FIND ped-item-res OF ped-item NO-LOCK NO-ERROR.
        IF NOT AVAIL ped-item-res THEN NEXT.

        IF ped-item-res.qt-pedida = 0 THEN NEXT.

        ASSIGN de-tot-rom = 0.
        FOR EACH ped-item-rom WHERE
                 ped-item-rom.nome-abrev = ped-venda.nome-abrev AND
                 ped-item-rom.nr-pedcli = ped-item.nr-pedcli AND
                 ped-item-rom.nr-sequencia = ped-item.nr-sequencia NO-LOCK.

            FIND ob-etiqueta WHERE
                 ob-etiqueta.cod-estabel = ped-item-rom.cod-estabel AND
                 ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                 NO-LOCK NO-ERROR.

            IF AVAIL ob-etiqueta THEN
               ASSIGN de-tot-rom = de-tot-rom + ob-etiqueta.quantidade.
        END.

        IF de-tot-rom <> it-nota-fisc.qt-faturada[1] THEN
           DISP nota-fiscal.nr-nota-fis
                ped-venda.nr-pedcli
                ped-item.nr-sequencia
                it-nota-fisc.it-codigo
                it-nota-fisc.qt-faturada[1]
                ped-item.qt-pedida
                de-tot-rom
                ped-venda.no-ab-reppri
                ped-venda.dt-emissao
                WITH WIDTH 550.
    END.
END.
