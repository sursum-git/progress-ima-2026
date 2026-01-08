DEF VAR de-tot-dev AS DEC.

OUTPUT TO spool/dev.txt.

FOR EACH docum-est WHERE
         docum-est.dt-trans >= 05.01.2010 NO-LOCK.

    IF docum-est.esp-docto <> 20 THEN NEXT.  /* s¢ devolu‡Æo */

    FOR EACH item-doc-est OF docum-est.
        FIND nota-fiscal WHERE
             nota-fiscal.cod-estabel = docum-est.cod-estabel AND
             nota-fiscal.serie = item-doc-est.serie-comp AND
             nota-fiscal.nr-nota-fis = item-doc-est.nro-comp
             NO-LOCK NO-ERROR.

        FIND it-nota-fisc OF nota-fiscal WHERE
             it-nota-fisc.nr-seq-fat = item-doc-est.seq-comp AND
             it-nota-fisc.it-codigo = item-doc-est.it-codigo
             NO-LOCK NO-ERROR.

        ASSIGN de-tot-dev = 0.
        FOR EACH dev-item-rom WHERE
                 dev-item-rom.nome-abrev = nota-fiscal.nome-ab-cli AND
                 dev-item-rom.nr-pedcli = it-nota-fisc.nr-pedcli AND
                 dev-item-rom.nr-sequencia = it-nota-fisc.nr-seq-ped AND
                 dev-item-rom.marca = "DEV" EXCLUSIVE-LOCK.

            ASSIGN de-tot-dev = de-tot-dev + dev-item-rom.quantidade.
        END.
        DISP item-doc-est.it-codigo
             item-doc-est.cod-refer
             item-doc-est.lote
             item-doc-est.quantidade
             de-tot-dev
             WITH WIDTH 550.
    END.
END.
