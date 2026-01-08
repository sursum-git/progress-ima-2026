DEF VAR i-qtd-fardos AS INT.
DEF VAR de-peso      AS DEC.
FOR EACH mp-entr-mat WHERE mp-entr-mat.nro-docto = 2500 NO-LOCK.
    FIND mp-entr-cam WHERE mp-entr-cam.nr-cdr = mp-entr-mat.nr-cdr NO-LOCK NO-ERROR.
    FOR EACH mp-fardo WHERE
             mp-fardo.nr-cdr = mp-entr-mat.nr-cdr.
        ASSIGN i-qtd-fardos = i-qtd-fardos + 1
               de-peso      = de-peso + mp-fardo.peso.
    END.
       DISP mp-entr-mat.nro-docto
            mp-entr-mat.nr-cdr
            mp-entr-mat.cod-emit
            mp-entr-cam.placa
            mp-entr-cam.dt-entrada
            mp-entr-mat.dt-recebimento
            mp-entr-mat.padrao[1]
            mp-entr-mat.peso-nf
            de-peso
            mp-entr-mat.qtd-fardos[1]
            i-qtd-fardos LABEL "QTD Fardos"
            WITH WIDTH 500.
END.
