DEF VAR de-tot-rd AS DEC.
DEF VAR de-tot-rp AS DEC.

OUTPUT TO m:\dif-cons.txt.
FOR EACH ordem-benefic WHERE
         ordem-benefic.dt-ob >= 01.01.2009 AND
         ordem-benefic.tipo-ordem = 3.

    ASSIGN de-tot-rp = 0
           de-tot-rd = 0.
    FOR EACH ob-etiqueta OF ordem-benefic NO-LOCK.
        IF ob-etiqueta.nr-lote BEGINS 'rp' THEN
           ASSIGN de-tot-rp = de-tot-rp + ob-etiqueta.quantidade.
        IF ob-etiqueta.nr-lote BEGINS 'rd' THEN
           ASSIGN de-tot-rd = de-tot-rd + ob-etiqueta.quantidade.
    END.

    DISP ordem-benefic.nr-ob
         ordem-benefic.dt-ob
         ordem-benefic.it-codigo
         ordem-benefic.cod-refer
         ordem-benefic.quantidade
         de-tot-rp
         de-tot-rd
         WITH WIDTH 550.


END.
