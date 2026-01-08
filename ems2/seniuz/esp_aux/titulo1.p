DEF VAR de-saldo    LIKE titulo.vl-saldo.
DEF VAR de-vl-baixa LIKE titulo.vl-saldo.

FOR EACH titulo WHERE titulo.ep-codigo   = 1
                  AND titulo.cod-estabel = "2"
                  AND titulo.cod-esp     = "dp"
                  AND titulo.serie       = ""
                  AND titulo.nr-docto    = "10003"
                NO-LOCK:
    
    assign de-saldo    = titulo.vl-saldo
           de-vl-baixa = 0.
    for each mov-tit where mov-tit.ep-codigo   = titulo.ep-codigo
                       and mov-tit.cod-estabel = titulo.cod-estabel
                       and mov-tit.cod-esp     = titulo.cod-esp
                       AND mov-tit.serie       = titulo.serie
                       and mov-tit.nr-docto    = titulo.nr-docto
                       and mov-tit.parcela     = titulo.parcela
                     no-lock:
        DISP mov-tit.transacao VIEW-AS FILL-IN
             mov-tit.vl-baixa.

        find esp-doc where esp-doc.cod-esp = titulo.cod-esp
                     no-lock no-error.
        if AVAIL esp-doc AND esp-doc.tipo = 2 then next.

        if mov-tit.transacao = 14 then
           assign de-saldo = de-saldo - mov-tit.vl-original.
        if mov-tit.transacao = 2 then
           assign de-saldo    = de-saldo + mov-tit.vl-baixa
                  de-vl-baixa = de-vl-baixa + mov-tit.vl-baixa.
        if mov-tit.transacao = 3 then
           assign de-saldo    = de-saldo + mov-tit.vl-baixa
                  de-vl-baixa = de-vl-baixa + mov-tit.vl-baixa.
        if mov-tit.transacao = 13 and mov-tit.lancamento = 1 then
           assign de-saldo = de-saldo + mov-tit.vl-baixa
                  de-vl-baixa = de-vl-baixa + mov-tit.vl-baixa.
        if mov-tit.transacao = 13 and mov-tit.lancamento = 2 then
           assign de-saldo = de-saldo - mov-tit.vl-baixa
                  de-vl-baixa = de-vl-baixa - mov-tit.vl-baixa.
    end.
    
    DISP titulo.ep-codigo
         titulo.cod-estabel
         titulo.cod-esp
         titulo.serie
         titulo.nr-docto
         titulo.parcela
         titulo.vl-saldo
         de-saldo
         de-vl-baixa.
END.
