DEF VAR i-prazo  AS INT.
DEF VAR de-valor AS DEC.

DEF TEMP-TABLE tt-work
    FIELD prazo  AS INT
    FIELD valor AS DEC FORMAT ">>>,>>>,>>9.99"
    INDEX ch-work prazo.
                          
FOR EACH nota-fiscal WHERE nota-fiscal.cod-estabel  = "2"
                       AND nota-fiscal.serie        = "1"
                       AND nota-fiscal.dt-emis-nota >= 01/01/2004
                       AND nota-fiscal.dt-emis-nota <= 12/31/2004
                       AND nota-fiscal.emite-dup     = YES
                     NO-LOCK:
    ASSIGN i-prazo  = 0
           de-valor = 0.
    for each fat-duplic 
        where fat-duplic.cod-estab = nota-fiscal.cod-estab
          and fat-duplic.serie     = nota-fiscal.serie
          and fat-duplic.nr-fatura = nota-fiscal.nr-nota-fis
        no-lock:
        assign i-prazo = i-prazo + ((fat-duplic.dt-vencimen - 
                                     fat-duplic.dt-emissao) *
                                     fat-duplic.vl-parcela)
               de-valor = de-valor + fat-duplic.vl-parcela.
    END.
    IF i-prazo < 0 THEN 
       ASSIGN i-prazo = 0.
    ASSIGN i-prazo = i-prazo / de-valor.
    FIND tt-work WHERE tt-work.prazo = i-prazo NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-work THEN DO:
       CREATE tt-work.
       ASSIGN tt-work.prazo = i-prazo
              tt-work.valor = de-valor.
    END.
    ELSE
       ASSIGN tt-work.valor = tt-work.valor + de-valor.
end.
FOR EACH tt-work BY tt-work.valor DESCEND:
    DISP tt-work.prazo
         tt-work.valor(TOTAL).
END.
