DEF VAR de-qtd AS DEC FORMAT ">>>,>>>,>>9.99".
DEF VAR de-qtd-conv AS DEC FORMAT ">>>,>>>,>>9.99".
DEF VAR de-vlr AS DEC FORMAT ">>>,>>>,>>9.99".

FOR EACH nota-fiscal WHERE nota-fiscal.cod-estabel = "2"
                       AND nota-fiscal.serie       = "1"
                       AND nota-fiscal.dt-emis-nota >= 05/01/2005
                       AND nota-fiscal.dt-emis-nota <= 05/31/2005
                       AND nota-fiscal.dt-cancela   = ?
                       AND nota-fiscal.emite-dup    = YES
                     NO-LOCK,
    EACH it-nota-fisc OF nota-fiscal WHERE it-nota-fisc.it-codigo >= "5"
                                       AND it-nota-fisc.it-codigo <= "5zzzz"
                                     NO-LOCK:
    ASSIGN de-qtd = de-qtd + it-nota-fisc.qt-faturada[1].
           de-vlr = de-vlr + it-nota-fisc.vl-tot-item.


    if it-nota-fisc.un-fatur[1] <> "m" then do:
       find item-ext where
            item-ext.it-codigo = it-nota-fisc.it-codigo no-lock no-error.
       IF AVAIL item-ext THEN 
          assign de-qtd-conv = de-qtd-conv + (it-nota-fisc.qt-faturada[1]
                                              * item-ext.fator-conv).
    end.
    ELSE
       ASSIGN de-qtd-conv = de-qtd-conv + it-nota-fisc.qt-faturada[1].

END.
DISP de-qtd
     de-vlr
     de-qtd-conv.
