DEF VAR de-saldo AS DEC FORMAT "->>>,>>>,>>9.99".
DEF VAR i-cont AS INT.

OUTPUT TO "c:\lixo\lixo.txt".

FOR EACH saldo-estoq WHERE saldo-estoq.cod-estabel = "2"
                       AND saldo-estoq.cod-depos   = "exp"
                       AND saldo-estoq.it-codigo BEGINS "5"
                       AND saldo-estoq.qtidade-atu <> 0
                     NO-LOCK
                     BREAK BY saldo-estoq.it-codigo:

    ASSIGN de-saldo = de-saldo + saldo-estoq.qtidade-atu
           i-cont = i-cont + 1.

    IF LAST-OF(saldo-estoq.it-codigo) THEN DO:
       FIND ITEM WHERE ITEM.it-codigo = saldo-estoq.it-codigo NO-LOCK.
       FIND item-ext WHERE item-ext.it-codigo = saldo-estoq.it-codigo NO-LOCK NO-ERROR.
       IF de-saldo <> 0 THEN
          DISP saldo-estoq.it-codigo
               ITEM.un
               item-ext.indigo WHEN AVAIL item-ext
               i-cont
               de-saldo.
       ASSIGN de-saldo = 0 
              i-cont = 0.
    END.
END.
OUTPUT CLOSE.
