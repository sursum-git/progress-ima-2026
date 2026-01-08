DEF VAR de-saldo-x LIKE saldo-estoq.qtidade-atu.
DEF VAR de-saldo-p LIKE saldo-estoq.qtidade-atu.
DEF VAR de-saldo-r LIKE saldo-estoq.qtidade-atu.

OUTPUT TO "c:/lixo/saldo-estoq.csv".
PUT "Artigo;" "Descricao;" "Peca;" "Rolo;" "Outros" SKIP.

FOR EACH saldo-estoq WHERE saldo-estoq.it-codigo BEGINS "5" NO-LOCK
    BREAK BY substr(saldo-estoq.it-codigo,3,3):
    IF SUBSTR(saldo-estoq.lote,1,1) = "p" THEN
       ASSIGN de-saldo-p = de-saldo-p + saldo-estoq.qtidade-atu.
    ELSE
    IF SUBSTR(saldo-estoq.lote,1,1) = "r" THEN
       ASSIGN de-saldo-r = de-saldo-r + saldo-estoq.qtidade-atu.
    ELSE
       ASSIGN de-saldo-x = de-saldo-x + saldo-estoq.qtidade-atu.

    IF LAST-OF(substr(saldo-estoq.it-codigo,3,3)) THEN DO:
       FIND ITEM WHERE ITEM.it-codigo = saldo-estoq.it-codigo NO-LOCK.
       IF de-saldo-p <> 0 OR
          de-saldo-r <> 0 OR
          de-saldo-x <> 0 THEN
       PUT substr(saldo-estoq.it-codigo,3,3) ";"
            ITEM.descricao-1 ";"
            de-saldo-p ";"
            de-saldo-r ";"
            de-saldo-x SKIP.
       ASSIGN de-saldo-p = 0
              de-saldo-r = 0
              de-saldo-x = 0.
    END.
END.
OUTPUT CLOSE.
