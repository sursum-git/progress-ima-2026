DEF VAR c-esp-docto AS CHAR.
DEF VAR c-descricao-db AS CHAR FORMAT "x(2000)".
OUTPUT TO c:/lixo/lixo_maio.csv CONVERT SOURCE "ibm850".
PUT "Trans;Item;Descricao;Conta;SubConta;Data;Quant;Valor;Descricao do debito direto" SKIP.
FOR EACH movto-estoq WHERE INT(movto-estoq.sc-codigo) <> 0 /* CCusto */
                       AND movto-estoq.tipo-trans = 2 /* Sa¡da */
                       AND movto-estoq.dt-trans >= 05/01/2010
                       AND movto-estoq.dt-trans <= 05/31/2010
                     NO-LOCK:
    {esinc/i-dsrb.i movto-estoq.esp-docto movto-estoq.esp-docto c-esp-docto} 

    FIND ITEM WHERE ITEM.it-codigo = movto-estoq.it-codigo NO-LOCK.

    ASSIGN c-descricao-db = REPLACE(REPLACE(movto-estoq.descricao-db,CHR(13)," "),CHR(10)," ").
    
    PUT c-esp-docto ";"
        movto-estoq.it-codigo ";"
        ITEM.desc-item ";"
        movto-estoq.ct-codigo ";"
        movto-estoq.sc-codigo ";"
        movto-estoq.dt-trans ";"
        movto-estoq.quantidade ";"
        movto-estoq.valor-mat-m[1] ";"
        c-descricao-db
        SKIP.
END.
OUTPUT CLOSE.
