/* Programa: exp_datasul.p
** Objetivo: Exportar dados para suporte da Datasul
*/

DEF VAR i-cont AS INT.
DEF VAR i-seq  AS INT INIT 2.
DEF VAR c-arquivo AS CHAR.

OUTPUT TO "c:/lixo/estabelec.exp".
FOR EACH estabelec.
    EXPORT estabelec.
END.
OUTPUT CLOSE.

OUTPUT TO "c:/lixo/deposito.exp".
FOR EACH deposito.
    EXPORT deposito.
END.
OUTPUT CLOSE.

OUTPUT TO "c:/lixo/movto-mat.exp".
FOR EACH movto-mat.
    EXPORT movto-mat.
END.
OUTPUT CLOSE.

ASSIGN c-arquivo = "c:\lixo\movto-estoq1.exp".
OUTPUT TO VALUE(c-arquivo).
FOR EACH movto-estoq NO-LOCK:
    EXPORT movto-estoq.
    ASSIGN i-cont = i-cont + 1.
    IF i-cont = 1000000 THEN do:
       OUTPUT CLOSE.
       ASSIGN c-arquivo = "c:\lixo\movto-estoq" + STRING(i-seq,"9") + ".exp".
       OUTPUT TO VALUE(c-arquivo).
       ASSIGN i-seq  = i-seq + 1
              i-cont = 0.
    END.
END.
OUTPUT CLOSE.

OUTPUT TO "c:/lixo/ord-prod.exp".
FOR EACH ord-prod.
    EXPORT ord-prod.
END.
OUTPUT CLOSE.

OUTPUT TO "c:/lixo/item-man.exp".
FOR EACH item-man.
    EXPORT item-man.
END.
OUTPUT CLOSE.

OUTPUT TO "c:/lixo/estrutura.exp".
FOR EACH estrutura.
    EXPORT estrutura.
END.
OUTPUT CLOSE.

OUTPUT TO "c:/lixo/item.exp".
FOR EACH item.
    EXPORT item.
END.
OUTPUT CLOSE.

OUTPUT TO "c:/lixo/item-uni-estab.exp".
FOR EACH item-uni-estab.
    EXPORT item-uni-estab.
END.
OUTPUT CLOSE.

OUTPUT TO "c:/lixo/lista-compon-item.exp".
FOR EACH lista-compon-item.
    EXPORT lista-compon-item.
END.
OUTPUT CLOSE.

OUTPUT TO "c:/lixo/item-lista-compon.exp".
FOR EACH item-lista-compon.
    EXPORT item-lista-compon.
END.
OUTPUT CLOSE.

OUTPUT TO "c:/lixo/saldo-estoq.exp".
FOR EACH saldo-estoq.
    EXPORT saldo-estoq.
END.
OUTPUT CLOSE.




