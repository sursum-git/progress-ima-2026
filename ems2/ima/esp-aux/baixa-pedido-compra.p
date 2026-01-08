DEF TEMP-TABLE tt-aux
    FIELD pedido AS INTEGER EXTENT 20.

DEF VAR i-ct AS INT.

/*
INPUT FROM d:\ton\pedidos3.csv.
REPEAT.
    CREATE tt-aux.
    IMPORT DELIMITER ";" tt-aux.
END.
INPUT CLOSE.
*/

CREATE tt-aux.
ASSIGN tt-aux.pedido[1] = 1027.
 

FOR EACH tt-aux.
    DISP tt-aux.pedido[1].
     
    DO i-ct = 1 TO 20.
       IF tt-aux.pedido[i-ct] <> 0 THEN
          RUN pi-baixa (INPUT tt-aux.pedido[i-ct] ).
    END.
END.


PROCEDURE pi-baixa.
    DEF INPUT PARAMETER p-pedido AS INTEGER.

    DEF VAR de-qtd-receb AS DEC.

    FOR EACH ordem-compra WHERE 
             ordem-compra.num-pedido = p-pedido AND 
             ordem-compra.situacao = 2 SHARE-LOCK.

        ASSIGN de-qtd-receb = 0.
        FOR EACH item-doc-est WHERE
                 item-doc-est.num-pedido = ordem-compra.num-pedido AND
                 item-doc-est.numero-ordem = ordem-compra.numero-ordem NO-LOCK.

            FIND docum-est OF item-doc-est NO-LOCK NO-ERROR.
            IF docum-est.ce-atual = NO THEN NEXT.

            ASSIGN de-qtd-receb = de-qtd-receb + item-doc-est.quantidade.
        END.

        FOR EACH prazo-compra OF ordem-compra WHERE
                 prazo-compra.quant-saldo <> 0 SHARE-LOCK.

            ASSIGN prazo-compra.dec-1 = prazo-compra.quantidade - de-qtd-receb
                   prazo-compra.quant-saldo = prazo-compra.quantidade - de-qtd-receb
                   prazo-compra.quant-receb = de-qtd-receb.

            ASSIGN prazo-compra.dec-1 = 0.

            IF prazo-compra.quant-saldo < 0 THEN
               ASSIGN prazo-compra.quant-saldo = 0.

        END.

        ASSIGN ordem-compra.situacao = 6. // Recebida

    END.
END PROCEDURE.
