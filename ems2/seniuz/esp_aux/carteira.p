/* Programa: carteira.p
** Gera arquivo .csv com Item/Referˆncia com carteira, reserva e estoque.
*/

DEF VAR de-cart-int AS DEC FORMAT ">,>>>,>>9.99".
DEF VAR de-cart-ext AS DEC FORMAT ">,>>>,>>9.99".

def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

OUTPUT TO "c:/lixo/lixo.csv".
PUT "Item;Descricao;Indigo;Referencia;Cart-Merc-Int;Cart-Merc-Ext;Reserva;Estoque;Credito;Parcial;Cond-Pag" SKIP.

FOR EACH ped-item WHERE ped-item.it-codigo >= "5" 
                    AND ped-item.it-codigo <= "5z"
                    AND LOOKUP(STRING(ped-item.cod-sit-item,"9"),"1,2,4,5") > 0 
                  NO-LOCK,
    EACH ITEM OF ped-item WHERE item.ge-codigo >= 50
                            AND item.ge-codigo <= 59
                          NO-LOCK
    BREAK BY ped-item.it-codigo
          BY ped-item.cod-refer:

    FIND ped-venda OF ped-item NO-LOCK.
    FIND emitente WHERE emitente.cod-emitente = ped-venda.cod-emitente NO-LOCK.
    IF emitente.pais = 'brasil' THEN
       ASSIGN de-cart-int = de-cart-int + (ped-item.qt-pedida - ped-item.qt-atendida).
    ELSE
       ASSIGN de-cart-ext = de-cart-ext + (ped-item.qt-pedida - ped-item.qt-atendida).

    IF LAST-OF(ped-item.cod-refer) THEN DO:
       FOR EACH ped-item-res OF ped-item WHERE ped-item-res.faturado = NO NO-LOCK.
           ACCUMULATE ped-item-res.qt-pedida(TOTAL).
       END.
       FOR EACH saldo-estoq WHERE saldo-estoq.it-codigo = ped-item.it-codigo
                              AND saldo-estoq.cod-refer = ped-item.cod-refer
                            NO-LOCK.
           ACCUMULATE saldo-estoq.qtidade-atu(TOTAL).
       END.
       FIND item-ext WHERE item-ext.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
       PUT ped-item.it-codigo ";"
           ITEM.desc-item FORMAT "x(36)" ";"
           item-ext.indigo ";"
           ped-item.cod-refer ";"
           de-cart-int ";"
           de-cart-ext ";"
           (ACCUM TOTAL ped-item-res.qt-pedida) ";"
           (ACCUM TOTAL saldo-estoq.qtidade-atu) ";"
           ped-venda.cod-sit-aval ";"
           ped-venda.ind-fat-par ";"
           ped-venda.cod-cond-pag
           SKIP.
       ASSIGN de-cart-int = 0
              de-cart-ext = 0.
    END.
END.

OUTPUT CLOSE.

run Execute in h-prog(input "excel.exe", input "c:\lixo\lixo.csv").
delete procedure h-prog.

