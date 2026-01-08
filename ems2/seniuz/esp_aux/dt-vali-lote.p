/* Programa: dt-vali-lote.p
** Objetivo: Acertar a data de validade dos lotes para itens come‡ados com 5
*/

FOR EACH saldo-estoq WHERE saldo-estoq.it-codigo BEGINS "5"
                       AND saldo-estoq.dt-vali-lote < TODAY.
    DISP saldo-estoq.it-codigo
         saldo-estoq.cod-refer
         saldo-estoq.lote
         saldo-estoq.dt-vali-lote.
    /*ASSIGN saldo-estoq.dt-vali-lote = 12/31/9999.*/
END.
