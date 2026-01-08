DEF VAR de-qtd AS DECIMAL.

FOR EACH movto-estoq WHERE
         movto-estoq.it-codigo = '175146' AND
         movto-estoq.cod-refer = 'b01'  AND
         movto-estoq.lote = 'cab01' NO-LOCK.

    //DISP movto-estoq.lote.

    IF movto-estoq.tipo-trans = 1 THEN
       ASSIGN de-qtd = de-qtd + movto-estoq.quantidade.
    ELSE 
       ASSIGN de-qtd = de-qtd - movto-estoq.quantidade.
END.

FIND saldo-estoq WHERE
     saldo-estoq.it-codigo = '175146' AND
     saldo-estoq.cod-refer = 'b01' AND 
     saldo-estoq.lote = 'cab01' SHARE-LOCK.

DISP saldo-estoq.qtidade-atu
     de-qtd.
