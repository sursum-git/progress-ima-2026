DEF INPUT PARAMETER p-dt-saldo AS DATE FORMAT "99/99/9999".
DEF INPUT PARAMETER p-cod-estab AS CHAR.
DEF INPUT PARAMETER p-it-codigo AS CHAR.
DEF INPUT PARAMETER p-cod-refer AS CHAR.
DEF INPUT PARAMETER p-nr-lote   AS CHAR.
DEF OUTPUT PARAMETER p-qtd LIKE saldo-estoq.qtidade-atu.

FOR EACH saldo-estoq WHERE
         saldo-estoq.cod-estabel = p-cod-estab AND
         saldo-estoq.it-codigo = p-it-codigo AND
         saldo-estoq.cod-refer = p-cod-refer AND
         saldo-estoq.lote = p-nr-lote
         USE-INDEX ITEM.
    ASSIGN p-qtd = p-qtd + saldo-estoq.qtidade-atu.
END.

FOR EACH movto-estoq WHERE
         movto-estoq.dt-trans > p-dt-saldo AND
         movto-estoq.it-codigo = p-it-codigo AND
         movto-estoq.cod-refer = p-cod-refer AND
         movto-estoq.lote = p-nr-lote
         NO-LOCK USE-INDEX data-item.

    ASSIGN p-qtd = IF movto-estoq.tipo-trans = 1
                    THEN p-qtd - movto-estoq.quantidade
                    ELSE p-qtd + movto-estoq.quantidade.

END.


