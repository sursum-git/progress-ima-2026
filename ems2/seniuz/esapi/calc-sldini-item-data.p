DEF INPUT  PARAMETER p-cod-estabel LIKE saldo-estoq.cod-estabel.
DEF INPUT  PARAMETER p-it-codigo   LIKE saldo-estoq.it-codigo.
DEF INPUT  PARAMETER p-cod-refer   LIKE saldo-estoq.cod-refer.
DEF INPUT  PARAMETER p-dt-saldo    AS DATE FORMAT "99/99/9999".
DEF OUTPUT PARAMETER p-qtd         LIKE saldo-estoq.qtidade-atu.

FOR EACH saldo-estoq WHERE
         saldo-estoq.cod-estabel = p-cod-estabel AND
         saldo-estoq.it-codigo   = p-it-codigo AND
         saldo-estoq.cod-refer   = p-cod-refer NO-LOCK.
    ASSIGN p-qtd = p-qtd + saldo-estoq.qtidade-atu.
END.

FOR EACH movto-estoq WHERE
         movto-estoq.it-codigo   = p-it-codigo AND
         movto-estoq.dt-trans    < p-dt-saldo AND
         movto-estoq.cod-refer   = p-cod-refer AND
         movto-estoq.cod-estabel = p-cod-estabel 
         NO-LOCK USE-INDEX data-item.

    ASSIGN p-qtd = IF movto-estoq.tipo-trans = 1
                   THEN p-qtd - movto-estoq.quantidade
                   ELSE p-qtd + movto-estoq.quantidade.

END.


