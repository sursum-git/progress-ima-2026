DEF BUFFER b-movto-estoq FOR movto-estoq.

    FIND FIRST param-global NO-LOCK NO-ERROR.
    FIND FIRST param-estoq NO-LOCK NO-ERROR.

FOR EACH b-movto-estoq WHERE
         b-movto-estoq.dt-trans >= 01.01.2012 AND
         b-movto-estoq.esp-docto = 21 NO-LOCK.

    FIND movto-estoq WHERE
         movto-estoq.dt-trans     = b-movto-estoq.dt-trans AND
         movto-estoq.it-codigo    = b-movto-estoq.it-codigo AND
         movto-estoq.cod-refer    = b-movto-estoq.cod-refer AND
         movto-estoq.tipo-trans   = 1 AND
         movto-estoq.esp-docto    = 6  AND
         movto-estoq.quantidade   = b-movto-estoq.quantidade
         NO-ERROR.

    IF AVAIL movto-estoq THEN DO.
       FIND item WHERE
            item.it-codigo = movto-estoq.it-codigo NO-ERROR.

       FIND item-estab WHERE
            item-estab.it-codigo   = movto-estoq.it-codigo AND
            item-estab.cod-estabel = movto-estoq.cod-estabel
            NO-ERROR.

       IF NOT CAN-DO("4,1",STRING(item.tipo-contr)) THEN DO:
            IF movto-estoq.esp-docto <> 1 AND
               movto-estoq.esp-docto <> 8 AND
               movto-estoq.esp-docto <> 27 THEN DO:

                FIND conta-contab WHERE
                     conta-contab.ep-codigo = param-global.empresa-prin AND 
                     conta-contab.ct-codigo = movto-estoq.ct-codigo AND 
                     conta-contab.sc-codigo = movto-estoq.sc-codigo
                     NO-LOCK NO-ERROR.

                IF AVAILABLE conta-contab AND
                   conta-contab.estoque = 1 THEN
                    IF movto-estoq.tipo-trans = 1 THEN
                        ASSIGN item.consumo-aad       = item.consumo-aad + movto-estoq.quantidade
                               item-estab.consumo-aad = item-estab.consumo-aad + movto-estoq.quantidade.
                    ELSE 
                        ASSIGN item.consumo-aad       = item.consumo-aad - movto-estoq.quantidade
                               item-estab.consumo-aad = item-estab.consumo-aad - movto-estoq.quantidade.
            END.
       END.

       IF item.tipo-contr        <> 4 AND 
           movto-estoq.quantidade <> 0 THEN DO:

            FIND saldo-estoq WHERE
                 saldo-estoq.it-codigo = movto-estoq.it-codigo AND
                 saldo-estoq.cod-estabel = movto-estoq.cod-estabel AND
                 saldo-estoq.cod-depos = movto-estoq.cod-depos AND
                 saldo-estoq.cod-localiz = movto-estoq.cod-localiz AND
                 saldo-estoq.lote = movto-estoq.lote AND
                 saldo-estoq.cod-refer = movto-estoq.cod-refer 
                 NO-ERROR.
            IF AVAILABLE saldo-estoq THEN DO:
                IF movto-estoq.tipo-trans = 1 THEN 
                   ASSIGN saldo-estoq.qtidade-atu = saldo-estoq.qtidade-atu - movto-estoq.quantidade.
                ELSE 
                   ASSIGN saldo-estoq.qtidade-atu = saldo-estoq.qtidade-atu + movto-estoq.quantidade.
            END.
       END.

       FIND FIRST movto-mat USE-INDEX num-seq WHERE
                  movto-mat.nr-ord-prod = movto-estoq.nr-ord-produ AND
                  movto-mat.num-sequen = movto-estoq.num-sequen NO-ERROR.
       IF AVAILABLE movto-mat THEN DO:
           DELETE movto-mat VALIDATE(TRUE,"").
       END.

       DELETE movto-estoq VALIDATE(TRUE,"").
    END.
END.


