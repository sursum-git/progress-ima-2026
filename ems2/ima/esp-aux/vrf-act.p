FOR EACH movto-estoq WHERE
         movto-estoq.dt-trans = 08.01.2016 AND 
         movto-estoq.esp-docto = 2 NO-LOCK.

    IF movto-estoq.quantidade = 0 THEN NEXT.

    FIND ITEM WHERE
         ITEM.it-codigo = movto-estoq.it-codigo.

    DISP movto-estoq.it-codigo
         movto-estoq.cod-refer
         movto-estoq.quantidade
         ITEM.ge-codigo
         WITH WIDTH 550.

END.

