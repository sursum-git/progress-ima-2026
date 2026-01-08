FOR EACH movto-estoq WHERE
         movto-estoq.dt-trans = 03.01.2012 AND
         movto-estoq.esp-docto = 6 AND
         movto-estoq.tipo-trans = 2
         USE-INDEX data-item NO-LOCK.

    IF movto-estoq.quantidade <= 10 THEN
       RUN esp_aux/transf-td-rm.p (INPUT ROWID(movto-estoq)).
END.
