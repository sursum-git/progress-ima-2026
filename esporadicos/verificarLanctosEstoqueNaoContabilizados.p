FOR EACH movto-estoq  NO-LOCK
    WHERE dt-trans >= 01.01.2017
    AND movto-estoq.contabilizado = NO.
    DISP dt-trans it-codigo movto-estoq.quantidade movto-estoq.valor-mat-m.

END.
