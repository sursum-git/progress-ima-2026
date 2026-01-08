FOR EACH movto-estoq EXCLUSIVE-LOCK
    WHERE movto-estoq.serie = 'enci'
    AND movto-estoq.ct-saldo <> movto-estoq.ct-codigo:
    ASSIGN movto-estoq.ct-saldo = movto-estoq.ct-codigo.
    DISP movto-estoq.ct-saldo movto-estoq.ct-codigo.
END.
