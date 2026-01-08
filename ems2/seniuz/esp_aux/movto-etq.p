FOR EACH movto-etq WHERE movto-etq.num-etiqueta = 423497 NO-LOCK.
    DISP movto-etq.nro-docto
         movto-etq.dt-trans
         movto-etq.esp-docto
         substr(movto-etq.char-1,1,150) FORMAT "x(150)"
        WITH WIDTH 200.
END.


