FOR EACH movto-etq WHERE
         movto-etq.num-etiqueta = 114030 NO-LOCK.
    DISP movto-etq.num-etiqueta
         movto-etq.dt-trans
         movto-etq.esp-docto
         movto-etq.char-1 FORMAT "x(150)"
         WITH WIDTH 550.
END.
