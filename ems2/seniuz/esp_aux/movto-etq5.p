FOR EACH movto-etq WHERE 
         movto-etq.dt-trans = 08.15.2022 NO-LOCK.

    IF NOT movto-etq.char-1 MATCHES "*wfiuza*" THEN NEXT.
    IF NOT SUBSTR(movto-etq.char-1,1,150) MATCHES "*12:24*" THEN NEXT.
    //IF NOT SUBSTR(movto-etq.char-1,1,150) MATCHES "*Qtde*" THEN NEXT.


    FIND ob-etiqueta WHERE
         ob-etiqueta.cod-estab = '5' AND
         ob-etiqueta.num-etiqueta = movto-etq.num-etiqueta NO-LOCK NO-ERROR.

    IF ob-etiqueta.cod-refer <> 'H79' THEN NEXT.

    DISP movto-etq.num-etiqueta
         ob-etiqueta.quantidade (TOTAL)
         ob-etiqueta.cod-refer
         SUBSTR(movto-etq.char-1,1,50) FORMAT "x(50)"
         WITH WIDTH 550.
END.
