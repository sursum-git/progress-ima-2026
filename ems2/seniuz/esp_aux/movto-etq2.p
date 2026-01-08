DEF BUFFER b-movto FOR movto-etq.

OUTPUT TO PRINTER.
FOR EACH movto-etq WHERE
         movto-etq.dt-trans >= 01.01.2009 AND
         movto-etq.esp-docto = 'DEL' NO-LOCK.

    FIND b-movto WHERE
         b-movto.num-etiqueta = movto-etq.num-etiqueta AND
         b-movto.dt-trans >= movto-etq.dt-trans AND
         b-movto.esp-docto <> 'DEL' NO-LOCK NO-ERROR.
    IF AVAIL b-movto THEN DO.
       FIND ob-etiqueta WHERE
            ob-etiqueta.num-etiqueta = movto-etq.num-etiqueta NO-LOCK NO-ERROR.

       DISP movto-etq.num-etiqueta
            ob-etiqueta.it-codigo WHEN AVAIL ob-etiqueta
            ob-etiqueta.cod-refer  WHEN AVAIL ob-etiqueta
            ob-etiqueta.quantidade  WHEN AVAIL ob-etiqueta SKIP
            SUBSTR(movto-etq.char-1,1,150) FORMAT "x(150)"
            WITH WIDTH 200.
    END.
END.


