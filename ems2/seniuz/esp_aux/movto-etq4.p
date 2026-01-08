OUTPUT TO PRINTER.
FOR EACH movto-etq WHERE 
         movto-etq.dt-trans >= 07.01.2012 AND 
         movto-etq.esp-docto = 'CON' NO-LOCK,
    FIRST ob-etiqueta WHERE
         ob-etiqueta.cod-estab = '1' AND
         ob-etiqueta.num-etiqueta = movto-etq.num-etiqueta NO-LOCK
         BY ob-etiqueta.it-codigo
         BY ob-etiqueta.cod-refer.

    IF NOT AVAIL ob-etiqueta THEN NEXT.
    IF ob-etiqueta.quantidade <> 0 THEN NEXT.

    FIND movto-estoq WHERE
         movto-estoq.it-codigo = ob-etiqueta.it-codigo AND
         movto-estoq.cod-refer = ob-etiqueta.cod-refer AND
         movto-estoq.dt-trans = movto-etq.dt-trans AND
         movto-estoq.esp-docto = 30 AND
         movto-estoq.tipo-trans = 2 NO-LOCK NO-ERROR.

    IF NOT AVAIL movto-estoq THEN DO.
       FIND movto-estoq WHERE
            movto-estoq.it-codigo = ob-etiqueta.it-codigo AND
            movto-estoq.cod-refer = ob-etiqueta.cod-refer AND
            movto-estoq.dt-trans = movto-etq.dt-trans AND
            movto-estoq.esp-docto = 6 AND
            movto-estoq.tipo-trans = 2 NO-LOCK NO-ERROR.

       IF NOT AVAIL movto-estoq THEN
          DISP movto-etq.num-etiqueta 
               ob-etiqueta.it-codigo
               ob-etiqueta.cod-refer
               movto-etq.dt-trans (COUNT)
               WITH WIDTH 200.
    END.
END.


