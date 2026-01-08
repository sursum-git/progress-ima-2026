DEF TEMP-TABLE tt-itens
    FIELD it-codigo AS CHAR
    FIELD cod-refer AS CHAR
    FIELD qtidade-e AS DEC
    FIELD qtidade-s AS DEC.

DEF BUFFER b-movto FOR movto-estoq.

DEF VAR de-tot as DEC.

FOR EACH movto-estoq WHERE
         movto-estoq.dt-trans  = 02.28.2012 AND
         movto-estoq.esp-docto = 6 AND
         movto-estoq.tipo-trans = 1 
         BY movto-estoq.dt-trans
         BY movto-estoq.it-codigo
         BY movto-estoq.cod-refer.

    FIND tt-itens WHERE
         tt-itens.it-codigo = movto-estoq.it-codigo AND
         tt-itens.cod-refer = movto-estoq.cod-refer NO-ERROR.
    IF NOT AVAIL tt-itens THEN DO.
       CREATE tt-itens.
       ASSIGN tt-itens.it-codigo = movto-estoq.it-codigo 
              tt-itens.cod-refer = movto-estoq.cod-refer.
    END.

    IF movto-estoq.tipo-trans = 1 THEN 
       ASSIGN tt-itens.qtidade-e = tt-itens.qtidade-e + movto-estoq.quantidade.
    ELSE     
       ASSIGN tt-itens.qtidade-s = tt-itens.qtidade-s + movto-estoq.quantidade.
END.


FOR EACH tt-itens WHERE 
         tt-itens.qtidade-s > 0 NO-LOCK.

    RUN pi-verifica.
END.

PROCEDURE pi-elimina.
    FOR EACH movto-estoq WHERE
             movto-estoq.dt-trans >= 02.28.2012 AND
             movto-estoq.it-codigo = tt-itens.it-codigo AND
             movto-estoq.cod-refer = tt-itens.cod-refer AND
             movto-estoq.esp-docto = 6 AND
             movto-estoq.tipo-trans = 1 AND
             movto-estoq.quantidade > 0 
             NO-LOCK.

        RUN esp/esce060a.p (INPUT ROWID(movto-estoq)). 
    END.
END PROCEDURE.

PROCEDURE pi-verifica.
   DEF VAR de-tot-e AS DEC.
   FOR EACH movto-estoq WHERE
            movto-estoq.dt-trans >= 02.28.2012 AND
            movto-estoq.it-codigo = tt-itens.it-codigo AND
            movto-estoq.cod-refer = tt-itens.cod-refer AND
            movto-estoq.esp-docto = 6  AND
            movto-estoq.tipo-trans = 1 AND
            movto-estoq.quantidade > 0 
            NO-LOCK.

       ASSIGN de-tot-e = de-tot-e + movto-estoq.quantidade.
   END.

   IF ABS(tt-itens.qtidade-s - de-tot-e) <= 15 THEN DO.
      RUN pi-elimina.

      IF tt-itens.qtidade-s > de-tot-e THEN
         RUN esp_aux/cria-rm.p (INPUT tt-itens.it-codigo, 
                                INPUT tt-itens.cod-refer,
                                INPUT ABS(tt-itens.qtidade-s - de-tot-e),
                                INPUT 02.28.2012).
   END.

END PROCEDURE.

