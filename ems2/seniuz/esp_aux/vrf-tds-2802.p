DEF BUFFER b-movto FOR movto-estoq.

DEF VAR de-tot as DEC.

FOR EACH movto-estoq WHERE
         movto-estoq.dt-trans = 02.28.2012 AND
         /*
         movto-estoq.it-codigo = '520000' AND
         movto-estoq.cod-refer = '619' AND
         */
         movto-estoq.esp-docto   = 6 
         /*
         movto-estoq.quantidade > 50   AND 
         movto-estoq.quantidade < 1000  */ NO-LOCK
         BY movto-estoq.dt-trans
         BY  movto-estoq.it-codigo
         BY movto-estoq.cod-refer.

    IF movto-estoq.tipo-trans = 1 THEN 
       ASSIGN de-tot = de-tot + movto-estoq.quantidade.
    ELSE 
       ASSIGN de-tot = de-tot - movto-estoq.quantidade.
    
    /*
    FIND b-movto WHERE
         b-movto.dt-trans >= movto-estoq.dt-trans AND
         b-movto.it-codigo = movto-estoq.it-codigo AND
         b-movto.cod-refer = movto-estoq.cod-refer AND
         b-movto.esp-docto = movto-estoq.esp-docto AND 
         b-movto.quantidade =  movto-estoq.quantidade AND
         b-movto.tipo-trans = IF movto-estoq.tipo-trans = 1
                              THEN 2 ELSE 1 
         NO-ERROR.

    IF AVAIL b-movto THEN DO.
       DISP movto-estoq.it-codigo
            movto-estoq.dt-trans
            movto-estoq.cod-refer
            INT(movto-estoq.tipo-trans)
            movto-estoq.quantidade 
            WITH WIDTH 550.

       RUN esp/esce060a.p (INPUT ROWID(movto-estoq)). 
       RUN esp/esce060a.p (INPUT ROWID(b-movto)). 
    END.
    */                                   
                     
    DISP movto-estoq.it-codigo
         movto-estoq.dt-trans
         movto-estoq.cod-refer
         INT(movto-estoq.tipo-trans) FORMAT "9" 
         movto-estoq.quantidade 
         WITH WIDTH 550.
                                           
    /*RUN esp/esce060a.p (INPUT ROWID(movto-estoq)). */
    
END.

DISP de-tot.

                     
