DEF VAR de-tot-ent AS DEC.
DEF VAR de-tot-sai AS DEC.

DEF BUFFER b-movto-estoq FOR movto-estoq.

FOR EACH movto-estoq WHERE
         movto-estoq.dt-trans  = 02.30.2012 AND
         movto-estoq.esp-docto = 33 NO-LOCK 
         BY movto-estoq.it-codigo
         BY movto-estoq.cod-refer
         BY movto-estoq.quantidade .

    
    FIND b-movto-estoq WHERE
         b-movto-estoq.dt-trans = movto-estoq.dt-trans AND
         b-movto-estoq.it-codigo = movto-estoq.it-codigo AND
         b-movto-estoq.cod-refer = movto-estoq.cod-refer AND
         b-movto-estoq.quantidade = movto-estoq.quantidade AND
         b-movto-estoq.esp-docto = movto-estoq.esp-docto AND
         b-movto-estoq.tipo-trans = IF movto-estoq.tipo-trans = 1
                                      THEN 2 ELSE 1
         NO-LOCK NO-ERROR.
    
    IF movto-estoq.tipo-trans = 1 THEN
       ASSIGN de-tot-ent = de-tot-ent + movto-estoq.quantidade.
    ELSE
       ASSIGN de-tot-sai = de-tot-sai + movto-estoq.quantidade.
        
                                      /*
        DISP movto-estoq.it-codigo
             movto-estoq.cod-refer
             movto-estoq.lote
             int(movto-estoq.tipo-trans) FORMAT "9" 
             movto-estoq.quantidade (TOTAL)
             de-tot-ent de-tot-sai
             WITH WIDTH 550.   
                                        */
                                    
        /*                        
        MESSAGE 'eliminar ?' 
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-conf AS LOGICAL.

        IF l-conf THEN
           RUN esp/esce060a.p (INPUT ROWID(movto-estoq)).    
          */                              
     

END.

DISP de-tot-ent - de-tot-sai
     IF de-tot-ent = de-tot-sai THEN 'OK' ELSE 'ERRO'.
