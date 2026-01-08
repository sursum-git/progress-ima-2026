DEF VAR de-tot-aca AS DEC.
DEF VAR de-tot-sob AS DEC.

OUTPUT TO PRINTER.
FOR EACH ord-prod WHERE
         ord-prod.dt-emissao >= 10.01.2006 AND
         ord-prod.dt-emissao <= 10.31.2006 AND
         ord-prod.cd-planejad = "automatico" NO-LOCK.

    ASSIGN de-tot-aca = 0.
    FOR EACH  movto-estoq WHERE
              movto-estoq.nr-ord-prod = ord-prod.nr-ord-prod 
              NO-LOCK USE-INDEX operacao.
        IF movto-estoq.esp-docto = 1 THEN
           ASSIGN de-tot-aca = de-tot-aca + movto-estoq.quantidade.

        IF movto-estoq.esp-docto = 8 THEN
           ASSIGN de-tot-aca = de-tot-aca - movto-estoq.quantidade.
    END.

    ASSIGN de-tot-sob = 0.
    FOR EACH  movto-estoq WHERE
              movto-estoq.nr-ord-prod = ord-prod.nr-ord-prod 
              NO-LOCK USE-INDEX operacao.
        IF movto-estoq.esp-docto =  35 THEN DO.
           IF movto-estoq.tipo-trans = 1 THEN
              ASSIGN de-tot-sob = de-tot-sob + movto-estoq.quantidade.
           ELSE
           ASSIGN de-tot-sob = de-tot-sob - movto-estoq.quantidade.
        END.
    END.

    IF de-tot-aca > 0 THEN DO.
       FIND FIRST movto-estoq WHERE
                  movto-estoq.nr-ord-prod = ord-prod.nr-ord-prod AND
                  movto-estoq.esp-docto = 28 
                  USE-INDEX operacao NO-LOCK NO-ERROR.
                                  
       IF NOT AVAIL movto-estoq THEN
          DISP ord-prod.nr-ord-prod
               ord-prod.it-codigo
               ord-prod.cod-refer
               de-tot-aca
               de-tot-sob.
    END.
END.
OUTPUT CLOSE.

