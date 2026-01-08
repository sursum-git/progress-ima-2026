OUTPUT TO p:\toninho\movto-act10.txt.
FOR EACH movto-estoq WHERE 
         movto-estoq.dt-trans = 10.01.2015 AND
         movto-estoq.esp-docto = 2 NO-LOCK.

    EXPORT movto-estoq.
    /*
     DISP movto-estoq.it-codigo
          movto-estoq.cod-refer
          INT(movto-estoq.tipo-trans)
          movto-estoq.quantidade
          movto-estoq.valor-mat-m[1]
          WITH WIDTH 550.  
          */
END.

