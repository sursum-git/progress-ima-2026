DEFINE TEMP-TABLE tt
    FIELD conta AS CHAR FORMAT 'x(12)'
    FIELD valor AS DECIMAL
    FIELD tipo  AS INT
    FIELD mes   AS INT
    FIELD ano   AS INT.
    
        
FOR EACH movto-estoq
    WHERE movto-estoq.dt-trans >= 01.01.2017
    AND   movto-estoq.dt-trans <= 08.31.2017 
    AND  movto-estoq.ct-codigo = movto-estoq.ct-saldo NO-LOCK.
    //DISP movto-estoq.valor-mat-m[1] WITH 1 COL WIDTH 550.
    FIND FIRST tt
        WHERE tt.conta = movto-estoq.ct-codigo
        AND   tt.tipo  = movto-estoq.tipo-trans
        AND   tt.ano   = YEAR(movto-estoq.dt-trans)
        AND   tt.mes   = MONTH(movto-estoq.dt-trans)  NO-LOCK NO-ERROR.
    IF NOT AVAIL tt THEN DO:
       CREATE tt.
       ASSIGN tt.conta = movto-estoq.ct-codigo
              tt.tipo  = movto-estoq.tipo-trans
              tt.ano   = YEAR(movto-estoq.dt-trans)
              tt.mes   = MONTH(movto-estoq.dt-trans) .
    END.
    ASSIGN tt.valor = tt.valor + movto-estoq.valor-mat-m[1].
    
 END.

 FOR EACH tt:
     DISP tt WITH WIDTH 550.
 END.
