DEF VAR de-qtd LIKE saldo-estoq.qtidade-atu.
DEF BUFFER b-movto-estoq FOR movto-estoq.

FOR EACH movto-estoq WHERE
         movto-estoq.dt-trans >= 02.01.2012 AND
         movto-estoq.dt-trans <= 02.29.2012 AND 
         movto-estoq.lote BEGINS 'RP' NO-LOCK.

    RUN esapi/calc-saldo-estq.p (INPUT 02.29.2012,
                                 INPUT movto-estoq.it-codigo,
                                 INPUT movto-estoq.cod-refer,
                                 INPUT movto-estoq.lote,
                                 OUTPUT de-qtd).
    
    IF de-qtd < 0 THEN DO.
       FIND b-movto-estoq WHERE
            b-movto-estoq.dt-trans > 02.29.2012 AND
            b-movto-estoq.it-codigo = movto-estoq.it-codigo AND
            b-movto-estoq.cod-refer = movto-estoq.cod-refer AND
            b-movto-estoq.lote = movto-estoq.lote AND
            b-movto-estoq.esp-docto = 33  AND
            b-movto-estoq.tipo-trans = 1 AND
            b-movto-estoq.quantidade = abs(de-qtd) 
            SHARE-LOCK NO-ERROR.

       IF AVAIL b-movto-estoq THEN
          ASSIGN b-movto-estoq.dt-trans = 02.29.2012.
    END.
END.



