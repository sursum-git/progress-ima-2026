DEF VAR de-qtd LIKE saldo-estoq.qtidade-atu.
DEF BUFFER b-movto-estoq FOR movto-estoq.

FOR EACH movto-estoq WHERE
         movto-estoq.dt-trans >= 03.01.2012 AND
         movto-estoq.dt-trans <= 03.31.2012 AND 
         movto-estoq.it-codigo = '535276' AND
         movto-estoq.it-codigo <> '' USE-INDEX data-item NO-LOCK,
    FIRST ITEM OF movto-estoq /*WHERE
          ITEM.ge-codigo >= 50 AND ITEM.ge-codigo <= 60 */
         BREAK BY movto-estoq.it-codigo
               BY movto-estoq.cod-refer .

    IF movto-estoq.lote <> movto-estoq.cod-refer THEN NEXT.

    RUN esapi/calc-saldo-estq.p (INPUT 03.31.2012,
                                 INPUT movto-estoq.it-codigo,
                                 INPUT movto-estoq.cod-refer,
                                 INPUT movto-estoq.lote,
                                 OUTPUT de-qtd).
    
    IF de-qtd < 0 THEN DO.
       DISP movto-estoq.it-codigo
            movto-estoq.cod-refer
            movto-estoq.lote
            de-qtd.
       /*
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
          */
    END.
END.



