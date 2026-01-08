DEF TEMP-TABLE tt-itens
    FIELD cod-estab    LIKE saldo-estoq.cod-estabel
    FIELD it-codigo    LIKE ob-etiqueta.it-codigo
    FIELD cod-refer    LIKE ob-etiqueta.cod-refer
    FIELD lote         LIKE ob-etiqueta.cod-refer
    FIELD new-lote     LIKE ob-etiqueta.nr-lote
    FIELD qtidade-atu  LIKE saldo-estoq.qtidade-atu
    INDEX indice1 IS PRIMARY it-codigo cod-refer.

DEF VAR c-erro AS CHAR.
DEF VAR de-qtd LIKE saldo-estoq.qtidade-atu.

DEF VAR i-ct AS INT.

FOR EACH saldo-estoq WHERE
         saldo-estoq.qtidade-atu > 0 NO-LOCK,
    FIRST ITEM OF saldo-estoq WHERE
          ITEM.ge-codigo >= 50 AND
          ITEM.ge-codigo <= 60 NO-LOCK.

    IF saldo-estoq.lote BEGINS 'ca' THEN NEXT.

    IF saldo-estoq.cod-refer = saldo-estoq.lote THEN NEXT.
    
    IF saldo-estoq.qt-aloc-ped <> 0 OR 
       saldo-estoq.qt-alocada <> 0 THEN NEXT.
    
    FIND tt-itens WHERE
         tt-itens.cod-estab = saldo-estoq.cod-estabel AND
         tt-itens.it-codigo = saldo-estoq.it-codigo  AND
         tt-itens.cod-refer = saldo-estoq.cod-refer AND
         tt-itens.lote = saldo-estoq.cod-refer 
         NO-ERROR.

    IF NOT AVAIL tt-itens THEN DO.
        CREATE tt-itens.
        ASSIGN tt-itens.cod-estab = saldo-estoq.cod-estab
               tt-itens.it-codigo = saldo-estoq.it-codigo 
               tt-itens.cod-refer = saldo-estoq.cod-refer
               tt-itens.lote = saldo-estoq.lote
               tt-itens.new-lote = saldo-estoq.cod-refer.
    END.
    ASSIGN tt-itens.qtidade-atu = tt-itens.qtidade-atu + saldo-estoq.qtidade-atu.
END.
          
FOR EACH tt-itens.
    DISP tt-itens.it-codigo
         tt-itens.cod-refer
         tt-itens.lote          
         tt-itens.qtidade-atu (COUNT).
END.

/*
DO TRANSACTION.

    FOR EACH tt-itens WHERE
             tt-itens.qtidade-atu >= 0.
    
        ASSIGN i-ct = i-ct + 1.

        DISP tt-itens.it-codigo
             tt-itens.cod-refer
             i-ct.
        PAUSE 0.
    
        RUN esapi/cria-movto-estoq.p (INPUT tt-itens.cod-estab,
                                      INPUT tt-itens.it-codigo,
                                      INPUT tt-itens.cod-refer,
                                      INPUT tt-itens.lote, 
                                      INPUT tt-itens.qtidade-atu,
                                      INPUT 33, /* TRA */ 
                                      INPUT 2,  /* Saida */ 
                                      INPUT "Retorno Convers∆o de Lote",
                                      OUTPUT c-erro).
    
        IF c-erro = 'ADM-ERROR' THEN  UNDO, NEXT.
    
        RUN esapi/cria-movto-estoq.p (INPUT tt-itens.cod-estab,
                                      INPUT tt-itens.it-codigo,
                                      INPUT tt-itens.cod-refer,
                                      INPUT tt-itens.cod-refer, 
                                      INPUT tt-itens.qtidade-atu,
                                      INPUT 33,  /* TRA */ 
                                      INPUT 1,  /* Entrada */ 
                                      INPUT "Retorno Convers∆o de Lote",
                                      OUTPUT c-erro). 
    END.


END.

*/
