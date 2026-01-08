DEF INPUT  PARAMETER p-cod-emit LIKE emitente.cod-emitente.
DEF OUTPUT PARAMETER p-restricao  AS LOG.
DEF OUTPUT PARAMETER p-motivo     AS CHAR.

FIND emitente WHERE
     emitente.cod-emitente = p-cod-emit NO-LOCK NO-ERROR.

RUN pi-inativo.
RUN pi-cheque-dev.
RUN pi-tit-atraso.

PROCEDURE pi-inativo.
    FIND LAST ped-venda WHERE
              ped-venda.cod-sit-ped <> 4 AND
              ped-venda.cod-sit-ped <> 6 AND
              ped-venda.nome-abrev = emitente.nome-abrev 
              USE-INDEX ch-pre-fat NO-LOCK NO-ERROR.
    
    IF AVAIL ped-venda THEN DO.
       ASSIGN p-restricao = NO.
       IF TRUNC((TODAY - ped-venda.dt-entrega) / 30,0) > emitente.nr-mesina THEN
          ASSIGN p-restricao = YES
                 p-motivo = IF p-motivo = ""
                            THEN "Inativo a " + STRING(TRUNC((TODAY - ped-venda.dt-entrega) / 30,0)) + " Meses" 
                            ELSE p-motivo + " // Inativo a " + STRING(TRUNC((TODAY - ped-venda.dt-entrega) / 30,0)) + " Meses" .
    END.
END.

PROCEDURE pi-cheque-dev.
    DEF VAR de-qt-cheque-dev AS INT.
    DEF VAR de-vlr-cheque-dev AS DEC.

    FOR EACH titulo WHERE 
             titulo.cod-emit  = emitente.cod-emit AND
             titulo.cod-esp = 'CQ' AND
             titulo.vl-saldo <> 0 NO-LOCK USE-INDEX emitente.

        IF titulo.dt-vencimen + 30 < TODAY THEN /* Apenas Cheques devolvidos … 30 dias */
           ASSIGN de-qt-cheque-dev = de-qt-cheque-dev + 1
                  de-vlr-cheque-dev = de-vlr-cheque-dev + titulo.vl-saldo.
    END.

    IF de-qt-cheque-dev > emitente.nr-cheque-devol OR
       de-vlr-cheque-dev > emitente.vl-max-devol THEN
       ASSIGN p-restricao = YES
              p-motivo = IF p-motivo = "" 
                         THEN "Cheques devolvidos"
                         ELSE p-motivo + " // Cheques devolvidos".
END PROCEDURE.

PROCEDURE pi-tit-atraso.
    DEF VAR de-tit-vencidos AS DEC.
    DEF VAR i-dias AS INT.

    FOR EACH titulo WHERE 
             titulo.cod-emit = emitente.cod-emit AND
             titulo.dt-vencimen < TODAY - 3 AND  /* Apenas D‚bitos Vencidos   3 dias */
             titulo.cod-esp = 'DP' AND
             titulo.vl-saldo <> 0 NO-LOCK BY titulo.dt-vencim DESCENDING.
    
        IF titulo.ep-codigo <> '1' AND
           titulo.cod-estabel <> '2' THEN NEXT.
                                                                                        
        /* Ignora duplicatas substituidas */
        IF CAN-FIND(FIRST mov-tit OF titulo WHERE mov-tit.baixa-subs) THEN NEXT.
                                                                                       
        ASSIGN de-tit-vencidos = de-tit-vencidos + titulo.vl-saldo
               i-dias = TODAY - titulo.dt-vencim.
    END.
    IF de-tit-vencidos > 0 THEN
       ASSIGN p-restricao = YES
              p-motivo = IF p-motivo = ""
                         THEN "D‚bitos vencidos … " + STRING(i-dias) + " dias"
                         ELSE p-motivo + " // D‚bitos vencidos … " + STRING(i-dias) + " dias".

    ASSIGN de-tit-vencidos = 0.
    FOR EACH titulo WHERE 
             titulo.cod-emit = emitente.cod-emit AND
             titulo.dt-vencimen > (TODAY - 90) AND
             titulo.cod-esp = 'DP' AND
             titulo.vl-saldo <> 0 NO-LOCK.

        IF titulo.ep-codigo <> '1' AND 
           titulo.cod-estabel <> '2' THEN NEXT.

        /* Ignora duplicatas substituidas */
        IF CAN-FIND(FIRST mov-tit OF titulo WHERE mov-tit.baixa-subs) THEN NEXT.
                                                                                       
        IF titulo.dt-ult-pagto > titulo.dt-vencimen THEN
           ASSIGN de-tit-vencidos = de-tit-vencidos + titulo.vl-saldo.
    END.
    IF de-tit-vencidos > 0 THEN
       ASSIGN p-restricao = YES
              p-motivo = IF p-motivo = ""
                         THEN "Atrasou pagamento nos 3 ultimos meses"
                         ELSE p-motivo + " // Atrasou pagamento nos 3 ultimos meses".
    
END PROCEDURE.

