DEF VAR de-abe AS DEC.
DEF VAR de-res AS DEC.

def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

OUTPUT TO "c:/lixo/lixo.txt" CONVERT SOURCE "ibm850".

FOR EACH ped-venda WHERE (ped-venda.cod-sit-ped < 3 OR ped-venda.cod-sit-ped = 5)
                     AND ped-venda.cod-cond-pag <> 1
                     AND (ped-venda.cod-sit-aval = 2 OR ped-venda.cod-sit-aval = 3)
                     AND ped-venda.ind-fat-par   = YES
                   NO-LOCK,
    EACH emitente WHERE emitente.cod-emitente = ped-venda.cod-emitente
                    AND emitente.pais         = 'brasil'
                  NO-LOCK:
    ASSIGN de-abe = 0
           de-res = 0.
    FOR EACH ped-item OF ped-venda WHERE (ped-item.cod-sit-item < 3 OR ped-item.cod-sit-item = 5)
                                   NO-LOCK.
        
        FIND ped-item-res OF ped-item NO-LOCK NO-ERROR.
        IF AVAIL ped-item-res AND ped-item-res.faturado = NO THEN
           ASSIGN de-res = de-res + ped-item-res.qt-pedida.

        ASSIGN de-abe = de-abe + (ped-item.qt-pedida - ped-item.qt-atendida).
    END.  
    IF /*de-abe <> 0 AND*/ de-res <> 0 THEN
       DISPLAY ped-venda.nome-abrev    LABEL "Cliente"
               ped-venda.nr-pedcli     LABEL "Pedido"
               de-abe(TOTAL)           LABEL "Aberto"
               de-res(TOTAL)           LABEL "Reserva"
               de-abe - de-res (TOTAL) LABEL "Saldo"
               WITH NO-LABELS WIDTH 132.
END.

OUTPUT CLOSE.

run Execute in h-prog(input "wordpad.exe", input "c:\lixo\lixo.txt").
delete procedure h-prog.


