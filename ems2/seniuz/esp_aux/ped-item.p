DEF VAR de-avista AS DEC.
DEF VAR de-toppra AS DEC.
DEF VAR de-export AS DEC.
DEF VAR de-n-aprv AS DEC.
DEF VAR de-outros AS DEC.
DEF VAR de-qtd-sld AS DEC.
DEF VAR de-qtd-res AS DEC.
DEF VAR dt-entrega-lim LIKE ped-item.dt-entrega INIT 11/30/2005.

OUTPUT TO "c:\temp\ped-aberto.txt".
PUT "Item;" "Descricao;" "Export;" "A Vista;" "Top-Prata;" "N/Aprov;" "Outros;"
    "Total;" "Reservado" SKIP.

FOR EACH cota-item NO-LOCK:
    FOR EACH ped-item WHERE ped-item.it-codigo    = cota-item.it-codigo
                        AND ped-item.cod-sit-item < 3
                        AND ped-item.dt-entrega   < dt-entrega-lim
                      NO-LOCK:
        
        FIND ped-venda OF ped-item NO-LOCK.
        FIND emitente WHERE emitente.cod-emitente = ped-venda.cod-emitente
                      NO-LOCK.
        FIND ITEM OF cota-item NO-LOCK.

        ASSIGN de-qtd-sld = ped-item.qt-pedida - 
                            ped-item.qt-atendida.
        IF emitente.natureza = 3 THEN
           ASSIGN de-export = de-export + de-qtd-sld.
        ELSE
        IF ped-venda.cod-cond-pag < 4 THEN
           ASSIGN de-avista = de-avista + de-qtd-sld.
        ELSE
        IF ped-venda.tp-pedido = "T" OR ped-venda.tp-pedido = "p" THEN
           ASSIGN de-toppra = de-toppra + de-qtd-sld.
        ELSE
        IF ped-venda.cod-sit-aval = 4 THEN
           ASSIGN de-n-aprv = de-n-aprv + de-qtd-sld.
        ELSE
           ASSIGN de-outros = de-outros + de-qtd-sld.

        FIND ped-item-res OF ped-item NO-LOCK NO-ERROR.
        IF AVAIL ped-item-res THEN
           ASSIGN de-qtd-res = de-qtd-res + ped-item-res.qt-pedida.
    END.

    IF cota-item.item-adic1 <> "" THEN
       FOR EACH ped-item WHERE ped-item.it-codigo    = cota-item.item-adic1
                           AND ped-item.cod-sit-item < 3
                           AND ped-item.dt-entrega   < dt-entrega-lim
                         NO-LOCK:

           FIND ped-venda OF ped-item NO-LOCK.
           FIND emitente WHERE emitente.cod-emitente = ped-venda.cod-emitente
                         NO-LOCK.
           ASSIGN de-qtd-sld = ped-item.qt-pedida - 
                               ped-item.qt-atendida.
           IF emitente.natureza = 3 THEN
              ASSIGN de-export = de-export + de-qtd-sld.
           ELSE
           IF ped-venda.cod-cond-pag < 4 THEN
              ASSIGN de-avista = de-avista + de-qtd-sld.
           ELSE
           IF ped-venda.tp-pedido = "T" OR ped-venda.tp-pedido = "p" THEN
              ASSIGN de-toppra = de-toppra + de-qtd-sld.
           ELSE
           IF ped-venda.cod-sit-aval = 4 THEN
              ASSIGN de-n-aprv = de-n-aprv + de-qtd-sld.
           ELSE
              ASSIGN de-outros = de-outros + de-qtd-sld.
        
           FIND ped-item-res OF ped-item NO-LOCK NO-ERROR.
           IF AVAIL ped-item-res THEN
              ASSIGN de-qtd-res = de-qtd-res + ped-item-res.qt-pedida.
       END.

    IF cota-item.item-adic2 <> "" THEN
       FOR EACH ped-item WHERE ped-item.it-codigo    = cota-item.item-adic2
                           AND ped-item.cod-sit-item < 3
                           AND ped-item.dt-entrega   < dt-entrega-lim
                         NO-LOCK:

           FIND ped-venda OF ped-item NO-LOCK.
           FIND emitente WHERE emitente.cod-emitente = ped-venda.cod-emitente
                         NO-LOCK.
           ASSIGN de-qtd-sld = ped-item.qt-pedida - 
                               ped-item.qt-atendida.
           IF emitente.natureza = 3 THEN
              ASSIGN de-export = de-export + de-qtd-sld.
           ELSE
           IF ped-venda.cod-cond-pag < 4 THEN
              ASSIGN de-avista = de-avista + de-qtd-sld.
           ELSE
           IF ped-venda.tp-pedido = "T" OR ped-venda.tp-pedido = "p" THEN
              ASSIGN de-toppra = de-toppra + de-qtd-sld.
           ELSE
           IF ped-venda.cod-sit-aval = 4 THEN
              ASSIGN de-n-aprv = de-n-aprv + de-qtd-sld.
           ELSE
              ASSIGN de-outros = de-outros + de-qtd-sld.
        
           FIND ped-item-res OF ped-item NO-LOCK NO-ERROR.
           IF AVAIL ped-item-res THEN
              ASSIGN de-qtd-res = de-qtd-res + ped-item-res.qt-pedida.
       END.

    IF cota-item.item-adic3 <> "" THEN
       FOR EACH ped-item WHERE ped-item.it-codigo    = cota-item.item-adic3
                           AND ped-item.cod-sit-item < 3
                           AND ped-item.dt-entrega   < dt-entrega-lim
                         NO-LOCK:

           FIND ped-venda OF ped-item NO-LOCK.
           FIND emitente WHERE emitente.cod-emitente = ped-venda.cod-emitente
                         NO-LOCK.
           ASSIGN de-qtd-sld = ped-item.qt-pedida - 
                               ped-item.qt-atendida.
           IF emitente.natureza = 3 THEN
              ASSIGN de-export = de-export + de-qtd-sld.
           ELSE
           IF ped-venda.cod-cond-pag < 4 THEN
              ASSIGN de-avista = de-avista + de-qtd-sld.
           ELSE
           IF ped-venda.tp-pedido = "T" OR ped-venda.tp-pedido = "p" THEN
              ASSIGN de-toppra = de-toppra + de-qtd-sld.
           ELSE
           IF ped-venda.cod-sit-aval = 4 THEN
              ASSIGN de-n-aprv = de-n-aprv + de-qtd-sld.
           ELSE
              ASSIGN de-outros = de-outros + de-qtd-sld.
        
           FIND ped-item-res OF ped-item NO-LOCK NO-ERROR.
           IF AVAIL ped-item-res THEN
              ASSIGN de-qtd-res = de-qtd-res + ped-item-res.qt-pedida.
       END.

    IF cota-item.item-adic4 <> "" THEN
       FOR EACH ped-item WHERE ped-item.it-codigo    = cota-item.item-adic4
                           AND ped-item.cod-sit-item < 3
                           AND ped-item.dt-entrega   < dt-entrega-lim
                         NO-LOCK:

           FIND ped-venda OF ped-item NO-LOCK.
           FIND emitente WHERE emitente.cod-emitente = ped-venda.cod-emitente
                         NO-LOCK.
           ASSIGN de-qtd-sld = ped-item.qt-pedida - 
                               ped-item.qt-atendida.
           IF emitente.natureza = 3 THEN
              ASSIGN de-export = de-export + de-qtd-sld.
           ELSE
           IF ped-venda.cod-cond-pag < 4 THEN
              ASSIGN de-avista = de-avista + de-qtd-sld.
           ELSE
           IF ped-venda.tp-pedido = "T" OR ped-venda.tp-pedido = "p" THEN
              ASSIGN de-toppra = de-toppra + de-qtd-sld.
           ELSE
           IF ped-venda.cod-sit-aval = 4 THEN
              ASSIGN de-n-aprv = de-n-aprv + de-qtd-sld.
           ELSE
              ASSIGN de-outros = de-outros + de-qtd-sld.

           FIND ped-item-res OF ped-item NO-LOCK NO-ERROR.
           IF AVAIL ped-item-res THEN
              ASSIGN de-qtd-res = de-qtd-res + ped-item-res.qt-pedida.
        END.

    IF cota-item.item-adic5 <> "" THEN
       FOR EACH ped-item WHERE ped-item.it-codigo    = cota-item.item-adic5
                           AND ped-item.cod-sit-item < 3
                           AND ped-item.dt-entrega   < dt-entrega-lim
                         NO-LOCK:
           
           FIND ped-venda OF ped-item NO-LOCK.
           FIND emitente WHERE emitente.cod-emitente = ped-venda.cod-emitente
                      NO-LOCK.
           ASSIGN de-qtd-sld = ped-item.qt-pedida - 
                               ped-item.qt-atendida.
           IF emitente.natureza = 3 THEN
              ASSIGN de-export = de-export + de-qtd-sld.
           ELSE
           IF ped-venda.cod-cond-pag < 4 THEN
              ASSIGN de-avista = de-avista + de-qtd-sld.
           ELSE
           IF ped-venda.tp-pedido = "T" OR ped-venda.tp-pedido = "p" THEN
              ASSIGN de-toppra = de-toppra + de-qtd-sld.
           ELSE
           IF ped-venda.cod-sit-aval = 4 THEN
              ASSIGN de-n-aprv = de-n-aprv + de-qtd-sld.
           ELSE
              ASSIGN de-outros = de-outros + de-qtd-sld.
           
           FIND ped-item-res OF ped-item NO-LOCK NO-ERROR.
           IF AVAIL ped-item-res THEN
              ASSIGN de-qtd-res = de-qtd-res + ped-item-res.qt-pedida.
       END.

    IF cota-item.item-adic6 <> "" THEN
       FOR EACH ped-item WHERE ped-item.it-codigo    = cota-item.item-adic6
                           AND ped-item.cod-sit-item < 3
                           AND ped-item.dt-entrega   < dt-entrega-lim
                         NO-LOCK:

           FIND ped-venda OF ped-item NO-LOCK.
           FIND emitente WHERE emitente.cod-emitente = ped-venda.cod-emitente
                         NO-LOCK.
           ASSIGN de-qtd-sld = ped-item.qt-pedida - 
                               ped-item.qt-atendida.
           IF emitente.natureza = 3 THEN
              ASSIGN de-export = de-export + de-qtd-sld.
           ELSE
           IF ped-venda.cod-cond-pag < 4 THEN
              ASSIGN de-avista = de-avista + de-qtd-sld.
           ELSE
           IF ped-venda.tp-pedido = "T" OR ped-venda.tp-pedido = "p" THEN
              ASSIGN de-toppra = de-toppra + de-qtd-sld.
           ELSE
           IF ped-venda.cod-sit-aval = 4 THEN
              ASSIGN de-n-aprv = de-n-aprv + de-qtd-sld.
           ELSE
              ASSIGN de-outros = de-outros + de-qtd-sld.

          FIND ped-item-res OF ped-item NO-LOCK NO-ERROR.
          IF AVAIL ped-item-res THEN
             ASSIGN de-qtd-res = de-qtd-res + ped-item-res.qt-pedida.
       END.

    put cota-item.it-codigo ";"
        ITEM.desc-item FORMAT "x(36)" ";" 
        de-export ";"
        de-avista ";"
        de-toppra ";"
        de-n-aprv ";"
        de-outros ";"
        (de-export + de-avista + de-toppra + de-n-aprv + de-outros) ";"
        de-qtd-res
        SKIP.
    ASSIGN de-export = 0
           de-avista = 0
           de-toppra = 0
           de-n-aprv = 0
           de-outros = 0
           de-qtd-res = 0.
END.
OUTPUT CLOSE.
