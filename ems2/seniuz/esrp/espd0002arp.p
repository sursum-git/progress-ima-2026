/* Temporary Table Definitions ---                                      */
{esinc/espd0002.i}

DEF VAR c-grupo-rep AS CHAR.
DEF VAR c-rep-grp1  AS CHAR INIT "20,65,68,19,39,28,112,2,12,162".
DEF VAR c-rep-grp2  AS CHAR INIT "119,11,27,161,82,107,81,5".
DEF VAR c-rep-grp3  AS CHAR INIT "102,122,150,101,163,165,3,158,9,175,130,100,160,154,72".

define temp-table tt-raw-digita 
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

define NEW GLOBAL SHARED temp-table tt-pedidos no-undo
       field nr-pedcli       like ped-venda.nr-pedcli
       field qt-aberto       LIKE ped-item.qt-atendida
       field perc-pronto     AS   DEC FORMAT ">>9.99"
       field qt-reserva      LIKE ped-item.qt-atendida
       FIELD it-ares         AS   INT
       FIELD atendido        AS   CHAR FORMAT "x(30)"
       FIELD marca           AS   LOG INIT YES
       index ch-pedido nr-pedcli.

def var l-passou as log.
def var i-itens-ares  as int.
def var de-tot-aux2 as dec.
def var de-tot-aux  as dec.
def var c-ficou-pronto as char format "x(30)".
def var da-ult-separ like ped-item-res.dt-trans init "01/01/0001".
DEF VAR l-continua AS LOG.
DEF VAR i-cont AS INT.
DEF VAR c-situacoes AS CHAR.

FOR EACH tt-pedidos.
    DELETE tt-pedidos.
END.

def var h-acomp     as handle  no-undo.

run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp(input "Relat¢rio").

for each ped-venda where
         ped-venda.cod-estabel  >= tt-param.estabel-ini      AND
         ped-venda.cod-estabel  <= tt-param.estabel-fin      AND
         ped-venda.nr-pedido    >= tt-param.pedido-ini       AND
         ped-venda.nr-pedido    <= tt-param.pedido-fin       AND  
         ped-venda.dt-emissao   >= tt-param.dt-emissao-ini   AND  
         ped-venda.dt-emissao   <= tt-param.dt-emissao-fin   AND  
         ped-venda.dt-entrega   >= tt-param.dt-entrega-ini   AND  
         ped-venda.dt-entrega   <= tt-param.dt-entrega-fin   AND  
         ped-venda.cod-emitente >= tt-param.cod-emit-ini     AND  
         ped-venda.cod-emitente <= tt-param.cod-emit-fin     AND  
         ped-venda.no-ab-reppri >= tt-param.no-ab-reppri-ini AND  
         ped-venda.no-ab-reppri <= tt-param.no-ab-reppri-fin AND 
         ped-venda.nome-transp  >= tt-param.nome-transp-ini  AND 
         ped-venda.nome-transp  <= tt-param.nome-transp-fin  NO-LOCK:

    RUN pi-acompanhar in h-acomp(input ped-venda.nr-pedido).

    FIND repres WHERE repres.nome-abrev = ped-venda.no-ab-reppri NO-LOCK NO-ERROR.
    IF AVAIL repres THEN DO:
       IF LOOKUP(STRING(repres.cod-rep),c-rep-grp1) <> 0 THEN
          ASSIGN c-grupo-rep = '1'.
       ELSE
       IF LOOKUP(STRING(repres.cod-rep),c-rep-grp2) <> 0 THEN
          ASSIGN c-grupo-rep = '2'.
       ELSE
       IF LOOKUP(STRING(repres.cod-rep),c-rep-grp3) <> 0 THEN
          ASSIGN c-grupo-rep = '3'.
       ELSE
       ASSIGN c-grupo-rep = '0'.

       IF c-grupo-rep < tt-param.grupo-rep-ini OR
          c-grupo-rep > tt-param.grupo-rep-fin THEN NEXT.
    END.

    /* Separa‡Æo Avulsa */ 
    IF NOT CAN-FIND(FIRST ped-item OF ped-venda) THEN DO.
       CREATE tt-pedidos.
       ASSIGN tt-pedidos.nr-pedcli = ped-venda.nr-pedcli
              tt-pedidos.atendido  = "Sep. Avulsa".
       NEXT.
    END.

    FIND emitente WHERE emitente.cod-emitente = ped-venda.cod-emitente
                  NO-LOCK NO-ERROR.
    IF AVAIL emitente THEN
       IF (emitente.pais =  "brasil" AND tt-param.mercado = "E") OR 
          (emitente.pais <> "brasil" AND tt-param.mercado = "I") THEN NEXT.

    ASSIGN c-situacoes = "SSSSSS".
    IF (tt-param.sit-total = NO AND
        NOT CAN-FIND(FIRST ped-item OF ped-venda WHERE
                           ped-item.cod-sit-item <> 3)) OR
       (tt-param.sit-total = YES AND
        NOT CAN-FIND(FIRST ped-item OF ped-venda WHERE
                           ped-item.cod-sit-item = 3)) THEN
       OVERLAY(c-situacoes,1,1,"S") = "N".

    IF (tt-param.sit-aberto = NO AND
        NOT CAN-FIND(FIRST ped-item OF ped-venda WHERE
                           ped-item.cod-sit-item <> 1)) OR
       (tt-param.sit-aberto = YES AND
        NOT CAN-FIND(FIRST ped-item OF ped-venda WHERE
                           ped-item.cod-sit-item = 1)) THEN
       OVERLAY(c-situacoes,2,1,"S") = "N".

    IF (tt-param.sit-parcial = NO AND
        NOT CAN-FIND(FIRST ped-item OF ped-venda WHERE
                           ped-item.cod-sit-item <> 2)) OR
       (tt-param.sit-parcial = YES AND
        NOT CAN-FIND(FIRST ped-item OF ped-venda WHERE
                           ped-item.cod-sit-item = 2)) THEN
       OVERLAY(c-situacoes,3,1,"S") = "N".

    IF (tt-param.sit-pendentes = NO AND
        NOT CAN-FIND(FIRST ped-item OF ped-venda WHERE
                           ped-item.cod-sit-item <> 4)) OR
       (tt-param.sit-pendentes = YES AND
        NOT CAN-FIND(FIRST ped-item OF ped-venda WHERE
                           ped-item.cod-sit-item = 4)) THEN
       OVERLAY(c-situacoes,4,1,"S") = "N".

    IF (tt-param.sit-suspensos = NO AND
        NOT CAN-FIND(FIRST ped-item OF ped-venda WHERE
                           ped-item.cod-sit-item <> 5)) OR
       (tt-param.sit-suspensos = YES AND
        NOT CAN-FIND(FIRST ped-item OF ped-venda WHERE
                           ped-item.cod-sit-item = 5)) THEN
       OVERLAY(c-situacoes,5,1,"S") = "N".

    IF (tt-param.sit-cancelados = NO AND
        NOT CAN-FIND(FIRST ped-item OF ped-venda WHERE
                           ped-item.cod-sit-item <> 6)) OR
       (tt-param.sit-cancelados = YES AND
        NOT CAN-FIND(FIRST ped-item OF ped-venda WHERE
                           ped-item.cod-sit-item = 6)) THEN
       OVERLAY(c-situacoes,6,1,"S") = "N".

    IF INDEX (c-situacoes,"S") = 0 THEN NEXT.

    IF ped-venda.tp-pedido <> tt-param.tp-pedido AND
        tt-param.tp-pedido <> "" THEN NEXT.

    IF tt-param.cond-credito = "A" AND
       (ped-venda.cod-sit-aval = 1 OR ped-venda.cod-sit-aval > 3) THEN NEXT.

    IF tt-param.cond-credito = "N" AND
       (ped-venda.cod-sit-aval <> 1 AND ped-venda.cod-sit-aval <= 3) THEN NEXT.
        
    IF tt-param.cond-pagto = "V" AND
       (ped-venda.cod-cond-pag <> 1 AND ped-venda.cod-cond-pag <> 2) THEN NEXT.

    IF tt-param.cond-pagto = "P" AND
       (ped-venda.cod-cond-pag = 1 OR ped-venda.cod-cond-pag = 2) THEN NEXT.
        
    IF (tt-param.aceita-parc = "A" AND ped-venda.ind-fat-par = NO) OR
       (tt-param.aceita-parc = "N" AND ped-venda.ind-fat-par = YES) THEN NEXT.
       
    ASSIGN i-itens-ares   = 0              de-tot-aux     = 0
           de-tot-aux2    = 0              c-ficou-pronto = "*"
           da-ult-separ   = 01/01/0001     l-passou       = no.

    FOR EACH ped-item WHERE
             ped-item.nome-abrev = ped-venda.nome-abrev AND
             ped-item.nr-pedcli  = ped-venda.nr-pedcli NO-LOCK:

        /*-- Verifica se ‚ ¡ndigo --*/ 
        FIND ITEM WHERE ITEM.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM THEN NEXT.
        /*
        FIND item-ext WHERE 
             item-ext.it-codigo = ITEM.it-codigo NO-LOCK NO-ERROR.
        IF AVAIL item-ext THEN
           IF (item-ext.indigo = NO  AND tt-param.so-indigo = YES) OR
              (item-ext.indigo = YES AND tt-param.exc-indigo = YES) THEN
              NEXT.
        */

        /*-- Verifica corte comercial --*/
        FIND ped-item-ext OF ped-item NO-LOCK NO-ERROR.
        IF AVAIL ped-item-ext THEN DO.
           IF ped-item-ext.corte-comerc < tt-param.corte-comerc-ini OR
              ped-item-ext.corte-comerc > tt-param.corte-comerc-fin THEN
              NEXT.
        END.

        ASSIGN l-continua = YES.
        DO i-cont = 1 TO EXTENT(tt-param.it-codigo).
           IF tt-param.it-codigo[i-cont] <> "" THEN DO:
              IF ITEM.it-codigo = tt-param.it-codigo[i-cont] THEN DO.
                 ASSIGN l-continua = YES.
                 LEAVE.
              END.
              ASSIGN l-continua = NO.
           END.
        END.
        IF NOT l-continua THEN NEXT.

        IF  tt-param.sit-total = NO 
        AND ped-item.cod-sit-item = 3 THEN NEXT.

        IF  tt-param.sit-aberto = NO
        AND ped-item.cod-sit-item = 1 THEN NEXT.

        if  tt-param.sit-parcial = no
        and ped-item.cod-sit-item = 2  then next.

        if  tt-param.sit-pendentes = no
        and ped-item.cod-sit-item = 4 then next.

        if  tt-param.sit-suspensos = no
        and ped-item.cod-sit-item = 5 then next.

        if  tt-param.sit-cancelados = no
        and ped-item.cod-sit-item = 6 then next.

        /*-- Verifica se reserva foi completada --*/
        FIND ped-item-res WHERE
             ped-item-res.nome-abrev   = ped-item.nome-abrev AND
             ped-item-res.nr-pedcli    = ped-item.nr-pedcli AND
             ped-item-res.it-codigo    = ped-item.it-codigo AND
             ped-item-res.cod-refer    = ped-item.cod-refer AND
             ped-item-res.nr-sequencia = ped-item.nr-sequencia NO-LOCK NO-ERROR.

        IF AVAIL ped-item-res AND tt-param.it-reservados = NO THEN NEXT.

        if avail ped-item-res and c-ficou-pronto <> "" then do:
           if da-ult-separ < ped-item-res.dt-trans then
              assign da-ult-separ = ped-item-res.dt-trans.
           assign c-ficou-pronto = "Pronto em: " + 
                  string(da-ult-separ,"99/99/9999") + " - " +
                  string(da-ult-separ -
                         ped-venda.dt-implant,"->>9") + " Dias".
        end.
        else
        if  ped-item.cod-sit-item <> 6 
        and ped-item.cod-sit-item <> 3 then 
            assign c-ficou-pronto = "".
        else
        if  ped-item.cod-sit-item =  6 
        and c-ficou-pronto        <> "" then do:
            if da-ult-separ < ped-item.dt-canseq then
               assign da-ult-separ = ped-item.dt-canseq.
            assign c-ficou-pronto = "Pronto em: " + 
                   string(da-ult-separ,"99/99/9999") + " - " +
                   string(da-ult-separ -
                          ped-venda.dt-implant,"->>9") + " Dias".
        end.
        /*----------------------------------------*/ 

        IF NOT AVAIL ped-item-res THEN
           IF ped-item.cod-sit-item < 3 THEN
              ASSIGN i-itens-ares = i-itens-ares + 1.
           
        IF tt-param.sit-total      = NO AND ped-item.cod-sit-item = 3
        OR tt-param.sit-cancelados = NO AND ped-item.cod-sit-item = 6
        OR (ped-item.qt-pedida -
            ped-item.qt-atendida +
            ped-item.qt-devolvida -
            ped-item.qt-pendente <= 0 AND tt-param.sit-total = NO) THEN 
            NEXT.
        
        assign l-passou = yes.
        for each ped-item-res
            where ped-item-res.nome-abrev   = ped-item.nome-abrev
              and ped-item-res.nr-pedcli    = ped-item.nr-pedcli
              and ped-item-res.nr-sequencia = ped-item.nr-sequencia
              and ped-item-res.it-codigo    = ped-item.it-codigo
              AND ped-item-res.cod-refer    = ped-item.cod-refer
              and ped-item-res.faturado     = no
                  no-lock:
             assign de-tot-aux = de-tot-aux + ped-item-res.qt-pedida.
        end.
        assign de-tot-aux2 = de-tot-aux2 + ped-item.qt-pedida
                                         - ped-item.qt-atendida.
    END.

    IF l-passou = NO THEN NEXT.

    ASSIGN l-passou = NO.

    IF ((de-tot-aux * 100) / de-tot-aux2) < tt-param.perc-minres THEN NEXT.
    
    IF de-tot-aux < de-tot-aux2 AND de-tot-aux > 0 AND
       de-tot-aux < tt-param.qtd-minima THEN NEXT.
    
    IF i-itens-ares < tt-param.min-it-ares 
    OR i-itens-ares > tt-param.max-it-ares THEN NEXT.

    CREATE tt-pedidos.
    ASSIGN tt-pedidos.nr-pedcli   = ped-venda.nr-pedcli
           tt-pedidos.qt-aberto   = de-tot-aux2
           tt-pedidos.perc-pronto = de-tot-aux * 100 / de-tot-aux2
           tt-pedidos.qt-reserva  = de-tot-aux
           tt-pedidos.it-ares     = i-itens-ares
           tt-pedidos.atendido    = c-ficou-pronto.
end.
run pi-finalizar in h-acomp.

