DEF TEMP-TABLE tt-cond-ped NO-UNDO LIKE cond-ped
    FIELD r-rowid AS ROWID.

DEF VAR i-prazo-medio AS INT.
DEF VAR hBoCondPagtoPed     AS HANDLE NO-UNDO.

DEF VAR de-tot-prazo        LIKE cond-ped.nr-dias-venc.
DEF VAR i-ct AS INT.
DEF VAR i-prazo-medio-ori AS INT.

RUN esbo/boCondPagtoPed.p PERSISTENT SET hboCondPagtoPed.

FOR EACH ped-venda WHERE
         ped-venda.cod-sit-ped = 1 NO-LOCK.

    FIND ped-venda-ext WHERE
         ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND
         ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.

    IF ped-venda.cod-port = 9 THEN NEXT.


    DISP ped-venda.nr-pedcli.

    ASSIGN de-tot-prazo = 0
           i-ct = 0.

    FOR EACH cond-ped OF ped-venda NO-LOCK.

        MESSAGE cond-ped.data-pagto
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

        CREATE tt-cond-ped.
        BUFFER-COPY cond-ped TO tt-cond-ped.


        IF cond-ped.data-pagto <> ? THEN
           ASSIGN de-tot-prazo = de-tot-prazo + (cond-ped.data-pagto - ped-venda.dt-implant).
        ELSE
           ASSIGN de-tot-prazo = de-tot-prazo + cond-ped.nr-dias-venc.


        ASSIGN i-ct = i-ct + 1.
    END.


    ASSIGN i-prazo-medio-ori = de-tot-prazo / i-ct.


    RUN setTipoCalc IN hBoCondPagtoPed(3).
    RUN setTTCondPed IN hBoCondPagtoPed(INPUT TABLE tt-cond-ped).
    RUN calcularPrazoMedio IN hBoCondPagtoPed.
    RUN getPrazoMedio IN hBoCondPagtoPed(OUTPUT i-prazo-medio).

    DISP ped-venda-ext.tp-pagto
         ped-venda.cod-port
         i-prazo-medio
         i-prazo-medio-ori.

   // ASSIGN ped-venda.cod-port = 9.
END.
