DEF VAR dt-ult-mov LIKE titulo.dt-emissao.
DEF VAR de-deb-aberto AS DEC FORMAT ">>>,>>>,>>9.99".
DEF VAR de-ped-aberto AS DEC FORMAT ">>>,>>>,>>9.99".
DEF VAR l-vendor AS LOG FORMAT "Sim/Nao".
DEF VAR c-nom-rep LIKE repres.nome.

OUTPUT TO "c:\temp\clientes.txt".

PUT "Codigo;"
    "Ult.Fatura;"
    "Nome;"
    "Repres;" 
    "Lim.Credito;"
    "Deb.Aberto;"
    "Ped.Aberto;"
    "Vendor"
    SKIP.

FOR EACH emitente WHERE emitente.identific <> 2 NO-LOCK.
    ASSIGN dt-ult-mov = ?.
    FIND FIRST titulo WHERE titulo.cod-emitente = emitente.cod-emitente
                        AND titulo.dt-emissao   > 12/31/2004
                      NO-LOCK NO-ERROR.
    IF NOT AVAIL titulo THEN 
       NEXT.
    ELSE
       ASSIGN dt-ult-mov = titulo.dt-emissao.

    FIND FIRST titulo WHERE titulo.cod-emitente = emitente.cod-emitente
                        AND ((titulo.vl-saldo > 0 AND titulo.dt-vencimen  < (TODAY - 2)) OR
                             (titulo.vl-saldo = 0 AND titulo.dt-ult-pagto > (titulo.dt-vencimen + 2)))
                       NO-LOCK NO-ERROR.
    IF AVAIL titulo THEN NEXT.
    ASSIGN l-vendor       = NO
           de-deb-aberto = 0
           de-ped-aberto = 0
           c-nom-rep     = "".
    FIND repres WHERE repres.cod-rep = emitente.cod-rep NO-LOCK NO-ERROR.
    IF AVAIL repres THEN
       ASSIGN c-nom-rep = repres.nome.
    FOR EACH titulo WHERE titulo.cod-emitente = emitente.cod-emitente NO-LOCK.
        ASSIGN de-deb-aberto = de-deb-aberto + titulo.vl-saldo.
    END.
    FOR EACH ped-venda WHERE ped-venda.nome-abrev  = emitente.nome-abrev
                         AND ped-venda.cod-sit-ped < 3
                       NO-LOCK.
        FOR EACH ped-item OF ped-venda WHERE ped-item.cod-sit-item < 3 NO-LOCK.
            ASSIGN de-ped-aberto = de-ped-aberto + (ped-item.qt-pedida - ped-item.qt-atendida).
        END.
        IF ped-venda.tp-pedido = "v" THEN
           ASSIGN l-vendor = YES.
    END.
    PUT emitente.cod-emitente ";"
        dt-ult-mov ";"
        emitente.nome-emit ";"
        c-nom-rep ";" 
        emitente.lim-credito ";"
        de-deb-aberto ";"
        de-ped-aberto ";"
        l-vendor
        SKIP.
END.
OUTPUT CLOSE.
