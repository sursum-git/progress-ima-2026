def var c-arquivo as char format "x(40)" init "c:/temp/pm-???mmaaaa.txt".
def var da-data-ini like nota-fiscal.dt-emis-nota label "Data inicial".
def var da-data-fim like nota-fiscal.dt-emis-nota label "Data final".

def temp-table w-work
    field it-codigo    like it-nota-fisc.it-codigo
    FIELD cod-refer    LIKE it-nota-fisc.cod-refer
    field aliquota-icm like it-nota-fisc.aliquota-icm
    field quantidade   as dec format ">>>,>>>,>>9.99"
    field valor        as dec format ">>>,>>>,>>9.99"
    INDEX ch-work it-codigo
                  cod-refer
                  aliquota-icm.
    
update da-data-ini
       da-data-fim.

/* ------------ Topazio --------------*/       
assign c-arquivo = "c:/temp/pm-top" + 
                   string(month(da-data-ini),"99") +
                   string(year(da-data-ini),"9999") + 

                   ".txt".
output to value(c-arquivo).
put "Tear - Preco medio de venda por aliquota de ICM - Topazio - " 
    da-data-ini " a " da-data-fim
    skip(1).

for each nota-fiscal where nota-fiscal.cod-estabel  = "2"
                       and nota-fiscal.serie        = "1"
                       and nota-fiscal.dt-emis-nota >= da-data-ini
                       and nota-fiscal.dt-emis-nota <= da-data-fim
                       and nota-fiscal.esp-docto     = 22 /*"nfs"*/
                       and nota-fiscal.dt-cancela    = ?
                     no-lock,
    
    each ped-venda where ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
                     and ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli
                     and (ped-venda.tp-pedido = "T" or  
                          ped-venda.tp-pedido = "W")
                   no-lock:
    
    for each it-nota-fisc of nota-fiscal no-lock,
        each natur-oper 
             where natur-oper.nat-operacao = it-nota-fisc.nat-operacao
               and natur-oper.emite-duplic = yes
             no-lock:
        
        find first w-work where w-work.it-codigo    = it-nota-fisc.it-codigo
                            AND w-work.cod-refer    = it-nota-fisc.cod-refer
                            and w-work.aliquota-icm = it-nota-fisc.aliquota-icm
                          no-lock no-error.
                          
        if not avail w-work then do:
           create w-work.
           assign w-work.it-codigo    = it-nota-fisc.it-codigo
                  w-work.cod-refer    = it-nota-fisc.cod-refer
                  w-work.aliquota-icm = it-nota-fisc.aliquota-icm
                  w-work.quantidade   = 0
                  w-work.valor        = 0.
        end.
        assign w-work.quantidade = w-work.quantidade +
                                   it-nota-fisc.qt-faturada[1]
               w-work.valor      = w-work.valor + (it-nota-fisc.vl-preori *
                                   it-nota-fisc.qt-faturada[1]).
    end.
end.

for each w-work:
    find item where item.it-codigo = w-work.it-codigo no-lock.
    display w-work.it-codigo      label "Item" FORMAT "x(6)"
            item.descricao-1      label "Descricao"
            w-work.cod-refer      LABEL "Refer"
            w-work.aliquota-icm   label "% ICM"
            w-work.quantidade     label "Quantidade"
            w-work.valor          label "Valor"
            w-work.valor /
            w-work.quantidade     label "P.Medio"
            with width 132.
end.
output close.
for each w-work:
    delete w-work.
end.

/* ------------- Prata ---------------*/       
assign c-arquivo = "c:/temp/pm-pra" + 
                   string(month(da-data-ini),"99") +
                   string(year(da-data-ini),"9999") + 
                   ".txt".
output to value(c-arquivo).
put "Tear - Preco medio de venda por aliquota de ICM - Prata - " 
    da-data-ini " a " da-data-fim
    skip(1).

for each nota-fiscal where nota-fiscal.cod-estabel  = "2"
                       and nota-fiscal.serie        = "1"
                       and nota-fiscal.dt-emis-nota >= da-data-ini
                       and nota-fiscal.dt-emis-nota <= da-data-fim
                       and nota-fiscal.esp-docto     = 22 /*"nfs"*/
                       and nota-fiscal.dt-cancela    = ?
                     no-lock,
    
    each ped-venda where ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
                     and ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli
                     and ped-venda.tp-pedido = "P"  
                   no-lock:
    
    for each it-nota-fisc of nota-fiscal no-lock,
        each natur-oper 
             where natur-oper.nat-operacao = it-nota-fisc.nat-operacao
               and natur-oper.emite-duplic = yes
             no-lock:
        
        find first w-work where w-work.it-codigo    = it-nota-fisc.it-codigo
                            AND w-work.cod-refer    = it-nota-fisc.cod-refer
                            and w-work.aliquota-icm = it-nota-fisc.aliquota-icm
                          no-lock no-error.
                          
        if not avail w-work then do:
           create w-work.
           assign w-work.it-codigo    = it-nota-fisc.it-codigo
                  w-work.cod-refer    = it-nota-fisc.cod-refer
                  w-work.aliquota-icm = it-nota-fisc.aliquota-icm
                  w-work.quantidade   = 0
                  w-work.valor        = 0.
        end.
        assign w-work.quantidade = w-work.quantidade +
                                   it-nota-fisc.qt-faturada[1]
               w-work.valor      = w-work.valor + (it-nota-fisc.vl-preori *
                                   it-nota-fisc.qt-faturada[1]).
    end.
end.

for each w-work:
    find item where item.it-codigo = w-work.it-codigo no-lock.
    display w-work.it-codigo      label "Item" FORMAT "x(6)"
            item.descricao-1      label "Descricao"
            w-work.cod-refer      LABEL "Refer"
            w-work.aliquota-icm   label "% ICM"
            w-work.quantidade     label "Quantidade"
            w-work.valor          label "Valor"
            w-work.valor /
            w-work.quantidade     label "P.Medio"
            with width 132.
end.
output close.
for each w-work:
    delete w-work.
end.

/* ------------- Normal --------------*/       
assign c-arquivo = "c:/temp/pm-nor" + 
                   string(month(da-data-ini),"99") +
                   string(year(da-data-ini),"9999") + 
                   ".txt".
output to value(c-arquivo).
put "Tear - Preco medio de venda por aliquota de ICM - Normal - " 
    da-data-ini " a " da-data-fim
    skip(1).

for each nota-fiscal where nota-fiscal.cod-estabel  = "2"
                       and nota-fiscal.serie        = "1"
                       and nota-fiscal.dt-emis-nota >= da-data-ini
                       and nota-fiscal.dt-emis-nota <= da-data-fim
                       and nota-fiscal.esp-docto     = 22 /*"nfs"*/
                       and nota-fiscal.dt-cancela    = ?
                     no-lock,
    
    each ped-venda where ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
                     and ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli
                     and ped-venda.tp-pedido  <> "W"
                     and ped-venda.tp-pedido  <> "T"
                     and ped-venda.tp-pedido  <> "P"
                   no-lock:
    
    for each it-nota-fisc of nota-fiscal no-lock,
        each natur-oper 
             where natur-oper.nat-operacao = it-nota-fisc.nat-operacao
               and natur-oper.emite-duplic = yes
             no-lock:
        
        find first w-work where w-work.it-codigo    = it-nota-fisc.it-codigo
                            AND w-work.cod-refer    = it-nota-fisc.cod-refer
                            and w-work.aliquota-icm = it-nota-fisc.aliquota-icm
                          no-lock no-error.
                          
        if not avail w-work then do:
           create w-work.
           assign w-work.it-codigo    = it-nota-fisc.it-codigo
                  w-work.cod-refer    = it-nota-fisc.cod-refer
                  w-work.aliquota-icm = it-nota-fisc.aliquota-icm
                  w-work.quantidade   = 0
                  w-work.valor        = 0.
        end.
        assign w-work.quantidade = w-work.quantidade +
                                   it-nota-fisc.qt-faturada[1]
               w-work.valor      = w-work.valor + (it-nota-fisc.vl-preori *
                                   it-nota-fisc.qt-faturada[1]).
    end.
end.

for each w-work:
    find item where item.it-codigo = w-work.it-codigo no-lock.
    display w-work.it-codigo      label "Item" FORMAT "x(6)"
            item.descricao-1      label "Descricao"
            w-work.cod-refer      LABEL "Refer"
            w-work.aliquota-icm   label "% ICM"
            w-work.quantidade     label "Quantidade"
            w-work.valor          label "Valor"
            w-work.valor /
            w-work.quantidade     label "P.Medio"
            with width 132.
end.
output close.
for each w-work:
    delete w-work.
end.

/* ------------- Todos ---------------*/       
assign c-arquivo = "c:/temp/pm-tod" + 
                   string(month(da-data-ini),"99") +
                   string(year(da-data-ini),"9999") + 
                   ".txt".
output to value(c-arquivo).
put "Tear - Preco medio de venda por aliquota de ICM - Todos - " 
    da-data-ini " a " da-data-fim
    skip(1).

for each nota-fiscal where nota-fiscal.cod-estabel  = "2"
                       and nota-fiscal.serie        = "1"
                       and nota-fiscal.dt-emis-nota >= da-data-ini
                       and nota-fiscal.dt-emis-nota <= da-data-fim
                       and nota-fiscal.esp-docto     = 22 /*"nfs"*/
                       and nota-fiscal.dt-cancela    = ?
                     no-lock:
    
    for each it-nota-fisc of nota-fiscal no-lock,
        each natur-oper 
             where natur-oper.nat-operacao = it-nota-fisc.nat-operacao
               and natur-oper.emite-duplic = yes
             no-lock:
        
        find first w-work where w-work.it-codigo    = it-nota-fisc.it-codigo
                            AND w-work.cod-refer    = it-nota-fisc.cod-refer
                            and w-work.aliquota-icm = it-nota-fisc.aliquota-icm
                          no-lock no-error.
                          
        if not avail w-work then do:
           create w-work.
           assign w-work.it-codigo    = it-nota-fisc.it-codigo
                  w-work.cod-refer    = it-nota-fisc.cod-refer
                  w-work.aliquota-icm = it-nota-fisc.aliquota-icm
                  w-work.quantidade   = 0
                  w-work.valor        = 0.
        end.
        assign w-work.quantidade = w-work.quantidade +
                                   it-nota-fisc.qt-faturada[1]
               w-work.valor      = w-work.valor + (it-nota-fisc.vl-preori *
                                   it-nota-fisc.qt-faturada[1]).
    end.
end.

for each w-work:
    find item where item.it-codigo = w-work.it-codigo no-lock.
    display w-work.it-codigo      label "Item" FORMAT "x(6)"
            item.descricao-1      label "Descricao"
            w-work.cod-refer      LABEL "Refer"
            w-work.aliquota-icm   label "% ICM"
            w-work.quantidade     label "Quantidade"
            w-work.valor          label "Valor"
            w-work.valor /
            w-work.quantidade     label "P.Medio"
            with width 132.
end.
output close.
for each w-work:
    delete w-work.
end.
