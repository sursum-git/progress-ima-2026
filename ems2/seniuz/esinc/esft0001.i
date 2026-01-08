assign de-vl-total = 0.

for each w-work where (w-work.brasil = {1} AND {2} = NO)
                   or (w-work.indigo = {2} AND {2} = YES): 
    assign de-vl-total = de-vl-total + w-work.valor.
end.
/*
for each w-work where w-work.brasil = {1}.
    assign de-vl-total = de-vl-total + w-work.valor.
end.
*/
FOR each w-work where (w-work.brasil = {1} AND {2} = NO)
                   or (w-work.indigo = {2} AND {2} = YES)
                break by w-work.un
                      by w-work.it-codigo:
    find item where item.it-codigo = w-work.it-codigo no-lock.
    find first w-work2 where w-work2.un = w-work.un
                         and w-work2.brasil = w-work.brasil.
    assign i-prazo = w-work.prazo / w-work.valor
           de-perc-qtd = (w-work.quantidade * 100) / w-work2.quantidade
           de-preco-medio = w-work.valor / w-work.quantidade.
    if  i-prazo = ? then
        assign i-prazo = 0.
    if  de-preco-medio = ? then
        assign de-preco-medio = 0.
    assign de-preco-medio-ant = w-work.valor-ant / w-work.quantidade-ant.
    if  de-preco-medio-ant = ? then
        assign de-preco-medio-ant = 0.
    assign de-perc-medio = ((de-preco-medio  / de-preco-medio-ant) - 1) * 100
           de-perc-valor = (w-work.valor * 100) / de-vl-total.
    if  de-perc-medio > 999
    or  de-perc-medio < -999 then
        assign de-perc-medio = 999.
    if  w-work.valor-ant = 0
    or  w-work.quantidade-ant = 0 then
        assign de-perc-medio = 0.
    assign i-tot-prazo-un  = i-tot-prazo-un + (i-prazo * w-work.valor)
           i-tot-prazo-ger = i-tot-prazo-ger + (i-prazo * w-work.valor)
           i-tot-prazo     = i-tot-prazo + (i-prazo * w-work.valor).
    DISPLAY w-work.it-codigo
            item.desc-item
            i-prazo
            w-work.quantidade
            de-perc-qtd
            w-work.un
            de-preco-medio
            de-perc-medio
            w-work.valor
            de-perc-valor
            with frame f-detalhe.
    down with frame f-detalhe.
    
    IF tt-param.gerar-excel THEN DO:
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 1
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-valor  = w-work.it-codigo.
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 2
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-valor  = item.desc-item.
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num    = 1
              tt-dados.planilha-num   = 1
              tt-dados.celula-coluna  = 3
              tt-dados.celula-linha   = i-lin-excel
              tt-dados.celula-formato = "##0"
              tt-dados.celula-alinhamento-horizontal = 1
              tt-dados.celula-valor   = string(i-prazo).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num    = 1
              tt-dados.planilha-num   = 1
              tt-dados.celula-coluna  = 4
              tt-dados.celula-linha   = i-lin-excel
              tt-dados.celula-formato = "###.###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor   = string(w-work.quantidade).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 5
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(de-perc-qtd).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 6
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-alinhamento-horizontal = 1
              tt-dados.celula-valor  = w-work.un.
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 7
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "##.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(de-preco-medio).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 8
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(de-perc-medio).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 9
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "###.###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(w-work.valor).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 10
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(de-perc-valor).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num    = 1
              tt-dados.planilha-num   = 1
              tt-dados.celula-coluna  = 11
              tt-dados.celula-linha   = i-lin-excel
              tt-dados.celula-formato = "###.###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor   = string(w-work.quantidade-conv).
       ASSIGN i-lin-excel = i-lin-excel + 1.
    END.

    assign de-tot-valor-un       = de-tot-valor-un + w-work.valor
           de-tot-valor-ger      = de-tot-valor-ger + w-work.valor
           de-tot-qtd-un         = de-tot-qtd-un + w-work.quantidade
           de-tot-qtd-ger        = de-tot-qtd-ger + w-work.quantidade
           de-tot-quantidade     = de-tot-quantidade + w-work.quantidade
           de-tot-valor          = de-tot-valor + w-work.valor
           de-valor-ant          = de-valor-ant + w-work.valor-ant
           de-tot-quantidade-ant = de-tot-quantidade-ant + w-work.quantidade-ant
           de-tot-valor-ant      = de-tot-valor-ant + w-work.valor-ant
           de-quantidade-ant     = de-quantidade-ant + w-work.quantidade-ant.
    if  last-of(w-work.un) then do:
        assign de-perc-qtd    = 100
               de-perc-valor  = (de-tot-valor-un * 100) / de-vl-total
               de-preco-medio = de-tot-valor-un / de-tot-qtd-un.
        if  de-preco-medio = ? then
            assign de-preco-medio = 0.
        assign de-preco-medio-ant = w-work2.valor-ant / w-work2.quantidade-ant.
        if  de-preco-medio-ant = ? then
            assign de-preco-medio-ant = 0.
        assign de-perc-medio = ((de-preco-medio  / de-preco-medio-ant) - 1) * 100.
        if  de-perc-valor = ? then
            assign de-perc-valor = 0.
        if  de-perc-medio > 999
        or  de-perc-medio < -999 then
            assign de-perc-medio = 999.
        if  de-perc-medio = ? then
            assign de-perc-medio = 0.
        assign i-prazo = i-tot-prazo-un / de-tot-valor-un.
        if  i-prazo = ? then
            assign i-prazo = 0.
        DISPLAY "Total da unidade" @ item.desc-item
                de-tot-qtd-un      @ w-work.quantidade
                i-prazo
                de-perc-qtd
                de-preco-medio
                de-perc-medio
                de-tot-valor-un    @ w-work.valor
                de-perc-valor
                with frame f-detalhe.
        down with frame f-detalhe.
        
        IF tt-param.gerar-excel THEN DO:
           CREATE tt-dados.
           ASSIGN tt-dados.arquivo-num   = 1
                  tt-dados.planilha-num  = 1
                  tt-dados.celula-coluna = 2
                  tt-dados.celula-linha  = i-lin-excel
                  tt-dados.celula-valor  = "Total da unidade".
           CREATE tt-dados.
           ASSIGN tt-dados.arquivo-num    = 1
                  tt-dados.planilha-num   = 1
                  tt-dados.celula-coluna  = 3
                  tt-dados.celula-linha   = i-lin-excel
                  tt-dados.celula-formato = "##0"
                  tt-dados.celula-alinhamento-horizontal = 1
                  tt-dados.celula-valor   = string(i-prazo).
           CREATE tt-dados.
           ASSIGN tt-dados.arquivo-num    = 1
                  tt-dados.planilha-num   = 1
                  tt-dados.celula-coluna  = 4
                  tt-dados.celula-linha   = i-lin-excel
                  tt-dados.celula-formato = "###.###.##0,00"
                  tt-dados.celula-alinhamento-horizontal = 5
                  tt-dados.celula-valor   = string(de-tot-qtd-un).
           CREATE tt-dados.
           ASSIGN tt-dados.arquivo-num    = 1
                  tt-dados.planilha-num   = 1
                  tt-dados.celula-coluna  = 5
                  tt-dados.celula-linha   = i-lin-excel
                  tt-dados.celula-formato = "##0,00"
                  tt-dados.celula-alinhamento-horizontal = 5
                  tt-dados.celula-valor   = string(de-perc-qtd).
           CREATE tt-dados.
           ASSIGN tt-dados.arquivo-num   = 1
                  tt-dados.planilha-num  = 1
                  tt-dados.celula-coluna = 7
                  tt-dados.celula-linha  = i-lin-excel
                  tt-dados.celula-formato = "##.##0,00"
                  tt-dados.celula-alinhamento-horizontal = 5
                  tt-dados.celula-valor  = string(de-preco-medio).
           CREATE tt-dados.
           ASSIGN tt-dados.arquivo-num   = 1
                  tt-dados.planilha-num  = 1
                  tt-dados.celula-coluna = 8
                  tt-dados.celula-linha  = i-lin-excel
                  tt-dados.celula-formato = "##0,00"
                  tt-dados.celula-alinhamento-horizontal = 5
                  tt-dados.celula-valor  = string(de-perc-medio).
           CREATE tt-dados.
           ASSIGN tt-dados.arquivo-num   = 1
                  tt-dados.planilha-num  = 1
                  tt-dados.celula-coluna = 9
                  tt-dados.celula-linha  = i-lin-excel
                  tt-dados.celula-formato = "###.###.##0,00"
                  tt-dados.celula-alinhamento-horizontal = 5
                  tt-dados.celula-valor  = string(de-tot-valor-un).
           CREATE tt-dados.
           ASSIGN tt-dados.arquivo-num   = 1
                  tt-dados.planilha-num  = 1
                  tt-dados.celula-coluna = 10
                  tt-dados.celula-linha  = i-lin-excel
                  tt-dados.celula-formato = "##0,00"
                  tt-dados.celula-alinhamento-horizontal = 5
                  tt-dados.celula-valor  = string(de-perc-valor).
           ASSIGN i-lin-excel = i-lin-excel + 2.
        END.

        assign de-tot-qtd-un   = 0
               i-tot-prazo-un  = 0
               de-tot-valor-un = 0.
    end.
end.
assign de-perc-qtd    = 100
       de-perc-valor  = 100
       de-preco-medio = de-tot-valor-ger / de-tot-qtd-ger.
if  de-preco-medio = ? then
    assign de-preco-medio = 0.
assign de-preco-medio-ant = de-valor-ant / de-quantidade-ant.
if  de-preco-medio-ant = ? then
    assign de-preco-medio-ant = 0.
assign de-perc-medio = ((de-preco-medio / de-preco-medio-ant) - 1) * 100.
if  de-perc-medio > 999
or  de-perc-medio < -999 then
    assign de-perc-medio = 999.
if  de-perc-medio = ? then
    assign de-perc-medio = 0.
assign i-prazo = i-tot-prazo-ger / de-tot-valor-ger.
if  i-prazo = ? then
    assign i-prazo = 0.
disp "Total Faturamento" @ item.desc-item
      de-tot-qtd-ger     @ w-work.quantidade
      i-prazo
      de-perc-qtd
      de-preco-medio
      de-perc-medio
      de-tot-valor-ger   @ w-work.valor
      de-perc-valor
      with frame f-detalhe.
down with frame f-detalhe.
assign de-tot-qtd-ger    = 0
       i-tot-prazo-ger   = 0
       de-tot-valor-ger  = 0
       de-valor-ant      = 0
       de-quantidade-ant = 0.

