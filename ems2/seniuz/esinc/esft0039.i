assign de-vl-total = 0.

for each w-work where w-work.brasil = {1}:
    assign de-vl-total = de-vl-total + w-work.valor.
end.

FOR EACH w-work WHERE
         w-work.valor      > 0 AND
         w-work.quantidade > 0 AND
         w-work.brasil     = {1} 
         break by w-work.un
               BY w-work.aliquota-icm
               by w-work.it-codigo
               BY w-work.cod-refer:
    find item where item.it-codigo = w-work.it-codigo no-lock.
    find first w-work2 where w-work2.un           = w-work.un
                         and w-work2.aliquota-icm = w-work.aliquota-icm
                         and w-work2.brasil       = w-work.brasil.
    assign i-prazo        = w-work.prazo / w-work.valor
           de-perc-qtd    = (w-work.quantidade * 100) / w-work2.quantidade
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
    if  w-work.valor-ant      = 0
    or  w-work.quantidade-ant = 0 then
        assign de-perc-medio = 0.
    assign i-tot-prazo-un  = i-tot-prazo-un + (i-prazo * w-work.valor)
           i-tot-prazo-ger = i-tot-prazo-ger + (i-prazo * w-work.valor)
           i-tot-prazo     = i-tot-prazo + (i-prazo * w-work.valor).
    DISPLAY w-work.it-codigo
            w-work.cod-refer
            item.desc-item   
            w-work.aliquota-icm
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
    assign de-tot-valor-un        = de-tot-valor-un + w-work.valor
            de-tot-valor-ger      = de-tot-valor-ger + w-work.valor
            de-tot-qtd-un         = de-tot-qtd-un + w-work.quantidade
            de-tot-qtd-ger        = de-tot-qtd-ger + w-work.qt-soma
            de-tot-quantidade     = de-tot-quantidade + w-work.qt-soma
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

