assign de-saldo-item = de-qtidade-atu - de-res-item
       c-descricao   = item.descricao-1 + item.descricao-2.
assign de-nao-reserva = de-car-item - de-res-item.
if de-nao-reserva < 0 then
   assign de-nao-reserva = 0.    
       
assign de-tot-est    = de-saldo-item - de-nao-reserva.

if de-tot-est >= de-sld-min then do:      
      
   assign l-lista-item = yes.

   if (de-saldo-item < 0 and tt-param.l-sldneg = no)
   or (de-saldo-item = 0 and tt-param.l-sldzer = no)
   or (de-saldo-item > 0 and tt-param.l-sldpos = no) then
      assign l-lista-item = no.

   if l-prim-vez = yes then
      assign c-item-ant = item.it-codigo
             l-prim-vez = no.

   if c-item-ant <> item.it-codigo and l-lista-item = yes then do:
      assign c-item-ant = item.it-codigo.
      down 1 with frame f-detalhe.
   end.

   if l-lista-item then do:
      display c-cod-estabel
              item.it-codigo
              item-ext.colecao
              c-descricao
              item.un
              de-saldo-item when tt-param.c-opc-rel = "D" or tt-param.c-opc-rel = "A"
              c-cod-refer
              de-tot-est    when tt-param.c-opc-rel = "V" or tt-param.c-opc-rel = "A"
              with frame f-detalhe.
      down with frame f-detalhe.
   
      assign de-tot-qtd-fam = de-tot-qtd-fam + de-qtidade-atu
             de-tot-res-fam = de-tot-res-fam + de-res-item
             de-tot-vda-fam = de-tot-vda-fam + de-tot-est
             l-lista-fam    = yes.
      if item.un = "M" then 
         assign de-qtd-ger-m = de-qtd-ger-m + de-saldo-item
                de-qtd-vda-m = de-qtd-vda-m + de-tot-est.
      else
         assign de-qtd-ger-kg = de-qtd-ger-kg + de-saldo-item
                de-qtd-vda-kg = de-qtd-vda-kg + de-tot-est.
                
      if saldo-estoq.cod-refer = "peca" then
         assign de-fam-peca-vda = de-fam-peca-vda + de-tot-est
                de-fam-peca-dis = de-fam-peca-dis + de-saldo-item
                de-tot-peca-vda = de-tot-peca-vda + de-tot-est
                de-tot-peca-dis = de-tot-peca-dis + de-saldo-item.
      else
         assign de-fam-rolo-vda = de-fam-rolo-vda + de-tot-est
                de-fam-rolo-dis = de-fam-rolo-dis + de-saldo-item
                de-tot-rolo-vda = de-tot-rolo-vda + de-tot-est
                de-tot-rolo-dis = de-tot-rolo-dis + de-saldo-item.
   end.
end.

assign de-qtidade-atu = 0
       de-res-item    = 0
       de-car-item    = 0 
       de-saldo-item  = 0
       de-tot-est     = 0.


