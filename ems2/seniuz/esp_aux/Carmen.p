def var de-tot-valor as dec  format ">>>,>>>,>>9.99".
def var de-tot-geral as dec  format ">>>,>>>,>>9.99". 
def var de-tot-dia   as dec  format ">>>,>>>,>>9.99". 


form
    nota-fiscal.dt-emis-nota  column-label "Data Emissao"
    it-nota-fisc.class-fiscal column-label "Class.Fiscal"
    classif-fisc.descricao    column-label "Descricao"
    de-tot-valor              column-label "Valor Total"
    with no-box 55 down width 132 with frame f-detalhe.

OUTPUT TO "c:/lixo/teste.txt".         

for each nota-fiscal where nota-fiscal.cod-estabel =  "3"
                      and nota-fiscal.dt-emis-nota >= 11/01/2005
                      and nota-fiscal.dt-emis-nota <= 11/30/2005
                      and nota-fiscal.esp-docto    =  22
                      and nota-fiscal.serie        =  "ld"
                      and nota-fiscal.dt-cancela   =  ?
                          no-lock,
    each it-nota-fisc of nota-fiscal no-lock
                         break by nota-fiscal.dt-emis-nota
                               by it-nota-fisc.class-fiscal:
  
    assign de-tot-valor = de-tot-valor + it-nota-fisc.vl-merc-liq.
    assign de-tot-dia   = de-tot-dia   + it-nota-fisc.vl-merc-liq.
     
    if first-of(nota-fisca.dt-emis-nota) then
       display nota-fiscal.dt-emis-nota
               with frame f-detalhe.
               
    if last-of(it-nota-fisc.class-fiscal) then do:
       find first classif-fisc
            where classif-fisc.class-fiscal = it-nota-fisc.class-fiscal
                  no-lock no-error.
    
       display it-nota-fisc.class-fiscal
               classif-fisc.descricao
               de-tot-valor
               with frame f-detalhe.
       down with frame f-detalhe.
    
       assign de-tot-geral = de-tot-geral + de-tot-valor
              de-tot-valor = 0.
    end.
       
    if last-of(nota-fiscal.dt-emis-nota) then do:
       display "Total Dia" @ nota-fiscal.dt-emis-nota
               de-tot-dia  @ de-tot-valor
               with frame f-detalhe.
       down 2 with frame f-detalhe.
       assign de-tot-dia = 0.
    end.   
end.

display "Total"      @ nota-fiscal.dt-emis-nota
        de-tot-geral @ de-tot-valor
        WITH FRAME f-detalhe.

assign de-tot-valor = 0
       de-tot-dia   = 0
       de-tot-geral = 0.       

output close.
