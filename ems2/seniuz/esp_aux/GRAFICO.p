/* Programa: grafico.p
**
*/

{utp/utapi011.i}

Create tt-atributos.
Assign tt-atributos.cod-versao-integracao = 3
       tt-atributos.graphtype             = 3
       tt-atributos.datalabels            = 1
       tt-atributos.graphtitle            = 'Metros Vendidos X Cancelados X Faturados.'
       tt-atributos.lefttitle             = 'Quantidade em METROS.'
       tt-atributos.lefttitlestyle        = 1

       tt-atributos.bottomtitle           = 'D I A S'
       tt-atributos.numgraph              = 1.

Create tt-sets.
Assign tt-sets.NumSet   = 1 
       tt-sets.NumGraph = 1
       tt-sets.ColorSet = 1
       tt-sets.legendText = "Metros Vendidos".

Create tt-sets.
Assign tt-sets.NumSet   = 2
       tt-sets.NumGraph = 1
       tt-sets.ColorSet = 2
       tt-sets.legendText = "Metros Cancelados".

Create tt-sets.
Assign tt-sets.NumSet   = 3 
       tt-sets.NumGraph = 1
       tt-sets.ColorSet = 4
       tt-sets.legendText = "Metros Faturados".

create tt-points-2.
assign tt-points-2.NumPoint  = 1
       tt-points-2.NumGraph  = 1
       tt-points-2.labeltext = "01".
create tt-points-2.
assign tt-points-2.NumPoint  = 2 
       tt-points-2.NumGraph  = 1
       tt-points-2.labeltext = "02".

create tt-points-2.
assign tt-points-2.NumPoint  = 3
       tt-points-2.NumGraph  = 1
       tt-points-2.labeltext = "03".
create tt-points-2.

/* Valores dos Metros Vendidos */
Create tt-dados.
Assign tt-dados.NumPoint   = 1
       tt-dados.NumSet     = 1
       tt-dados.NumGraph   = 1
       tt-dados.graphdata  = 1000.
Create tt-dados.
Assign tt-dados.NumPoint   = 2
       tt-dados.NumSet     = 1
       tt-dados.NumGraph   = 1
       tt-dados.graphdata  = 2000.

Create tt-dados.
Assign tt-dados.NumPoint   = 3
       tt-dados.NumSet     = 1
       tt-dados.NumGraph   = 1
       tt-dados.graphdata  = 3000.

/* Valores Metros Cancelados */
Create tt-dados.
Assign tt-dados.NumPoint   = 1
       tt-dados.NumSet     = 2
       tt-dados.NumGraph   = 1
       tt-dados.graphdata  = 1500.
Create tt-dados.
Assign tt-dados.NumPoint   = 2
       tt-dados.NumSet     = 2
       tt-dados.NumGraph   = 1
       tt-dados.graphdata  = 2250.

Create tt-dados.
Assign tt-dados.NumPoint   = 3
       tt-dados.NumSet     = 2
       tt-dados.NumGraph   = 1
       tt-dados.graphdata  = 2500.

/* Valores dos Metros Faturados */
Create tt-dados.
Assign tt-dados.NumPoint   = 1
       tt-dados.NumSet     = 3
       tt-dados.NumGraph   = 1
       tt-dados.graphdata  = 2850.
Create tt-dados.
Assign tt-dados.NumPoint   = 2
       tt-dados.NumSet     = 3
       tt-dados.NumGraph   = 1
       tt-dados.graphdata  = 2250.

Create tt-dados.
Assign tt-dados.NumPoint   = 3
       tt-dados.NumSet     = 3
       tt-dados.NumGraph   = 1
       tt-dados.graphdata  = 4200.

Def var h-utapi011 as handle no-undo.

Run utp/utapi011.p persistent set h-utapi011.

Run pi-execute in h-utapi011 (INPUT  table tt-atributos,
                              input  table tt-points-2,
                              input  table tt-sets,
                              input  table tt-dados,
                              input  table tt-ylabels,
                              output table tt-erros).

if return-value = "NOK" then do: 
   for each tt-erros: 
       disp cod-erro desc-erro FORMAT "x(100)" with 1 col width 500. 
   end.
end.                  

Delete procedure h-utapi011.
