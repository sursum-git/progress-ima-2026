/* Programa: ESFT031.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Faturamento
** Objetivo: Gerar o relatorio Pedidos de Vendas por Item/Cota (Ex-Familia/Cota)
** Autor...: Gilvando de Souza Araujo - Agosto/1999
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Programa: ESFT031.P  =>  ESPD0019RP.P
**   Autor...: Prodb - Toninho
**   Data....: 13/01/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESPD0019RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       FIELD item-ini         LIKE cota-item.it-codigo
       FIELD item-fin         LIKE cota-item.it-codigo
       FIELD da-entr1-ini     LIKE ped-venda.dt-entrega
       FIELD da-entr1-fim     LIKE ped-venda.dt-entrega
       FIELD da-entr2-ini     LIKE ped-venda.dt-entrega
       FIELD da-entr2-fim     LIKE ped-venda.dt-entrega
       FIELD da-entr3-ini     LIKE ped-venda.dt-entrega
       FIELD da-entr3-fim     LIKE ped-venda.dt-entrega
       FIELD da-quota1-ini    LIKE ped-venda.dt-entrega
       FIELD da-quota1-fim    LIKE ped-venda.dt-entrega
       FIELD da-quota2-ini    LIKE ped-venda.dt-entrega
       FIELD da-quota2-fim    LIKE ped-venda.dt-entrega
       FIELD impr-param       AS LOGICAL.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

DEFINE TEMP-TABLE w-work
       field it-codigo     like item.it-codigo
       FIELD acabamento    AS CHAR FORMAT "x(2)"
       field qtd-entrega01 as dec
       field qtd-entrega02 as dec
       field qtd-entrega03 as dec
       field qtd-entrega11 as dec
       field qtd-entrega12 as dec
       field qtd-entrega13 as dec
       field qtd-venda1    as dec
       field qtd-venda2    as dec
       field qtd-venda3    as dec
       field qtd-cancela1  as dec
       field qtd-cancela2  as dec
       field qtd-cancela3  as dec.

def var c-it-descricao as char format "x(30)".
def var da-entr1      like ped-venda.dt-entrega.
def var c-susp-cota1  as char format "x(1)".
def var c-susp-cota2  as char format "x(1)".
def var l-acumule-res as log.
def var de-quantidade as dec.
def var de-quant-adic as dec.
def var de-qtd-conv   as dec.
def var de-qtd-conv1  as dec.
def var de-est-prod   as dec.
def var de-tot-est    as dec.
def var de-tot-prod   as dec.
def var de-tot-cota1  as dec.
def var de-tot-cota2  as dec.

def var de-tot-ent01 as dec.
def var de-tot-ent02 as dec.
def var de-tot-ent03 as dec.
def var de-tot-ent11 as dec.
def var de-tot-ent12 as dec.
def var de-tot-ent13 as dec.
def var de-tot-ent4  as dec.
def var de-tot-ven1  as dec.
def var de-tot-ven2  as dec.
def var de-tot-ven3  as dec.
def var de-tot-ven4  as dec.
def var de-tot-can1  as dec.
def var de-tot-can2  as dec.
def var de-tot-can3  as dec.
def var de-tot-can4  as dec.
def var de-qtd-can1  as dec.
def var de-qtd-can2  as dec.
def var de-qtd-can3  as dec.
def var de-qtd-can4  as dec.
def var de-qtd-ent01 as dec.
def var de-qtd-ent02 as dec.
def var de-qtd-ent03 as dec.
def var de-qtd-ent11 as dec.
def var de-qtd-ent12 as dec.
def var de-qtd-ent13 as dec.
def var de-qtd-ent4  as dec.
def var de-qtd-ven1  as dec.
def var de-qtd-ven2  as dec.
def var de-qtd-ven3  as dec.
def var de-qtd-ven4  as dec.
def var de-sld-ent1  as dec.
def var de-sld-ent2  as dec.

form
    cota-item.it-codigo    no-label  FORMAT "x(6)" AT 1
    c-it-descricao         no-label  at 10
    cota-item.estoque      no-label  format "->>>>,>>9"
    cota-item.cart-mes-ant no-label  format ">>,>>>,>>9"
    de-est-prod            no-label  format "->>>>,>>9"
    c-susp-cota1           no-label  at 71
    de-qtd-ent01           no-label  format "->>,>>>,>>9"
    cota-item.cota2        no-label  format "->>>>,>>9"
    c-susp-cota2           no-label  at 94
    de-qtd-ent02           no-label  format "->>,>>>,>>9"
    de-qtd-ent03           no-label  format "->>>,>>>,>>9"
    de-qtd-ent4            no-label  format "->>>,>>>,>>9"
    with no-box 55 down width 132 STREAM-IO frame f-detalhe no-label.

form header
    "Ate " + substr(string(da-entr1-fim,"99/99/9999"),4,7) at  73 format "x(11)"
    substr(string(da-entr2-fim,"99/99/9999"),4,7)          at 100
    substr(string(da-entr3-ini,"99/99/9999"),4,7) + "..."  at 110 format "x(10)"
    "TOTAL"                                                at 128
    "ITEM     DESCRICAO"                                   at   1
    "ESTQ/PROD   CART.ANT    COTA-1  PR0/PR1/SLD"          at  41 
    "COTA-2  CA0/CA1/SLD     CARTEIRA     CARTEIRA"        at  88
    "-------- ------------------------------"              at   1
    "---------  --------- --------- ------------"
    "--------- ------------ ------------ ------------"
    with width 132 no-labels no-box page-top STREAM-IO frame f-cabecalho.

form
    tt-param.item-ini       label "Item de......." AT 1
    "a"                                            AT 34
    tt-param.item-fin       no-labels
    tt-param.da-entr1-ini   label "1a. Entrega de" AT 1
    "a"                                            AT 34
    tt-param.da-entr1-fim   no-labels          
    tt-param.da-entr2-ini   label "2a. Entrega de" AT 1
    "a"                                            AT 34
    tt-param.da-entr2-fim   no-labels          
    tt-param.da-entr3-ini   label "3a. Entrega de" AT 1
    "a"                                            AT 34
    tt-param.da-entr3-fim   no-labels 
    tt-param.da-quota1-ini  label "Quota-1 de...." AT 1
    "a"                                            AT 34
    tt-param.da-quota1-fim  no-labels          
    tt-param.da-quota2-ini  label "Quota-2 de...." AT 1
    "a"                                            AT 34
    tt-param.da-quota2-fim  no-labels 
    with no-box side-labels width 132 STREAM-IO frame f-parlis.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i PEDIDOS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Pedidos_de_Vendas_por_Item/Cota * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
VIEW FRAME f-cabecalho.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (INPUT RETURN-VALUE).

assign da-entr1 = date(month(tt-param.da-entr1-fim),1,year(tt-param.da-entr1-fim)).

for each w-work:
   delete w-work.
end.

/* Gera arquivo de trabalho c/pedidos em aberto/cancelados p/item */
for each ped-venda use-index ch-tabfin WHERE ped-venda.cod-sit-ped <> 3
                                       NO-LOCK:
    run pi-acompanhar in h-acomp (INPUT "Pedidos Abertos/Cancelados: " +
                                  STRING(ped-venda.cod-sit-ped,"9") + " " +
                                  ped-venda.nr-pedcli + " ").
    
    for each ped-item of ped-venda 
        where ped-item.cod-sit-item <> 3
          AND ped-item.it-codigo    >= tt-param.item-ini
          AND ped-item.it-codigo    <= tt-param.item-fin
        NO-LOCK:
        
        assign de-quantidade = ped-item.qt-pedida - ped-item.qt-pendente -
                               ped-item.qt-atendida + ped-item.qt-devolvida.

        if ped-item.cod-sit-item <> 6 then do:
           find ped-item-res WHERE
                ped-item-res.it-codigo    = ped-item.it-codigo AND
                ped-item-res.nr-sequencia = ped-item.nr-sequencia AND
                ped-item-res.nr-pedcli    = ped-item.nr-pedcli AND
                ped-item-res.nome-abrev   = ped-item.nome-abrev
                no-lock no-error.

           if avail ped-item-res then do:
              assign l-acumule-res = no.
              if ped-item-res.faturado = yes then do:
                 find nota-fiscal WHERE
                      nota-fiscal.cod-estabel = ped-item-res.cod-estabel AND
                      nota-fiscal.serie       = ped-item-res.serie AND
                      nota-fiscal.nr-nota-fis = string(ped-item-res.nr-nota-fis)
                      no-lock no-error.
                 if avail nota-fiscal then
                    if nota-fiscal.ind-sit-nota < 3 AND
                       nota-fiscal.dt-cancela   = ? then
                       assign l-acumule-res = yes.
              end.
              else
                 assign l-acumule-res = yes.

              if l-acumule-res then
                 assign de-quantidade = de-quantidade - ped-item-res.qt-pedida.
           end.
        end.
       
        find item where 
             item.it-codigo = ped-item.it-codigo no-lock no-error.
        if item.un <> "m" then do:
           find item where 
                 item.it-codigo = ped-item.it-codigo no-lock no-error.
           find item-ext WHERE
                item-ext.it-codigo = item.it-codigo no-lock no-error.
           if avail item-ext then
              assign de-qtd-conv = de-quantidade * item-ext.fator-conv.
           else
              assign de-qtd-conv = de-quantidade.
        end.
        else
           assign de-qtd-conv = de-quantidade.
  
        FIND FIRST w-work WHERE w-work.it-codigo  = item.it-codigo 
                            AND w-work.acabamento = SUBSTR(ped-item.cod-refer,1,2)
                          NO-ERROR.

        if not avail w-work then do:
           create w-work.
           assign w-work.it-codigo     = item.it-codigo
                  w-work.acabamento    = SUBSTR(ped-item.cod-refer,1,2)
                  w-work.qtd-entrega01 = 0      
                  w-work.qtd-entrega03 = 0      
                  w-work.qtd-entrega12 = 0      
                  w-work.qtd-venda1    = 0      
                  w-work.qtd-venda3    = 0      
                  w-work.qtd-cancela2  = 0      
                  w-work.qtd-entrega02 = 0
                  w-work.qtd-entrega11 = 0
                  w-work.qtd-entrega13 = 0
                  w-work.qtd-venda2    = 0
                  w-work.qtd-cancela1  = 0
                  w-work.qtd-cancela3  = 0.
        end.
        if ped-item.cod-sit-item = 6 then do:
           if ped-item.dt-entrega     <  tt-param.da-quota1-ini then do:
              if ped-venda.dt-implant <  tt-param.da-quota1-ini AND
                 ped-item.dt-canseq   >= tt-param.da-quota1-ini AND
                 ped-item.dt-canseq   <= tt-param.da-quota1-fim then
                 assign w-work.qtd-cancela1 = w-work.qtd-cancela1 + de-qtd-conv.
           end.
           if ped-item.dt-entrega     >= tt-param.da-quota2-ini
           and ped-item.dt-entrega    <= tt-param.da-quota2-fim then do:
              if ped-venda.dt-implant <  tt-param.da-quota2-ini AND
                 ped-item.dt-canseq   >= tt-param.da-quota1-ini AND
                 ped-item.dt-canseq   <= tt-param.da-quota1-fim then
                 assign w-work.qtd-cancela2 = w-work.qtd-cancela2 + de-qtd-conv.
           end. 
           /* suspenso porque nÆo est  implantado quota 3 ---------------------
           if ped-item.dt-entrega >= tt-param.da-entr3-ini then do:
              if ped-venda.dt-implant <  tt-param.da-entr1-ini AND
                 ped-item.dt-canseq   >= tt-param.da-entr1-ini AND
                 ped-item.dt-canseq   <= tt-param.da-entr1-fim then
                 assign w-work.qtd-cancela3 = w-work.qtd-cancela3 + de-qtd-conv.
           end.
           -------------------------------------------------------------------*/                                
        end.
        else do:           
           if ped-item.dt-entrega >= tt-param.da-entr1-ini AND
              ped-item.dt-entrega <= tt-param.da-entr1-fim then do:
              if ped-venda.cod-priori = 1 then
                 assign w-work.qtd-entrega11 = w-work.qtd-entrega11 + de-qtd-conv.
              else
                 assign w-work.qtd-entrega01 = w-work.qtd-entrega01 + de-qtd-conv.
           end.
           else
              if ped-item.dt-entrega >= tt-param.da-entr2-ini AND
                 ped-item.dt-entrega <= tt-param.da-entr2-fim then do:
                 if ped-venda.cod-priori = 1 then
                    assign w-work.qtd-entrega12 = w-work.qtd-entrega12 + de-qtd-conv.
                 else
                    assign w-work.qtd-entrega02 = w-work.qtd-entrega02 + de-qtd-conv.
              end.                                     
           else 
              if ped-item.dt-entrega >= tt-param.da-entr3-ini AND
                 ped-item.dt-entrega <= tt-param.da-entr3-fim then do:
                 if ped-venda.cod-priori = 1 then
                    assign w-work.qtd-entrega13 = w-work.qtd-entrega13 + de-qtd-conv.
                 ELSE 
                    assign w-work.qtd-entrega03 = w-work.qtd-entrega03 + de-qtd-conv.
              end.                                    
        end.
    end.
end.
                     
/* Grava no arquivo de trabalho pedidos implantados no mes p/item */
for each ped-venda use-index ch-implant 
    WHERE ped-venda.dt-implant >= tt-param.da-quota1-ini
      AND ped-venda.dt-implant <= tt-param.da-quota2-fim
    NO-LOCK:
    
    run pi-acompanhar in h-acomp (INPUT "Pedidos do Mˆs: " + ped-venda.nr-pedcli).

    IF ped-venda.cod-sit-ped = 6 THEN next.

    for each ped-item of ped-venda no-lock:
   
        assign de-quantidade = ped-item.qt-pedida.

        find item where 
             item.it-codigo = ped-item.it-codigo no-lock no-error.
        if item.un <> "m" then do:
           find item where 
                item.it-codigo = ped-item.it-codigo no-lock no-error.

           find item-ext where 
                item-ext.it-codigo = ped-item.it-codigo no-lock no-error.

           if avail item-ext then
              assign de-qtd-conv = de-quantidade * item-ext.fator-conv.
           else
              assign de-qtd-conv = de-quantidade.
        end.
        else
           assign de-qtd-conv = de-quantidade.
  
        FIND FIRST w-work WHERE w-work.it-codigo  = item.it-codigo
                            AND w-work.acabamento = SUBSTR(ped-item.cod-refer,1,2)
                          NO-ERROR.
        if not avail w-work then do:
           create w-work.
           assign w-work.it-codigo     = item.it-codigo
                  w-work.acabamento    = SUBSTR(ped-item.cod-refer,1,2)
                  w-work.qtd-entrega01 = 0      
                  w-work.qtd-entrega03 = 0      
                  w-work.qtd-entrega12 = 0      
                  w-work.qtd-venda1    = 0      
                  w-work.qtd-venda3    = 0      
                  w-work.qtd-cancela2  = 0      
                  w-work.qtd-entrega02 = 0
                  w-work.qtd-entrega11 = 0
                  w-work.qtd-entrega13 = 0
                  w-work.qtd-venda2    = 0
                  w-work.qtd-cancela1  = 0
                  w-work.qtd-cancela3  = 0.
        end.

        if ped-item.dt-entrega >= tt-param.da-quota1-ini AND
           ped-item.dt-entrega <= tt-param.da-quota1-fim AND
           ped-venda.cod-priori < 1 then
           assign w-work.qtd-venda1 = w-work.qtd-venda1 + de-qtd-conv.
        else
        if ped-item.dt-entrega >= tt-param.da-quota2-ini AND
           ped-item.dt-entrega <= tt-param.da-quota2-fim AND
           ped-venda.cod-priori < 1 then
           assign w-work.qtd-venda2 = w-work.qtd-venda2 + de-qtd-conv.
        else 
        /* est  sendo usado tt-param.da-quota2 provisoramente, at‚ ser
           definido pelo Walter espa‡o no relat¢iro para impl.da quota3 */
        if ped-item.dt-entrega >= tt-param.da-quota2-ini AND
           ped-item.dt-entrega <= tt-param.da-quota2-fim AND
           ped-venda.cod-priori < 1 then
           assign w-work.qtd-venda3 = w-work.qtd-venda3 + de-qtd-conv.
    end.
end.
/* Gilvando - 29.01.2007 --------------
DEF STREAM lixo.
OUTPUT STREAM lixo TO C:/lixo/lixo.csv.
PUT STREAM lixo "Item;Acb;Entrega01;Entrega11;Entrega02;Entrega12;Entrega03;Entrega13;Venda1;Venda2;Venda3;Cancel-1;Cancel-2" SKIP.
FOR EACH w-work.
    PUT STREAM lixo
        w-work.it-codigo ";"
        w-work.acabamento ";"
        w-work.qtd-entrega01 ";"
        w-work.qtd-entrega11 ";"
        w-work.qtd-entrega02 ";"
        w-work.qtd-entrega12 ";"
        w-work.qtd-entrega03 ";"
        w-work.qtd-entrega13 ";"
        w-work.qtd-venda1 ";"
        w-work.qtd-venda2 ";"
        w-work.qtd-venda3 ";"
        w-work.qtd-cancela1 ";"
        w-work.qtd-cancela2
        SKIP.
END.
OUTPUT STREAM lixo CLOSE.
-------------------------------------*/
for each cota-item where 
         cota-item.it-codigo >= tt-param.item-ini AND
         cota-item.it-codigo <= tt-param.item-fin NO-LOCK:
       
    run pi-acompanhar in h-acomp (INPUT "ITEM: " + cota-item.it-codigo).
                                                             
    assign de-qtd-ent01 = 0      de-qtd-ent02 = 0      de-qtd-ent03 = 0
           de-qtd-ent11 = 0      de-qtd-ent12 = 0      de-qtd-ent13 = 0
           de-qtd-ven1  = 0      de-qtd-ven2  = 0      de-qtd-ven3  = 0
           de-qtd-can1  = 0      de-qtd-can2  = 0      de-qtd-can3  = 0.

    FOR EACH  w-work where
              w-work.it-codigo = cota-item.it-codigo NO-LOCK:
        assign de-qtd-ent01 = de-qtd-ent01 + w-work.qtd-entrega01
               de-qtd-ent02 = de-qtd-ent02 + w-work.qtd-entrega02
               de-qtd-ent03 = de-qtd-ent03 + w-work.qtd-entrega03
               de-qtd-ent11 = de-qtd-ent11 + w-work.qtd-entrega11
               de-qtd-ent12 = de-qtd-ent12 + w-work.qtd-entrega12
               de-qtd-ent13 = de-qtd-ent13 + w-work.qtd-entrega13
               de-qtd-ven1  = de-qtd-ven1  + w-work.qtd-venda1
               de-qtd-ven2  = de-qtd-ven2  + w-work.qtd-venda2
               de-qtd-ven3  = de-qtd-ven3  + w-work.qtd-venda3
               de-qtd-can1  = de-qtd-can1  + w-work.qtd-cancela1
               de-qtd-can2  = de-qtd-can2  + w-work.qtd-cancela2
               de-qtd-can3  = de-qtd-can3  + w-work.qtd-cancela3.
    END.
             
    FOR EACH w-work WHERE w-work.it-codigo = cota-item.item-adic1 
                      AND (w-work.acabamento = cota-item.acab-adic1 OR
                           cota-item.acab-adic1 = "") 
                    NO-LOCK:
           assign de-qtd-ent01 = de-qtd-ent01 + w-work.qtd-entrega01
                  de-qtd-ent02 = de-qtd-ent02 + w-work.qtd-entrega02
                  de-qtd-ent03 = de-qtd-ent03 + w-work.qtd-entrega03
                  de-qtd-ent11 = de-qtd-ent11 + w-work.qtd-entrega11
                  de-qtd-ent12 = de-qtd-ent12 + w-work.qtd-entrega12
                  de-qtd-ent13 = de-qtd-ent13 + w-work.qtd-entrega13
                  de-qtd-ven1  = de-qtd-ven1 + w-work.qtd-venda1
                  de-qtd-ven2  = de-qtd-ven2 + w-work.qtd-venda2
                  de-qtd-ven3  = de-qtd-ven3 + w-work.qtd-venda3
                  de-qtd-can1  = de-qtd-can1 + w-work.qtd-cancela1
                  de-qtd-can2  = de-qtd-can2 + w-work.qtd-cancela2
                  de-qtd-can3  = de-qtd-can3 + w-work.qtd-cancela3.
    END.

    FOR EACH w-work WHERE w-work.it-codigo = cota-item.item-adic2 
                      AND (w-work.acabamento = cota-item.acab-adic2 OR
                           cota-item.acab-adic2 = "") 
                    NO-LOCK:
           assign de-qtd-ent01 = de-qtd-ent01 + w-work.qtd-entrega01
                  de-qtd-ent02 = de-qtd-ent02 + w-work.qtd-entrega02
                  de-qtd-ent03 = de-qtd-ent03 + w-work.qtd-entrega03
                  de-qtd-ent11 = de-qtd-ent11 + w-work.qtd-entrega11
                  de-qtd-ent12 = de-qtd-ent12 + w-work.qtd-entrega12
                  de-qtd-ent13 = de-qtd-ent13 + w-work.qtd-entrega13
                  de-qtd-ven1  = de-qtd-ven1 + w-work.qtd-venda1
                  de-qtd-ven2  = de-qtd-ven2 + w-work.qtd-venda2
                  de-qtd-ven3  = de-qtd-ven3 + w-work.qtd-venda3
                  de-qtd-can1  = de-qtd-can1 + w-work.qtd-cancela1
                  de-qtd-can2  = de-qtd-can2 + w-work.qtd-cancela2
                  de-qtd-can3  = de-qtd-can3 + w-work.qtd-cancela3.
    END.

    FOR EACH w-work WHERE w-work.it-codigo = cota-item.item-adic3 
                      AND (w-work.acabamento = cota-item.acab-adic3 OR
                           cota-item.acab-adic3 = "") 
                    NO-LOCK:
           assign de-qtd-ent01 = de-qtd-ent01 + w-work.qtd-entrega01
                  de-qtd-ent02 = de-qtd-ent02 + w-work.qtd-entrega02
                  de-qtd-ent03 = de-qtd-ent03 + w-work.qtd-entrega03
                  de-qtd-ent11 = de-qtd-ent11 + w-work.qtd-entrega11
                  de-qtd-ent12 = de-qtd-ent12 + w-work.qtd-entrega12
                  de-qtd-ent13 = de-qtd-ent13 + w-work.qtd-entrega13
                  de-qtd-ven1  = de-qtd-ven1 + w-work.qtd-venda1
                  de-qtd-ven2  = de-qtd-ven2 + w-work.qtd-venda2
                  de-qtd-ven3  = de-qtd-ven3 + w-work.qtd-venda3
                  de-qtd-can1  = de-qtd-can1 + w-work.qtd-cancela1
                  de-qtd-can2  = de-qtd-can2 + w-work.qtd-cancela2
                  de-qtd-can3  = de-qtd-can3 + w-work.qtd-cancela3.
    END.

    FOR EACH w-work WHERE w-work.it-codigo = cota-item.item-adic4 
                      AND (w-work.acabamento = cota-item.acab-adic4 OR
                           cota-item.acab-adic4 = "") 
                    NO-LOCK:
           assign de-qtd-ent01 = de-qtd-ent01 + w-work.qtd-entrega01
                  de-qtd-ent02 = de-qtd-ent02 + w-work.qtd-entrega02
                  de-qtd-ent03 = de-qtd-ent03 + w-work.qtd-entrega03
                  de-qtd-ent11 = de-qtd-ent11 + w-work.qtd-entrega11
                  de-qtd-ent12 = de-qtd-ent12 + w-work.qtd-entrega12
                  de-qtd-ent13 = de-qtd-ent13 + w-work.qtd-entrega13
                  de-qtd-ven1  = de-qtd-ven1 + w-work.qtd-venda1
                  de-qtd-ven2  = de-qtd-ven2 + w-work.qtd-venda2
                  de-qtd-ven3  = de-qtd-ven3 + w-work.qtd-venda3
                  de-qtd-can1  = de-qtd-can1 + w-work.qtd-cancela1
                  de-qtd-can2  = de-qtd-can2 + w-work.qtd-cancela2
                  de-qtd-can3  = de-qtd-can3 + w-work.qtd-cancela3.
    END.

    FOR EACH w-work WHERE w-work.it-codigo = cota-item.item-adic5 
                      AND (w-work.acabamento = cota-item.acab-adic5 OR
                           cota-item.acab-adic5 = "") 
                    NO-LOCK:
           assign de-qtd-ent01 = de-qtd-ent01 + w-work.qtd-entrega01
                  de-qtd-ent02 = de-qtd-ent02 + w-work.qtd-entrega02
                  de-qtd-ent03 = de-qtd-ent03 + w-work.qtd-entrega03
                  de-qtd-ent11 = de-qtd-ent11 + w-work.qtd-entrega11
                  de-qtd-ent12 = de-qtd-ent12 + w-work.qtd-entrega12
                  de-qtd-ent13 = de-qtd-ent13 + w-work.qtd-entrega13
                  de-qtd-ven1  = de-qtd-ven1 + w-work.qtd-venda1
                  de-qtd-ven2  = de-qtd-ven2 + w-work.qtd-venda2
                  de-qtd-ven3  = de-qtd-ven3 + w-work.qtd-venda3
                  de-qtd-can1  = de-qtd-can1 + w-work.qtd-cancela1
                  de-qtd-can2  = de-qtd-can2 + w-work.qtd-cancela2
                  de-qtd-can3  = de-qtd-can3 + w-work.qtd-cancela3.
    END.

    FOR EACH w-work WHERE w-work.it-codigo = cota-item.item-adic6 
                      AND (w-work.acabamento = cota-item.acab-adic6 OR
                           cota-item.acab-adic6 = "") 
                    NO-LOCK:
           assign de-qtd-ent01 = de-qtd-ent01 + w-work.qtd-entrega01
                  de-qtd-ent02 = de-qtd-ent02 + w-work.qtd-entrega02
                  de-qtd-ent03 = de-qtd-ent03 + w-work.qtd-entrega03
                  de-qtd-ent11 = de-qtd-ent11 + w-work.qtd-entrega11
                  de-qtd-ent12 = de-qtd-ent12 + w-work.qtd-entrega12
                  de-qtd-ent13 = de-qtd-ent13 + w-work.qtd-entrega13
                  de-qtd-ven1  = de-qtd-ven1 + w-work.qtd-venda1
                  de-qtd-ven2  = de-qtd-ven2 + w-work.qtd-venda2
                  de-qtd-ven3  = de-qtd-ven3 + w-work.qtd-venda3
                  de-qtd-can1  = de-qtd-can1 + w-work.qtd-cancela1
                  de-qtd-can2  = de-qtd-can2 + w-work.qtd-cancela2
                  de-qtd-can3  = de-qtd-can3 + w-work.qtd-cancela3.
    END.

    FOR EACH w-work WHERE w-work.it-codigo = cota-item.item-adic7 
                      AND (w-work.acabamento = cota-item.acab-adic7 OR
                           cota-item.acab-adic7 = "") 
                    NO-LOCK:
           assign de-qtd-ent01 = de-qtd-ent01 + w-work.qtd-entrega01
                  de-qtd-ent02 = de-qtd-ent02 + w-work.qtd-entrega02
                  de-qtd-ent03 = de-qtd-ent03 + w-work.qtd-entrega03
                  de-qtd-ent11 = de-qtd-ent11 + w-work.qtd-entrega11
                  de-qtd-ent12 = de-qtd-ent12 + w-work.qtd-entrega12
                  de-qtd-ent13 = de-qtd-ent13 + w-work.qtd-entrega13
                  de-qtd-ven1  = de-qtd-ven1 + w-work.qtd-venda1
                  de-qtd-ven2  = de-qtd-ven2 + w-work.qtd-venda2
                  de-qtd-ven3  = de-qtd-ven3 + w-work.qtd-venda3
                  de-qtd-can1  = de-qtd-can1 + w-work.qtd-cancela1
                  de-qtd-can2  = de-qtd-can2 + w-work.qtd-cancela2
                  de-qtd-can3  = de-qtd-can3 + w-work.qtd-cancela3.
    END.
    
    FOR EACH w-work WHERE w-work.it-codigo = cota-item.item-adic8 
                      AND (w-work.acabamento = cota-item.acab-adic8 OR
                           cota-item.acab-adic8 = "") 
                    NO-LOCK:
           assign de-qtd-ent01 = de-qtd-ent01 + w-work.qtd-entrega01
                  de-qtd-ent02 = de-qtd-ent02 + w-work.qtd-entrega02
                  de-qtd-ent03 = de-qtd-ent03 + w-work.qtd-entrega03
                  de-qtd-ent11 = de-qtd-ent11 + w-work.qtd-entrega11
                  de-qtd-ent12 = de-qtd-ent12 + w-work.qtd-entrega12
                  de-qtd-ent13 = de-qtd-ent13 + w-work.qtd-entrega13
                  de-qtd-ven1  = de-qtd-ven1 + w-work.qtd-venda1
                  de-qtd-ven2  = de-qtd-ven2 + w-work.qtd-venda2
                  de-qtd-ven3  = de-qtd-ven3 + w-work.qtd-venda3
                  de-qtd-can1  = de-qtd-can1 + w-work.qtd-cancela1
                  de-qtd-can2  = de-qtd-can2 + w-work.qtd-cancela2
                  de-qtd-can3  = de-qtd-can3 + w-work.qtd-cancela3.
    END.
    
    FOR EACH w-work WHERE w-work.it-codigo = cota-item.item-adic9 
                      AND (w-work.acabamento = cota-item.acab-adic9 OR
                           cota-item.acab-adic9 = "") 
                    NO-LOCK:
           assign de-qtd-ent01 = de-qtd-ent01 + w-work.qtd-entrega01
                  de-qtd-ent02 = de-qtd-ent02 + w-work.qtd-entrega02
                  de-qtd-ent03 = de-qtd-ent03 + w-work.qtd-entrega03
                  de-qtd-ent11 = de-qtd-ent11 + w-work.qtd-entrega11
                  de-qtd-ent12 = de-qtd-ent12 + w-work.qtd-entrega12
                  de-qtd-ent13 = de-qtd-ent13 + w-work.qtd-entrega13
                  de-qtd-ven1  = de-qtd-ven1 + w-work.qtd-venda1
                  de-qtd-ven2  = de-qtd-ven2 + w-work.qtd-venda2
                  de-qtd-ven3  = de-qtd-ven3 + w-work.qtd-venda3
                  de-qtd-can1  = de-qtd-can1 + w-work.qtd-cancela1
                  de-qtd-can2  = de-qtd-can2 + w-work.qtd-cancela2
                  de-qtd-can3  = de-qtd-can3 + w-work.qtd-cancela3.
    END.
    
    FOR EACH w-work WHERE w-work.it-codigo = cota-item.item-adic10 
                      AND (w-work.acabamento = cota-item.acab-adic10 OR
                           cota-item.acab-adic10 = "") 
                    NO-LOCK:
           assign de-qtd-ent01 = de-qtd-ent01 + w-work.qtd-entrega01
                  de-qtd-ent02 = de-qtd-ent02 + w-work.qtd-entrega02
                  de-qtd-ent03 = de-qtd-ent03 + w-work.qtd-entrega03
                  de-qtd-ent11 = de-qtd-ent11 + w-work.qtd-entrega11
                  de-qtd-ent12 = de-qtd-ent12 + w-work.qtd-entrega12
                  de-qtd-ent13 = de-qtd-ent13 + w-work.qtd-entrega13
                  de-qtd-ven1  = de-qtd-ven1 + w-work.qtd-venda1
                  de-qtd-ven2  = de-qtd-ven2 + w-work.qtd-venda2
                  de-qtd-ven3  = de-qtd-ven3 + w-work.qtd-venda3
                  de-qtd-can1  = de-qtd-can1 + w-work.qtd-cancela1
                  de-qtd-can2  = de-qtd-can2 + w-work.qtd-cancela2
                  de-qtd-can3  = de-qtd-can3 + w-work.qtd-cancela3.
    END.

    FOR EACH w-work WHERE w-work.it-codigo = cota-item.item-adic11 
                      AND (w-work.acabamento = cota-item.acab-adic11 OR
                           cota-item.acab-adic11 = "") 
                    NO-LOCK:
           assign de-qtd-ent01 = de-qtd-ent01 + w-work.qtd-entrega01
                  de-qtd-ent02 = de-qtd-ent02 + w-work.qtd-entrega02
                  de-qtd-ent03 = de-qtd-ent03 + w-work.qtd-entrega03
                  de-qtd-ent11 = de-qtd-ent11 + w-work.qtd-entrega11
                  de-qtd-ent12 = de-qtd-ent12 + w-work.qtd-entrega12
                  de-qtd-ent13 = de-qtd-ent13 + w-work.qtd-entrega13
                  de-qtd-ven1  = de-qtd-ven1 + w-work.qtd-venda1
                  de-qtd-ven2  = de-qtd-ven2 + w-work.qtd-venda2
                  de-qtd-ven3  = de-qtd-ven3 + w-work.qtd-venda3
                  de-qtd-can1  = de-qtd-can1 + w-work.qtd-cancela1
                  de-qtd-can2  = de-qtd-can2 + w-work.qtd-cancela2
                  de-qtd-can3  = de-qtd-can3 + w-work.qtd-cancela3.
    END.

    FOR EACH w-work WHERE w-work.it-codigo = cota-item.item-adic12 
                      AND (w-work.acabamento = cota-item.acab-adic12 OR
                           cota-item.acab-adic12 = "") 
                    NO-LOCK:
           assign de-qtd-ent01 = de-qtd-ent01 + w-work.qtd-entrega01
                  de-qtd-ent02 = de-qtd-ent02 + w-work.qtd-entrega02
                  de-qtd-ent03 = de-qtd-ent03 + w-work.qtd-entrega03
                  de-qtd-ent11 = de-qtd-ent11 + w-work.qtd-entrega11
                  de-qtd-ent12 = de-qtd-ent12 + w-work.qtd-entrega12
                  de-qtd-ent13 = de-qtd-ent13 + w-work.qtd-entrega13
                  de-qtd-ven1  = de-qtd-ven1 + w-work.qtd-venda1
                  de-qtd-ven2  = de-qtd-ven2 + w-work.qtd-venda2
                  de-qtd-ven3  = de-qtd-ven3 + w-work.qtd-venda3
                  de-qtd-can1  = de-qtd-can1 + w-work.qtd-cancela1
                  de-qtd-can2  = de-qtd-can2 + w-work.qtd-cancela2
                  de-qtd-can3  = de-qtd-can3 + w-work.qtd-cancela3.
    END.

    FOR EACH w-work WHERE w-work.it-codigo = cota-item.item-adic13 
                      AND (w-work.acabamento = cota-item.acab-adic13 OR
                           cota-item.acab-adic13 = "") 
                    NO-LOCK:
           assign de-qtd-ent01 = de-qtd-ent01 + w-work.qtd-entrega01
                  de-qtd-ent02 = de-qtd-ent02 + w-work.qtd-entrega02
                  de-qtd-ent03 = de-qtd-ent03 + w-work.qtd-entrega03
                  de-qtd-ent11 = de-qtd-ent11 + w-work.qtd-entrega11
                  de-qtd-ent12 = de-qtd-ent12 + w-work.qtd-entrega12
                  de-qtd-ent13 = de-qtd-ent13 + w-work.qtd-entrega13
                  de-qtd-ven1  = de-qtd-ven1 + w-work.qtd-venda1
                  de-qtd-ven2  = de-qtd-ven2 + w-work.qtd-venda2
                  de-qtd-ven3  = de-qtd-ven3 + w-work.qtd-venda3
                  de-qtd-can1  = de-qtd-can1 + w-work.qtd-cancela1
                  de-qtd-can2  = de-qtd-can2 + w-work.qtd-cancela2
                  de-qtd-can3  = de-qtd-can3 + w-work.qtd-cancela3.
    END.

    FOR EACH w-work WHERE w-work.it-codigo = cota-item.item-adic14 
                      AND (w-work.acabamento = cota-item.acab-adic14 OR
                           cota-item.acab-adic14 = "") 
                    NO-LOCK:
           assign de-qtd-ent01 = de-qtd-ent01 + w-work.qtd-entrega01
                  de-qtd-ent02 = de-qtd-ent02 + w-work.qtd-entrega02
                  de-qtd-ent03 = de-qtd-ent03 + w-work.qtd-entrega03
                  de-qtd-ent11 = de-qtd-ent11 + w-work.qtd-entrega11
                  de-qtd-ent12 = de-qtd-ent12 + w-work.qtd-entrega12
                  de-qtd-ent13 = de-qtd-ent13 + w-work.qtd-entrega13
                  de-qtd-ven1  = de-qtd-ven1 + w-work.qtd-venda1
                  de-qtd-ven2  = de-qtd-ven2 + w-work.qtd-venda2
                  de-qtd-ven3  = de-qtd-ven3 + w-work.qtd-venda3
                  de-qtd-can1  = de-qtd-can1 + w-work.qtd-cancela1
                  de-qtd-can2  = de-qtd-can2 + w-work.qtd-cancela2
                  de-qtd-can3  = de-qtd-can3 + w-work.qtd-cancela3.
    END.

    FOR EACH w-work WHERE w-work.it-codigo = cota-item.item-adic15 
                      AND (w-work.acabamento = cota-item.acab-adic15 OR
                           cota-item.acab-adic15 = "") 
                    NO-LOCK:
           assign de-qtd-ent01 = de-qtd-ent01 + w-work.qtd-entrega01
                  de-qtd-ent02 = de-qtd-ent02 + w-work.qtd-entrega02
                  de-qtd-ent03 = de-qtd-ent03 + w-work.qtd-entrega03
                  de-qtd-ent11 = de-qtd-ent11 + w-work.qtd-entrega11
                  de-qtd-ent12 = de-qtd-ent12 + w-work.qtd-entrega12
                  de-qtd-ent13 = de-qtd-ent13 + w-work.qtd-entrega13
                  de-qtd-ven1  = de-qtd-ven1 + w-work.qtd-venda1
                  de-qtd-ven2  = de-qtd-ven2 + w-work.qtd-venda2
                  de-qtd-ven3  = de-qtd-ven3 + w-work.qtd-venda3
                  de-qtd-can1  = de-qtd-can1 + w-work.qtd-cancela1
                  de-qtd-can2  = de-qtd-can2 + w-work.qtd-cancela2
                  de-qtd-can3  = de-qtd-can3 + w-work.qtd-cancela3.
    END.

    assign de-tot-est   = de-tot-est  + cota-item.estoque
           de-tot-prod  = de-tot-prod + cota-item.producao
           de-tot-cota1 = de-tot-cota1 + cota-item.cota1
           de-tot-cota2 = de-tot-cota2 + cota-item.cota2
           de-tot-ent01 = de-tot-ent01 + de-qtd-ent01
           de-tot-ent02 = de-tot-ent02 + de-qtd-ent02
           de-tot-ent03 = de-tot-ent03 + de-qtd-ent03
           de-tot-ent11 = de-tot-ent11 + de-qtd-ent11
           de-tot-ent12 = de-tot-ent12 + de-qtd-ent12
           de-tot-ent13 = de-tot-ent13 + de-qtd-ent13
           de-tot-ven1  = de-tot-ven1 + de-qtd-ven1
           de-tot-ven2  = de-tot-ven2 + de-qtd-ven2
           de-tot-ven3  = de-tot-ven3 + de-qtd-ven3
           de-tot-can1  = de-tot-can1 + de-qtd-can1
           de-tot-can2  = de-tot-can2 + de-qtd-can2
           de-tot-can3  = de-tot-can3 + de-qtd-can3.
   
    assign de-qtd-ent4  = de-qtd-ent01 + de-qtd-ent02 + de-qtd-ent03 +
                          de-qtd-ent11 + de-qtd-ent12 + de-qtd-ent13.
   
    /* Walter 16.02.2005 
       Se for lan‡ado valor em Cota1, o sistema ir  considerar esse valor como
       estoque. SenÆo, ser  usada a formula abaixo:
    */
    IF cota-item.cota1 <> 0 THEN
       assign de-est-prod = cota-item.cota1.
    ELSE
       ASSIGN de-est-prod = cota-item.estoque + 
                            cota-item.producao -
                            cota-item.cart-mes-ant.

    assign de-sld-ent1  = de-est-prod - de-qtd-ven1 + de-qtd-can1
           de-sld-ent2  = cota-item.cota2 - de-qtd-ven2 + de-qtd-can2.
    
    if cota-item.susp-cota1 then
       assign c-susp-cota1 = "s".
    else
       assign c-susp-cota1 = " ".

    if cota-item.susp-cota2 then
       assign c-susp-cota2 = "s".
    else
       assign c-susp-cota2 = " ".
       
    find ITEM where item.it-codigo = cota-item.it-codigo no-lock no-error.

    IF AVAIL ITEM THEN 
       ASSIGN c-it-descricao = ITEM.descricao-1 + ITEM.descricao-2.
    ELSE
       ASSIGN c-it-descricao = "".

    display cota-item.it-codigo
            c-it-descricao
            cota-item.estoque
            cota-item.cart-mes-ant
            de-est-prod
            de-qtd-ent01
            c-susp-cota1
            cota-item.cota2
            c-susp-cota2
            de-qtd-ent02
            de-qtd-ent03
            de-qtd-ent4
            with frame f-detalhe.
    down with frame f-detalhe.
    display de-qtd-ent11 @ de-qtd-ent01
            de-qtd-ent12 @ de-qtd-ent02
            de-qtd-ent13 @ de-qtd-ent03
            with frame f-detalhe.
    down with frame f-detalhe.
    display cota-item.producao @ cota-item.estoque
            de-sld-ent1        @ de-qtd-ent01
            de-sld-ent2        @ de-qtd-ent02
            with frame f-detalhe.
    down 2 with frame f-detalhe.
end.

assign de-sld-ent1 = de-tot-cota1 - de-tot-ven1 + de-tot-can1
       de-sld-ent2 = de-tot-cota2 - de-tot-ven2 + de-tot-can2.
      
assign de-est-prod = de-tot-est + de-tot-prod
       de-tot-ent4 = de-tot-ent01 + de-tot-ent02 + de-tot-ent03 +
                     de-tot-ent11 + de-tot-ent12 + de-tot-ent13.
      
display "Totais"      @ cota-item.it-codigo
        de-tot-est    @ cota-item.estoque
        de-est-prod
        de-tot-ent01  @ de-qtd-ent01
        de-tot-cota2  @ cota-item.cota2
        de-tot-ent02  @ de-qtd-ent02
        de-tot-ent03  @ de-qtd-ent03
        de-tot-ent4   @ de-qtd-ent4
        with frame f-detalhe.
down with frame f-detalhe.
display de-tot-ent11  @ de-qtd-ent01
        de-tot-ent12  @ de-qtd-ent02
        de-tot-ent13  @ de-qtd-ent03
        with frame f-detalhe.
down with frame f-detalhe.
display de-tot-prod  @ cota-item.estoque 
        de-sld-ent1  @ de-qtd-ent01
        de-sld-ent2  @ de-qtd-ent02
        with frame f-detalhe.
down 2 with frame f-detalhe.

assign de-tot-est   = 0    de-tot-prod  = 0     de-tot-cota1 = 0
       de-tot-cota2 = 0    de-tot-ent01 = 0     de-tot-ent02 = 0
       de-tot-ent03 = 0    de-tot-ent11 = 0     de-tot-ent12 = 0
       de-tot-ent13 = 0    de-tot-ent4  = 0     de-tot-ven1  = 0
       de-tot-ven2  = 0    de-tot-ven3  = 0     de-tot-ven4  = 0
       de-tot-can1  = 0    de-tot-can2  = 0     de-tot-can3  = 0
       de-tot-can4  = 0.

IF tt-param.impr-param THEN DO.
   PAGE.
   PUT "***----------------- PAR¶METROS ----------------***"
       SKIP.
   display tt-param.item-ini   
           tt-param.item-fin   
           tt-param.da-entr1-ini    
           tt-param.da-entr1-fim    
           tt-param.da-entr2-ini    
           tt-param.da-entr2-fim    
           tt-param.da-entr3-ini    
           tt-param.da-entr3-fim    
           tt-param.da-quota1-ini
           tt-param.da-quota1-fim
           tt-param.da-quota2-ini
           tt-param.da-quota2-fim
           with frame f-parlis.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

