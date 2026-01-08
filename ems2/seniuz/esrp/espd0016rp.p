/* Programa: ESPD036.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Faturamento
** Objetivo: Gerar o relatorio de Pedidos de Vendas por Item/Desenho/Cor
** Autor...: Gilvando de Souza Araujo - Junho/96
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Programa: ESPD051.P  =>  ESPD0016RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 17/03/2005
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESPD0016RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino        as integer
       field arquivo        as char format "x(35)"
       field usuario        as char format "x(12)"
       field data-exec      as date
       field hora-exec      as integer
       field classifica     as integer
       FIELD repres-ini     LIKE ped-venda.no-ab-reppri 
       FIELD repres-fin     LIKE ped-venda.no-ab-reppri
       FIELD cliente-ini    LIKE ped-venda.nome-abrev
       FIELD cliente-fin    LIKE ped-venda.nome-abrev
       FIELD grupo-ini      LIKE ITEM.ge-codigo
       FIELD grupo-fin      LIKE ITEM.ge-codigo
       FIELD item-ini       LIKE ITEM.it-codigo
       FIELD item-fin       LIKE ITEM.it-codigo
       FIELD refer-ini      LIKE ped-item.cod-refer
       FIELD refer-fin      LIKE ped-item.cod-refer
       FIELD desenho-ini    AS CHAR FORMAT "x(4)"
       FIELD desenho-fin    AS CHAR FORMAT "x(4)"
       FIELD cod-obsol-ini  LIKE ref-item-ext.cod-obsoleto
       FIELD cod-obsol-fin  LIKE ref-item-ext.cod-obsoleto
       FIELD dt-entr-ini    LIKE ped-venda.dt-entrega   
       FIELD dt-entr-fin    LIKE ped-venda.dt-entrega   
       FIELD dt-impl-ini    LIKE ped-venda.dt-implant
       FIELD dt-impl-fin    LIKE ped-venda.dt-implant
       FIELD tipo-rel       AS INTEGER
       FIELD desc-tipo-rel  AS CHAR FORMAT "x(10)"
       FIELD tipo-acab      AS INTEGER
       FIELD desc-tipo-acab AS CHAR FORMAT "x(10)"
       FIELD impr-param     AS LOGICAL.

define temp-table tt-digita no-undo
       field desenho  AS CHAR FORMAT "x(4)"
       index id desenho.

define temp-table tt-raw-digita
       field raw-digita as raw.

def TEMP-TABLE tt-work 
    field cod-desenho as char format "x(13)"
    field qtd-pedida  as dec  format ">>>,>>>,>>9.99"
    INDEX id cod-desenho.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

DEF VAR i-cont         AS INT.
DEF VAR de-qtd-rep     AS DEC  FORMAT ">>>,>>>,>>9.99".
DEF VAR de-qtd-cli     AS DEC  FORMAT ">>>,>>>,>>9.99".
DEF VAR de-qtd-var     AS DEC  FORMAT ">>>,>>>,>>9.99".
def var de-qtd-des     as dec  format ">>>,>>>,>>9.99".
def var de-qtd-item    as dec  format ">>>,>>>,>>9.99".
def var de-qtd-total   as dec  format ">>>,>>>,>>9.99".
def var c-descricao    as char format "x(36)".
DEF VAR c-lista-des    AS CHAR.

FOR EACH tt-raw-digita.
    CREATE tt-digita.
    RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
    IF c-lista-des <> "" THEN
       ASSIGN c-lista-des = c-lista-des + ",".
    ASSIGN c-lista-des = c-lista-des + tt-digita.desenho.
END.

form
    tt-param.repres-ini     LABEL "Representante.." 
    "a"  AT 34  
    tt-param.repres-fin     NO-LABELS SKIP                          
    tt-param.cliente-ini    LABEL "Cliente........"       
    "a"  AT 34                        
    tt-param.cliente-fin    NO-LABELS SKIP                 
    tt-param.grupo-ini      LABEL "Grupo Estoque.."       
    "a"  AT 34                        
    tt-param.grupo-fin      NO-LABELS SKIP                 
    tt-param.item-ini       LABEL "Item..........."       
    "a"  AT 34                        
    tt-param.item-fin       NO-LABELS SKIP                 
    tt-param.refer-ini      LABEL "Referˆncia....."       
    "a"  AT 34                             
    tt-param.refer-fin      NO-LABELS SKIP
    tt-param.desenho-ini    LABEL "Desenho........"
    "a"  AT 34
    tt-param.desenho-fin    NO-LABELS SKIP
    tt-param.cod-obsol-ini  LABEL "Cod.Obsoleto..."
    "a"  AT 34
    tt-param.cod-obsol-fin  NO-LABELS SKIP
    tt-param.dt-entr-ini    LABEL "Data Entrega..."           
    "a"  AT 34                                                
    tt-param.dt-entr-fin    NO-LABELS SKIP                    
    tt-param.dt-impl-ini    LABEL "Data Implant..."           
    "a"  AT 34                          
    tt-param.dt-impl-fin    NO-LABELS SKIP
    tt-param.desc-tipo-rel  LABEL "Tipo Relatorio." SKIP
    tt-param.desc-tipo-acab LABEL "Tipo Acabamento" SKIP
    with no-box side-labels width 132 STREAM-IO frame f-parlis.

form
    ped-venda.no-ab-reppri LABEL "Repres"
    ped-venda.nome-abrev   LABEL "Cliente"
    ped-item.it-codigo     label "Item" 
    c-descricao            LABEL "Descricao"
    ped-item.cod-refer     LABEL "Referencia" FORMAT "XX XXXX"
    referencia.descricao   label "Descricao"
    de-qtd-var             label "Quantidade"
    with NO-LABEL no-box 55 down width 132 STREAM-IO frame f-rep-cli.

form
    ped-item.it-codigo     label "Item" 
    c-descricao            LABEL "Descricao"
    ped-item.cod-refer     LABEL "Referencia" FORMAT "XX XXXX"
    referencia.descricao   label "Descricao"
    de-qtd-var             label "Quantidade"
    with NO-LABEL no-box 55 down width 132 STREAM-IO frame f-detalhe.

form
    tt-work.cod-desenho   label "Item/Desenho" format "XXXXXX XX XXXX"
    tt-work.qtd-pedida    label "Quantidade"
    with no-box no-attr-space width 132 STREAM-IO 55 down frame f-rank.

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
{utp/ut-liter.i Listagem_Pedidos_Venda_Item/Desenho/Cor * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

IF tt-param.tipo-rel = 1 THEN DO: /* Por Repres/Cliente */
   for each ped-item where ped-item.it-codigo             >= tt-param.item-ini
                       and ped-item.it-codigo             <= tt-param.item-fin
                       and ped-item.cod-refer             >= tt-param.refer-ini
                       and ped-item.cod-refer             <= tt-param.refer-fin
                       and substr(ped-item.cod-refer,3,4) >= tt-param.desenho-ini
                       and substr(ped-item.cod-refer,3,4) <= tt-param.desenho-fin
                       and (lookup(substr(ped-item.cod-refer,3,4),c-lista-des) <> 0 or 
                                                                  c-lista-des = "")
                       and ((substr(ped-item.cod-refer,7,1) = "0" and tt-param.tipo-acab = 1) or
                            (substr(ped-item.cod-refer,7,1) > "0" and tt-param.tipo-acab = 2) or
                                                                     (tt-param.tipo-acab = 3))
                       and ped-item.cod-sit-item <> 6 
                     no-lock,
       each ped-venda of ped-item 
            where ped-venda.dt-implant   >= tt-param.dt-impl-ini
              and ped-venda.dt-implant   <= tt-param.dt-impl-fin
              and ped-venda.dt-entrega   >= tt-param.dt-entr-ini
              and ped-venda.dt-entrega   <= tt-param.dt-entr-fin
              and ped-venda.no-ab-reppri >= tt-param.repres-ini
              and ped-venda.no-ab-reppri <= tt-param.repres-fin
            no-lock,
       EACH ref-item-ext WHERE ref-item-ext.it-codigo    =  ped-item.it-codigo
                           AND ref-item-ext.cod-refer    =  ped-item.cod-refer
                           AND ref-item-ext.cod-obsoleto >= tt-param.cod-obsol-ini
                           AND ref-item-ext.cod-obsoleto <= tt-param.cod-obsol-fin
                         NO-LOCK
       break BY ped-venda.no-ab-reppri
             BY ped-venda.nome-abrev
             by ped-item.it-codigo
             BY substr(ped-item.cod-refer,1,6)
             BY ped-item.cod-refer:

       run pi-acompanhar in h-acomp (input "Pedido: " + ped-venda.nr-pedcli).

       ASSIGN de-qtd-rep   = de-qtd-rep   + ped-item.qt-pedida
              de-qtd-cli   = de-qtd-cli   + ped-item.qt-pedida
              de-qtd-var   = de-qtd-var   + ped-item.qt-pedida
              de-qtd-des   = de-qtd-des   + ped-item.qt-pedida
              de-qtd-item  = de-qtd-item  + ped-item.qt-pedida
              de-qtd-total = de-qtd-total + ped-item.qt-pedida.

       if last-of(ped-item.cod-refer) then do:
          FIND ITEM WHERE ITEM.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
          FIND referencia WHERE referencia.cod-refer = ped-item.cod-refer NO-LOCK NO-ERROR.
          assign c-descricao = item.descricao-1 + item.descricao-2.
          display ped-venda.no-ab-reppri WHEN FIRST-OF(ped-venda.no-ab-reppri)
                  ped-venda.nome-abrev   WHEN FIRST-OF(ped-venda.nome-abrev)
                  ped-item.it-codigo
                  c-descricao
                  ped-item.cod-refer FORMAT "XX XXXX X"
                  referencia.descricao FORMAT "x(3)" WHEN AVAIL referencia
                  de-qtd-var
                  with frame f-rep-cli.
          down with frame f-rep-cli.
          ASSIGN de-qtd-var = 0.
       END.

       if last-of(substr(ped-item.cod-refer,1,6)) then do:
          display "Total Desenho"    @ ped-item.it-codigo
                  ped-item.cod-refer WHEN tt-param.tipo-rel = 2
                                     @ c-descricao
                  de-qtd-des         @ de-qtd-var
                  with frame f-rep-cli.
          DOWN 2 with frame f-rep-cli.
          find first tt-work 
               where tt-work.cod-desenho = ped-item.it-codigo + substr(ped-item.cod-refer,1,6)
               no-lock no-error.
          if not avail tt-work then do:
             create tt-work.
             assign tt-work.cod-desenho = ped-item.it-codigo + substr(ped-item.cod-refer,1,6)
                    tt-work.qtd-pedida  = 0.
          end.
          assign tt-work.qtd-pedida = tt-work.qtd-pedida + de-qtd-des.
          ASSIGN de-qtd-des = 0.
       end.

       if last-of(ped-item.it-codigo) then do:
          display "Total Item"        @ ped-item.it-codigo
                   ped-item.it-codigo @ c-descricao 
                  de-qtd-item         @ de-qtd-var
                  with frame f-rep-cli.
          DOWN 2 with frame f-rep-cli.
          assign de-qtd-item = 0.
       end.

       if last-of(ped-venda.nome-abrev) then do:
          display "Total Cliente"      @ ped-item.it-codigo
                  ped-venda.nome-abrev @ c-descricao 
                  de-qtd-cli           @ de-qtd-var
                  with frame f-rep-cli.
          DOWN 2 with frame f-rep-cli.
          assign de-qtd-cli = 0.
       end.
       
       if last-of(ped-venda.no-ab-reppri) then do:
          display "Total Repres"         @ ped-item.it-codigo
                  ped-venda.no-ab-reppri @ c-descricao 
                  de-qtd-rep             @ de-qtd-var
                  with frame f-rep-cli.
          DOWN 2 with frame f-rep-cli.
          assign de-qtd-rep = 0.
       end.
   end. 
   display "Total"      @ ped-item.it-codigo
           de-qtd-total @ de-qtd-var
           with frame f-rep-cli.
   assign de-qtd-total = 0.        
END.
ELSE DO: /* Detalhado, Resumido */
   for each ped-item where ped-item.it-codigo            >= tt-param.item-ini
                      and ped-item.it-codigo             <= tt-param.item-fin
                      and ped-item.cod-refer             >= tt-param.refer-ini
                      and ped-item.cod-refer             <= tt-param.refer-fin
                      and substr(ped-item.cod-refer,3,4) >= tt-param.desenho-ini
                      and substr(ped-item.cod-refer,3,4) <= tt-param.desenho-fin
                      and (lookup(substr(ped-item.cod-refer,3,4),c-lista-des) <> 0 or 
                                                                 c-lista-des = "")
                      and ((substr(ped-item.cod-refer,7,1) = "0" and tt-param.tipo-acab = 1) or
                           (substr(ped-item.cod-refer,7,1) > "0" and tt-param.tipo-acab = 2) or
                                                                    (tt-param.tipo-acab = 3))
                     and ped-item.cod-sit-item <> 6 
                    no-lock,
       each ped-venda of ped-item 
            where ped-venda.dt-implant   >= tt-param.dt-impl-ini
              and ped-venda.dt-implant   <= tt-param.dt-impl-fin
              and ped-venda.dt-entrega   >= tt-param.dt-entr-ini
              and ped-venda.dt-entrega   <= tt-param.dt-entr-fin
              and ped-venda.no-ab-reppri >= tt-param.repres-ini
              and ped-venda.no-ab-reppri <= tt-param.repres-fin
       no-lock,
       EACH ref-item-ext WHERE ref-item-ext.it-codigo    =  ped-item.it-codigo
                           AND ref-item-ext.cod-refer    =  ped-item.cod-refer
                           AND ref-item-ext.cod-obsoleto >= tt-param.cod-obsol-ini
                           AND ref-item-ext.cod-obsoleto <= tt-param.cod-obsol-fin
                         NO-LOCK
       break by ped-item.it-codigo
             BY substr(ped-item.cod-refer,1,6)
             BY ped-item.cod-refer:
        
       run pi-acompanhar in h-acomp (input "Pedido: " + ped-venda.nr-pedcli).
      
       ASSIGN de-qtd-var   = de-qtd-var   + ped-item.qt-pedida
              de-qtd-des   = de-qtd-des   + ped-item.qt-pedida
              de-qtd-item  = de-qtd-item  + ped-item.qt-pedida
              de-qtd-total = de-qtd-total + ped-item.qt-pedida.
  
       if last-of(ped-item.cod-refer) then do:
          FIND ITEM WHERE ITEM.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
          FIND referencia WHERE referencia.cod-refer = ped-item.cod-refer NO-LOCK NO-ERROR.
          assign c-descricao = item.descricao-1 + item.descricao-2.
          if tt-param.tipo-rel = 2 then do: /* Detalhado */
             display ped-item.it-codigo
                     c-descricao
                     ped-item.cod-refer FORMAT "XX XXXX X"
                     referencia.descricao FORMAT "x(3)" WHEN AVAIL referencia
                     de-qtd-var
                     with frame f-detalhe.
             down with frame f-detalhe.
          end.
          ASSIGN de-qtd-var = 0.
       END.
  
       if last-of(substr(ped-item.cod-refer,1,6)) then do:
          if tt-param.tipo-rel < 4 then do: /* Resumido ou Detalhado ou Por Repres/Cliente */
             display "Total Desenho"    @ ped-item.it-codigo
                     ped-item.cod-refer WHEN tt-param.tipo-rel = 3
                                        @ c-descricao
                     de-qtd-des         @ de-qtd-var
                     with frame f-detalhe.
            DOWN IF tt-param.tipo-rel = 2 THEN 2 
                                          ELSE 1 with frame f-detalhe.
          END.
          find first tt-work 
               where tt-work.cod-desenho = ped-item.it-codigo + substr(ped-item.cod-refer,1,6)
               no-lock no-error.
          if not avail tt-work then do:
             create tt-work.
             assign tt-work.cod-desenho = ped-item.it-codigo + substr(ped-item.cod-refer,1,6)
                    tt-work.qtd-pedida  = 0.
          end.
          assign tt-work.qtd-pedida = tt-work.qtd-pedida + de-qtd-des.
          ASSIGN de-qtd-des = 0.
       end.
      
       if last-of(ped-item.it-codigo) then do:
          if tt-param.tipo-rel < 4 then do: /* Resumido ou Detalhado ou Por Repres/Cliente */
             display "Total Item"       @ ped-item.it-codigo
                     ped-item.it-codigo WHEN tt-param.tipo-rel = 3
                                        @ c-descricao 
                     de-qtd-item        @ de-qtd-var
                     with frame f-detalhe.
             DOWN 2 with frame f-detalhe.
          end.   
          assign de-qtd-item = 0.
       end.
   end. 
   IF tt-param.tipo-rel < 4 THEN /* Resumido ou Detalhado */
      display "Total"      @ ped-item.it-codigo
              de-qtd-total @ de-qtd-var
              with frame f-detalhe.
   assign de-qtd-total = 0.        
END.

assign i-cont = 1.
PAGE.
put skip(1)
    "*-OS 10 DESENHOS MAIS VENDIDOS-*" skip
    "================================"
    skip(1).

for each tt-work by tt-work.qtd-pedida descend:
    if i-cont < 10 then do:
       display tt-work.cod-desenho
               tt-work.qtd-pedida
               with frame f-rank.
       down with frame f-rank.
    end.
    assign i-cont = i-cont + 1.
end.
for each tt-work:
    delete tt-work.
end.

IF tt-param.impr-param THEN DO:
   PAGE.
   PUT "****--------------- PAR¶METROS ----------------****".

   DISPLAY tt-param.repres-ini              
           tt-param.repres-fin     
           tt-param.cliente-ini             
           tt-param.cliente-fin    
           tt-param.grupo-ini               
           tt-param.grupo-fin      
           tt-param.item-ini                
           tt-param.item-fin       
           tt-param.refer-ini               
           tt-param.refer-fin     
           tt-param.desenho-ini   
           tt-param.desenho-fin 
           tt-param.cod-obsol-ini 
           tt-param.cod-obsol-fin 
           tt-param.dt-entr-ini             
           tt-param.dt-entr-fin    
           tt-param.dt-impl-ini             
           tt-param.dt-impl-fin    
           tt-param.desc-tipo-rel  
           tt-param.desc-tipo-acab
           WITH FRAME f-parlis.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.


