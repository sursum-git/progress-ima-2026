/* Programa: ESFT039.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Faturamento
** Objetivo: Listar o relatorio de Vendas por Item/Preco.
** Autor...: Gilvando Souza Araujo - Novembro/2002
** Obs.....: Especifico da TEAR TEXTIL IND.COM.LTDA.
**
** Conversao para EMS 2.04:
**   Programa: ESFT039.P  =>  ESFT0003RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 16/09/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESP ESFT0003RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       field classifica       as integer
       FIELD estab-ini        like nota-fiscal.cod-estabel              
       FIELD estab-fin        like nota-fiscal.cod-estabel   
       FIELD repres-ini       LIKE nota-fiscal.cod-rep 
       FIELD repres-fin       LIKE nota-fiscal.cod-rep
       FIELD grupo-ini        LIKE ITEM.ge-codigo
       FIELD grupo-fin        LIKE ITEM.ge-codigo
       FIELD item-ini         LIKE ITEM.it-codigo
       FIELD item-fin         LIKE ITEM.it-codigo
       FIELD dt-emis-ini      LIKE nota-fiscal.dt-emis-nota
       FIELD dt-emis-fin      LIKE nota-fiscal.dt-emis-nota
       FIELD c-emit-dup       AS CHAR FORMAT "x" 
       FIELD c-nota-can       AS CHAR format "x" 
       FIELD c-tipo-merc      AS CHAR FORMAT "x"
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

def TEMP-TABLE tt-work  /* Itens sem fator de conversao */
    field it-codigo like item.it-codigo
    INDEX ch-work it-codigo.

def var de-vlr-ite-per07 as dec extent 2 format ">>>,>>>,>>9.99".
def var de-vlr-ite-per12 as dec extent 2 format ">>>,>>>,>>9.99".
def var de-vlr-ite-per99 as dec extent 2 format ">>>,>>>,>>9.99".
def var de-vlr-ite-ldf07 as dec extent 2 format ">>>,>>>,>>9.99".
def var de-vlr-ite-ldf12 as dec extent 2 format ">>>,>>>,>>9.99".
def var de-vlr-ite-ldf99 as dec extent 2 format ">>>,>>>,>>9.99".
def var de-vlr-ite-ret07 as dec extent 2 format ">>>,>>>,>>9.99".
def var de-vlr-ite-ret12 as dec extent 2 format ">>>,>>>,>>9.99".
def var de-vlr-ite-ret99 as dec extent 2 format ">>>,>>>,>>9.99".
def var de-qtd-ite-per07 as dec extent 2 format ">>,>>>,>>9.99".
def var de-qtd-ite-per12 as dec extent 2 format ">>,>>>,>>9.99".
def var de-qtd-ite-per99 as dec extent 2 format ">>,>>>,>>9.99".
def var de-qtd-ite-ldf07 as dec extent 2 format ">>,>>>,>>9.99".
def var de-qtd-ite-ldf12 as dec extent 2 format ">>,>>>,>>9.99".
def var de-qtd-ite-ldf99 as dec extent 2 format ">>,>>>,>>9.99".
def var de-qtd-ite-ret07 as dec extent 2 format ">>,>>>,>>9.99".
def var de-qtd-ite-ret12 as dec extent 2 format ">>,>>>,>>9.99".
def var de-qtd-ite-ret99 as dec extent 2 format ">>,>>>,>>9.99".
def var de-vlr-ger-per07 as dec extent 2 format ">>>,>>>,>>9.99".
def var de-vlr-ger-per12 as dec extent 2 format ">>>,>>>,>>9.99".
def var de-vlr-ger-per99 as dec extent 2 format ">>>,>>>,>>9.99".
def var de-vlr-ger-ldf07 as dec extent 2 format ">>>,>>>,>>9.99".
def var de-vlr-ger-ldf12 as dec extent 2 format ">>>,>>>,>>9.99".
def var de-vlr-ger-ldf99 as dec extent 2 format ">>>,>>>,>>9.99".
def var de-vlr-ger-ret07 as dec extent 2 format ">>>,>>>,>>9.99".
def var de-vlr-ger-ret12 as dec extent 2 format ">>>,>>>,>>9.99".
def var de-vlr-ger-ret99 as dec extent 2 format ">>>,>>>,>>9.99".
def var de-qtd-ger-per07 as dec extent 2 format ">>,>>>,>>9.99".
def var de-qtd-ger-per12 as dec extent 2 format ">>,>>>,>>9.99".
def var de-qtd-ger-per99 as dec extent 2 format ">>,>>>,>>9.99".
def var de-qtd-ger-ldf07 as dec extent 2 format ">>,>>>,>>9.99".
def var de-qtd-ger-ldf12 as dec extent 2 format ">>,>>>,>>9.99".
def var de-qtd-ger-ldf99 as dec extent 2 format ">>,>>>,>>9.99".
def var de-qtd-ger-ret07 as dec extent 2 format ">>,>>>,>>9.99".
def var de-qtd-ger-ret12 as dec extent 2 format ">>,>>>,>>9.99".
def var de-qtd-ger-ret99 as dec extent 2 format ">>,>>>,>>9.99".
def var de-p-medio       as dec extent 3 format ">>>9.99".
def var de-vlr-tot       as dec extent 2 format ">>>,>>>,>>9.99".
def var de-qtd-tot       as dec extent 2 format ">>,>>>,>>9.99".
def var i-cont           as int.
def var c-classific      as char format "x(9)".
def var l-imp-desc       as log.
DEF VAR l-falta-fator    AS LOG.
def var de-qtd-conv      like it-nota-fisc.qt-faturada[1].

form
    tt-param.estab-ini    LABEL "Estabelecimento de" AT  1
    "a"                                              AT 40
    tt-param.estab-fin    NO-LABELS
    tt-param.repres-ini   LABEL "Representante de.." AT  1
    "a"                                              AT 40
    tt-param.repres-fin   NO-LABELS
    tt-param.grupo-ini    LABEL "Grupo Estoque de.." AT  1
    "a"                                              AT 40
    tt-param.grupo-fin    NO-LABELS                
    tt-param.item-ini     LABEL "Item de..........." AT  1
    "a"                                              AT 40
    tt-param.item-fin     NO-LABELS
    tt-param.dt-emis-ini  LABEL "Data EmissÆo de..." AT  1
    "a"                                              AT 40
    tt-param.dt-emis-fin  NO-LABELS
    with no-box side-labels width 132 STREAM-IO frame f-parlis.

form
    item.it-codigo       label "Item"      FORMAT "x(6)"
    item.descricao-1     label "Descricao"
    c-classific          label "Classific"
    de-vlr-ite-per07[1]  label "Valor Normal"
    de-qtd-ite-per07[1]  label "Qtde.Normal"
    de-p-medio[1]        label "Pr.Med"
    de-vlr-ite-per07[2]  label "Valor Topazio"
    de-qtd-ite-per07[2]  label "Qtde.Topazio"
    de-p-medio[2]        label "Pr.Med"
    de-p-medio[3]        label "PM.Ger"
    with no-box 55 down width 132 STREAM-IO frame f-detalhe no-label.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i FATURAMENTO * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Faturamento_por_Item/Pre‡o * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each tt-work.
    delete tt-work.
end.
        
for each nota-fiscal WHERE nota-fiscal.cod-estabel  >= tt-param.estab-ini   
                       and nota-fiscal.cod-estabel  <= tt-param.estab-fin   
                       and nota-fiscal.cod-rep      >= tt-param.repres-ini  
                       and nota-fiscal.cod-rep      <= tt-param.repres-fin  
                       and nota-fiscal.dt-emis-nota >= tt-param.dt-emis-ini 
                       and nota-fiscal.dt-emis-nota <= tt-param.dt-emis-fin
                       AND nota-fiscal.dt-cancela   =  ? 
                     no-lock,
    each it-nota-fisc of nota-fiscal where it-nota-fisc.it-codigo >= tt-param.item-ini 
                                       AND it-nota-fisc.it-codigo <= tt-param.item-fin 
                                     no-lock,
    each item where item.it-codigo  = it-nota-fisc.it-codigo
                AND item.ge-codigo >= tt-param.grupo-ini 
                and item.ge-codigo <= tt-param.grupo-fin 
              no-lock
    break by it-nota-fisc.it-codigo:

    run pi-acompanhar in h-acomp (input "Nota Fiscal: " + nota-fiscal.nr-nota-fis).

    find ped-venda WHERE ped-venda.nome-abrev = nota-fiscal.nome-ab-cli 
                     and ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli 
                   no-lock no-error.
  
    find natur-oper WHERE natur-oper.nat-operacao = it-nota-fisc.nat-operacao
                    no-lock no-error.
    if  avail natur-oper 
    and natur-oper.emite-duplic = yes then do:
        /*------ Conversao de M para Kg ------- */
        if item.un <> "m" then do:
           FIND item-ext WHERE ITEM-ext.it-codigo = ITEM.it-codigo
                         NO-LOCK NO-ERROR.
           if AVAIL item-ext then
              assign de-qtd-conv = it-nota-fisc.qt-faturada[1] * 
                                   item-ext.fator-conv.
           else do:
              assign l-falta-fator = yes
                     de-qtd-conv   = it-nota-fisc.qt-faturada[1].
              find first tt-work where
                         tt-work.it-codigo = item.it-codigo
                         no-lock no-error.
              if not avail tt-work then do:
                 create tt-work.
                 assign tt-work.it-codigo = item.it-codigo.
              end.
           end.   
        end.
        ELSE
           ASSIGN de-qtd-conv = it-nota-fisc.qt-faturada[1].

        if substr(it-nota-fisc.cod-refer,3,5) = "99510" then do: /*LD*/
           if it-nota-fisc.aliquota-icm = 7 then do:
              assign de-vlr-ite-ldf07[1] = de-vlr-ite-ldf07[1] +
                                           it-nota-fisc.vl-tot-item
                     de-qtd-ite-ldf07[1] = de-qtd-ite-ldf07[1] +
                                           de-qtd-conv.
              if ped-venda.tp-pedido = "t"
              or ped-venda.tp-pedido = "w" then
                 assign de-vlr-ite-ldf07[2] = de-vlr-ite-ldf07[2] +
                                              it-nota-fisc.vl-tot-item
                        de-qtd-ite-ldf07[2] = de-qtd-ite-ldf07[2] +
                                              de-qtd-conv.
           end.
           else
           if it-nota-fisc.aliquota-icm = 12 then do:
              assign de-vlr-ite-ldf12[1] = de-vlr-ite-ldf12[1] +
                                           it-nota-fisc.vl-tot-item
                     de-qtd-ite-ldf12[1] = de-qtd-ite-ldf12[1] +
                                           de-qtd-conv.
              if ped-venda.tp-pedido = "t"
              or ped-venda.tp-pedido = "w" then
                 assign de-vlr-ite-ldf12[2] = de-vlr-ite-ldf12[2] +
                                              it-nota-fisc.vl-tot-item
                        de-qtd-ite-ldf12[2] = de-qtd-ite-ldf12[2] +
                                              de-qtd-conv.
           end.
           else do: /* Outras aliquotas */
              assign de-vlr-ite-ldf99[1] = de-vlr-ite-ldf99[1] +
                                           it-nota-fisc.vl-tot-item
                     de-qtd-ite-ldf99[1] = de-qtd-ite-ldf99[1] +
                                           de-qtd-conv.
              if ped-venda.tp-pedido = "t"
              or ped-venda.tp-pedido = "w" then
                 assign de-vlr-ite-ldf99[2] = de-vlr-ite-ldf99[2] +
                                              it-nota-fisc.vl-tot-item
                        de-qtd-ite-ldf99[2] = de-qtd-ite-ldf99[2] +
                                              de-qtd-conv.
           end.
        end.
        else
        if substr(it-nota-fisc.cod-refer,3,5) = "99000"
        or substr(it-nota-fisc.cod-refer,3,5) = "99520" then do: /*Ret*/
           if it-nota-fisc.aliquota-icm = 7 then do:
              assign de-vlr-ite-ret07[1] = de-vlr-ite-ret07[1] +
                                           it-nota-fisc.vl-tot-item
                     de-qtd-ite-ret07[1] = de-qtd-ite-ret07[1] +
                                           de-qtd-conv.
              if ped-venda.tp-pedido = "t"
              or ped-venda.tp-pedido = "w" then
                 assign de-vlr-ite-ret07[2] = de-vlr-ite-ret07[2] +
                                              it-nota-fisc.vl-tot-item
                        de-qtd-ite-ret07[2] = de-qtd-ite-ret07[2] +
                                              de-qtd-conv.
           end.
           else
           if it-nota-fisc.aliquota-icm = 12 then do:
              assign de-vlr-ite-ret12[1] = de-vlr-ite-ret12[1] +
                                           it-nota-fisc.vl-tot-item
                     de-qtd-ite-ret12[1] = de-qtd-ite-ret12[1] +
                                           de-qtd-conv.
              if ped-venda.tp-pedido = "t"
              or ped-venda.tp-pedido = "w" then
                 assign de-vlr-ite-ret12[2] = de-vlr-ite-ret12[2] +
                                              it-nota-fisc.vl-tot-item
                        de-qtd-ite-ret12[2] = de-qtd-ite-ret12[2] +
                                              de-qtd-conv.
           end.
           else do: /* Outras aliquotas */
              assign de-vlr-ite-ldf99[1] = de-vlr-ite-ldf99[1] +
                                           it-nota-fisc.vl-tot-item
                     de-qtd-ite-ldf99[1] = de-qtd-ite-ldf99[1] +
                                           de-qtd-conv.
              if ped-venda.tp-pedido = "t"
              or ped-venda.tp-pedido = "w" then
                 assign de-vlr-ite-ldf99[2] = de-vlr-ite-ldf99[2] +
                                              it-nota-fisc.vl-tot-item
                        de-qtd-ite-ldf99[2] = de-qtd-ite-ldf99[2] +
                                              de-qtd-conv.
           end.
        end.
        else do: /*Perf*/
           if it-nota-fisc.aliquota-icm = 7 then do:
              assign de-vlr-ite-per07[1] = de-vlr-ite-per07[1] +
                                           it-nota-fisc.vl-tot-item
                     de-qtd-ite-per07[1] = de-qtd-ite-per07[1] +
                                           de-qtd-conv.
              if ped-venda.tp-pedido = "t"
              or ped-venda.tp-pedido = "w" then
                 assign de-vlr-ite-per07[2] = de-vlr-ite-per07[2] +
                                              it-nota-fisc.vl-tot-item
                        de-qtd-ite-per07[2] = de-qtd-ite-per07[2] +
                                              de-qtd-conv.
           end.
           else
           if it-nota-fisc.aliquota-icm = 12 then do:
              assign de-vlr-ite-per12[1] = de-vlr-ite-per12[1] +
                                           it-nota-fisc.vl-tot-item
                     de-qtd-ite-per12[1] = de-qtd-ite-per12[1] +
                                           de-qtd-conv.
              if ped-venda.tp-pedido = "t"
              or ped-venda.tp-pedido = "w" then
                 assign de-vlr-ite-per12[2] = de-vlr-ite-per12[2] +
                                              it-nota-fisc.vl-tot-item
                        de-qtd-ite-per12[2] = de-qtd-ite-per12[2] +
                                              de-qtd-conv.
           end.
           else do: /* Outras aliquotas */
              assign de-vlr-ite-per99[1] = de-vlr-ite-per99[1] +
                                           it-nota-fisc.vl-tot-item
                     de-qtd-ite-per99[1] = de-qtd-ite-per99[1] +
                                           de-qtd-conv.
              if ped-venda.tp-pedido = "t"
              or ped-venda.tp-pedido = "w" then
                 assign de-vlr-ite-per99[2] = de-vlr-ite-per99[2] +
                                              it-nota-fisc.vl-tot-item
                        de-qtd-ite-per99[2] = de-qtd-ite-per99[2] +
                                              de-qtd-conv.
           end.
        end.
    end.
     
    if last-of(it-nota-fisc.it-codigo) then do:
       do i-cont = 1 to 2:
          assign de-vlr-tot[i-cont] = de-vlr-ite-per07[i-cont] +
                                      de-vlr-ite-per12[i-cont] +
                                      de-vlr-ite-per99[i-cont] +
                                      de-vlr-ite-ldf07[i-cont] +
                                      de-vlr-ite-ldf12[i-cont] +
                                      de-vlr-ite-ldf99[i-cont] +
                                      de-vlr-ite-ret07[i-cont] +
                                      de-vlr-ite-ret12[i-cont] +
                                      de-vlr-ite-ret99[i-cont]
                 de-qtd-tot[i-cont] = de-qtd-ite-per07[i-cont] +
                                      de-qtd-ite-per12[i-cont] +
                                      de-qtd-ite-per99[i-cont] +
                                      de-qtd-ite-ldf07[i-cont] +
                                      de-qtd-ite-ldf12[i-cont] +
                                      de-qtd-ite-ldf99[i-cont] +
                                      de-qtd-ite-ret07[i-cont] +
                                      de-qtd-ite-ret12[i-cont] +
                                      de-qtd-ite-ret99[i-cont]
                 de-vlr-ger-per07[i-cont] = de-vlr-ger-per07[i-cont] +
                                            de-vlr-ite-per07[i-cont]  
                 de-vlr-ger-per12[i-cont] = de-vlr-ger-per12[i-cont] +
                                            de-vlr-ite-per12[i-cont] 
                 de-vlr-ger-per99[i-cont] = de-vlr-ger-per99[i-cont] +
                                            de-vlr-ite-per99[i-cont]
                 de-vlr-ger-ldf07[i-cont] = de-vlr-ger-ldf07[i-cont] +
                                            de-vlr-ite-ldf07[i-cont] 
                 de-vlr-ger-ldf12[i-cont] = de-vlr-ger-ldf12[i-cont] +
                                            de-vlr-ite-ldf12[i-cont]
                 de-vlr-ger-ldf99[i-cont] = de-vlr-ger-ldf99[i-cont] +
                                            de-vlr-ite-ldf99[i-cont]
                 de-vlr-ger-ret07[i-cont] = de-vlr-ger-ret07[i-cont] +
                                            de-vlr-ite-ret07[i-cont] 
                 de-vlr-ger-ret12[i-cont] = de-vlr-ger-ret12[i-cont] +
                                            de-vlr-ite-ret12[i-cont]
                 de-vlr-ger-ret99[i-cont] = de-vlr-ger-ret99[i-cont] +
                                            de-vlr-ite-ret99[i-cont]
                 de-qtd-ger-per07[i-cont] = de-qtd-ger-per07[i-cont] +
                                            de-qtd-ite-per07[i-cont] 
                 de-qtd-ger-per12[i-cont] = de-qtd-ger-per12[i-cont] +
                                            de-qtd-ite-per12[i-cont]
                 de-qtd-ger-per99[i-cont] = de-qtd-ger-per99[i-cont] +
                                            de-qtd-ite-per99[i-cont]
                 de-qtd-ger-ldf07[i-cont] = de-qtd-ger-ldf07[i-cont] +
                                            de-qtd-ite-ldf07[i-cont] 
                 de-qtd-ger-ldf12[i-cont] = de-qtd-ger-ldf12[i-cont] +
                                            de-qtd-ite-ldf12[i-cont]
                 de-qtd-ger-ldf99[i-cont] = de-qtd-ger-ldf99[i-cont] +
                                            de-qtd-ite-ldf99[i-cont]
                 de-qtd-ger-ret07[i-cont] = de-qtd-ger-ret07[i-cont] +
                                            de-qtd-ite-ret07[i-cont] 
                 de-qtd-ger-ret12[i-cont] = de-qtd-ger-ret12[i-cont] +
                                            de-qtd-ite-ret12[i-cont]
                 de-qtd-ger-ret99[i-cont] = de-qtd-ger-ret99[i-cont] +
                                            de-qtd-ite-ret99[i-cont].
       end.
       
       if de-vlr-tot[1] <> 0 or de-vlr-tot[2] <> 0 or
          de-qtd-tot[1] <> 0 or de-qtd-tot[2] <> 0 then do:
       
       assign de-p-medio[1] = de-vlr-ite-per07[1] / de-qtd-ite-per07[1]
              de-p-medio[2] = de-vlr-ite-per07[2] / de-qtd-ite-per07[2]
              de-p-medio[3] = (de-vlr-ite-per07[1] +
                              de-vlr-ite-per07[2]) / de-qtd-ite-per07[1] 
              c-classific   = "PE ICM-07".
       if de-vlr-ite-per07[1] <> 0
       or de-qtd-ite-per07[1] <> 0 then do:
          display item.it-codigo
                  item.descricao-1
                  c-classific
                  de-vlr-ite-per07[1]
                  de-qtd-ite-per07[1]
                  de-p-medio[1]
                  de-vlr-ite-per07[2]
                  de-qtd-ite-per07[2]
                  de-p-medio[2]
                  de-p-medio[3]
                  with frame f-detalhe.
          down with frame f-detalhe.
          assign l-imp-desc = yes.
       end.
       assign de-p-medio[1] = de-vlr-ite-per12[1] / de-qtd-ite-per12[1]
              de-p-medio[2] = de-vlr-ite-per12[2] / de-qtd-ite-per12[2]
              de-p-medio[3] = (de-vlr-ite-per12[1] +
                              de-vlr-ite-per12[2]) / de-qtd-ite-per12[1]
              c-classific   = "PE ICM-12".
       if de-vlr-ite-per12[1] <> 0
       or de-qtd-ite-per12[1] <> 0 then do:
          display item.it-codigo when l-imp-desc = no
                  item.descricao-1 when l-imp-desc = no
                  c-classific
                  de-vlr-ite-per12[1] @ de-vlr-ite-per07[1]
                  de-qtd-ite-per12[1] @ de-qtd-ite-per07[1]
                  de-p-medio[1]
                  de-vlr-ite-per12[2] @ de-vlr-ite-per07[2]
                  de-qtd-ite-per12[2] @ de-qtd-ite-per07[2]
                  de-p-medio[2]
                  de-p-medio[3]
                  with frame f-detalhe.
          down with frame f-detalhe.
          if l-imp-desc = no then
             assign l-imp-desc = yes.
       end.
       assign de-p-medio[1] = de-vlr-ite-per99[1] / de-qtd-ite-per99[1]
              de-p-medio[2] = de-vlr-ite-per99[2] / de-qtd-ite-per99[2]
              de-p-medio[3] = (de-vlr-ite-per99[1] +
                              de-vlr-ite-per99[2]) / de-qtd-ite-per99[1]
              c-classific   = "PE ICM-99".
       if de-vlr-ite-per99[1] <> 0 
       or de-qtd-ite-per99[1] <> 0 then do:
          display item.it-codigo when l-imp-desc = no
                  item.descricao-1 when l-imp-desc = no
                  c-classific
                  de-vlr-ite-per99[1] @ de-vlr-ite-per07[1]
                  de-qtd-ite-per99[1] @ de-qtd-ite-per07[1]
                  de-p-medio[1]
                  de-vlr-ite-per99[2] @ de-vlr-ite-per07[2]
                  de-qtd-ite-per99[2] @ de-qtd-ite-per07[2]
                  de-p-medio[2]
                  de-p-medio[3]
                  with frame f-detalhe.
          down with frame f-detalhe.
          if l-imp-desc = no then
             assign l-imp-desc = yes.
       end.
       assign de-p-medio[1] = de-vlr-ite-ldf07[1] / de-qtd-ite-ldf07[1]
              de-p-medio[2] = de-vlr-ite-ldf07[2] / de-qtd-ite-ldf07[2]
              de-p-medio[3] = (de-vlr-ite-ldf07[1] +
                              de-vlr-ite-ldf07[2]) / de-qtd-ite-ldf07[1]
              c-classific   = "LD ICM-07".
       if de-vlr-ite-ldf07[1] <> 0
       or de-qtd-ite-ldf07[1] <> 0 then do:
          display item.it-codigo when l-imp-desc = no
                  item.descricao-1 when l-imp-desc = no
                  c-classific
                  de-vlr-ite-ldf07[1] @ de-vlr-ite-per07[1]
                  de-qtd-ite-ldf07[1] @ de-qtd-ite-per07[1]
                  de-p-medio[1]
                  de-vlr-ite-ldf07[2] @ de-vlr-ite-per07[2]
                  de-qtd-ite-ldf07[2] @ de-qtd-ite-per07[2]
                  de-p-medio[2]
                  de-p-medio[3]
                  with frame f-detalhe.
          down with frame f-detalhe.
          if l-imp-desc = no then
             assign l-imp-desc = yes.
       end.
       assign de-p-medio[1] = de-vlr-ite-ldf12[1] / de-qtd-ite-ldf12[1]
              de-p-medio[2] = de-vlr-ite-ldf12[2] / de-qtd-ite-ldf12[2]
              de-p-medio[3] = (de-vlr-ite-ldf12[1] +
                              de-vlr-ite-ldf12[2]) / de-qtd-ite-ldf12[1]
              c-classific   = "LD ICM-12".
       if de-vlr-ite-ldf12[1] <> 0 
       or de-qtd-ite-ldf12[1] <> 0 then do:
          display item.it-codigo when l-imp-desc = no
                  item.descricao-1 when l-imp-desc = no
                  c-classific
                  de-vlr-ite-ldf12[1] @ de-vlr-ite-per07[1]
                  de-qtd-ite-ldf12[1] @ de-qtd-ite-per07[1]
                  de-p-medio[1]
                  de-vlr-ite-ldf12[2] @ de-vlr-ite-per07[2]
                  de-qtd-ite-ldf12[2] @ de-qtd-ite-per07[2]
                  de-p-medio[2]
                  de-p-medio[3]
                  with frame f-detalhe.
          down with frame f-detalhe.
          if l-imp-desc = no then
             assign l-imp-desc = yes.
       end.
       assign de-p-medio[1] = de-vlr-ite-ldf99[1] / de-qtd-ite-ldf99[1]
              de-p-medio[2] = de-vlr-ite-ldf99[2] / de-qtd-ite-ldf99[2]
              de-p-medio[3] = (de-vlr-ite-ldf99[1] +
                              de-vlr-ite-ldf99[2]) / de-qtd-ite-ldf99[1]
              c-classific   = "LD ICM-99".
       if de-vlr-ite-ldf99[1] <> 0
       or de-qtd-ite-ldf99[1] <> 0 then do:
          display item.it-codigo when l-imp-desc = no
                  item.descricao-1 when l-imp-desc = no
                  c-classific
                  de-vlr-ite-ldf99[1] @ de-vlr-ite-per07[1]
                  de-qtd-ite-ldf99[1] @ de-qtd-ite-per07[1]
                  de-p-medio[1]
                  de-vlr-ite-ldf99[2] @ de-vlr-ite-per07[2]
                  de-qtd-ite-ldf99[2] @ de-qtd-ite-per07[2]
                  de-p-medio[2]
                  de-p-medio[3]
                  with frame f-detalhe.
          down with frame f-detalhe.
          if l-imp-desc = no then
             assign l-imp-desc = yes.
       end.
       assign de-p-medio[1] = de-vlr-ite-ret07[1] / de-qtd-ite-ret07[1]
              de-p-medio[2] = de-vlr-ite-ret07[2] / de-qtd-ite-ret07[2]
              de-p-medio[3] = (de-vlr-ite-ret07[1] +
                              de-vlr-ite-ret07[2]) / de-qtd-ite-ret07[1]
              c-classific   = "RT ICM-07".
       if de-vlr-ite-ret07[1] <> 0 
       or de-qtd-ite-ret07[1] <> 0 then do:
          display item.it-codigo when l-imp-desc = no
                  item.descricao-1 when l-imp-desc = no
                  c-classific
                  de-vlr-ite-ret07[1] @ de-vlr-ite-per07[1]
                  de-qtd-ite-ret07[1] @ de-qtd-ite-per07[1]
                  de-p-medio[1]
                  de-vlr-ite-ret07[2] @ de-vlr-ite-per07[2]
                  de-qtd-ite-ret07[2] @ de-qtd-ite-per07[2]
                  de-p-medio[2]
                  de-p-medio[3]
                  with frame f-detalhe.
          down with frame f-detalhe.
          if l-imp-desc = no then
             assign l-imp-desc = yes. 
       end.
       assign de-p-medio[1] = de-vlr-ite-ret12[1] / de-qtd-ite-ret12[1]
              de-p-medio[2] = de-vlr-ite-ret12[2] / de-qtd-ite-ret12[2]
              de-p-medio[3] = (de-vlr-ite-ret12[1] +
                              de-vlr-ite-ret12[2]) / de-qtd-ite-ret12[1]
              c-classific   = "RT ICM-12".
       if de-vlr-ite-ret12[1] <> 0 
       or de-qtd-ite-ret12[1] <> 0 then do:
          display item.it-codigo when l-imp-desc = no
                  item.descricao-1 when l-imp-desc = no
                  c-classific
                  de-vlr-ite-ret12[1] @ de-vlr-ite-per07[1]
                  de-qtd-ite-ret12[1] @ de-qtd-ite-per07[1]
                  de-p-medio[1]
                  de-vlr-ite-ret12[2] @ de-vlr-ite-per07[2]
                  de-qtd-ite-ret12[2] @ de-qtd-ite-per07[2]
                  de-p-medio[2]
                  de-p-medio[3]
                  with frame f-detalhe.
          down with frame f-detalhe.
       end.
       assign de-p-medio[1] = de-vlr-ite-ret99[1] / de-qtd-ite-ret99[1]
              de-p-medio[2] = de-vlr-ite-ret99[2] / de-qtd-ite-ret99[2]
              de-p-medio[3] = (de-vlr-ite-ret99[1] +
                              de-vlr-ite-ret99[2]) / de-qtd-ite-ret99[1]
              c-classific   = "RT ICM-99".
       if de-vlr-ite-ret99[1] <> 0 
       or de-qtd-ite-ret99[1] <> 0 then do:
          display item.it-codigo when l-imp-desc = no
                  item.descricao-1 when l-imp-desc = no
                  c-classific
                  de-vlr-ite-ret99[1] @ de-vlr-ite-per07[1]
                  de-qtd-ite-ret99[1] @ de-qtd-ite-per07[1]
                  de-p-medio[1]
                  de-vlr-ite-ret99[2] @ de-vlr-ite-per07[2]
                  de-qtd-ite-ret99[2] @ de-qtd-ite-per07[2]
                  de-p-medio[2]
                  de-p-medio[3]
                  with frame f-detalhe.
          down with frame f-detalhe.
          if l-imp-desc = no then
             assign l-imp-desc = yes.
       end.
       assign de-p-medio[1] = de-vlr-tot[1] / de-qtd-tot[1]
              de-p-medio[2] = de-vlr-tot[2] / de-qtd-tot[2]
              de-p-medio[3] = (de-vlr-tot[1] + de-vlr-tot[2]) /
                                               de-qtd-tot[1]
              c-classific   = "TOTAL.....".
       if de-vlr-tot[1] <> 0 
       or de-qtd-tot[1] <> 0 then do:
          display c-classific
                  de-vlr-tot[1] @ de-vlr-ite-per07[1]
                  de-qtd-tot[1] @ de-qtd-ite-per07[1]
                  de-p-medio[1]
                  de-vlr-tot[2] @ de-vlr-ite-per07[2]
                  de-qtd-tot[2] @ de-qtd-ite-per07[2]
                  de-p-medio[2]
                  de-p-medio[3]
                  with frame f-detalhe.
          down 2 with frame f-detalhe.
       end.
       assign l-imp-desc = no.
       do i-cont = 1 to 2:          
          assign de-vlr-ite-per07[i-cont] = 0
                 de-vlr-ite-per12[i-cont] = 0
                 de-vlr-ite-per99[i-cont] = 0
                 de-vlr-ite-ldf07[i-cont] = 0
                 de-vlr-ite-ldf12[i-cont] = 0
                 de-vlr-ite-ldf99[i-cont] = 0
                 de-vlr-ite-ret07[i-cont] = 0
                 de-vlr-ite-ret12[i-cont] = 0
                 de-vlr-ite-ret99[i-cont] = 0
                 de-qtd-ite-per07[i-cont] = 0
                 de-qtd-ite-per12[i-cont] = 0
                 de-qtd-ite-per99[i-cont] = 0
                 de-qtd-ite-ldf07[i-cont] = 0
                 de-qtd-ite-ldf12[i-cont] = 0
                 de-qtd-ite-ldf99[i-cont] = 0
                 de-qtd-ite-ret07[i-cont] = 0
                 de-qtd-ite-ret12[i-cont] = 0
                 de-qtd-ite-ret99[i-cont] = 0.
       end.
      end.       
    end.
end.

do i-cont = 1 to 2:
   assign de-vlr-tot[i-cont] = de-vlr-ger-per07[i-cont] +
                               de-vlr-ger-per12[i-cont] +
                               de-vlr-ger-per99[i-cont] +
                               de-vlr-ger-ldf07[i-cont] +
                               de-vlr-ger-ldf12[i-cont] +
                               de-vlr-ger-ldf99[i-cont] +
                               de-vlr-ger-ret07[i-cont] +
                               de-vlr-ger-ret12[i-cont] +
                               de-vlr-ger-ret99[i-cont]
          de-qtd-tot[i-cont] = de-qtd-ger-per07[i-cont] +
                               de-qtd-ger-per12[i-cont] +
                               de-qtd-ger-per99[i-cont] +
                               de-qtd-ger-ldf07[i-cont] +
                               de-qtd-ger-ldf12[i-cont] +
                               de-qtd-ger-ldf99[i-cont] +
                               de-qtd-ger-ret07[i-cont] +
                               de-qtd-ger-ret12[i-cont] +
                               de-qtd-ger-ret99[i-cont].
end.
       
if de-vlr-tot[1] <> 0
or de-vlr-tot[2] <> 0
or de-qtd-tot[1] <> 0
or de-qtd-tot[2] <> 0 then do:
   assign de-p-medio[1] = de-vlr-ger-per07[1] / de-qtd-ger-per07[1]
          de-p-medio[2] = de-vlr-ger-per07[2] / de-qtd-ger-per07[2]
          de-p-medio[3] = (de-vlr-ger-per07[1] + de-vlr-ger-per07[2]) /
                                                 de-qtd-ger-per07[1]
          c-classific   = "PE ICM-07".
   
   if de-vlr-ger-per07[1] <> 0 
   or de-qtd-ger-per07[1] <> 0 then do:
      display "Total Geral"        @ item.descricao-1
               c-classific
               de-vlr-ger-per07[1] @ de-vlr-ite-per07[1]
               de-qtd-ger-per07[1] @ de-qtd-ite-per07[1]
               de-p-medio[1]
               de-vlr-ger-per07[2] @ de-vlr-ite-per07[2]
               de-qtd-ger-per07[2] @ de-qtd-ite-per07[2]
               de-p-medio[2]
               de-p-medio[3]
               with frame f-detalhe.
      down with frame f-detalhe.
      assign l-imp-desc = yes.
   end.      
   assign de-p-medio[1] = de-vlr-ger-per12[1] / de-qtd-ger-per12[1]
          de-p-medio[2] = de-vlr-ger-per12[2] / de-qtd-ger-per12[2]
          de-p-medio[3] = (de-vlr-ger-per12[1] + de-vlr-ger-per12[2]) /
                                                 de-qtd-ger-per12[1]
          c-classific   = "PE ICM-12".
          
   if de-vlr-ger-per12[1] <> 0
   or de-qtd-ger-per12[1] <> 0 then do:
      display "Total Geral" when l-imp-desc = no @ ITEM.descricao-1
              c-classific
              de-vlr-ger-per12[1] @ de-vlr-ite-per07[1]
              de-qtd-ger-per12[1] @ de-qtd-ite-per07[1]
              de-p-medio[1]
              de-vlr-ger-per12[2] @ de-vlr-ite-per07[2]
              de-qtd-ger-per12[2] @ de-qtd-ite-per07[2]
              de-p-medio[2]
              de-p-medio[3]
              with frame f-detalhe.
      down with frame f-detalhe.
      if l-imp-desc = no then
         assign l-imp-desc = yes.
   end.   
   assign de-p-medio[1] = de-vlr-ger-per99[1] / de-qtd-ger-per99[1]
          de-p-medio[2] = de-vlr-ger-per99[2] / de-qtd-ger-per99[2]
          de-p-medio[3] = (de-vlr-ger-per99[1] + de-vlr-ger-per99[2]) /
                                                 de-qtd-ger-per99[1]
          c-classific   = "PE ICM-99".
          
   if de-vlr-ger-per99[1] <> 0
   or de-qtd-ger-per99[1] <> 0 then do:
      display "Total Geral" when l-imp-desc = no @ item.descricao-1
              c-classific
              de-vlr-ger-per99[1] @ de-vlr-ite-per07[1]
              de-qtd-ger-per99[1] @ de-qtd-ite-per07[1]
              de-p-medio[1]
              de-vlr-ger-per99[2] @ de-vlr-ite-per07[2]
              de-qtd-ger-per99[2] @ de-qtd-ite-per07[2]
              de-p-medio[2]
              de-p-medio[3]
              with frame f-detalhe.
      down with frame f-detalhe.
      if l-imp-desc = no then
         assign l-imp-desc = yes.
   end.
   assign de-p-medio[1] = de-vlr-ger-ldf07[1] / de-qtd-ger-ldf07[1]
          de-p-medio[2] = de-vlr-ger-ldf07[2] / de-qtd-ger-ldf07[2]
          de-p-medio[3] = (de-vlr-ger-ldf07[1] + de-vlr-ger-ldf07[2]) /
                                                 de-qtd-ger-ldf07[1]
          c-classific   = "LD ICM-07".
   if de-vlr-ger-ldf07[1] <> 0 
   or de-qtd-ger-ldf07[1] <> 0 then do:
      display "Total Geral" when l-imp-desc = no @ item.descricao-1
              c-classific
              de-vlr-ger-ldf07[1] @ de-vlr-ite-per07[1]
              de-qtd-ger-ldf07[1] @ de-qtd-ite-per07[1]
              de-p-medio[1]
              de-vlr-ger-ldf07[2] @ de-vlr-ite-per07[2]
              de-qtd-ger-ldf07[2] @ de-qtd-ite-per07[2]
              de-p-medio[2]
              de-p-medio[3]
              with frame f-detalhe.
      down with frame f-detalhe.
      if l-imp-desc = no then
         assign l-imp-desc = yes.
   end. 
   assign de-p-medio[1] = de-vlr-ger-ldf12[1] / de-qtd-ger-ldf12[1]
          de-p-medio[2] = de-vlr-ger-ldf12[2] / de-qtd-ger-ldf12[2]
          de-p-medio[3] = (de-vlr-ger-ldf12[1] + de-vlr-ger-ldf12[2]) /
                                                 de-qtd-ger-ldf12[1]
          c-classific   = "LD ICM-12".
   if de-vlr-ger-ldf12[1] <> 0
   or de-qtd-ger-ldf12[1] <> 0 then do:
      display "Total Geral" when l-imp-desc = no @ item.descricao-1
              c-classific
              de-vlr-ger-ldf12[1] @ de-vlr-ite-per07[1]
              de-qtd-ger-ldf12[1] @ de-qtd-ite-per07[1]
              de-p-medio[1]
              de-vlr-ger-ldf12[2] @ de-vlr-ite-per07[2]
              de-qtd-ger-ldf12[2] @ de-qtd-ite-per07[2]
              de-p-medio[2]
              de-p-medio[3]
              with frame f-detalhe.
     down with frame f-detalhe.
     if l-imp-desc = no then
        assign l-imp-desc = yes.
   end.
   assign de-p-medio[1] = de-vlr-ger-ldf99[1] / de-qtd-ger-ldf99[1]
          de-p-medio[2] = de-vlr-ger-ldf99[2] / de-qtd-ger-ldf99[2]
          de-p-medio[3] = (de-vlr-ger-ldf99[1] + de-vlr-ger-ldf99[2]) /
                                                 de-qtd-ger-ldf99[1]
          c-classific   = "LD ICM-99".
   if de-vlr-ger-ldf99[1] <> 0
   or de-qtd-ger-ldf99[1] <> 0 then do:
      display "Total Geral" when l-imp-desc = no @ item.descricao-1
              c-classific
              de-vlr-ger-ldf99[1] @ de-vlr-ite-per07[1]
              de-qtd-ger-ldf99[1] @ de-qtd-ite-per07[1]
              de-p-medio[1]
              de-vlr-ger-ldf99[2] @ de-vlr-ite-per07[2]
              de-qtd-ger-ldf99[2] @ de-qtd-ite-per07[2]
              de-p-medio[2]
              de-p-medio[3]
              with frame f-detalhe.
     down with frame f-detalhe.
     if l-imp-desc = no then
        assign l-imp-desc = yes.
   end.
   assign de-p-medio[1] = de-vlr-ger-ret07[1] / de-qtd-ger-ret07[1]
          de-p-medio[2] = de-vlr-ger-ret07[2] / de-qtd-ger-ret07[2]
          de-p-medio[3] = (de-vlr-ger-ret07[1] + de-vlr-ger-ret07[2]) /
                                                 de-qtd-ger-ret07[1]
          c-classific   = "RT ICM-07".
   if de-vlr-ger-ret07[1] <> 0
   or de-qtd-ger-ret07[1] <> 0 then do:
      display "Total Geral" when l-imp-desc = no @ item.descricao-1
              c-classific
              de-vlr-ger-ret07[1] @ de-vlr-ite-per07[1]
              de-qtd-ger-ret07[1] @ de-qtd-ite-per07[1]
              de-p-medio[1]
              de-vlr-ger-ret07[2] @ de-vlr-ite-per07[2]
              de-qtd-ger-ret07[2] @ de-qtd-ite-per07[2]
              de-p-medio[2]
              de-p-medio[3]
              with frame f-detalhe.
      down with frame f-detalhe.
      if l-imp-desc = no then
         assign l-imp-desc = yes.
   end.
   assign de-p-medio[1] = de-vlr-ger-ret12[1] / de-qtd-ger-ret12[1]
          de-p-medio[2] = de-vlr-ger-ret12[2] / de-qtd-ger-ret12[2]
          de-p-medio[3] = (de-vlr-ger-ret12[1] + de-vlr-ger-ret12[2]) /
                                                 de-qtd-ger-ret12[1]
          c-classific   = "RT ICM-12".
   if de-vlr-ger-ret12[1] <> 0
   or de-qtd-ger-ret12[1] <> 0 then do:
      display "Total Geral" when l-imp-desc = no @ item.descricao-1
              c-classific
              de-vlr-ger-ret12[1] @ de-vlr-ite-per07[1]
              de-qtd-ger-ret12[1] @ de-qtd-ite-per07[1]
              de-p-medio[1]
              de-vlr-ger-ret12[2] @ de-vlr-ite-per07[2]
              de-qtd-ger-ret12[2] @ de-qtd-ite-per07[2]
              de-p-medio[2]
              de-p-medio[3]
              with frame f-detalhe.
      down with frame f-detalhe.
      if l-imp-desc = no then
         assign l-imp-desc = yes.
   end.
   assign de-p-medio[1] = de-vlr-ger-ret99[1] / de-qtd-ger-ret99[1]
          de-p-medio[2] = de-vlr-ger-ret99[2] / de-qtd-ger-ret99[2]
          de-p-medio[3] = (de-vlr-ger-ret99[1] + de-vlr-ger-ret99[2]) /
                                                 de-qtd-ger-ret99[1]
          c-classific   = "RT ICM-99".
   if de-vlr-ger-ret99[1] <> 0
   or de-qtd-ger-ret99[1] <> 0 then do:
      display "Total Geral" when l-imp-desc = no @ item.descricao-1
              c-classific
              de-vlr-ger-ret99[1] @ de-vlr-ite-per07[1]
              de-qtd-ger-ret99[1] @ de-qtd-ite-per07[1]
              de-p-medio[1]
              de-vlr-ger-ret99[2] @ de-vlr-ite-per07[2]
              de-qtd-ger-ret99[2] @ de-qtd-ite-per07[2]
              de-p-medio[2]
              de-p-medio[3]
              with frame f-detalhe.
      down with frame f-detalhe.
      if l-imp-desc = no then
         assign l-imp-desc = yes.
   end.
   assign de-p-medio[1] = de-vlr-tot[1] / de-qtd-tot[1]
          de-p-medio[2] = de-vlr-tot[2] / de-qtd-tot[2]
          de-p-medio[3] = (de-vlr-tot[1] + de-vlr-tot[2]) /
                                           de-qtd-tot[1]
          c-classific   = "TOTAL.....".
   if de-vlr-tot[1] <> 0
   or de-qtd-tot[1] <> 0 then
      display c-classific
              de-vlr-tot[1] @ de-vlr-ite-per07[1]
              de-qtd-tot[1] @ de-qtd-ite-per07[1]
              de-p-medio[1]
              de-vlr-tot[2] @ de-vlr-ite-per07[2]
              de-qtd-tot[2] @ de-qtd-ite-per07[2]
              de-p-medio[2]
              de-p-medio[3]
              with frame f-detalhe.
      
   do i-cont = 1 to 2:          
      assign de-vlr-ger-per07[i-cont] = 0
             de-vlr-ger-per12[i-cont] = 0
             de-vlr-ger-per99[i-cont] = 0
             de-vlr-ger-ldf07[i-cont] = 0
             de-vlr-ger-ldf12[i-cont] = 0
             de-vlr-ger-ldf99[i-cont] = 0
             de-vlr-ger-ret07[i-cont] = 0
             de-vlr-ger-ret12[i-cont] = 0
             de-vlr-ger-ret99[i-cont] = 0
             de-qtd-ger-per07[i-cont] = 0
             de-qtd-ger-per12[i-cont] = 0
             de-qtd-ger-per99[i-cont] = 0
             de-qtd-ger-ldf07[i-cont] = 0
             de-qtd-ger-ldf12[i-cont] = 0
             de-qtd-ger-ldf99[i-cont] = 0
             de-qtd-ger-ret07[i-cont] = 0
             de-qtd-ger-ret12[i-cont] = 0
             de-qtd-ger-ret99[i-cont] = 0.
   end.
end. 

if l-falta-fator then do:
   page.
   put "Atencao ! - Ha itens sem fator de conversao:"
       skip(1).
   for each tt-work:
       display tt-work.it-codigo @ item.it-codigo
               with frame f-detalhe.
       down with frame f-detalhe.
   end.
   for each tt-work.
       delete tt-work.
   end.
end.   

IF tt-param.impr-param THEN DO:
   PAGE.
   PUT "*****----- PAR¶METROS ------*****"
       SKIP(1).

   DISPLAY tt-param.estab-ini       
           tt-param.estab-fin                                             
           tt-param.repres-ini      
           tt-param.repres-fin    
           tt-param.grupo-ini       
           tt-param.grupo-fin     
           tt-param.item-ini        
           tt-param.item-fin      
           tt-param.dt-emis-ini    
           tt-param.dt-emis-fin       
           WITH FRAME f-parlis.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.



