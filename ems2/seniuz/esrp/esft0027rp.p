/* Programa: ESFT023.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Faturamento
** Objetivo: Gerar o relatorio de Faturamento por Representante/Estado
** Autor...: Gilvando de Souza Araujo - Maio/97
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL / SANTA ELISABETH
**
** Conversao para EMS 2.04:
**   Programa: ESFT023.P  =>  ESFT0027RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 22/01/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESP ESFT0027RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       field classifica       as integer
       FIELD c-estab-ini      like nota-fiscal.cod-estabel              
       FIELD c-estab-fim      like nota-fiscal.cod-estabel   
       FIELD c-repres-ini     LIKE nota-fiscal.no-ab-reppri 
       FIELD c-repres-fim     LIKE nota-fiscal.no-ab-reppri
       FIELD c-cliente-ini    LIKE nota-fiscal.nome-ab-cli
       FIELD c-cliente-fim    LIKE nota-fiscal.nome-ab-cli
       FIELD i-grupo-ini      LIKE ITEM.ge-codigo
       FIELD i-grupo-fim      LIKE ITEM.ge-codigo
       FIELD c-item-ini       LIKE ITEM.it-codigo
       FIELD c-item-fim       LIKE ITEM.it-codigo
       FIELD c-espdoc-ini     like nota-fiscal.esp-docto
       FIELD c-espdoc-fim     like nota-fiscal.esp-docto
       FIELD da-emis-ini      LIKE nota-fiscal.dt-emis-nota
       FIELD da-emis-fim      LIKE nota-fiscal.dt-emis-nota
       FIELD i-condpag-ini    like nota-fiscal.cod-cond-pag
       FIELD i-condpag-fim    like nota-fiscal.cod-cond-pag
       FIELD c-natoper-ini    like nota-fiscal.nat-operacao
       FIELD c-natoper-fim    like nota-fiscal.nat-operacao 
       FIELD c-emit-dup       AS   CHAR FORMAT "x" 
       FIELD c-nota-can       AS   CHAR format "x" 
       FIELD c-tipo-merc      AS   CHAR FORMAT "x"
       FIELD impr-param       AS   LOGICAL.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

def TEMP-TABLE w-work
    field no-ab-reppri like nota-fiscal.no-ab-reppri
    field estado       like emitente.estado
    field cidade       like emitente.cidade
    field quantidade   as dec format "->>>>,>>9.9999"
    field valor        as dec format "->,>>>,>>9.99"
    INDEX ch-work no-ab-reppri
                  estado
                  cidade.

def TEMP-TABLE tt-work  /* Itens sem fator de conversao */
    field it-codigo like item.it-codigo
    INDEX ch-work it-codigo.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

def var de-qtd-not as dec format "->>>>,>>9.9999".
def var de-qtd-est as dec format "->>>>,>>9.9999".
def var de-qtd-rep as dec format "->>>>,>>9.9999".
def var de-qtd-ger as dec format "->>>>,>>9.9999".
def var de-vlr-not as dec format "->>>>,>>9.9999".
DEF VAR de-qt-conv AS DEC FORMAT "->>>>,>>9.9999".
def var de-vlr-est as dec format "->,>>>,>>9.99".
def var de-vlr-rep as dec format "->,>>>,>>9.99".
def var de-vlr-ger as dec format "->,>>>,>>9.99".
def var c-opcao as char.
def var l-imp-tot-est as log.
def var l-prim-vez as log.
DEF VAR l-falta-fator AS LOG.

form
    tt-param.c-estab-ini    LABEL "Estabelecimento de" AT 6
    "a"                                                AT 46
    tt-param.c-estab-fim    NO-LABELS
    tt-param.c-repres-ini   LABEL "Representante de"   AT 8
    "a"                                                AT 46
    tt-param.c-repres-fim   NO-LABELS
    tt-param.c-cliente-ini  LABEL "Cliente de"         AT 14
    "a"                                                AT 46
    tt-param.c-cliente-fim  NO-LABELS  
    tt-param.i-grupo-ini    LABEL "Grupo Estoque de"   AT 8
    "a"                                                AT 46
    tt-param.i-grupo-fim    NO-LABELS                
    tt-param.c-item-ini     LABEL "Item de"            AT 17
    "a"                                                AT 46
    tt-param.c-item-fim     NO-LABELS
    tt-param.c-espdoc-ini   LABEL "Esp‚cie NF de"      AT 11
    "a"                                                AT 46
    tt-param.c-espdoc-fim   NO-LABELS
    tt-param.da-emis-ini    LABEL "Data EmissÆo de"    AT 9
    "a"                                                AT 46
    tt-param.da-emis-fim    NO-LABELS
    tt-param.i-condpag-ini  LABEL "Cond Pagto de"      AT 11
    "a"                                                AT 46
    tt-param.i-condpag-fim  NO-LABELS
    tt-param.c-natoper-ini  LABEL "Nat Opera‡Æo de"    AT 9
    "a"                                                AT 46
    tt-param.c-natoper-fim  NO-LABELS
    tt-param.c-emit-dup     LABEL "Emite Duplicata"    AT 9  FORMAT "x(15)"      
    tt-param.c-nota-can     LABEL "Notas Canceladas"   AT 8  FORMAT "x(15)" 
    tt-param.c-tipo-merc    LABEL "Mercado"            AT 17 FORMAT "x(15)" 
    with no-box side-labels width 132 STREAM-IO frame f-parlis.

form
    repres.cod-rep       LABEL "Repres"
    w-work.no-ab-reppri  label "Nome Abreviado"
    w-work.estado        label "UF"
    w-work.cidade        label "Cidade"
    w-work.quantidade    label "Quantidade"
    w-work.valor         label "Valor"
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
{utp/ut-liter.i Faturamento_por_Representante/Estado * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

FOR each nota-fiscal where nota-fiscal.cod-estabel  >= tt-param.c-estab-ini
                       and nota-fiscal.cod-estabel  <= tt-param.c-estab-fim
                       and nota-fiscal.nome-ab-cli  >= tt-param.c-cliente-ini
                       and nota-fiscal.nome-ab-cli  <= tt-param.c-cliente-fim
                       and nota-fiscal.no-ab-reppri >= tt-param.c-repres-ini
                       and nota-fiscal.no-ab-reppri <= tt-param.c-repres-fim
                       and nota-fiscal.dt-emis-nota >= tt-param.da-emis-ini
                       and nota-fiscal.dt-emis-nota <= tt-param.da-emis-fim
                       and nota-fiscal.cod-cond-pag >= tt-param.i-condpag-ini
                       and nota-fiscal.cod-cond-pag <= tt-param.i-condpag-fim
                       and nota-fiscal.esp-docto    >= tt-param.c-espdoc-ini
                       and nota-fiscal.esp-docto    <= tt-param.c-espdoc-fim
                       and nota-fiscal.nat-operacao >= tt-param.c-natoper-ini
                       and nota-fiscal.nat-operacao <= tt-param.c-natoper-fim
                     no-lock.

   run pi-acompanhar in h-acomp (input "Nota Fiscal: " + nota-fiscal.nr-nota-fis).
   
   if (c-emit-dup = "N" and nota-fiscal.emite-duplic = yes)
   or (c-emit-dup = "S" and nota-fiscal.emite-duplic = no) then next.

   if (c-nota-can = "N" and nota-fiscal.dt-cancela <> ?)
   or (c-nota-can = "S" and nota-fiscal.dt-cancela = ?) then next.

   if (c-tipo-merc = "I" and nota-fiscal.mercado <> 1)
   or (c-tipo-merc = "E" and nota-fiscal.mercado =  1) then next.

   assign de-qtd-not = 0
          de-vlr-not = 0.

   for each it-nota-fisc of nota-fiscal WHERE
            it-nota-fisc.it-codigo >= tt-param.c-item-ini AND
            it-nota-fisc.it-codigo <= tt-param.c-item-fim NO-LOCK,
       EACH ITEM OF it-nota-fisc WHERE
            item.ge-codigo >= tt-param.i-grupo-ini AND
            item.ge-codigo <= tt-param.i-grupo-fim NO-LOCK.

            if it-nota-fisc.un-fatur[1] <> "m" then do:
               FIND item-ext WHERE ITEM-ext.it-codigo = it-nota-fisc.it-codigo
                             NO-LOCK NO-ERROR.
               if AVAIL item-ext then
                  assign de-qt-conv = it-nota-fisc.qt-faturada[1] * 
                                      item-ext.fator-conv.
               else do:
                  assign l-falta-fator = yes
                         de-qt-conv    = it-nota-fisc.qt-faturada[1].
                  find first tt-work where
                             tt-work.it-codigo = it-nota-fisc.it-codigo
                             no-lock no-error.
                  if not avail tt-work then do:
                     create tt-work.
                     assign tt-work.it-codigo = it-nota-fisc.it-codigo.
                  end.
               end.   
            end.
            ELSE
               ASSIGN de-qt-conv = it-nota-fisc.qt-faturada[1].

            assign de-qtd-not = de-qtd-not + de-qt-conv
                   de-vlr-not = de-vlr-not + it-nota-fisc.vl-tot-item.
   end.
   
   IF de-qtd-not = 0 AND de-vlr-not = 0 THEN NEXT.

   find emitente where emitente.cod-emitente = nota-fiscal.cod-emitente
                 no-lock.
   find w-work where w-work.no-ab-reppri = nota-fiscal.no-ab-reppri
                 and w-work.estado       = emitente.estado
                 and w-work.cidade       = emitente.cidade
               no-lock no-error.
   if not avail w-work then do:
      create w-work.
      assign w-work.no-ab-reppri = nota-fiscal.no-ab-reppri
             w-work.estado       = emitente.estado
             w-work.cidade       = emitente.cidade
             w-work.quantidade   = 0
             w-work.valor        = 0.
   end.          
   assign w-work.quantidade = w-work.quantidade + de-qtd-not
          w-work.valor      = w-work.valor      + de-vlr-not.
          
end. /* nota-fiscal */

for each w-work break by w-work.no-ab-reppri
                      by w-work.estado
                      by w-work.cidade:
    assign de-qtd-rep = de-qtd-rep + w-work.quantidade
           de-qtd-est = de-qtd-est + w-work.quantidade
           de-qtd-ger = de-qtd-ger + w-work.quantidade
           de-vlr-rep = de-vlr-rep + w-work.valor
           de-vlr-est = de-vlr-est + w-work.valor
           de-vlr-ger = de-vlr-ger + w-work.valor.
    if de-qtd-est > w-work.quantidade then
       assign l-imp-tot-est = yes.
      
    if first-of(w-work.no-ab-reppri) then
       find repres where repres.nome-abrev = w-work.no-ab-reppri
                         no-lock.
    display repres.cod-rep      when first-of(w-work.no-ab-reppri)
            w-work.no-ab-reppri when first-of(w-work.no-ab-reppri) 
            w-work.estado       when first-of(w-work.estado)
            w-work.cidade       
            w-work.quantidade
            w-work.valor
            with frame f-detalhe.
    down with frame f-detalhe.
   
    if last-of(w-work.no-ab-reppri) then do:
       if l-imp-tot-est then do:
          display "Total Estado" @ w-work.cidade
                  de-qtd-est     @ w-work.quantidade
                  de-vlr-est     @ w-work.valor
                  with frame f-detalhe.
          down with frame f-detalhe.
       end.
       assign de-qtd-est    = 0
              de-vlr-est    = 0
              l-imp-tot-est = no.
     
       display "Total Repres."  @ w-work.no-ab-reppri
               de-qtd-rep       @ w-work.quantidade
               de-vlr-rep       @ w-work.valor
               with frame f-detalhe.
       down with frame f-detalhe.
       put skip(1).
       assign de-qtd-rep = 0
              de-vlr-rep = 0.
    end.       
    else
       if last-of(w-work.estado) then do:
          if  de-qtd-est <> 0 
          and l-imp-tot-est then do:
             display "Total Estado" @ w-work.cidade
                     de-qtd-est     @ w-work.quantidade
                     de-vlr-est     @ w-work.valor
                     with frame f-detalhe.
             down with frame f-detalhe.
          end.
          assign de-qtd-est    = 0
                 de-vlr-est    = 0
                 l-imp-tot-est = no.
       end.
end. /* w-work */

display "Total Geral" @ w-work.no-ab-reppri
        de-qtd-ger    @ w-work.quantidade
        de-vlr-ger    @ w-work.valor
        with frame f-detalhe.
down with frame f-detalhe.
assign de-qtd-ger = 0
       de-vlr-ger = 0.

if l-falta-fator then do:
   page.
   put "Atencao ! - Ha itens sem fator de conversao:"
       skip(1).
   for each tt-work:
       FIND ITEM WHERE ITEM.it-codigo = tt-work.it-codigo
                 NO-LOCK NO-ERROR.
       IF AVAIL ITEM THEN
          PUT tt-work.it-codigo
              ITEM.desc-item
              SKIP.
       ELSE
          PUT tt-work.it-codigo
              SKIP.
   end.
   for each tt-work.
       delete tt-work.
   end.
end.  

IF tt-param.impr-param THEN DO:
   PAGE.
   PUT "*****----- PAR¶METROS ------*****"
       SKIP(1).

   ASSIGN tt-param.c-emit-dup  = IF tt-param.c-emit-dup = "S"
                                 THEN "Sim"
                                 ELSE IF tt-param.c-emit-dup = "N"
                                      THEN "NÆo" 
                                      ELSE "Todos"
           tt-param.c-nota-can = IF tt-param.c-nota-can = "S"
                                 THEN "Sim"
                                 ELSE IF tt-param.c-nota-can = "N"
                                      THEN "NÆo" 
                                      ELSE "Todas"
          tt-param.c-tipo-merc = IF tt-param.c-nota-can = "I"
                                 THEN "Interno"
                                 ELSE IF tt-param.c-nota-can = "E"
                                      THEN "Externo" 
                                      ELSE "Todos".

   DISPLAY tt-param.c-estab-ini     tt-param.c-estab-fim                                             
           tt-param.c-repres-ini    tt-param.c-repres-fim    
           tt-param.c-cliente-ini   tt-param.c-cliente-fim   
           tt-param.i-grupo-ini     tt-param.i-grupo-fim     
           tt-param.c-item-ini      tt-param.c-item-fim      
           tt-param.c-espdoc-ini    tt-param.c-espdoc-fim   
           tt-param.da-emis-ini     tt-param.da-emis-fim       
           tt-param.i-condpag-ini   tt-param.i-condpag-fim
           tt-param.c-natoper-ini   tt-param.c-natoper-fim  
           tt-param.c-emit-dup      
           tt-param.c-nota-can    
           tt-param.c-tipo-merc 
           WITH FRAME f-parlis.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.



