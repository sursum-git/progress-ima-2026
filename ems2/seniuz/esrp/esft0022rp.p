/* Programa: ESFT019.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Faturamento
** Objetivo: Gerar o relatorio de Notas Fiscais emitidas por Cliente
** Autor...: Gilvando de Souza Araujo - Setembro/96
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL / SANTA ELISABETH
**
** Conversao para EMS 2.04:
**   Programa: ESFT019.P  =>  ESFT0022RP.P
**   Autor...: Prodb - Toninho
**   Data....: 16/01/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESFT0022RP 2.04.00.000}

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
       FIELD c-ref-ini        LIKE it-nota-fisc.cod-refer
       FIELD c-ref-fim        LIKE it-nota-fisc.cod-refer
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
       FIELD qualidade        AS INTEGER
       FIELD desc-qualidade   AS CHAR FORMAT "x(10)"
       FIELD l-tipo-rel       AS   LOG  FORMAT "Detalhado/Resumido"
       FIELD c-tp-pedido      AS   CHAR FORMAT "x"
       FIELD c-tp-pagto       AS   CHAR
       FIELD l-artigo         AS   LOG  format "Codigo/Descricao"
       FIELD l-pedido         AS   LOG  format "Nosso/Representante"
       FIELD l-nf-dev-ent     AS   LOG
       FIELD impr-param       AS   LOGICAL.

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

def var c-artigo as char.
def var c-pedido as char.
def var de-qtd-not as dec format ">>,>>>,>>9.99".
def var de-qtd-cli as dec format ">>,>>>,>>9.99".
def var de-qtd-ger as dec format ">>,>>>,>>9.99".
def var de-vlr-not as dec format ">>,>>>,>>9.99".
def var de-vlr-cli as dec format ">>,>>>,>>9.99".
def var de-vlr-ger as dec format ">>,>>>,>>9.99".

form
    tt-param.c-estab-ini    LABEL "Estabelecimento.." AT  1
    "a"                                               AT 37
    tt-param.c-estab-fim    NO-LABELS
    tt-param.c-repres-ini   LABEL "Representante...." AT  1
    "a"                                               AT 37
    tt-param.c-repres-fim   NO-LABELS
    tt-param.c-cliente-ini  LABEL "Cliente.........." AT  1
    "a"                                               AT 37
    tt-param.c-cliente-fim  NO-LABELS               
    tt-param.i-grupo-ini    LABEL "Grupo Estoque...." AT  1
    "a"                                               AT 37
    tt-param.i-grupo-fim    NO-LABELS                
    tt-param.c-item-ini     LABEL "Item............." AT  1
    "a"                                               AT 37
    tt-param.c-item-fim     NO-LABELS
    tt-param.c-espdoc-ini   LABEL "Esp‚cie NF......." AT  1
    "a"                                               AT 37
    tt-param.c-espdoc-fim   NO-LABELS               
    tt-param.da-emis-ini    LABEL "Data EmissÆo....." AT  1
    "a"                                               AT 37
    tt-param.da-emis-fim    NO-LABELS
    tt-param.i-condpag-ini  LABEL "Cond Pagto......." AT  1
    "a"                                               AT 37
    tt-param.i-condpag-fim  NO-LABELS
    tt-param.c-natoper-ini  LABEL "Nat Operacao....." AT  1
    "a"                                               AT 37
    tt-param.c-natoper-fim  NO-LABELS
    tt-param.c-emit-dup     LABEL "Emite Duplicata.." AT  1 FORMAT "x(15)"      
    tt-param.c-nota-can     LABEL "Notas Canceladas." AT  1 FORMAT "x(15)" 
    tt-param.c-tipo-merc    LABEL "Mercado.........." AT  1 FORMAT "x(15)" 
    tt-param.desc-qualidade LABEL "Qualidade........" AT  1
    tt-param.l-tipo-rel     LABEL "Tipo de Relatorio" AT  1
    tt-param.c-tp-pedido    LABEL "Tipo de Pedido..." AT  1 FORMAT "x(15)" 
    tt-param.l-artigo       LABEL "Artigo por......." AT  1
    tt-param.l-pedido       LABEL "Nro do Pedido...." AT  1
    with no-box side-labels width 132 STREAM-IO frame f-parlis.

form
    nota-fiscal.cod-emitente     label "Cod. "          format "99999"
    nota-fiscal.nome-ab-cli      label "Cliente Abrv"
    nota-fiscal.cod-estabel      label "Est"             
    nota-fiscal.nr-nota-fis      label "N.Fiscal"       FORMAT "x(9)" 
    nota-fiscal.dt-emis-nota     label "Dt.Emissao"
    nota-fiscal.nr-parcelas      label "Pa"     
    nota-fiscal.no-ab-reppri     label "Representante"
    c-pedido                     label "Pedido"         format "x(8)"
    it-nota-fisc.nr-seq-fat      label "Seq"            format "999"
    c-artigo                     label "Artigo"         format "x(19)"
    it-nota-fisc.qt-faturada[1]  label "Quantidade"     format ">>>,>>9.99"
    it-nota-fisc.vl-preori       label "Vlr Unit"       format ">>>>>9.99"
    it-nota-fisc.vl-tot-item     label "Valor Total"
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
{utp/ut-liter.i Listagem_Notas_Fiscais_Emitidas_por_Cliente * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

FOR EACH nota-fiscal WHERE 
         nota-fiscal.no-ab-reppri >= tt-param.c-repres-ini   AND
         nota-fiscal.no-ab-reppri <= tt-param.c-repres-fim   AND
         nota-fiscal.cod-estabel  >= tt-param.c-estab-ini    AND
         nota-fiscal.cod-estabel  <= tt-param.c-estab-fim    AND
         nota-fiscal.nome-ab-cli  >= tt-param.c-cliente-ini  AND
         nota-fiscal.nome-ab-cli  <= tt-param.c-cliente-fim  AND
         nota-fiscal.dt-emis-nota >= tt-param.da-emis-ini    AND
         nota-fiscal.dt-emis-nota <= tt-param.da-emis-fim    AND
         nota-fiscal.cod-cond-pag >= tt-param.i-condpag-ini  AND
         nota-fiscal.cod-cond-pag <= tt-param.i-condpag-fim  AND
/*          nota-fiscal.esp-docto    >= tt-param.c-espdoc-ini   AND  */
/*          nota-fiscal.esp-docto    <= tt-param.c-espdoc-fim   AND  */
         nota-fiscal.nat-operacao >= tt-param.c-natoper-ini  AND
         nota-fiscal.nat-operacao <= tt-param.c-natoper-fim  AND
         ((nota-fiscal.emite-dup = YES AND tt-param.c-emit-dup = "S") OR
          (nota-fiscal.emite-dup = NO  AND tt-param.c-emit-dup = "N") OR
          (tt-param.c-emit-dup = "T"))                       AND
         ((nota-fiscal.dt-cancela <> ? AND tt-param.c-nota-can = "S") OR
          (nota-fiscal.dt-cancela = ?  AND tt-param.c-nota-can = "N") OR
          (tt-param.c-nota-can = "T"))                       AND
         ((nota-fiscal.mercado <> 1 AND tt-param.c-tipo-merc = "I") OR
          (nota-fiscal.mercado = 1  AND tt-param.c-tipo-merc = "E") OR
          (tt-param.c-tipo-merc = "T")) NO-LOCK,
    EACH ped-venda WHERE 
         ped-venda.nome-abrev = nota-fiscal.nome-ab-cli AND
         ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli AND
        (ped-venda.tp-pedido = tt-param.c-tp-pedido OR
         tt-param.c-tp-pedido = "") NO-LOCK,
    EACH ped-venda-ext WHERE
         ped-venda-ext.nr-pedido = ped-venda.nr-pedido AND
         (ped-venda-ext.tp-pagto = tt-param.c-tp-pagto OR
          tt-param.c-tp-pagto = 'Todos') NO-LOCK,
    EACH it-nota-fisc OF nota-fiscal WHERE
         it-nota-fisc.it-codigo >= tt-param.c-item-ini AND
         it-nota-fisc.it-codigo <= tt-param.c-item-fim NO-LOCK,
    EACH ITEM WHERE
         item.it-codigo = it-nota-fisc.it-codigo AND
         item.ge-codigo >= tt-param.i-grupo-ini AND
         item.ge-codigo <= tt-param.i-grupo-fim NO-LOCK,
    FIRST ped-item WHERE
          ped-item.nome-abrev   = it-nota-fisc.nome-ab-cli AND
          ped-item.nr-pedcli    = it-nota-fisc.nr-pedcli AND
          ped-item.nr-sequencia = it-nota-fisc.nr-seq-ped AND
          ped-item.it-codigo    = it-nota-fisc.it-codigo AND
          ped-item.cod-refer    = it-nota-fisc.cod-refer NO-LOCK,
    EACH ped-item-ext OF ped-item WHERE
         (SUBSTR(ped-item-ext.lote,2,1) = "P" AND tt-param.qualidade = 1) OR
         (SUBSTR(ped-item-ext.lote,2,1) = "D" AND tt-param.qualidade = 2) OR
         (tt-param.qualidade = 3) NO-LOCK
    BREAK BY nota-fiscal.nome-ab-cli
          BY nota-fiscal.nr-nota-fis:

    run pi-acompanhar in h-acomp (input nota-fiscal.nr-nota-fis).

    IF tt-param.l-pedido THEN
       ASSIGN c-pedido = SUBSTR(nota-fiscal.nr-pedcli,1,9).
    ELSE
       IF AVAIL ped-venda THEN
          ASSIGN c-pedido = SUBSTR(ped-venda.nr-pedrep,1,9).

    if tt-param.l-artigo then
       assign c-artigo = string(substr(item.it-codigo,1,6) +
                         SUBSTR(it-nota-fisc.cod-refer,1,7), "XXXXXX XX XXXX X").
    else
       assign c-artigo = substr(item.descricao-1,1,18) +
                         substr(item.descricao-2,1,1).
    
    if tt-param.l-tipo-rel = yes then do:   /* Detalhado */
       display nota-fiscal.cod-emitente WHEN FIRST-OF(nota-fiscal.nome-ab-cli)
               nota-fiscal.nome-ab-cli  WHEN FIRST-OF(nota-fiscal.nome-ab-cli) 
               nota-fiscal.cod-estabel  WHEN FIRST-OF(nota-fiscal.nr-nota-fis) 
               nota-fiscal.nr-nota-fis  WHEN FIRST-OF(nota-fiscal.nr-nota-fis) 
               nota-fiscal.dt-emis-nota WHEN FIRST-OF(nota-fiscal.nr-nota-fis) 
               nota-fiscal.nr-parcelas  WHEN FIRST-OF(nota-fiscal.nr-nota-fis) 
               nota-fiscal.no-ab-reppri WHEN FIRST-OF(nota-fiscal.nr-nota-fis) 
               c-pedido                 WHEN FIRST-OF(nota-fiscal.nr-nota-fis) 
               it-nota-fisc.nr-seq-fat
               c-artigo
               it-nota-fisc.qt-faturada[1]
               it-nota-fisc.vl-preori
               it-nota-fisc.vl-tot-item
               with frame f-detalhe.
       down with frame f-detalhe.
    end.
    assign de-qtd-not = de-qtd-not + it-nota-fisc.qt-faturada[1]
           de-vlr-not = de-vlr-not + it-nota-fisc.vl-tot-item
           de-qtd-cli = de-qtd-cli + it-nota-fisc.qt-faturada[1]
           de-vlr-cli = de-vlr-cli + it-nota-fisc.vl-tot-item
           de-qtd-ger = de-qtd-ger + it-nota-fisc.qt-faturada[1]
           de-vlr-ger = de-vlr-ger + it-nota-fisc.vl-tot-item.

    IF LAST-OF(nota-fiscal.nr-nota-fis) THEN DO:
       IF tt-param.l-tipo-rel = YES THEN DO: /* Detalhado */                      
          DISPLAY "Total da Nota:" @ c-artigo                        
                  de-qtd-not       @ it-nota-fisc.qt-faturada[1]                        
                  de-vlr-not       @ it-nota-fisc.vl-tot-item
                  WITH FRAME f-detalhe.
          DOWN (2) WITH FRAME f-detalhe.
       END.
       ELSE DO: /* Resumido */
          DISPLAY nota-fiscal.cod-emitente WHEN FIRST-OF(nota-fiscal.nome-ab-cli)
                  nota-fiscal.nome-ab-cli  WHEN FIRST-OF(nota-fiscal.nome-ab-cli)
                  nota-fiscal.cod-estabel
                  nota-fiscal.nr-nota-fis
                  nota-fiscal.dt-emis-nota
                  nota-fiscal.nr-parcelas
                  nota-fiscal.no-ab-reppri
                  nota-fiscal.nr-pedcli     @ c-pedido
                  nota-fiscal.esp-docto     @ it-nota-fisc.nr-seq-fat
                  de-qtd-not                @ it-nota-fisc.qt-faturada[1]
                  de-vlr-not                @ it-nota-fisc.vl-tot-item
                  with FRAME f-detalhe.
          DOWN WITH FRAME f-detalhe.
       END.
       ASSIGN de-qtd-not = 0
              de-vlr-not = 0.
    END.
    
    IF LAST-OF(nota-fiscal.nome-ab-cli) then DO:
       DISPLAY "Total Cliente:" @ c-artigo                   
               de-qtd-cli       @ it-nota-fisc.qt-faturada[1]
               de-vlr-cli       @ it-nota-fisc.vl-tot-item   
               WITH FRAME f-detalhe.
       DOWN (2) WITH FRAME f-detalhe.
       assign de-qtd-cli = 0
              de-vlr-cli = 0.
    END.
END.

if de-qtd-ger <> 0 or de-vlr-ger <> 0 then
   put "Total Geral:"     at   1
       de-qtd-ger         at  92
       de-vlr-ger         at 119.

assign de-qtd-ger = 0
       de-vlr-ger = 0.

IF tt-param.impr-param THEN DO.
   PAGE.
   PUT "***----------------- PAR¶METROS -------------------***"
       SKIP.

   ASSIGN tt-param.c-emit-dup = IF tt-param.c-emit-dup = "S"
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
                                      ELSE "Todos"
          tt-param.c-tp-pedido = IF tt-param.c-tp-pedido = ""
                                 THEN "Todos" 
                                 ELSE tt-param.c-tp-pedido.

   DISPLAY tt-param.c-estab-ini              tt-param.c-estab-fim                                             
           tt-param.c-repres-ini             tt-param.c-repres-fim    
           tt-param.c-cliente-ini            tt-param.c-cliente-fim   
           tt-param.i-grupo-ini              tt-param.i-grupo-fim     
           tt-param.c-item-ini               tt-param.c-item-fim      
           tt-param.c-espdoc-ini             tt-param.c-espdoc-fim   
           tt-param.da-emis-ini              tt-param.da-emis-fim       
           tt-param.i-condpag-ini            tt-param.i-condpag-fim
           tt-param.c-natoper-ini            tt-param.c-natoper-fim  
           tt-param.c-emit-dup            
           tt-param.c-nota-can    
           tt-param.c-tipo-merc
           tt-param.desc-qualidade
           tt-param.l-tipo-rel   
           tt-param.c-tp-pedido  
           tt-param.l-artigo     
           tt-param.l-pedido     
           WITH FRAME f-parlis.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.



