/* Programa: ESFT006.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Faturamento
** Objetivo: Listar o relat¢iro do Faturamento por Representante.
** Autor...: Sandro Wiest/Gilvando de Souza Araujo - Junho/95
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Programa: ESFT006.P  =>  ESFT0004RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 20/01/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESFT0004RP 2.04.00.000}

define temp-table tt-param  no-undo
       field destino           as integer
       field arquivo           as char format "x(35)"
       field usuario           as char format "x(12)"
       field data-exec         as date
       field hora-exec         as integer
       field classifica        as integer
       field desc-classifica   as char format "x(40)"
       FIELD cod-estabel       like nota-fiscal.cod-estabel 
       FIELD ini-dt-emissao    LIKE nota-fiscal.dt-emis-nota
       FIELD fin-dt-emissao    LIKE nota-fiscal.dt-emis-nota
       FIELD ini-cod-emitente  LIKE nota-fiscal.cod-emitente
       FIELD fin-cod-emitente  LIKE nota-fiscal.cod-emitente
       FIELD ini-cod-rep       LIKE nota-fiscal.cod-rep
       FIELD fin-cod-rep       LIKE nota-fiscal.cod-rep
       FIELD ini-ge-codigo     LIKE item.ge-codigo
       FIELD fin-ge-codigo     LIKE item.ge-codigo
       FIELD gerar-excel       AS LOG FORMAT "Sim/NÆo"
       FIELD arq-excel         AS CHAR FORMAT "x(45)"
       FIELD imp-param         AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

def TEMP-TABLE w-aux
    field it-codigo   LIKE ITEM.it-codigo
    field cod-rep     LIKE repres.cod-rep
    field quantidade  as dec format ">>>,>>>,>>9.99" label "Quantidade"
    field valor       as dec format ">>>,>>>,>>9.99" label "Valor"
    field qtd-faturas as int format ">>9"
    field prazo-total as int format ">>>9"
    INDEX ch-aux it-codigo
                 cod-rep.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

def TEMP-TABLE tt-work  /* Itens sem fator de conversao */
    field it-codigo like item.it-codigo
    INDEX ch-work it-codigo.

def var de-quantidade as dec.
def var i-cont as int.
def var l-prim-vez as log.
def var de-preco-med as dec format ">>>,>>9.99" label "Preco-Med".
def var de-preco-med-tot like de-preco-med.
def var de-preco-med-ger like de-preco-med.
def var i-prazo-med as int format ">>9" label "PzMed".
def var i-prazo-med-tot like i-prazo-med.
def var i-prazo-med-ger like i-prazo-med.
def var i-prazos as int.
def var i-prazos-tot as int.
def var i-prazos-ger as int.
def var i-duplicatas as int.
def var i-duplicatas-tot as int.
def var i-duplicatas-ger as int.
def var de-qt-prevista as dec   format ">>,>>>,>>9.99" label "Qtd Prevista".
def var de-qt-real     as dec   format ">>,>>>,>>9.99" label "Qtd Real".
def var de-perc        as dec   format "->>9.99"       label "Perc".
def var de-valor       as dec   format ">>,>>>,>>9.99" label "Valor".

def var de-qt-prevista-ger as dec   format ">>,>>>,>>9.99" label "Qtd Prevista".
def var de-qt-real-ger     as dec   format ">>,>>>,>>9.99" label "Qtd Real".
def var de-valor-ger       as dec   format ">>,>>>,>>9.99" label "Valor".
def var de-quantidade-ger   like w-aux.quantidade.

def var de-qt-prevista-tot as dec   format ">>,>>>,>>9.99" label "Qtd Prevista".
def var de-qt-real-tot     as dec   format ">>,>>>,>>9.99" label "Qtd Real".
def var de-valor-tot       as dec   format ">>,>>>,>>9.99" label "Valor".
def var de-quantidade-tot  like w-aux.quantidade.

DEF VAR i-lin-excel   AS INTEGER INITIAL 1.
DEF VAR l-falta-fator AS LOG.

form 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.cod-estabel      LABEL "Estabelec....." SKIP
    tt-param.ini-dt-emissao   label "Data Emissao.."
    "A"  AT 30
    tt-param.fin-dt-emissao   NO-LABELS SKIP
    tt-param.ini-cod-emitente label "Cliente......."
    "A"  AT 30
    tt-param.fin-cod-emitente no-labels SKIP
    tt-param.ini-cod-rep      label "Representante."
    "A"  AT 30
    tt-param.fin-cod-rep      no-labels SKIP
    tt-param.ini-ge-codigo    label "Grupo Estoque."
    "A"  AT 30
    tt-param.fin-ge-codigo    no-labels SKIP
    tt-param.gerar-excel      LABEL "Gerar Excel..." SKIP
    tt-param.arq-excel        LABEL "Arquivo Excel."
    skip(2)
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

FORM 
    w-aux.cod-rep              
    repres.nome-abrev
    w-aux.it-codigo 
    item.descricao-1   
    de-qt-prevista
    w-aux.quantidade
    de-perc
    w-aux.valor
    de-preco-med
    i-prazo-med
    WITH NO-BOX 55 DOWN WIDTH 132 FRAME f-detalhe STREAM-IO.

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
{utp/ut-liter.i Faturamento_por_Representante * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

IF tt-param.gerar-excel THEN DO:
   {utp/utapi003.i}
   OS-DELETE VALUE(tt-param.arq-excel).
   CREATE tt-configuracao.
   ASSIGN tt-configuracao.versao-integracao   = 2
          tt-configuracao.arquivo-num         = 1
          tt-configuracao.arquivo             = tt-param.arq-excel
          tt-configuracao.total-planilha      = 1
          tt-configuracao.exibir-construcao   = YES 
          tt-configuracao.abrir-excel-termino = NO.

   CREATE tt-planilha.
   ASSIGN tt-planilha.arquivo-num       = 1
          tt-planilha.planilha-num      = 1
          tt-planilha.planilha-nome     = "Fat-Repres"
          tt-planilha.linhas-grade      = NO
          tt-planilha.largura-coluna    = 12
          tt-planilha.formatar-planilha = YES.

   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 1
          tt-dados.celula-linha  = 1
          tt-dados.celula-valor  = empresa.razao-social
          tt-dados.celula-fonte-tamanho = 24
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor     = 3.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 1
          tt-dados.celula-linha  = 3
          tt-dados.celula-valor  = "FATURAMENTO POR REPRESENTANTE"
          tt-dados.celula-fonte-tamanho = 14
          tt-dados.celula-fonte-negrito = YES.
   ASSIGN i-lin-excel = 5.
   
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 1
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-valor  = "Repres"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 2
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-valor  = "Nome Abreviado"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num    = 1
          tt-dados.planilha-num   = 1
          tt-dados.celula-coluna  = 3
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha   = i-lin-excel
          tt-dados.celula-valor   = "Item"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 4
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-valor  = "Descri‡Æo"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 5
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Qtd Prevista"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 6
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Quantidade"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 7
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Perc"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 8
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Valor"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num    = 1
          tt-dados.planilha-num   = 1
          tt-dados.celula-coluna  = 9
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha = i-lin-excel
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor = "Pre‡o-Med"
          tt-dados.celula-fonte-negrito  = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 10
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha = i-lin-excel
          tt-dados.celula-alinhamento-horizontal = 1
          tt-dados.celula-valor = "PzMed"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   ASSIGN i-lin-excel = i-lin-excel + 1.
END.

FOR EACH nota-fiscal 
    WHERE nota-fiscal.cod-estabel  =  tt-param.cod-estabel      
      AND nota-fiscal.dt-emis-nota >= tt-param.ini-dt-emissao   
      AND nota-fiscal.dt-emis-nota <= tt-param.fin-dt-emissao   
      AND nota-fiscal.dt-cancela   =  ?                         
      AND nota-fiscal.cod-emitente >= tt-param.ini-cod-emitente 
      AND nota-fiscal.cod-emitente <= tt-param.fin-cod-emitente 
      AND nota-fiscal.cod-rep      >= tt-param.ini-cod-rep      
      AND nota-fiscal.cod-rep      <= tt-param.fin-cod-rep
      AND nota-fiscal.emite-dup     = YES
    NO-LOCK:

    RUN pi-acompanhar in h-acomp (input nota-fiscal.nr-nota-fis).
   
    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK,
        FIRST item where
              item.it-codigo =  it-nota-fisc.it-codigo AND  
              item.ge-codigo >= tt-param.ini-ge-codigo AND
              item.ge-codigo <= tt-param.fin-ge-codigo
              no-lock.

        find first w-aux where w-aux.cod-rep   = nota-fiscal.cod-rep
                           and w-aux.it-codigo = item.it-codigo
                         no-error.
        if not avail w-aux then do:
           create w-aux.
           assign w-aux.cod-rep     = nota-fiscal.cod-rep
                  w-aux.it-codigo   = item.it-codigo
                  w-aux.quantidade  = 0
                  w-aux.qtd-faturas = 0
                  w-aux.prazo-total = 0.
        end.

        /*------ Conversao de M para Kg ------- */
        if item.un <> "m" then do:
           FIND item-ext WHERE ITEM-ext.it-codigo = ITEM.it-codigo
                         NO-LOCK NO-ERROR.
           if AVAIL item-ext then
              assign de-quantidade = it-nota-fisc.qt-faturada[1] * 
                                     item-ext.fator-conv.
           else do:
              assign l-falta-fator = yes
                     de-quantidade = it-nota-fisc.qt-faturada[1].
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
           ASSIGN de-quantidade = it-nota-fisc.qt-faturada[1].

        assign w-aux.quantidade = w-aux.quantidade + de-quantidade
                    w-aux.valor = w-aux.valor + it-nota-fisc.vl-tot-item.

        find cond-pagto where
             cond-pagto.cod-cond-pag = nota-fiscal.cod-cond-pag
             no-error.
        if avail cond-pagto then
           assign w-aux.qtd-faturas = w-aux.qtd-faturas
                                    + cond-pagto.num-parcelas
                  w-aux.prazo-total = w-aux.prazo-total
                                    + cond-pagto.prazos[1]
                                    + cond-pagto.prazos[2]
                                    + cond-pagto.prazos[3]
                                    + cond-pagto.prazos[4]
                                    + cond-pagto.prazos[5]
                                    + cond-pagto.prazos[6]
                                    + cond-pagto.prazos[7]
                                    + cond-pagto.prazos[8].
    END.
END.

for each w-aux 
    break by w-aux.cod-rep 
          by w-aux.it-codigo: 
    if first-of(w-aux.cod-rep) then do:
       find repres where repres.cod-rep = w-aux.cod-rep no-lock.
       display w-aux.cod-rep
               repres.nome-abrev
               WITH FRAME f-detalhe.

       IF tt-param.gerar-excel AND AVAIL repres THEN DO:
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 1
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-valor  = string(repres.cod-rep).
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 2
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-valor  = repres.nome-abrev.
       END.
    END.

    find cota-rep where cota-rep.cod-rep   = w-aux.cod-rep
                    and cota-rep.it-codigo = w-aux.it-codigo
                  no-lock no-error.
    if avail cota-rep then
       assign de-qt-prevista = cota-rep.qt-prevista[month(tt-param.ini-dt-emissao)].
    else
       assign de-qt-prevista = 0.
    if de-qt-prevista = 0 then
       assign de-perc = 100.
    else
    if w-aux.quantidade = 0 then
       assign de-perc = -100.
    else
    assign de-perc = ((w-aux.quantidade / de-qt-prevista) - 1) * 100.
    find item where item.it-codigo = w-aux.it-codigo
         no-lock no-error.
    if w-aux.quantidade <> 0 then
       assign de-preco-med = w-aux.valor / w-aux.quantidade.
    else
       assign de-preco-med = 0.
    if w-aux.qtd-faturas <> 0 then
       assign i-prazo-med = w-aux.prazo-total / w-aux.qtd-faturas.
    else
       assign i-prazo-med = 0.

    display w-aux.it-codigo
            item.descricao-1
            de-qt-prevista
            w-aux.quantidade
            de-perc
            w-aux.valor
            de-preco-med
            i-prazo-med
            WITH FRAME f-detalhe.
    DOWN WITH FRAME f-detalhe.

    IF tt-param.gerar-excel THEN DO:
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 3
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-valor  = w-aux.it-codigo.
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num    = 1
              tt-dados.planilha-num   = 1
              tt-dados.celula-coluna  = 4
              tt-dados.celula-linha   = i-lin-excel
              tt-dados.celula-valor   = item.descricao-1.
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num    = 1
              tt-dados.planilha-num   = 1
              tt-dados.celula-coluna  = 5
              tt-dados.celula-linha   = i-lin-excel
              tt-dados.celula-formato = "###.###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor   = string(de-qt-prevista).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 6
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "###.###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(w-aux.quantidade).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 7
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(de-perc).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 8
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "###.###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(w-aux.valor).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 9
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(de-preco-med).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 10
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-alinhamento-horizontal = 1
              tt-dados.celula-valor  = string(i-prazo-med).
       ASSIGN i-lin-excel = i-lin-excel + 1.
    END.

    assign de-qt-prevista-tot = de-qt-prevista-tot + de-qt-prevista
           de-quantidade-tot  = de-quantidade-tot + w-aux.quantidade
           de-valor-tot       = de-valor-tot + w-aux.valor
           de-qt-prevista-ger = de-qt-prevista-ger + de-qt-prevista
           de-quantidade-ger  = de-quantidade-ger + w-aux.quantidade
           de-valor-ger       = de-valor-ger + w-aux.valor
           i-prazos-ger       = i-prazos-ger + w-aux.prazo-total
           i-duplicatas-ger   = i-duplicatas-ger + w-aux.qtd-faturas
           i-prazos-tot       = i-prazos-tot + w-aux.prazo-total
           i-duplicatas-tot   = i-duplicatas-tot + w-aux.qtd-faturas.
    
    if last-of(w-aux.cod-rep) then do:
       if de-qt-prevista-tot = 0 then
          assign de-perc = 100.
       else
       if de-quantidade-tot = 0 then
          assign de-perc = -100.
       else
       assign de-perc = ((de-quantidade-tot /
                        de-qt-prevista-tot) - 1) * 100.
       if de-quantidade-tot <> 0 then
          assign de-preco-med-tot = de-valor-tot / de-quantidade-tot.
       else
          assign de-preco-med-tot = 0.
       if i-duplicatas-tot <> 0 then
          assign i-prazo-med-tot = i-prazos-tot / i-duplicatas-tot.
       else
          assign i-prazo-med-tot = 0.
       
       display "Total"            @ repres.nome-abrev
               de-qt-prevista-tot @ de-qt-prevista
               de-quantidade-tot  @ w-aux.quantidade
               de-valor-tot       @ w-aux.valor
               de-preco-med-tot   @ de-preco-med
               i-prazo-med-tot    @ i-prazo-med
               WITH FRAME f-detalhe.
       DOWN 2 WITH FRAME f-detalhe.

       IF tt-param.gerar-excel THEN DO:
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 2
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-valor  = "Total".
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 5
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-formato = "###.###.##0,00"
                 tt-dados.celula-alinhamento-horizontal = 5
                 tt-dados.celula-valor  = string(de-qt-prevista-tot).
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 6
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-formato = "###.###.##0,00"
                 tt-dados.celula-alinhamento-horizontal = 5
                 tt-dados.celula-valor  = string(de-quantidade-tot).
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 8
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-formato = "###.###.##0,00"
                 tt-dados.celula-alinhamento-horizontal = 5
                 tt-dados.celula-valor  = string(de-valor-tot).
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num    = 1
                 tt-dados.planilha-num   = 1
                 tt-dados.celula-coluna  = 9
                 tt-dados.celula-linha = i-lin-excel
                 tt-dados.celula-formato = "###.##0,00"
                 tt-dados.celula-alinhamento-horizontal = 5
                 tt-dados.celula-valor = string(de-preco-med-tot).
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 10
                 tt-dados.celula-linha = i-lin-excel
                 tt-dados.celula-alinhamento-horizontal = 1
                 tt-dados.celula-valor = string(i-prazo-med-tot).
          ASSIGN i-lin-excel = i-lin-excel + 2.
       END.

       assign de-valor-tot       = 0
              de-qt-prevista-tot = 0
              de-quantidade-tot  = 0
              i-prazos-tot       = 0
              i-duplicatas-tot   = 0.
    end.
end.

if de-qt-prevista-ger = 0 then
   assign de-perc = 100.
else
if de-quantidade-ger = 0 then
   assign de-perc = -100.
else
assign de-perc = ((de-quantidade-ger / de-qt-prevista-ger) - 1) * 100.
if de-quantidade-ger <> 0 then
   assign de-preco-med-ger = de-valor-ger / de-quantidade-ger.
else
   assign de-preco-med-ger = 0.
if i-duplicatas-ger <> 0 then
   assign i-prazo-med-ger = i-prazos-ger / i-duplicatas-ger.
else
   assign i-prazo-med-ger = 0.

display "Total Geral"      @ repres.nome-abrev
        de-qt-prevista-ger @ de-qt-prevista
        de-quantidade-ger  @ w-aux.quantidade
        de-valor-ger       @ w-aux.valor
        de-preco-med-ger   @ de-preco-med
        i-prazo-med-ger    @ i-prazo-med
        with frame f-detalhe.
DOWN WITH FRAME f-detalhe.

IF tt-param.gerar-excel THEN DO:
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 2
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-valor  = "Total Geral".
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 5
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-formato = "###.###.##0,00"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = string(de-qt-prevista-ger).
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 6
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-formato = "###,###.##0,00"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = string(de-quantidade-ger).
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 8
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-formato = "###.###.##0,00"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = string(de-valor-ger).
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num    = 1
          tt-dados.planilha-num   = 1
          tt-dados.celula-coluna  = 9
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha = i-lin-excel
          tt-dados.celula-formato = "###.##0,00"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor = string(de-preco-med-ger).
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 10
          tt-dados.celula-linha = i-lin-excel
          tt-dados.celula-alinhamento-horizontal = 1
          tt-dados.celula-valor = string(i-prazo-med-ger).
   ASSIGN i-lin-excel = i-lin-excel + 2.
END.

RUN utp/utapi003.p (INPUT-OUTPUT TABLE tt-configuracao,
                    INPUT-OUTPUT TABLE tt-planilha,
                    INPUT-OUTPUT TABLE tt-dados,
                    INPUT-OUTPUT TABLE tt-grafico,
                    INPUT-OUTPUT TABLE tt-erros).
/*
if return-value = "nok" then do: 
for each tt-erros: 
    DISPLAY tt-erros with 1 col width 500. 
end.
end.                  
*/

assign de-valor-ger       = 0
       de-qt-prevista-ger = 0
       de-quantidade-ger  = 0
       i-prazos-ger       = 0
       i-duplicatas-ger   = 0.

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

IF tt-param.imp-param THEN DO:
   PAGE.
   display tt-param.cod-estabel
           tt-param.ini-dt-emissao
           tt-param.fin-dt-emissao
           tt-param.ini-cod-emitente
           tt-param.fin-cod-emitente
           tt-param.ini-cod-rep
           tt-param.fin-cod-rep
           tt-param.ini-ge-codigo
           tt-param.fin-ge-codigo
           tt-param.gerar-excel
           tt-param.arq-excel  
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

