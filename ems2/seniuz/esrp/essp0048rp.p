/* Programa: ESSP0048RP.P (Chamado pelo programa ESSP0048.W)
** Objetivo: Imprimir o relat¢rio de Testes de Hidrofilidade 
**           (tabela: teste-hidr)
** Sistema.: Espec¡ficos
** M¢dulo..: Espec¡ficos
** Autor...: F bio Coelho Lanza - Dezembro/2003
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0048RP 2.04.00.000}
/* defini‡Æo das temp-tables para recebimento de parƒmetros */

define temp-table tt-param no-undo
       FIELD destino             AS INTEGER
       FIELD arquivo             AS CHAR FORMAT "x(35)"
       FIELD usuario             AS CHAR FORMAT "x(12)"
       FIELD data-exec           AS DATE
       FIELD hora-exec           AS INTEGER
       FIELD classifica          AS INTEGER
       FIELD desc-classifica     AS CHAR FORMAT "x(40)"
       FIELD fi-ini-fm-codigo    LIKE teste-hidr.fm-codigo
       FIELD fi-fin-fm-codigo    LIKE teste-hidr.fm-codigo
       FIELD fi-ini-fundo        LIKE teste-hidr.fundo
       FIELD fi-fin-fundo        LIKE teste-hidr.fundo
       FIELD fi-ini-data-teste   LIKE teste-hidr.data-teste
       FIELD fi-fin-data-teste   LIKE teste-hidr.data-teste
       FIELD fi-ini-nome-proc    LIKE teste-hidr.nome-proc
       FIELD fi-fin-nome-proc    LIKE teste-hidr.nome-proc
       FIELD fi-ini-cod-maq      LIKE teste-hidr.cod-maq
       FIELD fi-fin-cod-maq      LIKE teste-hidr.cod-maq
       FIELD fi-ini-num-ob       LIKE teste-hidr.num-ob
       FIELD fi-fin-num-ob       LIKE teste-hidr.num-ob
       FIELD fi-ini-res-esqrd    LIKE teste-hidr.res-esqrd
       FIELD fi-fin-res-esqrd    LIKE teste-hidr.res-esqrd
       FIELD fi-ini-res-centro   LIKE teste-hidr.res-centro
       FIELD fi-fin-res-centro   LIKE teste-hidr.res-centro
       FIELD fi-ini-res-direita  LIKE teste-hidr.res-direita
       FIELD fi-fin-res-direita  LIKE teste-hidr.res-direita
       FIELD fi-ini-res-aprov    LIKE teste-hidr.res-esqrd
       FIELD fi-fin-res-aprov    LIKE teste-hidr.res-esqrd
       FIELD tipo-relatorio      AS INTEGER
       FIELD desc-tipo-relat     AS CHAR FORMAT "x(20)"
       FIELD imp-param           AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

/* recebimento de parƒmetros */
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

def var de-tot-res-esq as dec.
def var de-tot-res-cen as dec.
def var de-tot-res-dir as dec.
def var i-cont-fam     as int format ">>>9".
def var i-cont-dat     as int format ">>>9".
def var i-cont-ger     as int.
def var i-cont-esq-rep as int.
def var i-cont-cen-rep as int.
def var i-cont-dir-rep as int.
def var i-cont-esq-apr as int.
def var i-cont-cen-apr as int.
def var i-cont-dir-apr as int.

/* defini‡Æo de frames do relat¢rio */
FORM 
   "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
   tt-param.desc-classifica    LABEL "Classific."    SKIP
   tt-param.fi-ini-fm-codigo   label "Familia..."
   "A"  AT 30
   tt-param.fi-fin-fm-codigo   NO-LABELS             SKIP
   tt-param.fi-ini-fundo       label "Fundo....."
   "A"  AT 30
   tt-param.fi-fin-fundo       no-labels             SKIP
   tt-param.fi-ini-data-teste  LABEL "Data......"
   "A"  AT 30
   tt-param.fi-fin-data-teste  NO-LABELS             SKIP
   tt-param.fi-ini-nome-proc   LABEL "Processo.."
   "A"  AT 30
   tt-param.fi-fin-nome-proc   NO-LABELS             SKIP
   tt-param.fi-ini-cod-maq     LABEL "Maquina..."
   "A"  AT 30
   tt-param.fi-fin-cod-maq     NO-LABELS             SKIP
   tt-param.fi-ini-num-ob      LABEL "Ord.Benef."
   "A"  AT 30
   tt-param.fi-fin-num-ob      NO-LABELS             SKIP 
   tt-param.fi-ini-res-esqrd   LABEL "Res.Esqrd."
   "A"  AT 30
   tt-param.fi-fin-res-esqrd   NO-LABELS             SKIP 
   tt-param.fi-ini-res-centro  LABEL "Res.Centro"
   "A"  AT 30
   tt-param.fi-fin-res-centro  NO-LABELS             SKIP 
   tt-param.fi-ini-res-direita LABEL "Res.Direit"
   "A"  AT 30
   tt-param.fi-fin-res-direita NO-LABELS             SKIP
   tt-param.fi-ini-res-aprov   LABEL "Test.Aprov"
   "A"  AT 30
   tt-param.fi-fin-res-aprov   NO-LABELS             SKIP
   tt-param.desc-tipo-rel      LABEL "Tipo Relat"
   with FRAME f-param side-labels no-box WIDTH 133 STREAM-IO.

FORM   
   teste-hidr.fm-codigo    label "Familia"
   familia.descricao       label "Descricao"
   teste-hidr.fundo        label "Fundo"
   teste-hidr.data-teste   label "Data-Teste"
   teste-hidr.cod-maq      label "Maq."
   maq-benef.descricao     label "Descricao"
   teste-hidr.num-seq      label "S"
   teste-hidr.nome-proc    label "Processo"
   teste-hidr.num-ob       label "OB"
   teste-hidr.res-esqrd    label "ResEsq"
   teste-hidr.res-centro   label "ResCen"
   teste-hidr.res-direita  label "ResDir"
   i-cont-fam              label "Tsts"
   with frame f-analitico-fam  NO-LABEL WIDTH 133 down stream-io.

FORM   
   teste-hidr.fm-codigo    label "Familia"
   familia.descricao       label "Descricao"
   teste-hidr.res-esqrd    label "ResEsq"
   teste-hidr.res-centro   label "ResCen"
   teste-hidr.res-direita  label "ResDir"
   i-cont-fam              label "Tsts"
   with frame f-sintetico-fam NO-LABEL WIDTH 133 down stream-io.

FORM
   teste-hidr.data-teste   label "Data-Teste"
   teste-hidr.fm-codigo    label "Familia"
   familia.descricao       label "Descricao"
   teste-hidr.fundo        label "Cor"
   teste-hidr.cod-maq      label "Maq."
   maq-benef.descricao     label "Descricao"
   teste-hidr.num-seq      label "S"
   teste-hidr.nome-proc    label "Processo"
   teste-hidr.num-ob       label "OB"
   teste-hidr.res-esqrd    label "ResEsq"
   teste-hidr.res-centro   label "ResCen"
   teste-hidr.res-direita  label "ResDir"
   i-cont-dat              label "Tsts"
   with frame f-analitico-dat NO-LABEL WIDTH 133 down stream-io.

FORM   
    teste-hidr.data-teste   label "Data"
    teste-hidr.res-esqrd    label "ResEsq"
    teste-hidr.res-centro   label "ResCen"
    teste-hidr.res-direita  label "ResDir"
    i-cont-dat              label "Tsts"
   with frame f-sintetico-dat NO-LABEL WIDTH 133 down stream-io.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i &STREAM="stream str-rp"}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i &STREAM="str-rp"}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i ESPECÖFICOS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Testes_de_Hidrofilidade * r}
assign c-titulo-relat = trim(return-value).

view stream str-rp frame f-cabec.
view stream str-rp frame f-rodape.
run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

if tt-param.classific = 1 then do: /* Por Fam¡lia */
   FOR each teste-hidr no-lock
      where teste-hidr.fm-codigo   >= tt-param.fi-ini-fm-codigo
        AND teste-hidr.fm-codigo   <= tt-param.fi-fin-fm-codigo
        AND teste-hidr.fundo       >= tt-param.fi-ini-fundo
        AND teste-hidr.fundo       <= tt-param.fi-fin-fundo
        AND teste-hidr.data-teste  >= tt-param.fi-ini-data-teste
        AND teste-hidr.data-teste  <= tt-param.fi-fin-data-teste
        AND teste-hidr.nome-proc   >= tt-param.fi-ini-nome-proc
        AND teste-hidr.nome-proc   <= tt-param.fi-fin-nome-proc
        AND teste-hidr.cod-maq     >= tt-param.fi-ini-cod-maq   
        AND teste-hidr.cod-maq     <= tt-param.fi-fin-cod-maq  
        AND teste-hidr.num-ob      >= tt-param.fi-ini-num-ob    
        AND teste-hidr.num-ob      <= tt-param.fi-fin-num-ob
        AND teste-hidr.res-esqrd   >= tt-param.fi-ini-res-esqrd
        AND teste-hidr.res-esqrd   <= tt-param.fi-fin-res-esqrd
        AND teste-hidr.res-centro  >= tt-param.fi-ini-res-centro
        AND teste-hidr.res-centro  <= tt-param.fi-fin-res-centro
        AND teste-hidr.res-direita >= tt-param.fi-ini-res-direita
        AND teste-hidr.res-direita <= tt-param.fi-fin-res-direita
      BREAK BY teste-hidr.fm-codigo
      on stop undo,leave:
      run pi-acompanhar in h-acomp (input string(teste-hidr.fm-codigo)).
      
      assign i-cont-fam     = i-cont-fam + 1
             de-tot-res-esq = de-tot-res-esq + teste-hidr.res-esqrd
             de-tot-res-cen = de-tot-res-cen + teste-hidr.res-centro
             de-tot-res-dir = de-tot-res-dir + teste-hidr.res-direita.

      /* Acumula para Estatisticas */
      assign i-cont-ger = i-cont-ger + 1.
      if  teste-hidr.res-esqrd >= tt-param.fi-ini-res-aprov
      and teste-hidr.res-esqrd <= tt-param.fi-fin-res-aprov then
          assign i-cont-esq-apr  =  i-cont-esq-apr + 1.
      else
          assign i-cont-esq-rep  =  i-cont-esq-rep + 1.

      if  teste-hidr.res-centro >= tt-param.fi-ini-res-aprov
      and teste-hidr.res-centro <= tt-param.fi-fin-res-aprov then
          assign i-cont-cen-apr   =  i-cont-cen-apr + 1.
      else
          assign i-cont-cen-rep   =  i-cont-cen-rep + 1.
       
      if  teste-hidr.res-direita >= tt-param.fi-ini-res-aprov
      and teste-hidr.res-direita <= tt-param.fi-fin-res-aprov then
          assign i-cont-dir-apr    =  i-cont-dir-apr + 1.
      else
          assign i-cont-dir-rep    =  i-cont-dir-rep + 1.

      IF tt-param.tipo-relatorio = 1 THEN do: /* Sint‚tico */
         IF LAST-OF(teste-hidr.fm-codigo) THEN DO:
            FIND familia WHERE familia.fm-codigo = teste-hidr.fm-codigo
                         NO-LOCK NO-ERROR.
            DISPLAY STREAM str-rp
                    teste-hidr.fm-codigo
                    familia.descricao
                    de-tot-res-esq / i-cont-fam @ teste-hidr.res-esqrd
                    de-tot-res-cen / i-cont-fam @ teste-hidr.res-centro
                    de-tot-res-dir / i-cont-fam @ teste-hidr.res-direita
                    i-cont-fam
                    WITH FRAME f-sintetico-fam.
            DOWN STREAM str-rp WITH FRAME f-sintetico-fam.
            ASSIGN i-cont-fam     = 0
                   de-tot-res-esq = 0
                   de-tot-res-cen = 0
                   de-tot-res-dir = 0.
         END.
      END.
      ELSE DO: /*Anal¡tico */
         FIND familia WHERE familia.fm-codigo = teste-hidr.fm-codigo
                      NO-LOCK NO-ERROR.
         find maq-benef where maq-benef.codigo = teste-hidr.cod-maq
                              no-lock no-error.
         display stream str-rp
                 teste-hidr.fm-codigo   
                  WHEN FIRST-OF(teste-hidr.fm-codigo)
                 familia.descricao       
                  WHEN first-of(teste-hidr.fm-codigo) AND AVAIL familia
                 teste-hidr.fundo
                 teste-hidr.data-teste
                 teste-hidr.cod-maq      
                 maq-benef.descricao
                 teste-hidr.num-seq
                 teste-hidr.nome-proc
                 teste-hidr.num-ob
                 teste-hidr.res-esqrd
                 teste-hidr.res-centro
                 teste-hidr.res-direita
                 with frame f-analitico-fam.
         down stream str-rp with frame f-analitico-fam.
         
         IF LAST-OF(teste-hidr.fm-codigo) THEN DO:
            DISPLAY STREAM str-rp
                    "M‚dia"                     @ teste-hidr.fm-codigo
                    de-tot-res-esq / i-cont-fam @ teste-hidr.res-esqrd
                    de-tot-res-cen / i-cont-fam @ teste-hidr.res-centro
                    de-tot-res-dir / i-cont-fam @ teste-hidr.res-direita
                    i-cont-fam
                    WITH FRAME f-analitico-fam.
            DOWN 2 STREAM str-rp WITH FRAME f-analitico-fam.
            ASSIGN i-cont-fam     = 0
                   de-tot-res-esq = 0
                   de-tot-res-cen = 0
                   de-tot-res-dir = 0.
         END.
      END.
   END.
end.
else do: /* Por Data */
   FOR each teste-hidr no-lock
      where teste-hidr.fm-codigo   >= tt-param.fi-ini-fm-codigo
        AND teste-hidr.fm-codigo   <= tt-param.fi-fin-fm-codigo
        AND teste-hidr.fundo       >= tt-param.fi-ini-fundo
        AND teste-hidr.fundo       <= tt-param.fi-fin-fundo
        AND teste-hidr.data-teste  >= tt-param.fi-ini-data-teste
        AND teste-hidr.data-teste  <= tt-param.fi-fin-data-teste
        AND teste-hidr.nome-proc   >= tt-param.fi-ini-nome-proc
        AND teste-hidr.nome-proc   <= tt-param.fi-fin-nome-proc
        AND teste-hidr.cod-maq     >= tt-param.fi-ini-cod-maq   
        AND teste-hidr.cod-maq     <= tt-param.fi-fin-cod-maq  
        AND teste-hidr.num-ob      >= tt-param.fi-ini-num-ob    
        AND teste-hidr.num-ob      <= tt-param.fi-fin-num-ob
        AND teste-hidr.res-esqrd   >= tt-param.fi-ini-res-esqrd
        AND teste-hidr.res-esqrd   <= tt-param.fi-fin-res-esqrd
        AND teste-hidr.res-centro  >= tt-param.fi-ini-res-centro
        AND teste-hidr.res-centro  <= tt-param.fi-fin-res-centro
        AND teste-hidr.res-direita >= tt-param.fi-ini-res-direita
        AND teste-hidr.res-direita <= tt-param.fi-fin-res-direita
   BREAK BY teste-hidr.data-teste
         BY teste-hidr.fm-codigo
   on stop undo,leave:
      run pi-acompanhar in h-acomp (input string(teste-hidr.fm-codigo)).

      assign i-cont-dat     = i-cont-dat + 1
             de-tot-res-esq = de-tot-res-esq + teste-hidr.res-esqrd
             de-tot-res-cen = de-tot-res-cen + teste-hidr.res-centro
             de-tot-res-dir = de-tot-res-dir + teste-hidr.res-direita.

      /* Acumula para Estatisticas */
      assign i-cont-ger = i-cont-ger + 1.
      if  teste-hidr.res-esqrd >= tt-param.fi-ini-res-aprov
      and teste-hidr.res-esqrd <= tt-param.fi-fin-res-aprov then
          assign i-cont-esq-apr  =  i-cont-esq-apr + 1.
      else
          assign i-cont-esq-rep  =  i-cont-esq-rep + 1.

      if  teste-hidr.res-centro >= tt-param.fi-ini-res-aprov
      and teste-hidr.res-centro <= tt-param.fi-fin-res-aprov then
          assign i-cont-cen-apr   =  i-cont-cen-apr + 1.
      else
          assign i-cont-cen-rep   =  i-cont-cen-rep + 1.
       
      if  teste-hidr.res-direita >= tt-param.fi-ini-res-aprov
      and teste-hidr.res-direita <= tt-param.fi-fin-res-aprov then
          assign i-cont-dir-apr    =  i-cont-dir-apr + 1.
      else
          assign i-cont-dir-rep    =  i-cont-dir-rep + 1.

      IF tt-param.tipo-relatorio = 1 THEN do: /* Sint‚tico */
         IF LAST-OF(teste-hidr.data-teste) THEN DO:
          DISPLAY STREAM str-rp
                  teste-hidr.data-teste
                  de-tot-res-esq / i-cont-dat @ teste-hidr.res-esqrd
                  de-tot-res-cen / i-cont-dat @ teste-hidr.res-centro
                  de-tot-res-dir / i-cont-dat @ teste-hidr.res-direita
                  i-cont-dat
                  WITH FRAME f-sintetico-dat.
          DOWN STREAM str-rp WITH FRAME f-sintetico-dat.
          ASSIGN i-cont-dat     = 0
                 de-tot-res-esq = 0
                 de-tot-res-cen = 0
                 de-tot-res-dir = 0.
         END.
      END.
      ELSE DO: /*Anal¡tico */
          FIND familia WHERE familia.fm-codigo = teste-hidr.fm-codigo
                       NO-LOCK NO-ERROR.
          find maq-benef where maq-benef.codigo = teste-hidr.cod-maq
                               no-lock no-error.
          display stream str-rp
                  teste-hidr.data-teste
                   WHEN FIRST-OF(teste-hidr.data-teste)
                  teste-hidr.fm-codigo   
                  familia.descricao       
                  teste-hidr.fundo
                  teste-hidr.cod-maq      
                  maq-benef.descricao
                  teste-hidr.num-seq
                  teste-hidr.nome-proc
                  teste-hidr.num-ob
                  teste-hidr.res-esqrd
                  teste-hidr.res-centro
                  teste-hidr.res-direita
                  with frame f-analitico-dat.
          down stream str-rp with frame f-analitico-dat.

          IF LAST-OF(teste-hidr.data-teste) THEN DO:
             DISPLAY STREAM str-rp
                     "M‚dia"                     @ teste-hidr.data-teste
                     de-tot-res-esq / i-cont-dat @ teste-hidr.res-esqrd
                     de-tot-res-cen / i-cont-dat @ teste-hidr.res-centro
                     de-tot-res-dir / i-cont-dat @ teste-hidr.res-direita
                     i-cont-dat
                     WITH FRAME f-analitico-dat.
             DOWN 2 STREAM str-rp WITH FRAME f-analitico-dat.
             ASSIGN i-cont-dat     = 0
                    de-tot-res-esq = 0
                    de-tot-res-cen = 0
                    de-tot-res-dir = 0.
          END.

      END.
   END.
END.

/* Estatisticas */
if line-count > 58 then
   PAGE STREAM str-rp.
PUT STREAM str-rp skip(1).
put STREAM str-rp "RESULTADOS ESTATISTICOS - "
"TESTES LISTADOS: " i-cont-ger    format ">>>9" skip(1)
"POSICAO    CLASSIFICACAO   Qtde.     %"          at   1
"POSICAO    CLASSIFICACAO   Qtde.     %"          at  42
"POSICAO    CLASSIFICACAO   Qtde.     %"          at  83 skip
"Esquerda   Aprov"                                at   1
tt-param.fi-ini-res-aprov         format ">>9"    at  18
"-"
tt-param.fi-fin-res-aprov         format ">>9" 
i-cont-esq-apr                    format ">>>9"   at  28
i-cont-esq-apr / i-cont-ger * 100 format ">>9.9"  at  35
"Centro     Aprov"                                at  42
tt-param.fi-ini-res-aprov         format ">>9"    at  59
"-"
tt-param.fi-fin-res-apr           format ">>9" 
i-cont-cen-apr                    format ">>>9"   at  69
i-cont-cen-apr / i-cont-ger * 100 format ">>9.9"  at  76
"Direita    Aprov"                                at  83
tt-param.fi-ini-res-aprov         format ">>9"    at 100
"-"
tt-param.fi-fin-res-aprov         format ">>9" 
i-cont-dir-apr                    format ">>>9"   at 110
i-cont-dir-apr / i-cont-ger * 100 format ">>9.9"  at 117 skip

"           Reprv"                                at   1
i-cont-esq-rep                    format ">>>9"   at  28
i-cont-esq-rep / i-cont-ger * 100 format ">>9.9"  at  35
"           Reprv"                                at  42
i-cont-cen-rep                    format ">>>9"   at  69
i-cont-cen-rep / i-cont-ger * 100 format ">>9.9"  at  76
"           Reprv"                                at  83 
i-cont-dir-rep                    format ">>>9"   at 110
i-cont-dir-rep / i-cont-ger * 100 format ">>9.9"  at 117
SKIP(1).
       
IF tt-param.imp-param THEN
   display STREAM str-rp
           tt-param.desc-classifica
           tt-param.fi-ini-fm-codigo              
           tt-param.fi-fin-fm-codigo              
           tt-param.fi-ini-fundo                
           tt-param.fi-fin-fundo
           tt-param.fi-ini-data-teste
           tt-param.fi-fin-data-teste
           tt-param.fi-ini-nome-proc
           tt-param.fi-fin-nome-proc
           tt-param.fi-ini-cod-maq
           tt-param.fi-fin-cod-maq
           tt-param.fi-ini-num-ob
           tt-param.fi-fin-num-ob
           tt-param.fi-ini-res-esqrd
           tt-param.fi-fin-res-esqrd
           tt-param.fi-ini-res-centro
           tt-param.fi-fin-res-centro
           tt-param.fi-ini-res-direita
           tt-param.fi-fin-res-direita
           tt-param.fi-ini-res-aprov
           tt-param.fi-fin-res-aprov
           tt-param.desc-tipo-rel
           with frame f-param.           

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i &STREAM="stream str-rp"}
run pi-finalizar in h-acomp.
return "OK":U.
