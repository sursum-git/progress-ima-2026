/* Programa: ESSP0059RP.P (Chamado pelo programa ESSP0059.W)
** Objetivo: Imprimir o relat¢rio de Testes de Carga de Goma 
**           (tabela: teste-cgoma)
** Sistema.: Espec¡ficos
** M¢dulo..: Espec¡ficos
** Autor...: F bio Coelho Lanza - Novembro/2003
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0059RP 2.04.00.000}
/* defini‡Æo das temp-tables para recebimento de parƒmetros */

define temp-table tt-param no-undo
       field destino             as integer
       field arquivo             as char format "x(35)"
       field usuario             as char format "x(12)"
       field data-exec           as date
       field hora-exec           as integer
       field classifica          as integer
       field desc-classifica     as char format "x(40)"
       FIELD fi-ini-fm-codigo    LIKE teste-cgoma.fm-codigo
       FIELD fi-fin-fm-codigo    LIKE teste-cgoma.fm-codigo
       FIELD fi-ini-data-teste   LIKE teste-cgoma.data-teste
       FIELD fi-fin-data-teste   LIKE teste-cgoma.data-teste
       FIELD fi-ini-tipo-goma    LIKE teste-cgoma.tipo-goma
       FIELD fi-fin-tipo-goma    LIKE teste-cgoma.tipo-goma
       FIELD fi-ini-cod-maq      LIKE teste-cgoma.cod-maq
       FIELD fi-fin-cod-maq      LIKE teste-cgoma.cod-maq
       FIELD fi-ini-umid-esqrd   LIKE teste-cgoma.umid-esqrd
       FIELD fi-fin-umid-esqrd   LIKE teste-cgoma.umid-esqrd
       FIELD fi-ini-umid-centro  LIKE teste-cgoma.umid-centro
       FIELD fi-fin-umid-centro  LIKE teste-cgoma.umid-centro
       FIELD fi-ini-umid-direita LIKE teste-cgoma.umid-direita
       FIELD fi-fin-umid-direita LIKE teste-cgoma.umid-direita
       FIELD fi-ini-cgom-esqrd   LIKE teste-cgoma.cgom-esqrd
       FIELD fi-fin-cgom-esqrd   LIKE teste-cgoma.cgom-esqrd
       FIELD fi-ini-cgom-centro  LIKE teste-cgoma.cgom-centro
       FIELD fi-fin-cgom-centro  LIKE teste-cgoma.cgom-centro
       FIELD fi-ini-cgom-direita LIKE teste-cgoma.cgom-direita
       FIELD fi-fin-cgom-direita LIKE teste-cgoma.cgom-direita
       FIELD fi-ini-ruim         AS INTEGER
       FIELD fi-fin-ruim         AS INTEGER
       FIELD fi-ini-regular      AS INTEGER
       FIELD fi-fin-regular      AS INTEGER
       FIELD fi-ini-bom          AS INTEGER
       FIELD fi-fin-bom          AS INTEGER
       FIELD fi-ini-otimo        AS INTEGER
       FIELD fi-fin-otimo        AS INTEGER
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

def var de-tot-umd-esq          as dec.
def var de-tot-umd-cen          as dec.
def var de-tot-umd-dir          as dec.
def var de-tot-crg-esq          as dec.
def var de-tot-crg-cen          as dec.
def var de-tot-crg-dir          as dec.
def var i-cont-fam              as int format ">>>9".
def var i-cont-dat              as int format ">>>9".
def var i-cont-ger              as int.
def var i-cont-umid-esq-ruim    as int.
def var i-cont-umid-cen-ruim    as int.
def var i-cont-umid-dir-ruim    as int.
def var i-cont-cgom-esq-ruim    as int.
def var i-cont-cgom-cen-ruim    as int.
def var i-cont-cgom-dir-ruim    as int.
def var i-cont-umid-esq-regular as int.
def var i-cont-umid-cen-regular as int.
def var i-cont-umid-dir-regular as int.
def var i-cont-cgom-esq-regular as int.
def var i-cont-cgom-cen-regular as int.
def var i-cont-cgom-dir-regular as int.
def var i-cont-umid-esq-bom     as int.
def var i-cont-umid-cen-bom     as int.
def var i-cont-umid-dir-bom     as int.
def var i-cont-cgom-esq-bom     as int.
def var i-cont-cgom-cen-bom     as int.
def var i-cont-cgom-dir-bom     as int.
def var i-cont-umid-esq-otimo   as int.
def var i-cont-umid-cen-otimo   as int.
def var i-cont-umid-dir-otimo   as int.
def var i-cont-cgom-esq-otimo   as int.
def var i-cont-cgom-cen-otimo   as int.
def var i-cont-cgom-dir-otimo   as int.

/* defini‡Æo de frames do relat¢rio */
FORM 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
   tt-param.desc-classifica     LABEL "Classific."    SKIP
   tt-param.fi-ini-fm-codigo    LABEL "Familia..."
   "A"  AT 35
   tt-param.fi-fin-fm-codigo    NO-LABELS             SKIP
   tt-param.fi-ini-data-teste   LABEL "Data......"
   "A"  AT 35
   tt-param.fi-fin-data-teste   NO-LABELS             SKIP
   tt-param.fi-ini-tipo-goma    LABEL "Tipo Goma."
   "A"  AT 35
   tt-param.fi-fin-tipo-goma    NO-LABELS             SKIP
   tt-param.fi-ini-cod-maq      LABEL "Maquina..."
   "A"  AT 35
   tt-param.fi-fin-cod-maq      NO-LABELS             SKIP
   tt-param.fi-ini-umid-esqrd   LABEL "U.Esquerda"
   "A"  AT 35
   tt-param.fi-fin-umid-esqrd   NO-LABELS             SKIP
   tt-param.fi-ini-umid-centro  LABEL "U. Centro."
   "A"  AT 35
   tt-param.fi-fin-umid-centro  NO-LABELS             SKIP 
   tt-param.fi-ini-umid-direita LABEL "U. Direita"
   "A"  AT 35
   tt-param.fi-fin-umid-direita NO-LABELS             SKIP 
   tt-param.fi-ini-cgom-esqrd   LABEL "CrG.Esqrd."
   "A"  AT 35
   tt-param.fi-fin-cgom-esqrd   NO-LABELS             SKIP 
   tt-param.fi-ini-cgom-centro  LABEL "CrG.Centro"
   "A"  AT 35
   tt-param.fi-fin-cgom-centro  NO-LABELS             SKIP
   tt-param.fi-ini-cgom-direita LABEL "CrG.Direit"
   "A"  AT 35
   tt-param.fi-fin-cgom-direita NO-LABELS             SKIP
   tt-param.fi-ini-ruim         LABEL "Res.Ruim.."
   "A"  AT 35
   tt-param.fi-fin-ruim         NO-LABELS             SKIP
   tt-param.fi-ini-regular      LABEL "Res.Regul."
   "A"  AT 35
   tt-param.fi-fin-regular      NO-LABELS             SKIP
   tt-param.fi-ini-bom          LABEL "Res. Bom.."
   "A"  AT 35
   tt-param.fi-fin-bom          NO-LABELS             SKIP
   tt-param.fi-ini-otimo        LABEL "Res. Otimo"
   "A"  AT 35
   tt-param.fi-fin-otimo        NO-LABELS             SKIP
   tt-param.desc-tipo-rel       LABEL "Tipo Relat"
   with FRAME f-param side-labels no-box WIDTH 133 STREAM-IO.

FORM   
   teste-cgoma.fm-codigo     label "Familia"
   familia.descricao         label "Descricao"
   teste-cgoma.data-teste    label "Data-Teste"
   teste-cgoma.cod-maq       label "Maq."
   maq-benef.descricao       label "Descricao"
   teste-cgoma.num-seq       label "NS"
   teste-cgoma.umid-esqrd    label "UmdEsq"
   teste-cgoma.umid-centro   label "UmdCen"
   teste-cgoma.umid-direita  label "UmdDir"
   teste-cgoma.cgom-esqrd    label "CrGEsq"
   teste-cgoma.cgom-centro   label "CrGCen"
   teste-cgoma.cgom-direita  label "CrGDir" 
   i-cont-fam                label "Tsts"
   with frame f-analitico-fam NO-LABEL WIDTH 133 down stream-io.

FORM   
   teste-cgoma.fm-codigo     label "Familia"
   familia.descricao         label "Descricao"
   teste-cgoma.umid-esqrd    label "UmdEsq"
   teste-cgoma.umid-centro   label "UmdCen"
   teste-cgoma.umid-direita  label "UmdDir"
   teste-cgoma.cgom-esqrd    label "CrGEsq"
   teste-cgoma.cgom-centro   label "CrGCen"
   teste-cgoma.cgom-direita  label "CrGDir" 
   i-cont-fam                label "Tsts"
   with frame f-sintetico-fam NO-LABEL WIDTH 133 down stream-io.

FORM
   teste-cgoma.data-teste    label "Data-Teste"  
   teste-cgoma.fm-codigo     label "Familia"
   familia.descricao         label "Descricao"
   teste-cgoma.cod-maq       label "Maq."
   maq-benef.descricao       label "Descricao"
   teste-cgoma.num-seq       label "NS"
   teste-cgoma.umid-esqrd    label "UmdEsq"
   teste-cgoma.umid-centro   label "UmdCen"
   teste-cgoma.umid-direita  label "UmdDir"
   teste-cgoma.cgom-esqrd    label "CrGEsq"
   teste-cgoma.cgom-centro   label "CrGCen"
   teste-cgoma.cgom-direita  label "CrGDir" 
   i-cont-dat                label "Tsts"
   with frame f-analitico-dat NO-LABEL WIDTH 133 down stream-io.

FORM   
    teste-cgoma.fm-codigo     label "Familia"
    familia.descricao         label "Descricao"
    teste-cgoma.umid-esqrd    label "UmdEsq"
    teste-cgoma.umid-centro   label "UmdCen"
    teste-cgoma.umid-direita  label "UmdDir"
    teste-cgoma.cgom-esqrd    label "CrGEsq"
    teste-cgoma.cgom-centro   label "CrGCen"
    teste-cgoma.cgom-direita  label "CrGDir" 
    i-cont-dat                label "Tsts"
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
{utp/ut-liter.i Testes_da_Carga_de_Goma * r}
assign c-titulo-relat = trim(return-value).

view stream str-rp frame f-cabec.
view stream str-rp frame f-rodape.
run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

if tt-param.classific = 1 then do: /* Por Fam¡lia */
   FOR each teste-cgoma no-lock
      where teste-cgoma.fm-codigo    >= tt-param.fi-ini-fm-codigo
        AND teste-cgoma.fm-codigo    <= tt-param.fi-fin-fm-codigo
        AND teste-cgoma.data-teste   >= tt-param.fi-ini-data-teste
        AND teste-cgoma.data-teste   <= tt-param.fi-fin-data-teste
        AND teste-cgoma.tipo-goma    >= tt-param.fi-ini-tipo-goma
        AND teste-cgoma.tipo-goma    <= tt-param.fi-fin-tipo-goma
        AND teste-cgoma.cod-maq      >= tt-param.fi-ini-cod-maq   
        AND teste-cgoma.cod-maq      <= tt-param.fi-fin-cod-maq  
        AND teste-cgoma.umid-esqrd   >= tt-param.fi-ini-umid-esqrd
        AND teste-cgoma.umid-esqrd   <= tt-param.fi-fin-umid-esqrd
        AND teste-cgoma.umid-centro  >= tt-param.fi-ini-umid-centro
        AND teste-cgoma.umid-centro  <= tt-param.fi-fin-umid-centro
        AND teste-cgoma.umid-direita >= tt-param.fi-ini-umid-direita
        AND teste-cgoma.umid-direita <= tt-param.fi-fin-umid-direita
      BREAK BY teste-cgoma.fm-codigo
      on stop undo,leave:
      run pi-acompanhar in h-acomp (input string(teste-cgoma.fm-codigo)).
      
      assign i-cont-fam     = i-cont-fam + 1
             de-tot-umd-esq = de-tot-umd-esq + teste-cgoma.umid-esqrd
             de-tot-umd-cen = de-tot-umd-cen + teste-cgoma.umid-centro
             de-tot-umd-dir = de-tot-umd-dir + teste-cgoma.umid-direita
             de-tot-crg-esq = de-tot-crg-esq + teste-cgoma.cgom-esqrd
             de-tot-crg-cen = de-tot-crg-cen + teste-cgoma.cgom-centro
             de-tot-crg-dir = de-tot-crg-dir + teste-cgoma.cgom-direita.

      /* Acumula para Estatisticas */
      /* Umidade Esquerda */
      assign i-cont-ger = i-cont-ger + 1.

      if  teste-cgoma.umid-esqrd >= tt-param.fi-ini-ruim
      and teste-cgoma.umid-esqrd <= tt-param.fi-fin-ruim then
          assign i-cont-umid-esq-ruim = i-cont-umid-esq-ruim + 1.
      else
      if  teste-cgoma.umid-esqrd >= tt-param.fi-ini-regular
      and teste-cgoma.umid-esqrd <= tt-param.fi-fin-regular then
          assign i-cont-umid-esq-regular = i-cont-umid-esq-regular + 1.
      else
      if  teste-cgoma.umid-esqrd >= tt-param.fi-ini-bom
      and teste-cgoma.umid-esqrd <= tt-param.fi-fin-bom then
          assign i-cont-umid-esq-bom = i-cont-umid-esq-bom + 1.
      else
      if  teste-cgoma.umid-esqrd >= tt-param.fi-ini-otimo
      and teste-cgoma.umid-esqrd <= tt-param.fi-fin-otimo then
          assign i-cont-umid-esq-otimo = i-cont-umid-esq-otimo + 1.
      /* Umidade Centro */
      if  teste-cgoma.umid-centro >= tt-param.fi-ini-ruim
      and teste-cgoma.umid-centro <= tt-param.fi-fin-ruim then
          assign i-cont-umid-cen-ruim = i-cont-umid-cen-ruim + 1.
      else
      if  teste-cgoma.umid-centro >= tt-param.fi-ini-regular
      and teste-cgoma.umid-centro <= tt-param.fi-fin-regular then
          assign i-cont-umid-cen-regular = i-cont-umid-cen-regular + 1.
      else
      if  teste-cgoma.umid-centro >= tt-param.fi-ini-bom
      and teste-cgoma.umid-centro <= tt-param.fi-fin-bom then
          assign i-cont-umid-cen-bom = i-cont-umid-cen-bom + 1.
      else
      if  teste-cgoma.umid-centro >= tt-param.fi-ini-otimo
      and teste-cgoma.umid-centro <= tt-param.fi-fin-otimo then
          assign i-cont-umid-cen-otimo = i-cont-umid-cen-otimo + 1.
      /* Umidade Direita */
      if  teste-cgoma.umid-direita >= tt-param.fi-ini-ruim
      and teste-cgoma.umid-direita <= tt-param.fi-fin-ruim then
          assign i-cont-umid-dir-ruim = i-cont-umid-dir-ruim + 1.
      else
      if  teste-cgoma.umid-direita >= tt-param.fi-ini-regular
      and teste-cgoma.umid-direita <= tt-param.fi-fin-regular then
          assign i-cont-umid-dir-regular = i-cont-umid-dir-regular + 1.
      else
      if  teste-cgoma.umid-direita >= tt-param.fi-ini-bom
      and teste-cgoma.umid-direita <= tt-param.fi-fin-bom then
          assign i-cont-umid-dir-bom = i-cont-umid-dir-bom + 1.
      else
      if  teste-cgoma.umid-direita >= tt-param.fi-ini-otimo
      and teste-cgoma.umid-direita <= tt-param.fi-fin-otimo then
          assign i-cont-umid-dir-otimo = i-cont-umid-dir-otimo + 1.
      
      /* Carga de Goma Esquerda */
      if  teste-cgoma.cgom-esqrd >= tt-param.fi-ini-ruim
      and teste-cgoma.cgom-esqrd <= tt-param.fi-fin-ruim then
          assign i-cont-cgom-esq-ruim = i-cont-cgom-esq-ruim + 1.
      else
      if  teste-cgoma.cgom-esqrd >= tt-param.fi-ini-regular
      and teste-cgoma.cgom-esqrd <= tt-param.fi-fin-regular then
          assign i-cont-cgom-esq-regular = i-cont-cgom-esq-regular + 1.
      else
      if  teste-cgoma.cgom-esqrd >= tt-param.fi-ini-bom
      and teste-cgoma.cgom-esqrd <= tt-param.fi-fin-bom then
          assign i-cont-cgom-esq-bom = i-cont-cgom-esq-bom + 1.
      else
      if  teste-cgoma.cgom-esqrd >= tt-param.fi-ini-otimo
      and teste-cgoma.cgom-esqrd <= tt-param.fi-fin-otimo then
          assign i-cont-cgom-esq-otimo = i-cont-cgom-esq-otimo + 1.
      /* Carga de Goma Centro */
      if  teste-cgoma.cgom-centro >= tt-param.fi-ini-ruim
      and teste-cgoma.cgom-centro <= tt-param.fi-fin-ruim then
          assign i-cont-cgom-cen-ruim = i-cont-cgom-cen-ruim + 1.
      else
      if  teste-cgoma.cgom-centro >= tt-param.fi-ini-regular
      and teste-cgoma.cgom-centro <= tt-param.fi-fin-regular then
          assign i-cont-cgom-cen-regular = i-cont-cgom-cen-regular + 1.
      else
      if  teste-cgoma.cgom-centro >= tt-param.fi-ini-bom
      and teste-cgoma.cgom-centro <= tt-param.fi-fin-bom then
          assign i-cont-cgom-cen-bom = i-cont-cgom-cen-bom + 1.
      else
      if  teste-cgoma.cgom-centro >= tt-param.fi-ini-otimo
      and teste-cgoma.cgom-centro <= tt-param.fi-fin-otimo then
          assign i-cont-cgom-cen-otimo = i-cont-cgom-cen-otimo + 1.
      /* Carga de Goma Direita */
      if  teste-cgoma.cgom-direita >= tt-param.fi-ini-ruim
      and teste-cgoma.cgom-direita <= tt-param.fi-fin-ruim then
          assign i-cont-cgom-dir-ruim = i-cont-cgom-dir-ruim + 1.
      else
      if  teste-cgoma.cgom-direita >= tt-param.fi-ini-regular
      and teste-cgoma.cgom-direita <= tt-param.fi-fin-regular then
          assign i-cont-cgom-dir-regular = i-cont-cgom-dir-regular + 1.
      else
      if  teste-cgoma.cgom-direita >= tt-param.fi-ini-bom
      and teste-cgoma.cgom-direita <= tt-param.fi-fin-bom then
          assign i-cont-cgom-dir-bom = i-cont-cgom-dir-bom + 1.
      else
      if  teste-cgoma.cgom-direita >= tt-param.fi-ini-otimo
      and teste-cgoma.cgom-direita <= tt-param.fi-fin-otimo then
          assign i-cont-cgom-dir-otimo = i-cont-cgom-dir-otimo + 1.
      
      IF tt-param.tipo-relatorio = 1 THEN do: /* Sint‚tico */
         IF LAST-OF(teste-cgoma.fm-codigo) THEN DO:
            FIND familia WHERE familia.fm-codigo = teste-cgoma.fm-codigo
                         NO-LOCK NO-ERROR.
            DISPLAY STREAM str-rp
                    teste-cgoma.fm-codigo
                    familia.descricao
                    de-tot-umd-esq / i-cont-fam @ teste-cgoma.umid-esqrd
                    de-tot-umd-cen / i-cont-fam @ teste-cgoma.umid-centro
                    de-tot-umd-dir / i-cont-fam @ teste-cgoma.umid-direita
                    de-tot-crg-esq / i-cont-fam @ teste-cgoma.cgom-esqrd
                    de-tot-crg-cen / i-cont-fam @ teste-cgoma.cgom-centro
                    de-tot-crg-dir / i-cont-fam @ teste-cgoma.cgom-direita
                    i-cont-fam
                    WITH FRAME f-sintetico-fam.
            DOWN STREAM str-rp WITH FRAME f-sintetico-fam.
            ASSIGN i-cont-fam     = 0
                   de-tot-umd-esq = 0
                   de-tot-umd-cen = 0
                   de-tot-umd-dir = 0
                   de-tot-crg-esq = 0
                   de-tot-crg-cen = 0
                   de-tot-crg-dir = 0.
         END.
      END.
      ELSE DO: /*Anal¡tico */
         FIND familia WHERE familia.fm-codigo = teste-cgoma.fm-codigo
                      NO-LOCK NO-ERROR.
         find maq-benef where maq-benef.codigo = teste-cgoma.cod-maq
                              no-lock no-error.
         display stream str-rp
                 teste-cgoma.fm-codigo   
                  WHEN FIRST-OF(teste-cgoma.fm-codigo)
                 familia.descricao       
                  WHEN first-of(teste-cgoma.fm-codigo) AND AVAIL familia
                 teste-cgoma.data-teste
                 teste-cgoma.cod-maq
                 maq-benef.descricao
                 teste-cgoma.num-seq
                 teste-cgoma.umid-esqrd
                 teste-cgoma.umid-centro
                 teste-cgoma.umid-direita
                 teste-cgoma.cgom-esqrd
                 teste-cgoma.cgom-centro
                 teste-cgoma.cgom-direita
                 with frame f-analitico-fam.
         down stream str-rp with frame f-analitico-fam.
         
         IF LAST-OF(teste-cgoma.fm-codigo) THEN DO:
            DISPLAY STREAM str-rp
                    "M‚dia"                     @ teste-cgoma.fm-codigo
                    de-tot-umd-esq / i-cont-fam @ teste-cgoma.umid-esqrd
                    de-tot-umd-cen / i-cont-fam @ teste-cgoma.umid-centro
                    de-tot-umd-dir / i-cont-fam @ teste-cgoma.umid-direita
                    de-tot-crg-esq / i-cont-fam @ teste-cgoma.cgom-esqrd
                    de-tot-crg-cen / i-cont-fam @ teste-cgoma.cgom-centro
                    de-tot-crg-dir / i-cont-fam @ teste-cgoma.cgom-direita
                    i-cont-fam
                    WITH FRAME f-analitico-fam.
            DOWN 2 STREAM str-rp WITH FRAME f-analitico-fam.
            ASSIGN i-cont-fam     = 0
                   de-tot-umd-esq = 0
                   de-tot-umd-cen = 0
                   de-tot-umd-dir = 0
                   de-tot-crg-esq = 0
                   de-tot-crg-cen = 0
                   de-tot-crg-dir = 0.
         END.
      END.
   END.
end.
else do: /* Por Data */
   FOR each teste-cgoma no-lock
      where teste-cgoma.fm-codigo    >= tt-param.fi-ini-fm-codigo
        AND teste-cgoma.fm-codigo    <= tt-param.fi-fin-fm-codigo
        AND teste-cgoma.data-teste   >= tt-param.fi-ini-data-teste
        AND teste-cgoma.data-teste   <= tt-param.fi-fin-data-teste
        AND teste-cgoma.tipo-goma    >= tt-param.fi-ini-tipo-goma
        AND teste-cgoma.tipo-goma    <= tt-param.fi-fin-tipo-goma
        AND teste-cgoma.cod-maq      >= tt-param.fi-ini-cod-maq   
        AND teste-cgoma.cod-maq      <= tt-param.fi-fin-cod-maq  
        AND teste-cgoma.umid-esqrd   >= tt-param.fi-ini-umid-esqrd
        AND teste-cgoma.umid-esqrd   <= tt-param.fi-fin-umid-esqrd
        AND teste-cgoma.umid-centro  >= tt-param.fi-ini-umid-centro
        AND teste-cgoma.umid-centro  <= tt-param.fi-fin-umid-centro
        AND teste-cgoma.umid-direita >= tt-param.fi-ini-umid-direita
        AND teste-cgoma.umid-direita <= tt-param.fi-fin-umid-direita
   BREAK BY teste-cgoma.data-teste
         BY teste-cgoma.fm-codigo
   on stop undo,leave:
      run pi-acompanhar in h-acomp (input string(teste-cgoma.fm-codigo)).

      assign i-cont-dat     = i-cont-dat + 1
             de-tot-umd-esq = de-tot-umd-esq + teste-cgoma.umid-esqrd
             de-tot-umd-cen = de-tot-umd-cen + teste-cgoma.umid-centro
             de-tot-umd-dir = de-tot-umd-dir + teste-cgoma.umid-direita
             de-tot-crg-esq = de-tot-crg-esq + teste-cgoma.cgom-esqrd
             de-tot-crg-cen = de-tot-crg-cen + teste-cgoma.cgom-centro
             de-tot-crg-dir = de-tot-crg-dir + teste-cgoma.cgom-direita.

      /* Acumula para Estatisticas */
      /* Umidade Esquerda */
      assign i-cont-ger = i-cont-ger + 1.

      if  teste-cgoma.umid-esqrd >= tt-param.fi-ini-ruim
      and teste-cgoma.umid-esqrd <= tt-param.fi-fin-ruim then
          assign i-cont-umid-esq-ruim = i-cont-umid-esq-ruim + 1.
      else
      if  teste-cgoma.umid-esqrd >= tt-param.fi-ini-regular
      and teste-cgoma.umid-esqrd <= tt-param.fi-fin-regular then
          assign i-cont-umid-esq-regular = i-cont-umid-esq-regular + 1.
      else
      if  teste-cgoma.umid-esqrd >= tt-param.fi-ini-bom
      and teste-cgoma.umid-esqrd <= tt-param.fi-fin-bom then
          assign i-cont-umid-esq-bom = i-cont-umid-esq-bom + 1.
      else
      if  teste-cgoma.umid-esqrd >= tt-param.fi-ini-otimo
      and teste-cgoma.umid-esqrd <= tt-param.fi-fin-otimo then
          assign i-cont-umid-esq-otimo = i-cont-umid-esq-otimo + 1.
      /* Umidade Centro */
      if  teste-cgoma.umid-centro >= tt-param.fi-ini-ruim
      and teste-cgoma.umid-centro <= tt-param.fi-fin-ruim then
          assign i-cont-umid-cen-ruim = i-cont-umid-cen-ruim + 1.
      else
      if  teste-cgoma.umid-centro >= tt-param.fi-ini-regular
      and teste-cgoma.umid-centro <= tt-param.fi-fin-regular then
          assign i-cont-umid-cen-regular = i-cont-umid-cen-regular + 1.
      else
      if  teste-cgoma.umid-centro >= tt-param.fi-ini-bom
      and teste-cgoma.umid-centro <= tt-param.fi-fin-bom then
          assign i-cont-umid-cen-bom = i-cont-umid-cen-bom + 1.
      else
      if  teste-cgoma.umid-centro >= tt-param.fi-ini-otimo
      and teste-cgoma.umid-centro <= tt-param.fi-fin-otimo then
          assign i-cont-umid-cen-otimo = i-cont-umid-cen-otimo + 1.
      /* Umidade Direita */
      if  teste-cgoma.umid-direita >= tt-param.fi-ini-ruim
      and teste-cgoma.umid-direita <= tt-param.fi-fin-ruim then
          assign i-cont-umid-dir-ruim = i-cont-umid-dir-ruim + 1.
      else
      if  teste-cgoma.umid-direita >= tt-param.fi-ini-regular
      and teste-cgoma.umid-direita <= tt-param.fi-fin-regular then
          assign i-cont-umid-dir-regular = i-cont-umid-dir-regular + 1.
      else
      if  teste-cgoma.umid-direita >= tt-param.fi-ini-bom
      and teste-cgoma.umid-direita <= tt-param.fi-fin-bom then
          assign i-cont-umid-dir-bom = i-cont-umid-dir-bom + 1.
      else
      if  teste-cgoma.umid-direita >= tt-param.fi-ini-otimo
      and teste-cgoma.umid-direita <= tt-param.fi-fin-otimo then
          assign i-cont-umid-dir-otimo = i-cont-umid-dir-otimo + 1.
      
      /* Carga de Goma Esquerda */
      if  teste-cgoma.cgom-esqrd >= tt-param.fi-ini-ruim
      and teste-cgoma.cgom-esqrd <= tt-param.fi-fin-ruim then
          assign i-cont-cgom-esq-ruim = i-cont-cgom-esq-ruim + 1.
      else
      if  teste-cgoma.cgom-esqrd >= tt-param.fi-ini-regular
      and teste-cgoma.cgom-esqrd <= tt-param.fi-fin-regular then
          assign i-cont-cgom-esq-regular = i-cont-cgom-esq-regular + 1.
      else
      if  teste-cgoma.cgom-esqrd >= tt-param.fi-ini-bom
      and teste-cgoma.cgom-esqrd <= tt-param.fi-fin-bom then
          assign i-cont-cgom-esq-bom = i-cont-cgom-esq-bom + 1.
      else
      if  teste-cgoma.cgom-esqrd >= tt-param.fi-ini-otimo
      and teste-cgoma.cgom-esqrd <= tt-param.fi-fin-otimo then
          assign i-cont-cgom-esq-otimo = i-cont-cgom-esq-otimo + 1.
      /* Carga de Goma Centro */
      if  teste-cgoma.cgom-centro >= tt-param.fi-ini-ruim
      and teste-cgoma.cgom-centro <= tt-param.fi-fin-ruim then
          assign i-cont-cgom-cen-ruim = i-cont-cgom-cen-ruim + 1.
      else
      if  teste-cgoma.cgom-centro >= tt-param.fi-ini-regular
      and teste-cgoma.cgom-centro <= tt-param.fi-fin-regular then
          assign i-cont-cgom-cen-regular = i-cont-cgom-cen-regular + 1.
      else
      if  teste-cgoma.cgom-centro >= tt-param.fi-ini-bom
      and teste-cgoma.cgom-centro <= tt-param.fi-fin-bom then
          assign i-cont-cgom-cen-bom = i-cont-cgom-cen-bom + 1.
      else
      if  teste-cgoma.cgom-centro >= tt-param.fi-ini-otimo
      and teste-cgoma.cgom-centro <= tt-param.fi-fin-otimo then
          assign i-cont-cgom-cen-otimo = i-cont-cgom-cen-otimo + 1.
      /* Carga de Goma Direita */
      if  teste-cgoma.cgom-direita >= tt-param.fi-ini-ruim
      and teste-cgoma.cgom-direita <= tt-param.fi-fin-ruim then
          assign i-cont-cgom-dir-ruim = i-cont-cgom-dir-ruim + 1.
      else
      if  teste-cgoma.cgom-direita >= tt-param.fi-ini-regular
      and teste-cgoma.cgom-direita <= tt-param.fi-fin-regular then
          assign i-cont-cgom-dir-regular = i-cont-cgom-dir-regular + 1.
      else
      if  teste-cgoma.cgom-direita >= tt-param.fi-ini-bom
      and teste-cgoma.cgom-direita <= tt-param.fi-fin-bom then
          assign i-cont-cgom-dir-bom = i-cont-cgom-dir-bom + 1.
      else
      if  teste-cgoma.cgom-direita >= tt-param.fi-ini-otimo
      and teste-cgoma.cgom-direita <= tt-param.fi-fin-otimo then
          assign i-cont-cgom-dir-otimo = i-cont-cgom-dir-otimo + 1.
      
      IF tt-param.tipo-relatorio = 1 THEN do: /* Sint‚tico */
         IF LAST-OF(teste-cgoma.data-teste) THEN DO:
          DISPLAY STREAM str-rp
                  teste-cgoma.data-teste
                  de-tot-umd-esq / i-cont-dat @ teste-cgoma.umid-esqrd
                  de-tot-umd-cen / i-cont-dat @ teste-cgoma.umid-centro
                  de-tot-umd-dir / i-cont-dat @ teste-cgoma.umid-direita
                  de-tot-crg-esq / i-cont-dat @ teste-cgoma.cgom-esqrd
                  de-tot-crg-cen / i-cont-dat @ teste-cgoma.cgom-centro
                  de-tot-crg-dir / i-cont-dat @ teste-cgoma.cgom-direita
                  i-cont-dat
                  WITH FRAME f-sintetico-dat.
          DOWN STREAM str-rp WITH FRAME f-sintetico-dat.
          ASSIGN i-cont-dat     = 0
                 de-tot-umd-esq = 0
                 de-tot-umd-cen = 0
                 de-tot-umd-dir = 0
                 de-tot-crg-esq = 0
                 de-tot-crg-cen = 0
                 de-tot-crg-dir = 0.
         END.
      END.
      ELSE DO: /*Anal¡tico */
          FIND familia WHERE familia.fm-codigo = teste-cgoma.fm-codigo
                       NO-LOCK NO-ERROR.
          find maq-benef where maq-benef.codigo = teste-cgoma.cod-maq
                               no-lock no-error.
          display stream str-rp
                  teste-cgoma.data-teste
                   WHEN FIRST-OF(teste-cgoma.data-teste)
                  teste-cgoma.fm-codigo   
                  familia.descricao       
                  teste-cgoma.cod-maq
                  maq-benef.descricao
                  teste-cgoma.num-seq
                  teste-cgoma.umid-esqrd
                  teste-cgoma.umid-centro
                  teste-cgoma.umid-direita
                  teste-cgoma.cgom-esqrd
                  teste-cgoma.cgom-centro
                  teste-cgoma.cgom-direita
                  with frame f-analitico-dat.
          down stream str-rp with frame f-analitico-dat.

          IF LAST-OF(teste-cgoma.data-teste) THEN DO:
             DISPLAY STREAM str-rp
                     "M‚dia"                     @ teste-cgoma.data-teste
                     de-tot-umd-esq / i-cont-dat @ teste-cgoma.umid-esqrd
                     de-tot-umd-cen / i-cont-dat @ teste-cgoma.umid-centro
                     de-tot-umd-dir / i-cont-dat @ teste-cgoma.umid-direita
                     de-tot-crg-esq / i-cont-dat @ teste-cgoma.cgom-esqrd
                     de-tot-crg-cen / i-cont-dat @ teste-cgoma.cgom-centro
                     de-tot-crg-dir / i-cont-dat @ teste-cgoma.cgom-direita
                     i-cont-dat
                     WITH FRAME f-analitico-dat.
             DOWN 2 STREAM str-rp WITH FRAME f-analitico-dat.
             ASSIGN i-cont-dat     = 0
                    de-tot-umd-esq = 0
                    de-tot-umd-cen = 0
                    de-tot-umd-dir = 0
                    de-tot-crg-esq = 0
                    de-tot-crg-cen = 0
                    de-tot-crg-dir = 0.
          END.

      END.
   END.
END.

/* Estatisticas */
PAGE STREAM str-rp.
PUT STREAM str-rp skip(1).
put STREAM str-rp "RESULTADOS ESTATISTICOS - "
"TESTES LISTADOS: " i-cont-ger          format ">>>9"         skip(1)
"POSICAO   TESTE   CLASSIFICACAO      QTD.      %"        at   1 skip  
"------------------------------------------------"        at   1 skip  
"Esquerda  Umidade Ruim"                                  at   1
tt-param.fi-ini-ruim                       format ">>9"   at  28
"-"
tt-param.fi-fin-ruim                       format ">>9" 
i-cont-umid-esq-ruim                       format ">>>9"  at  38
i-cont-umid-esq-ruim / i-cont-ger * 100    format ">>9.9" at  44 skip 
"Regular"                                                 at  19
tt-param.fi-ini-regular                    format ">>9"   at  28
"-"
tt-param.fi-fin-regular                    format ">>9" 
i-cont-umid-esq-regular                    format ">>>9"  at  38
i-cont-umid-esq-regular / i-cont-ger * 100 format ">>9.9" at  44 skip 
"Bom"                                                     at  19
tt-param.fi-ini-bom                        format ">>9"   at  28
"-"
tt-param.fi-fin-bom                        format ">>9" 
i-cont-umid-esq-bom                        format ">>>9"  at  38
i-cont-umid-esq-bom / i-cont-ger * 100     format ">>9.9" at  44 skip 
"Otimo"                                                   at  19
tt-param.fi-ini-otimo                      format ">>9"   at  28
"-"
tt-param.fi-fin-otimo                      format ">>9" 
i-cont-umid-esq-otimo                      format ">>>9"  at  38
i-cont-umid-esq-otimo / i-cont-ger * 100   format ">>9.9" at  44 skip 
"Centro    Umidade Ruim"                                  at   1
tt-param.fi-ini-ruim                       format ">>9"   at  28
"-"
tt-param.fi-fin-ruim                       format ">>9" 
i-cont-umid-cen-ruim                       format ">>>9"  at  38
i-cont-umid-cen-ruim / i-cont-ger * 100    format ">>9.9" at  44 skip 
"Regular"                                                 at  19
tt-param.fi-ini-regular                    format ">>9"   at  28
"-"
tt-param.fi-fin-regular                    format ">>9" 
i-cont-umid-cen-regular                    format ">>>9"  at  38
i-cont-umid-cen-regular / i-cont-ger * 100 format ">>9.9" at  44 skip 
"Bom"                                                     at  19
tt-param.fi-ini-bom                        format ">>9"   at  28
"-"
tt-param.fi-fin-bom                        format ">>9" 
i-cont-umid-cen-bom                        format ">>>9"  at  38
i-cont-umid-cen-bom / i-cont-ger * 100     format ">>9.9" at  44 skip 
"Otimo"                                                   at  19
tt-param.fi-ini-otimo                      format ">>9"   at  28
"-"
tt-param.fi-fin-otimo                      format ">>9" 
i-cont-umid-cen-otimo                      format ">>>9"  at  38
i-cont-umid-cen-otimo / i-cont-ger * 100   format ">>9.9" at  44 skip 
"Direita   Umidade Ruim"                                  at   1
tt-param.fi-ini-ruim                       format ">>9"   at  28
"-"
tt-param.fi-fin-ruim                       format ">>9" 
i-cont-umid-dir-ruim                       format ">>>9"  at  38
i-cont-umid-dir-ruim / i-cont-ger * 100    format ">>9.9" at  44 skip 
"Regular"                                                 at  19
tt-param.fi-ini-regular                    format ">>9"   at  28
"-"
tt-param.fi-fin-regular                    format ">>9" 
i-cont-umid-dir-regular                    format ">>>9"  at  38
i-cont-umid-dir-regular / i-cont-ger * 100 format ">>9.9" at  44 skip 
"Bom"                                                     at  19
tt-param.fi-ini-bom                        format ">>9"   at  28
"-"
tt-param.fi-fin-bom                        format ">>9" 
i-cont-umid-dir-bom                        format ">>>9"  at  38
i-cont-umid-dir-bom / i-cont-ger * 100     format ">>9.9" at  44 skip 
"Otimo"                                                   at  19
tt-param.fi-ini-otimo                      format ">>9"   at  28
"-"
tt-param.fi-fin-otimo                      format ">>9" 
i-cont-umid-dir-otimo                      format ">>>9"  at  38
i-cont-umid-dir-otimo / i-cont-ger * 100   format ">>9.9" at  44 skip 
"------------------------------------------------"        at   1 skip  
"Esquerda  C. Goma Ruim"                                  at   1
tt-param.fi-ini-ruim                       format ">>9"   at  28
"-"
tt-param.fi-fin-ruim                       format ">>9" 
i-cont-cgom-esq-ruim                       format ">>>9"  at  38
i-cont-cgom-esq-ruim / i-cont-ger * 100    format ">>9.9" at  44 skip 
"Regular"                                                 at  19
tt-param.fi-ini-regular                    format ">>9"   at  28
"-"
tt-param.fi-fin-regular                    format ">>9" 
i-cont-cgom-esq-regular                    format ">>>9"  at  38
i-cont-cgom-esq-regular / i-cont-ger * 100 format ">>9.9" at  44 skip 
"Bom"                                                     at  19
tt-param.fi-ini-bom                        format ">>9"   at  28
"-"
tt-param.fi-fin-bom                        format ">>9" 
i-cont-cgom-esq-bom                        format ">>>9"  at  38
i-cont-cgom-esq-bom / i-cont-ger * 100     format ">>9.9" at  44 skip 
"Otimo"                                                   at  19
tt-param.fi-ini-otimo                      format ">>9"   at  28
"-"
tt-param.fi-fin-otimo                      format ">>9" 
i-cont-cgom-esq-otimo                      format ">>>9"  at  38
i-cont-cgom-esq-otimo / i-cont-ger * 100   format ">>9.9" at  44 skip 
"Centro    C. Goma Ruim"                                  at   1
tt-param.fi-ini-ruim                       format ">>9"   at  28
"-"
tt-param.fi-fin-ruim                       format ">>9" 
i-cont-cgom-cen-ruim                       format ">>>9"  at  38
i-cont-cgom-cen-ruim / i-cont-ger * 100    format ">>9.9" at  44 skip 
"Regular"                                                 at  19
tt-param.fi-ini-regular                    format ">>9"   at  28
"-"
tt-param.fi-fin-regular                    format ">>9" 
i-cont-cgom-cen-regular                    format ">>>9"  at  38
i-cont-cgom-cen-regular / i-cont-ger * 100 format ">>9.9" at  44 skip 
"Bom"                                                     at  19
tt-param.fi-ini-bom                        format ">>9"   at  28
"-"
tt-param.fi-fin-bom                        format ">>9" 
i-cont-cgom-cen-bom                        format ">>>9"  at  38
i-cont-cgom-cen-bom / i-cont-ger * 100     format ">>9.9" at  44 skip 
"Otimo"                                                   at  19
tt-param.fi-ini-otimo                      format ">>9"   at  28
"-"
tt-param.fi-fin-otimo                      format ">>9" 
i-cont-cgom-cen-otimo                      format ">>>9"  at  38
i-cont-cgom-cen-otimo / i-cont-ger * 100   format ">>9.9" at  44 skip 
"Direita   C. Goma Ruim"                                  at   1
tt-param.fi-ini-ruim                       format ">>9"   at  28
"-"
tt-param.fi-fin-ruim                       format ">>9" 
i-cont-cgom-dir-ruim                       format ">>>9"  at  38
i-cont-cgom-dir-ruim / i-cont-ger * 100    format ">>9.9" at  44 skip 
"Regular"                                                 at  19
tt-param.fi-ini-regular                    format ">>9"   at  28
"-"
tt-param.fi-fin-regular                    format ">>9" 
i-cont-cgom-dir-regular                    format ">>>9"  at  38
i-cont-cgom-dir-regular / i-cont-ger * 100 format ">>9.9" at  44 skip 
"Bom"                                                     at  19
tt-param.fi-ini-bom                        format ">>9"   at  28
"-"
tt-param.fi-fin-bom                        format ">>9" 
i-cont-cgom-dir-bom                        format ">>>9"  at  38
i-cont-cgom-dir-bom / i-cont-ger * 100     format ">>9.9" at  44 skip 
"Otimo"                                                   at  19
tt-param.fi-ini-otimo                      format ">>9"   at  28
"-"
tt-param.fi-fin-otimo                      format ">>9" 
i-cont-cgom-dir-otimo                      format ">>>9"  at  38
i-cont-cgom-dir-otimo / i-cont-ger * 100   format ">>9.9" at  44 skip 
SKIP(1).
  
IF tt-param.imp-param THEN DO:
   PAGE STREAM str-rp.
   display STREAM str-rp
           tt-param.desc-classifica
           tt-param.fi-ini-fm-codigo
           tt-param.fi-fin-fm-codigo
           tt-param.fi-ini-data-teste
           tt-param.fi-fin-data-teste 
           tt-param.fi-ini-tipo-goma   
           tt-param.fi-fin-tipo-goma
           tt-param.fi-ini-cod-maq
           tt-param.fi-fin-cod-maq
           tt-param.fi-ini-umid-esqrd
           tt-param.fi-fin-umid-esqrd
           tt-param.fi-ini-umid-centro
           tt-param.fi-fin-umid-centro 
           tt-param.fi-ini-umid-direita
           tt-param.fi-fin-umid-direita 
           tt-param.fi-ini-cgom-esqrd
           tt-param.fi-fin-cgom-esqrd 
           tt-param.fi-ini-cgom-centro
           tt-param.fi-fin-cgom-centro
           tt-param.fi-ini-cgom-direita
           tt-param.fi-fin-cgom-direita
           tt-param.fi-ini-ruim
           tt-param.fi-fin-ruim
           tt-param.fi-ini-regular
           tt-param.fi-fin-regular
           tt-param.fi-ini-bom 
           tt-param.fi-fin-bom 
           tt-param.fi-ini-otimo
           tt-param.fi-fin-otimo
           tt-param.desc-tipo-rel
           with frame f-param.           
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i &STREAM="stream str-rp"}
run pi-finalizar in h-acomp.
return "OK":U.
