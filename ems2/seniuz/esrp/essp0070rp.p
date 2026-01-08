/* Programa: ESSP0063RP.P (Chamado pelo programa ESSP0070.W)
** Objetivo: Imprimir o relat¢rio de Testes de Produtos 
**           (tabela: teste-prod)
** Sistema.: Especificos
** M¢dulo..: Especificos
** Autor...: Fabio Coelho Lanza - Janeiro/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0070RP 2.04.00.000}
/* defini‡Æo das temp-tables para recebimento de parƒmetros */

define temp-table tt-param no-undo
       field destino             as integer
       field arquivo             as char format "x(35)"
       field usuario             as char format "x(12)"
       field data-exec           as date
       field hora-exec           as integer
       field classifica          as integer
       field desc-classifica     as char format "x(40)"
       FIELD fi-ini-num-of       LIKE teste-prod.num-of    
       FIELD fi-fin-num-of       LIKE teste-prod.num-of   
       FIELD fi-ini-data-teste   LIKE teste-prod.data-teste
       FIELD fi-fin-data-teste   LIKE teste-prod.data-teste
       FIELD fi-ini-coef-ph      LIKE teste-prod.coef-ph    
       FIELD fi-fin-coef-ph      LIKE teste-prod.coef-ph     
       FIELD fi-ini-mat-seca     LIKE teste-prod.mat-seca  
       FIELD fi-fin-mat-seca     LIKE teste-prod.mat-seca   
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

def var de-tot-coef-ph  as dec.
def var de-tot-mat-seca as dec.
def var i-cont-of       as int format ">>>9".
def var i-cont-dat      as int format ">>>9".
def var i-cont-ger      as int.

/* defini‡Æo de frames do relat¢rio */
FORM 
   "*------------ Parametros/Selecao -------------*" SKIP
   tt-param.desc-classifica    LABEL "Classificacao.."    SKIP
   tt-param.fi-ini-num-of      label "Ordem Fabricao."
   "A"  AT 30
   tt-param.fi-fin-num-of      NO-LABELS             SKIP
   tt-param.fi-ini-data-teste  label "Data-Teste....."
   "A"  AT 30
   tt-param.fi-fin-data-teste  no-labels             SKIP
   tt-param.fi-ini-coef-ph     LABEL "Coeficiente PH."
   "A"  AT 30
   tt-param.fi-fin-coef-ph     NO-LABELS             SKIP
   tt-param.fi-ini-mat-seca    LABEL "Materia Seca..."
   "A"  AT 30
   tt-param.fi-fin-mat-seca    NO-LABELS             SKIP
   tt-param.desc-tipo-rel      LABEL "Tipo Relatorio."
   with FRAME f-param side-labels no-box WIDTH 133 STREAM-IO.

FORM   
   teste-prod.num-of         label "Num-OF."
   teste-prod.data-teste     label "Data-Teste"
   teste-prod.num-seq        label "NS"
   teste-prod.cod-prod       label "Produto"
   teste-prod.asp-fisico     label "Aspecto Fisico"
   teste-prod.coef-ph        label "Coef-PH"
   teste-prod.mat-seca       label "Mat-Seca"
   i-cont-of                 label "Tsts"
   with frame f-analitico-of NO-LABEL WIDTH 133 down stream-io.

FORM   
   teste-prod.num-of         label "Num-OF."
   teste-prod.data-teste     label "Data-Teste"
   teste-prod.coef-ph        label "Coef-PH"
   teste-prod.mat-seca       label "Mat-Seca"
   i-cont-of                 label "Tsts"
   with frame f-sintetico-of NO-LABEL WIDTH 133 down stream-io.

FORM
   teste-prod.data-teste     label "Data-Teste"
   teste-prod.num-of         label "Num-OF."
   teste-prod.num-seq        label "NS"
   teste-prod.cod-prod       label "Produto"
   teste-prod.asp-fisico     label "Aspecto Fisico"
   teste-prod.coef-ph        label "Coef-PH"
   teste-prod.mat-seca       label "Mat-Seca"
   i-cont-dat                label "Tsts"
   with frame f-analitico-dat NO-LABEL WIDTH 133 down stream-io.

FORM   
    teste-prod.data-teste     label "Data-Teste"
    teste-prod.num-of         label "Num-OF."
    teste-prod.coef-ph        label "Coef-PH"
    teste-prod.mat-seca       label "Mat-Seca"
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
{utp/ut-liter.i Testes_de_Produtos * r}
assign c-titulo-relat = trim(return-value).

view stream str-rp frame f-cabec.
view stream str-rp frame f-rodape.
run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

if tt-param.classific = 1 then do: /* Por Ordem de Fabricacao */
   FOR each teste-prod no-lock
      where teste-prod.num-of        >= tt-param.fi-ini-num-of
        AND teste-prod.num-of        <= tt-param.fi-fin-num-of
        AND teste-prod.data-teste    >= tt-param.fi-ini-data-teste
        AND teste-prod.data-teste    <= tt-param.fi-fin-data-teste
        AND teste-prod.coef-ph       >= tt-param.fi-ini-coef-ph     
        AND teste-prod.coef-ph       <= tt-param.fi-fin-coef-ph     
        AND teste-prod.mat-seca      >= tt-param.fi-ini-mat-seca  
        AND teste-prod.mat-seca      <= tt-param.fi-fin-mat-seca  
      BREAK BY teste-prod.num-of      
      on stop undo,leave:
      run pi-acompanhar in h-acomp (input string(teste-prod.num-of)).
      
      assign i-cont-of       = i-cont-of + 1
             de-tot-coef-ph  = de-tot-coef-ph  + teste-prod.coef-ph
             de-tot-mat-seca = de-tot-mat-seca + teste-prod.mat-seca.

      IF tt-param.tipo-relatorio = 1 THEN do: /* Sintetico */
         IF LAST-OF(teste-prod.num-of) THEN DO:
            DISPLAY STREAM str-rp
                    teste-prod.num-of
                    teste-prod.data-teste
                    de-tot-coef-ph  / i-cont-of @ teste-prod.coef-ph
                    de-tot-mat-seca / i-cont-of @ teste-prod.mat-seca
                    i-cont-of
                    WITH FRAME f-sintetico-of.
            DOWN STREAM str-rp WITH FRAME f-sintetico-of.
            ASSIGN i-cont-of       = 0
                   de-tot-coef-ph  = 0
                   de-tot-mat-seca = 0.
         END.
      END.
      ELSE DO: /*Analitico */
         display stream str-rp
                 teste-prod.num-of   
                  WHEN FIRST-OF(teste-prod.num-of)
                 teste-prod.data-teste
                 teste-prod.num-seq 
                 teste-prod.cod-prod
                 teste-prod.asp-fisico
                 teste-prod.coef-ph      
                 teste-prod.mat-seca
                 with frame f-analitico-of.
         down stream str-rp with frame f-analitico-of.
         
         IF LAST-OF(teste-prod.num-of) THEN DO:
            DISPLAY STREAM str-rp
                    "Media"                     @ teste-prod.data-teste
                    de-tot-coef-ph  / i-cont-of @ teste-prod.coef-ph
                    de-tot-mat-seca / i-cont-of @ teste-prod.mat-seca
                    i-cont-of
                    WITH FRAME f-analitico-of.
            DOWN 2 STREAM str-rp WITH FRAME f-analitico-of.
            ASSIGN i-cont-of       = 0
                   de-tot-coef-ph  = 0
                   de-tot-mat-seca = 0.
         END.
      END.
   END.
end.
else do: /* Por Data */
   FOR each teste-prod no-lock
      where teste-prod.data-teste    >= tt-param.fi-ini-data-teste
        AND teste-prod.data-teste    <= tt-param.fi-fin-data-teste
        AND teste-prod.num-of        >= tt-param.fi-ini-num-of
        AND teste-prod.num-of        <= tt-param.fi-fin-num-of
        AND teste-prod.coef-ph       >= tt-param.fi-ini-coef-ph
        AND teste-prod.coef-ph       <= tt-param.fi-fin-coef-ph
        AND teste-prod.mat-seca      >= tt-param.fi-ini-mat-seca
        AND teste-prod.mat-seca      <= tt-param.fi-fin-mat-seca
   BREAK BY teste-prod.data-teste
         BY teste-prod.num-of
   on stop undo,leave:
      run pi-acompanhar in h-acomp (input string(teste-prod.data-teste)).

      assign i-cont-dat      = i-cont-dat + 1
             de-tot-coef-ph  = de-tot-coef-ph  + teste-prod.coef-ph
             de-tot-mat-seca = de-tot-mat-seca + teste-prod.mat-seca.

      IF tt-param.tipo-relatorio = 1 THEN do: /* Sintetico */
         IF LAST-OF(teste-prod.data-teste) THEN DO:
          DISPLAY STREAM str-rp
                  teste-prod.data-teste
                  teste-prod.num-of
                  de-tot-coef-ph  / i-cont-dat @ teste-prod.coef-ph
                  de-tot-mat-seca / i-cont-dat @ teste-prod.mat-seca
                  i-cont-dat
                  WITH FRAME f-sintetico-dat.
          DOWN STREAM str-rp WITH FRAME f-sintetico-dat.
          ASSIGN i-cont-dat       = 0
                 de-tot-coef-ph   = 0
                 de-tot-mat-seca  = 0.
         END.
      END.
      ELSE DO: /*Analitico */
          display stream str-rp
                  teste-prod.data-teste
                   WHEN FIRST-OF(teste-prod.data-teste)
                  teste-prod.num-of   
                  teste-prod.num-seq       
                  teste-prod.cod-prod
                  teste-prod.asp-fisico
                  teste-prod.coef-ph      
                  teste-prod.mat-seca
                  teste-prod.num-seq
                  with frame f-analitico-dat.
          down stream str-rp with frame f-analitico-dat.

          IF LAST-OF(teste-prod.data-teste) THEN DO:
             DISPLAY STREAM str-rp
                     "Media"                      @ teste-prod.cod-prod
                     de-tot-coef-ph  / i-cont-dat @ teste-prod.coef-ph
                     de-tot-mat-seca / i-cont-dat @ teste-prod.mat-seca
                     i-cont-dat
                     WITH FRAME f-analitico-dat.
             DOWN 2 STREAM str-rp WITH FRAME f-analitico-dat.
             ASSIGN i-cont-dat      = 0
                    de-tot-coef-ph  = 0
                    de-tot-mat-seca = 0.
          END.

      END.
   END.
END.
       
IF tt-param.imp-param THEN
   display STREAM str-rp
           tt-param.desc-classifica
           tt-param.fi-ini-num-of              
           tt-param.fi-fin-num-of              
           tt-param.fi-ini-data-teste                
           tt-param.fi-fin-data-teste
           tt-param.fi-ini-coef-ph
           tt-param.fi-fin-coef-ph
           tt-param.fi-ini-mat-seca
           tt-param.fi-fin-mat-seca
           tt-param.desc-tipo-rel
           with frame f-param.           

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i &STREAM="stream str-rp"}
run pi-finalizar in h-acomp.
return "OK":U.
