/* Programa: ESSP0039RP.P (Chamado pelo programa ESSP0039.W)
** Objetivo: Imprimir a listagem do cadastro de Amostras por Fam¡lia 
**           (tabela: amostra-fam)
** Sistema.: Espec¡ficos
** M¢dulo..: Espec¡ficos
** Autor...: Gilvando Souza Araujo - Novembro/2003
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0039RP 2.04.00.000}
/* defini‡Æo das temp-tables para recebimento de parƒmetros */

DEFINE TEMP-TABLE tt-param NO-UNDO
       FIELD destino           AS INTEGER
       FIELD arquivo           AS CHAR format "x(35)"
       FIELD usuario           AS CHAR format "x(12)"
       FIELD data-exec         AS DATE
       FIELD hora-exec         AS INTEGER
       FIELD classifica        AS INTEGER
       FIELD desc-classifica   AS CHAR FORMAT "x(40)"
       FIELD fi-ini-fm-codigo  LIKE amostra-fam.fm-codigo
       FIELD fi-fin-fm-codigo  LIKE amostra-fam.fm-codigo
       FIELD fi-ini-it-codigo  LIKE amostra-fam.it-codigo
       FIELD fi-fin-it-codigo  LIKE amostra-fam.it-codigo
       FIELD fi-ini-ano-mes    LIKE amostra-fam.ano-mes
       FIELD fi-fin-ano-mes    LIKE amostra-fam.ano-mes.

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

/* defini‡Æo de frames do relat¢rio */

FORM 
   "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
   tt-param.desc-classifica    LABEL "Classific."    SKIP
   tt-param.fi-ini-fm-codigo   label "Familia..."
   "A"  AT 30
   tt-param.fi-fin-fm-codigo   NO-LABELS             SKIP
   tt-param.fi-ini-it-codigo   label "Item......"
   "A"  AT 30
   tt-param.fi-fin-it-codigo   NO-LABELS             SKIP
   tt-param.fi-ini-ano-mes     label "Data......"
   "A"  AT 30
   tt-param.fi-fin-ano-mes     no-labels             SKIP
   skip(1)
   with FRAME f-param side-labels no-box width 132 STREAM-IO.

FORM   
   amostra-fam.fm-codigo
   amostra-fam.it-codigo FORMAT "x(10)"
   amostra-fam.ano-mes
   amostra-fam.num-amostra
   amostra-fam.tipo-tear 
   amostra-fam.merceriz 
   amostra-fam.observ 
   amostra-fam.larg-ini 
   amostra-fam.enc-urd
   amostra-fam.enc-tra 
   amostra-fam.larg-fin
   amostra-fam.dentra-ini
   amostra-fam.dentra-fin
   amostra-fam.num-ob 
   with frame f-amostra-fam-fam WIDTH 132 down stream-io.

FORM   
   amostra-fam.it-codigo FORMAT "x(10)"
   amostra-fam.fm-codigo
   amostra-fam.ano-mes
   amostra-fam.num-amostra
   amostra-fam.tipo-tear 
   amostra-fam.merceriz 
   amostra-fam.observ 
   amostra-fam.larg-ini 
   amostra-fam.enc-urd
   amostra-fam.enc-tra 
   amostra-fam.larg-fin
   amostra-fam.dentra-ini
   amostra-fam.dentra-fin
   amostra-fam.num-ob 
   with frame f-amostra-fam-ite WIDTH 132 down stream-io.

FORM
    amostra-fam.ano-mes
    amostra-fam.fm-codigo
    amostra-fam.it-codigo FORMAT "x(10)"
    amostra-fam.num-amostra
    amostra-fam.tipo-tear 
    amostra-fam.merceriz 
    amostra-fam.observ 
    amostra-fam.larg-ini 
    amostra-fam.enc-urd
    amostra-fam.enc-tra 
    amostra-fam.larg-fin
    amostra-fam.dentra-ini
    amostra-fam.dentra-fin
    amostra-fam.num-ob
    with frame f-amostra-fam-dat WIDTH 132 down stream-io.

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
{utp/ut-liter.i Amostras_por_Fam¡lia * r}
assign c-titulo-relat = trim(return-value).

view stream str-rp frame f-cabec.
view stream str-rp frame f-rodape.
run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

display STREAM str-rp
        tt-param.desc-classifica
        tt-param.fi-ini-fm-codigo              
        tt-param.fi-fin-fm-codigo
        tt-param.fi-ini-it-codigo
        tt-param.fi-fin-it-codigo
        tt-param.fi-ini-ano-mes                
        tt-param.fi-fin-ano-mes                
        with frame f-param.           

if tt-param.classific = 1 then do: /* Por Fam¡lia */
   FOR each amostra-fam no-lock
      where amostra-fam.fm-codigo >= tt-param.fi-ini-fm-codigo
        AND amostra-fam.fm-codigo <= tt-param.fi-fin-fm-codigo
        AND amostra-fam.it-codigo >= tt-param.fi-ini-it-codigo
        AND amostra-fam.it-codigo <= tt-param.fi-fin-it-codigo
        AND amostra-fam.ano-mes   >= tt-param.fi-ini-ano-mes
        AND amostra-fam.ano-mes   <= tt-param.fi-fin-ano-mes
      on stop undo,leave:
      run pi-acompanhar in h-acomp (input string(amostra-fam.fm-codigo)).
  
      display stream str-rp
              amostra-fam.fm-codigo
              amostra-fam.it-codigo
              amostra-fam.ano-mes
              amostra-fam.num-amostra
              amostra-fam.tipo-tear 
              amostra-fam.merceriz 
              amostra-fam.observ 
              amostra-fam.larg-ini 
              amostra-fam.enc-urd
              amostra-fam.enc-tra 
              amostra-fam.larg-fin
              amostra-fam.dentra-ini
              amostra-fam.dentra-fin
              amostra-fam.num-ob
       with frame f-amostra-fam-fam.
     down stream str-rp with frame f-amostra-fam-fam.
   END.
end.
ELSE
if tt-param.classific = 2 then do: /* Por Ötem */
   FOR each amostra-fam no-lock
      where amostra-fam.fm-codigo >= tt-param.fi-ini-fm-codigo
        AND amostra-fam.fm-codigo <= tt-param.fi-fin-fm-codigo
        AND amostra-fam.it-codigo >= tt-param.fi-ini-it-codigo
        AND amostra-fam.it-codigo <= tt-param.fi-fin-it-codigo
        AND amostra-fam.ano-mes   >= tt-param.fi-ini-ano-mes
        AND amostra-fam.ano-mes   <= tt-param.fi-fin-ano-mes
      BY amostra-fam.it-codigo
      BY amostra-fam.fm-codigo
      BY amostra-fam.ano-mes
      on stop undo,leave:
      run pi-acompanhar in h-acomp (input string(amostra-fam.fm-codigo)).
   
      display stream str-rp
              amostra-fam.fm-codigo
              amostra-fam.it-codigo
              amostra-fam.ano-mes
              amostra-fam.num-amostra
              amostra-fam.tipo-tear 
              amostra-fam.merceriz 
              amostra-fam.observ 
              amostra-fam.larg-ini 
              amostra-fam.enc-urd
              amostra-fam.enc-tra 
              amostra-fam.larg-fin
              amostra-fam.dentra-ini
              amostra-fam.dentra-fin
              amostra-fam.num-ob
       with frame f-amostra-fam-ite.
     down stream str-rp with frame f-amostra-fam-ite.
   END.
end.
else do: /* Por Data */
    FOR each amostra-fam no-lock
       where amostra-fam.fm-codigo >= tt-param.fi-ini-fm-codigo
         AND amostra-fam.fm-codigo <= tt-param.fi-fin-fm-codigo
         AND amostra-fam.it-codigo >= tt-param.fi-ini-it-codigo
         AND amostra-fam.it-codigo <= tt-param.fi-fin-it-codigo
         AND amostra-fam.ano-mes   >= tt-param.fi-ini-ano-mes
         AND amostra-fam.ano-mes   <= tt-param.fi-fin-ano-mes
    BY amostra-fam.ano-mes
    BY amostra-fam.fm-codigo
    BY amostra-fam.it-codigo
    on stop undo,leave:
       run pi-acompanhar in h-acomp (input string(amostra-fam.fm-codigo)).
       display stream str-rp
               amostra-fam.ano-mes
               amostra-fam.fm-codigo
               amostra-fam.it-codigo
               amostra-fam.num-amostra
               amostra-fam.tipo-tear 
               amostra-fam.merceriz 
               amostra-fam.observ 
               amostra-fam.larg-ini 
               amostra-fam.enc-urd
               amostra-fam.enc-tra 
               amostra-fam.larg-fin
               amostra-fam.dentra-ini
               amostra-fam.dentra-fin
               amostra-fam.num-ob
       with frame f-amostra-fam-dat.
      down stream str-rp with frame f-amostra-fam-dat.
    END.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i &STREAM="stream str-rp"}
run pi-finalizar in h-acomp.
return "OK":U.
