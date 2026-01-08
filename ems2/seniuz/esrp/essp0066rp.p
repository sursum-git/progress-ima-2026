/* Programa: ESSP0066RP.P (Chamado pelo programa ESSP0066.W)
** Objetivo: Imprimir a listagem do cadastro de Testes de ASTM
**           (tabela: teste-astm)
** Sistema.: Espec¡fico
** M¢dulo..: Espec¡fico
** Autor...: Fabio Coelho Lanza - Janeiro/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0066RP 2.04.00.000}
/* defini‡Æo das temp-tables para recebimento de parƒmetros */

DEFINE TEMP-TABLE tt-param NO-UNDO
       FIELD destino           AS INTEGER
       FIELD arquivo           AS CHAR FORMAT "x(35)"
       FIELD usuario           AS CHAR FORMAT "x(12)"
       FIELD data-exec         AS DATE
       FIELD hora-exec         AS INTEGER
       FIELD classifica        AS INTEGER
       FIELD desc-classifica   AS CHAR FORMAT "x(40)"
       FIELD fi-ini-fm-codigo  LIKE teste-astm.fm-codigo
       FIELD fi-fin-fm-codigo  LIKE teste-astm.fm-codigo
       FIELD fi-ini-it-codigo  LIKE teste-astm.it-codigo
       FIELD fi-fin-it-codigo  LIKE teste-astm.it-codigo
       FIELD fi-ini-ano-mes    LIKE teste-astm.ano-mes    
       FIELD fi-fin-ano-mes    LIKE teste-astm.ano-mes.

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
   tt-param.fi-ini-ano-mes     label "Periodo..."
   "A"  AT 30
   tt-param.fi-fin-ano-mes     no-labels             SKIP
   skip(1)
   with FRAME f-param side-labels no-box width 132 STREAM-IO.

FORM   
   teste-astm.fm-codigo          LABEL "Familia"
   ITEM.it-codigo FORMAT "x(6)"  LABEL "Item"
   ITEM.desc-item FORMAT "x(36)" LABEL "Descri‡Æo"
   teste-astm.ano-mes            LABEL "Ano-Mˆs"
   teste-astm.num-amostra        LABEL "Amo"
   teste-astm.tipo-tear          LABEL "Tear"
   teste-astm.merceriz           LABEL "Merc"
   teste-astm.num-ob             LABEL "Num.OB"
   teste-astm.enc-tra            LABEL "EncTra"
   teste-astm.enc-urd            LABEL "EmcUrd"
   teste-astm.dentra-ini         LABEL "DeTrIn"
   teste-astm.dentra-fin         LABEL "DeTfFn"
   teste-astm.observ             LABEL "Observ"
   with frame f-teste-astm-fam WITH NO-LABEL WIDTH 132 down stream-io.

FORM
    teste-astm.ano-mes            LABEL "Ano-Mˆs"
    teste-astm.fm-codigo          LABEL "Familia"
    ITEM.it-codigo FORMAT "x(6)"  LABEL "Item"     
    ITEM.desc-item FORMAT "x(36)" LABEL "Descri‡Æo"
    teste-astm.num-amostra        LABEL "Amo"    
    teste-astm.tipo-tear          LABEL "Tear"   
    teste-astm.merceriz           LABEL "Merc"   
    teste-astm.num-ob             LABEL "Num.OB" 
    teste-astm.enc-tra            LABEL "EncTra" 
    teste-astm.enc-urd            LABEL "EmcUrd" 
    teste-astm.dentra-ini         LABEL "DeTrIn" 
    teste-astm.dentra-fin         LABEL "DeTfFn" 
    teste-astm.observ             LABEL "Observ" 
   with frame f-teste-astm-dat WITH NO-LABEL WIDTH 132 down stream-io.

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
{utp/ut-liter.i Cadastro_de_Testes_de_ASTM * r}
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

if tt-param.classific = 1 then do:
  FOR each teste-astm no-lock
     where teste-astm.fm-codigo >= tt-param.fi-ini-fm-codigo
       AND teste-astm.fm-codigo <= tt-param.fi-fin-fm-codigo
       AND teste-astm.it-codigo >= tt-param.fi-ini-it-codigo
       AND teste-astm.it-codigo <= tt-param.fi-fin-it-codigo
       AND teste-astm.ano-mes   >= tt-param.fi-ini-ano-mes    
       AND teste-astm.ano-mes   <= tt-param.fi-fin-ano-mes    
     on stop undo,leave:
     run pi-acompanhar in h-acomp (input string(teste-astm.fm-codigo)).
     
     FIND ITEM WHERE ITEM.it-codigo = teste-astm.it-codigo NO-LOCK NO-ERROR.

	 display stream str-rp
             teste-astm.fm-codigo
             ITEM.it-codigo WHEN AVAIL ITEM
             ITEM.desc-item WHEN AVAIL ITEM
             teste-astm.ano-mes   
             teste-astm.num-amostra
             teste-astm.tipo-tear
             teste-astm.merceriz
             teste-astm.num-ob    
             teste-astm.enc-tra  
             teste-astm.enc-urd 
             teste-astm.dentra-ini
             teste-astm.dentra-fin
             teste-astm.observ 
      with frame f-teste-astm-fam.
	down stream str-rp with frame f-teste-astm-fam.
  END.
end.
else do:
    FOR each teste-astm no-lock
       where teste-astm.fm-codigo >= tt-param.fi-ini-fm-codigo
         AND teste-astm.fm-codigo <= tt-param.fi-fin-fm-codigo
         AND teste-astm.it-codigo >= tt-param.fi-ini-it-codigo
         AND teste-astm.it-codigo <= tt-param.fi-fin-it-codigo
         AND teste-astm.ano-mes   >= tt-param.fi-ini-ano-mes    
         AND teste-astm.ano-mes   <= tt-param.fi-fin-ano-mes    
    BY teste-astm.ano-mes       
    on stop undo,leave:
       run pi-acompanhar in h-acomp (input string(teste-astm.ano-mes)).
       
       FIND ITEM WHERE ITEM.it-codigo = teste-astm.it-codigo NO-LOCK NO-ERROR.

       display stream str-rp
               teste-astm.ano-mes
               teste-astm.fm-codigo
               ITEM.it-codigo WHEN AVAIL ITEM
               ITEM.desc-item WHEN AVAIL ITEM
               teste-astm.num-amostra
               teste-astm.tipo-tear
               teste-astm.merceriz
               teste-astm.num-ob    
               teste-astm.enc-tra  
               teste-astm.enc-urd 
               teste-astm.dentra-ini
               teste-astm.dentra-fin
               teste-astm.observ 
       with frame f-teste-astm-dat.
      down stream str-rp with frame f-teste-astm-dat.
    END.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i &STREAM="stream str-rp"}
run pi-finalizar in h-acomp.
return "OK":U.
