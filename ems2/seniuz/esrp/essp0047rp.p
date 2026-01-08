/* Programa: ESSP0047RP.P (Chamado pelo programa ESSP0047.W)
** Objetivo: Imprimir a listagem do cadastro de Testes de Hidrofilidade 
**           (tabela: teste-hidr)
** Sistema.: Espec¡ficos
** M¢dulo..: Espec¡ficos
** Autor...: Gilvando Souza Araujo - Novembro/2003
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0047RP 2.04.00.000}
/* defini‡Æo das temp-tables para recebimento de parƒmetros */

define temp-table tt-param no-undo
       field destino           as integer
       field arquivo           as char format "x(35)"
       field usuario           as char format "x(12)"
       field data-exec         as date
       field hora-exec         as integer
       field classifica        as integer
       field desc-classifica   as char format "x(40)"
       FIELD fi-ini-fm-codigo  LIKE teste-hidr.fm-codigo
       FIELD fi-fin-fm-codigo  LIKE teste-hidr.fm-codigo
       FIELD fi-ini-data-teste LIKE teste-hidr.data-teste
       FIELD fi-fin-data-teste LIKE teste-hidr.data-teste.

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
   tt-param.fi-ini-data-teste  label "Data......"
   "A"  AT 30                  
   tt-param.fi-fin-data-teste  no-labels             SKIP
   skip(1)
   with FRAME f-param side-labels no-box width 132 STREAM-IO.

FORM   
   teste-hidr.fm-codigo
   teste-hidr.fundo
   teste-hidr.data-teste
   teste-hidr.cod-maq
   teste-hidr.num-seq
   teste-hidr.nome-proc
   teste-hidr.num-ob
   teste-hidr.res-esqrd
   teste-hidr.res-centro
   teste-hidr.res-direita
   teste-hidr.observ 
   with frame f-teste-hidr-fam WIDTH 132 down stream-io.

FORM
   teste-hidr.data-teste
   teste-hidr.fm-codigo
   teste-hidr.fundo
   teste-hidr.cod-maq
   teste-hidr.num-seq
   teste-hidr.nome-proc
   teste-hidr.num-ob
   teste-hidr.res-esqrd
   teste-hidr.res-centro
   teste-hidr.res-direita
   teste-hidr.observ 
   with frame f-teste-hidr-dat WIDTH 132 down stream-io.

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
{utp/ut-liter.i Cadastro_de_Testes_de_Hidrofilidade * r}
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
        tt-param.fi-ini-data-teste                
        tt-param.fi-fin-data-teste                
        with frame f-param.           

if tt-param.classific = 1 then do:
  FOR each teste-hidr no-lock
     where teste-hidr.fm-codigo  >= tt-param.fi-ini-fm-codigo
       AND teste-hidr.fm-codigo  <= tt-param.fi-fin-fm-codigo
       AND teste-hidr.data-teste >= tt-param.fi-ini-data-teste
       AND teste-hidr.data-teste <= tt-param.fi-fin-data-teste
     on stop undo,leave:
     run pi-acompanhar in h-acomp (input string(teste-hidr.fm-codigo)).

	 display stream str-rp
             teste-hidr.fm-codigo
             teste-hidr.fundo
             teste-hidr.data-teste
             teste-hidr.cod-maq
             teste-hidr.num-seq
             teste-hidr.nome-proc
             teste-hidr.num-ob
             teste-hidr.res-esqrd
             teste-hidr.res-centro
             teste-hidr.res-direita
             teste-hidr.observ 
      with frame f-teste-hidr-fam.
	down stream str-rp with frame f-teste-hidr-fam.
  END.
end.
else do:
    FOR each teste-hidr no-lock
       where teste-hidr.fm-codigo  >= tt-param.fi-ini-fm-codigo
         AND teste-hidr.fm-codigo  <= tt-param.fi-fin-fm-codigo
         AND teste-hidr.data-teste >= tt-param.fi-ini-data-teste
         AND teste-hidr.data-teste <= tt-param.fi-fin-data-teste
    BY teste-hidr.data-teste
    BY teste-hidr.fm-codigo
    BY teste-hidr.fundo
    BY teste-hidr.cod-maq
    on stop undo,leave:
       run pi-acompanhar in h-acomp (input string(teste-hidr.fm-codigo)).
       display stream str-rp
               teste-hidr.data-teste
               teste-hidr.fm-codigo
               teste-hidr.fundo
               teste-hidr.cod-maq
               teste-hidr.num-seq
               teste-hidr.nome-proc
               teste-hidr.num-ob
               teste-hidr.res-esqrd
               teste-hidr.res-centro
               teste-hidr.res-direita
               teste-hidr.observ 
       with frame f-teste-hidr-dat.
      down stream str-rp with frame f-teste-hidr-dat.
    END.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i &STREAM="stream str-rp"}
run pi-finalizar in h-acomp.
return "OK":U.
