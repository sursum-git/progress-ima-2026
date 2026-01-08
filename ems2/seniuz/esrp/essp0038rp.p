/* Programa: ESSP0038RP.P (Chamado pelo programa ESSP0038.W)
** Objetivo: Imprimir a listagem do cadastro de Testes de Amostras
**           (tabela: teste-amos)
** Sistema.: Espec¡fico
** M¢dulo..: Espec¡fico
** Autor...: Gilvando Souza Araujo - Novembro/2003
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0038RP 2.04.00.000}
/* defini‡Æo das temp-tables para recebimento de parƒmetros */

define temp-table tt-param no-undo
       field destino           as integer
       field arquivo           as char format "x(35)"
       field usuario           as char format "x(12)"
       field data-exec         as date
       field hora-exec         as integer
       field classifica        as integer
       field desc-classifica   as char format "x(40)"
       FIELD fi-ini-fm-codigo  LIKE teste-amos.fm-codigo
       FIELD fi-fin-fm-codigo  LIKE teste-amos.fm-codigo
       FIELD fi-ini-data-teste LIKE teste-amos.data-teste
       FIELD fi-fin-data-teste LIKE teste-amos.data-teste.

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
   teste-amos.fm-codigo
   teste-amos.data-teste
   teste-amos.num-seq
   teste-amos.cod-maq 
   teste-amos.num-ob 
   teste-amos.larg-ent 
   teste-amos.larg-sai 
   teste-amos.enc-tra 
   teste-amos.enc-urd 
   teste-amos.observ 
   with frame f-teste-amos-fam WIDTH 132 down stream-io.

FORM
    teste-amos.data-teste
    teste-amos.num-seq
    teste-amos.fm-codigo
    teste-amos.cod-maq 
    teste-amos.num-ob 
    teste-amos.larg-ent 
    teste-amos.larg-sai 
    teste-amos.enc-tra 
    teste-amos.enc-urd 
    teste-amos.observ 
   with frame f-teste-amos-dat WIDTH 132 down stream-io.

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
{utp/ut-liter.i Testes_de_Amostras * r}
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
  FOR each teste-amos no-lock
     where teste-amos.fm-codigo  >= tt-param.fi-ini-fm-codigo
       AND teste-amos.fm-codigo  <= tt-param.fi-fin-fm-codigo
       AND teste-amos.data-teste >= tt-param.fi-ini-data-teste
       AND teste-amos.data-teste <= tt-param.fi-fin-data-teste
     on stop undo,leave:
     run pi-acompanhar in h-acomp (input string(teste-amos.fm-codigo)).

	 display stream str-rp
             teste-amos.fm-codigo
             teste-amos.data-teste
             teste-amos.num-seq
             teste-amos.cod-maq 
             teste-amos.num-ob 
             teste-amos.larg-ent 
             teste-amos.larg-sai 
             teste-amos.enc-tra 
             teste-amos.enc-urd 
             teste-amos.observ 
      with frame f-teste-amos-fam.
	down stream str-rp with frame f-teste-amos-fam.
  END.
end.
else do:
    FOR each teste-amos no-lock
       where teste-amos.fm-codigo  >= tt-param.fi-ini-fm-codigo
         AND teste-amos.fm-codigo  <= tt-param.fi-fin-fm-codigo
         AND teste-amos.data-teste >= tt-param.fi-ini-data-teste
         AND teste-amos.data-teste <= tt-param.fi-fin-data-teste
    BY teste-amos.data-teste
    on stop undo,leave:
       run pi-acompanhar in h-acomp (input string(teste-amos.data-teste)).
       display stream str-rp
               teste-amos.data-teste
               teste-amos.num-seq
               teste-amos.fm-codigo
               teste-amos.cod-maq 
               teste-amos.num-ob 
               teste-amos.larg-ent 
               teste-amos.larg-sai 
               teste-amos.enc-tra 
               teste-amos.enc-urd 
               teste-amos.observ 
       with frame f-teste-amos-dat.
      down stream str-rp with frame f-teste-amos-dat.
    END.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i &STREAM="stream str-rp"}
run pi-finalizar in h-acomp.
return "OK":U.
