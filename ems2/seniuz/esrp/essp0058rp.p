/* Programa: ESSP0058RP.P (Chamado pelo programa ESSP0058.W)
** Objetivo: Imprimir a listagem do cadastro de Testes de Carga de Goma 
**           (tabela: teste-cgoma)
** Sistema.: Espec¡ficos
** M¢dulo..: Espec¡ficos
** Autor...: Fábio Coelho Lanza - Novembro/2003
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0058RP 2.04.00.000}
/* defini‡Æo das temp-tables para recebimento de parƒmetros */

define temp-table tt-param no-undo
       field destino           as integer
       field arquivo           as char format "x(35)"
       field usuario           as char format "x(12)"
       field data-exec         as date
       field hora-exec         as integer
       field classifica        as integer
       field desc-classifica   as char format "x(40)"
       FIELD fi-ini-fm-codigo  LIKE teste-cgoma.fm-codigo
       FIELD fi-fin-fm-codigo  LIKE teste-cgoma.fm-codigo
       FIELD fi-ini-data-teste LIKE teste-cgoma.data-teste
       FIELD fi-fin-data-teste LIKE teste-cgoma.data-teste.

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
   teste-cgoma.fm-codigo    LABEL "Familia"    
   teste-cgoma.data-teste   LABEL "Data Teste" 
   teste-cgoma.num-seq      LABEL "NS"         
   teste-cgoma.cod-maq      LABEL "Maquina"    
   teste-cgoma.tipo-goma    LABEL "Tipo Goma"  
   teste-cgoma.umid-esqrd   LABEL "UmEsq"      
   teste-cgoma.umid-centro  LABEL "UmCen"      
   teste-cgoma.umid-direita LABEL "UmDir"      
   teste-cgoma.cgom-esqrd   LABEL "CrgEsq"     
   teste-cgoma.cgom-centro  LABEL "CrgCen"     
   teste-cgoma.cgom-direita LABEL "CrgDir"     
   teste-cgoma.observ       LABEL "Observa‡Æo" 
   with frame f-teste-cgoma-fam NO-LABEL WIDTH 132 down stream-io.


FORM   
    teste-cgoma.fm-codigo    LABEL "Familia"    
    teste-cgoma.data-teste   LABEL "Data Teste" 
    teste-cgoma.num-seq      LABEL "NS"         
    teste-cgoma.cod-maq      LABEL "Maquina"    
    teste-cgoma.tipo-goma    LABEL "Tipo Goma"  
    teste-cgoma.umid-esqrd   LABEL "UmEsq"      
    teste-cgoma.umid-centro  LABEL "UmCen"      
    teste-cgoma.umid-direita LABEL "UmDir"      
    teste-cgoma.cgom-esqrd   LABEL "CrgEsq"     
    teste-cgoma.cgom-centro  LABEL "CrgCen"     
    teste-cgoma.cgom-direita LABEL "CrgDir"     
    teste-cgoma.observ       LABEL "Observa‡Æo" 
    with frame f-teste-cgoma-dat NO-LABEL WIDTH 132 down stream-io.

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
{utp/ut-liter.i Cadastro_de_Testes_da_Carga_de_Goma * r}
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
  FOR each teste-cgoma no-lock
     where teste-cgoma.fm-codigo  >= tt-param.fi-ini-fm-codigo
       AND teste-cgoma.fm-codigo  <= tt-param.fi-fin-fm-codigo
       AND teste-cgoma.data-teste >= tt-param.fi-ini-data-teste
       AND teste-cgoma.data-teste <= tt-param.fi-fin-data-teste
     on stop undo,leave:
     run pi-acompanhar in h-acomp (input string(teste-cgoma.fm-codigo)).

	 display stream str-rp
         teste-cgoma.fm-codigo
         teste-cgoma.data-teste
         teste-cgoma.num-seq
         teste-cgoma.cod-maq
         teste-cgoma.tipo-goma
         teste-cgoma.umid-esqrd
         teste-cgoma.umid-centro
         teste-cgoma.umid-direita
         teste-cgoma.cgom-esqrd
         teste-cgoma.cgom-centro
         teste-cgoma.cgom-direita
         teste-cgoma.observ 
         with frame f-teste-cgoma-fam.
	down stream str-rp with frame f-teste-cgoma-fam.
  END.
end.
else do:
    FOR each teste-cgoma no-lock
       where teste-cgoma.fm-codigo  >= tt-param.fi-ini-fm-codigo
         AND teste-cgoma.fm-codigo  <= tt-param.fi-fin-fm-codigo
         AND teste-cgoma.data-teste >= tt-param.fi-ini-data-teste
         AND teste-cgoma.data-teste <= tt-param.fi-fin-data-teste
       BY teste-cgoma.data-teste
       BY teste-cgoma.fm-codigo
       on stop undo,leave:
       run pi-acompanhar in h-acomp (input string(teste-cgoma.fm-codigo)).
       display stream str-rp
           teste-cgoma.fm-codigo
           teste-cgoma.data-teste
           teste-cgoma.num-seq
           teste-cgoma.cod-maq
           teste-cgoma.tipo-goma
           teste-cgoma.umid-esqrd
           teste-cgoma.umid-centro
           teste-cgoma.umid-direita
           teste-cgoma.cgom-esqrd
           teste-cgoma.cgom-centro
           teste-cgoma.cgom-direita
           teste-cgoma.observ 
           with frame f-teste-cgoma-dat.
      down stream str-rp with frame f-teste-cgoma-dat.
    END.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i &STREAM="stream str-rp"}
run pi-finalizar in h-acomp.
return "OK":U.
