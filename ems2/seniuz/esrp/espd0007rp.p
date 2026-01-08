/*****************************************************************************
* Programa: ESPD022                                                          *
* Autor...: Alexandre G E Guedes/Gilvando Souza Araujo                       *
* Funcao..: Emitir o relat¢rio de Aviso de Programa‡Æo do Beneficiamento.    *
* Data....: 26/09/95                                                         *
**
** Conversao para EMS 2.04:
**   Programa: ESPD022.P  =>  ESPD0007RP.P
**   Autor...: Prodb - Toninho
**   Data....: 08/01/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESPD0007RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino     as integer
       field arquivo     as char format "x(35)"
       field usuario     as char format "x(12)"
       field data-exec   as date
       field hora-exec   as integer
       field da-ini      LIKE ref-item-ext.dt-ult-prog
       field da-fim      LIKE ref-item-ext.dt-ult-prog
       field c-item-ini  LIKE ref-item-ext.it-codigo
       field c-item-fim  LIKE ref-item-ext.it-codigo
       FIELD c-ref-ini   LIKE ref-item-ext.cod-refer
       FIELD c-ref-fim   LIKE ref-item-ext.cod-refer
       field l-salta     AS LOG FORMAT "Sim/NÆo"
       field c-observ    AS CHAR 
       FIELD impr-param  AS LOGICAL.

define temp-table tt-raw-digita
       field raw-digita as raw.

DEFINE TEMP-TABLE tt-acabamento
       FIELD codigo LIKE acab-tecido.codigo
       INDEX ch-acab codigo.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

def var i-tot-prog-d  as int init 0.
def var i-tot-prog-f  as int init 0.
def var i-tot-prog-g  as int init 0.
def var c-item        as char format "x(9)"  label "Item".
DEF VAR c-refer       AS CHAR FORMAT "x(13)"  LABEL "Acb Refer".
def var c-descricao   as char format "x(26)" label "Descricao".
def var c-fundo       as char format "x(4)"  label "Fundo".
def var c-qg1         as char format "x(20)"
  init "____________________" label "     Prioridade     ".
def var c-qg2         as char format "x(20)"
  init "____________________" label "      Executado     ".
def var c-qg3         as char format "x(20)"
  init "____________________" label "    Numero de OB    ".

form
   c-item
   c-descricao
   c-fundo
   c-refer
   ref-item-ext.qtd-prog
   c-qg1
   c-qg2
   c-qg3
   with no-box 55 down width 132 STREAM-IO frame f-detalhe.

form
    tt-param.da-ini      label "Data Programacao"            AT 11
    "a"                                                      AT 46
    tt-param.da-fim      NO-LABELS
    tt-param.c-item-ini  label "Item"                        AT 23
    "a"                                                      AT 46      
    tt-param.c-item-fim  NO-LABELS
    tt-param.c-ref-ini   LABEL "Referencia"                  AT 23
    "a" 
    tt-param.c-ref-fim   NO-LABELS
    tt-param.l-salta     LABEL  "Salta pagina por Desenho ?" AT  1
    with no-box SIDE-LABEL width 132 STREAM-IO frame f-parlis.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* include padrÆo para impressÆo de campos editores no relat¢rio  */
{include/pi-edit.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i PEDIDOS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Aviso_Programa‡Æo_do_Beneficiamento * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each ref-item-ext where 
         ref-item-ext.dt-ult-prog >= tt-param.da-ini AND
         ref-item-ext.dt-ult-prog <= tt-param.da-fim AND
         ref-item-ext.it-codigo   >= tt-param.c-item-ini AND
         ref-item-ext.it-codigo   <= tt-param.c-item-fim AND
         ref-item-ext.cod-refer   >= tt-param.c-ref-ini AND 
         ref-item-ext.cod-refer   <= tt-param.c-ref-fim AND 
         ref-item-ext.qtd-prog > 0 no-lock
         break by ref-item-ext.it-codigo
               by substr(ref-item-ext.cod-refer,3,4).
                         
     run pi-acompanhar in h-acomp (INPUT ref-item-ext.dt-ult-prog).
     
     find item where
          item.it-codigo = ref-item-ext.it-codigo no-lock no-error.
          
     if not avail item then
        assign c-descricao = "ITEM SEM CADASTRO"
               c-fundo     = " ".
     else
        assign c-item      = item.it-codigo
               c-descricao = item.descricao-1 + item.descricao-2.

     find referencia where
          referencia.cod-refer = ref-item-ext.cod-refer no-lock no-error.
          
     IF AVAIL referencia THEN DO:
        FIND referencia-ext WHERE
             referencia-ext.cod-refer = referencia.cod-refer NO-LOCK NO-ERROR.

        assign c-fundo = if avail referencia-ext
                         then referencia-ext.cod-fundo
                         else " ".
        
        assign c-refer = substr(referencia.descricao,1,3) + " " +
                         substr(ref-item-ext.cod-refer,1,2) + " " +
                         substr(ref-item-ext.cod-refer,3,4) + "-" +
                         substr(ref-item-ext.cod-refer,7,1).
        FIND tt-acabamento WHERE tt-acabamento.codigo = INT(SUBSTR(ref-item-ext.cod-refer,1,2))
                           NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-acabamento THEN DO:
           CREATE tt-acabamento.
           ASSIGN tt-acabamento.codigo = INT(SUBSTR(ref-item-ext.cod-refer,1,2)).
        END.
     END.
     ELSE
        assign c-refer = substr(ref-item-ext.cod-refer,1,2) + " " +
                         substr(ref-item-ext.cod-refer,3,4) + "-" +
                         substr(ref-item-ext.cod-refer,7,1).
               
     if first-of(ref-item-ext.it-codigo) then 
        disp c-item
             c-descricao
             with frame f-detalhe.

     display c-refer
             c-fundo
             ref-item-ext.qtd-prog
             c-qg1
             c-qg2
             c-qg3 skip(1)
             with frame f-detalhe.
     down with frame f-detalhe.

     assign i-tot-prog-d = i-tot-prog-d + ref-item-ext.qtd-prog
            i-tot-prog-f = i-tot-prog-f + ref-item-ext.qtd-prog
            i-tot-prog-g = i-tot-prog-g + ref-item-ext.qtd-prog.
     
     if last-of(substr(ref-item-ext.cod-refer,3,4)) then do.
        disp "                  Total do" @ c-descricao
             "Desenho"                    @ c-refer
             i-tot-prog-d                 @ ref-item-ext.qtd-prog
             with frame f-detalhe.
        down with frame f-detalhe.
        assign i-tot-prog-d  = 0.
        if l-salta then page.
     end.
               
     if last-of(ref-item-ext.it-codigo) then do.
        disp "Total do Item" @ c-descricao
             i-tot-prog-f    @ ref-item-ext.qtd-prog
             with frame f-detalhe.
        down 1 with frame f-detalhe.
        assign i-tot-prog-f  = 0.
        if l-salta then page.
     end.
end.
disp "Total da Listagem" @ c-descricao
     i-tot-prog-g        @ ref-item-ext.qtd-prog
     with frame f-detalhe.
down 3 with frame f-detalhe.

RUN pi-print-editor (tt-param.c-observ, 132).
PUT SKIP(1) "Observa‡äes:" SKIP.
FOR EACH tt-editor:
   PUT tt-editor.conteudo SKIP.
END.

FIND FIRST tt-acabamento.
IF AVAIL tt-acabamento THEN DO:
   PUT skip(1)
       "Acabamentos:"
       SKIP.
   FOR EACH tt-acabamento:
       FIND acab-tecido WHERE acab-tecido.codigo = tt-acabamento.codigo NO-LOCK NO-ERROR.
       IF AVAIL acab-tecido THEN
          PUT acab-tecido.codigo " "
              acab-tecido.sigla  " "
              acab-tecido.descricao
              SKIP.
   END.
END.

IF tt-param.impr-param THEN DO:
   PAGE.
   PUT "*****----- PAR¶METROS ------*****"
       SKIP(1).
    
   display tt-param.da-ini
           tt-param.da-fim
           tt-param.c-item-ini
           tt-param.c-item-fim
           tt-param.c-ref-ini
           tt-param.c-ref-fim
           tt-param.l-salta
           with frame f-parlis.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

