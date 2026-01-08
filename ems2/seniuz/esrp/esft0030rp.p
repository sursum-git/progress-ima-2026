/* Programa: ESCD008
** Sistema.: Magnus da Datasul
** Modulo..: Cadastro
** Objetivo: Relatorio de Cadastro de Clientes Ativos por Representante
** Autor...: Gilvando de Souza Araujo
** Data....: Novembro/1997
** Obs.....: Programa especifico de SANTA ELISABETH/RENASCENCA
**
** Conversao para EMS 2.04:
**   Programa: ESCD008.P  =>  ESFT0030RP.P
**   Autor...: FµBIO COELHO LANZA
**   Data....: 09/02/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESFT0030RP 2.04.00.000}

define temp-table tt-param  no-undo
       field destino           as integer
       field arquivo           as char format "x(35)"
       field usuario           as char format "x(12)"
       field data-exec         as date
       field hora-exec         as integer
       field classifica        as integer
       field desc-classifica   as char format "x(40)"
       FIELD ini-dt-emis-nota  LIKE nota-fiscal.dt-emis-nota
       FIELD fin-dt-emis-nota  LIKE nota-fiscal.dt-emis-nota
       FIELD ini-cod-rep       LIKE nota-fiscal.cod-rep
       FIELD fin-cod-rep       LIKE nota-fiscal.cod-rep
       FIELD imp-las           AS LOG FORMAT "Sim/Nao"
       FIELD imp-param         AS LOG.


define temp-table tt-raw-digita
       field raw-digita as raw.

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

def var i-lim-credito as int format ">>>>>>>>>9".
def var c-cidade      as char format "x(22)" label "Cidade".
def var c-nome-emit   as char format "x(35)".
def var c-endereco    as char format "x(35)".
def var l-primvez     as log init yes.
def var i-result      as int.
def var i-pgnumb      as int.

form 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.ini-cod-rep      label "Representante..."
    "A"  AT 30
    tt-param.fin-cod-rep      NO-LABELS SKIP
    tt-param.ini-dt-emis-nota label "Data Emissao...."
    "A"  AT 30
    tt-param.fin-dt-emis-nota no-labels SKIP
    tt-param.imp-las          LABEL "Pula Folha Impar"
    skip(1)
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    "Representante: "
    repres.cod-rep format "99999"
    repres.nome
    c-cidade
    repres.estado
    repres.telefone[1]
    tt-param.ini-dt-emis-nota "a" tt-param.fin-dt-emis-nota
    skip(1)
    with no-box no-labels 55 down width 132 STREAM-IO FRAME f-repres.

form
    emitente.cod-emitente           column-label "Cod." format ">>>>9"
    c-nome-emit                     column-label "Nome"
    c-endereco                      column-label "Endereco"
    c-cidade
    emitente.estado
    emitente.telefone[1]            column-label "Telefone"
    i-lim-credito                   column-label "Lim.Cred."
    with no-box 55 down width 132 STREAM-IO frame f-detalhe.

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
{utp/ut-liter.i Cadastro_de_Clientes_Ativos_por_Representante * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each nota-fiscal where nota-fiscal.dt-emis-nota >= tt-param.ini-dt-emis-nota
                       and nota-fiscal.dt-emis-nota <= tt-param.fin-dt-emis-nota
                       and nota-fiscal.cod-rep      >= tt-param.ini-cod-rep
                       and nota-fiscal.cod-rep      <= tt-param.fin-cod-rep
                           no-lock,
    each emitente where emitente.cod-emitente = nota-fiscal.cod-emitente no-lock
                  break by nota-fiscal.cod-rep
                        by emitente.nome-emit:
      
    run pi-acompanhar in h-acomp (input nota-fiscal.cod-rep).

    if first-of (nota-fiscal.cod-rep) then do:
       find repres where repres.cod-rep = nota-fiscal.cod-rep no-lock no-error.
       assign c-cidade = substr(repres.cidade,1,22).
       if tt-param.imp-las = yes and l-primvez = no then do:
          page.
          assign i-result = page-number / 2.
          assign i-pgnumb = i-result * 2.
          if page-number = i-pgnumb then do:
             display " ".
             page.
          end.
       end.
       if l-primvez = yes then
          assign l-primvez = no.
       display repres.cod-rep
               repres.nome
               c-cidade
               repres.estado
               repres.telefone[1]
               tt-param.ini-dt-emis-nota
               tt-param.fin-dt-emis-nota
               with frame f-repres.
      down with frame f-repres.
   end.
   if first-of(emitente.nome-emit) then do:
      assign c-nome-emit   = substr(emitente.nome-emit,1,35)
             c-endereco    = substr(emitente.endereco,1,35)
             c-cidade      = substr(emitente.cidade,1,22)
             i-lim-credito = emitente.lim-credito.
      display emitente.cod-emitente
              c-nome-emit
              c-endereco
              c-cidade
              emitente.estado
              emitente.telefone[1]
              i-lim-credito
              with frame f-detalhe.
      down with frame f-detalhe.
   end.
end.

IF tt-param.imp-param THEN DO:
   PAGE.
   display tt-param.ini-dt-emis-nota
           tt-param.fin-dt-emis-nota 
           tt-param.ini-cod-rep     
           tt-param.fin-cod-rep     
           tt-param.imp-las
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

