/* Programa: ESCD001.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Cadastro         
** Objetivo: Listar Cadastro de Clientes
** Autor...: Fabio Coelho Lanza - Dezembro/1998
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Programa: ESCD001.P  =>  ESFT0014RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 21/01/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESFT0014RP 2.04.00.000}

define temp-table tt-param  no-undo
       field destino           as integer
       field arquivo           as char format "x(35)"
       field usuario           as char format "x(12)"
       field data-exec         as date
       field hora-exec         as integer
       field classifica        as integer
       field desc-classifica   as char format "x(40)"
       FIELD ini-cod-rep       LIKE emitente.cod-rep
       FIELD fin-cod-rep       LIKE emitente.cod-rep
       FIELD ini-grupo-cli     LIKE emitente.cod-gr-cli
       FIELD fin-grupo-cli     LIKE emitente.cod-gr-cli
       FIELD ini-nome-cli      LIKE emitente.nome-emit
       FIELD fin-nome-cli      LIKE emitente.nome-emit
       FIELD ini-estado        LIKE emitente.estado
       FIELD fin-estado        LIKE emitente.estado
       FIELD ini-cidade        LIKE emitente.cidade
       FIELD fin-cidade        LIKE emitente.cidade
       FIELD ini-cep           LIKE emitente.cep
       FIELD fin-cep           LIKE emitente.cep
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

form 
    "*---------------------------------- Parƒmetros/Sele‡Æo -----------------------------------*" SKIP
    tt-param.ini-cod-rep      label "Representante."
    "A"  AT 60
    tt-param.fin-cod-rep      no-labels SKIP
    tt-param.ini-grupo-cli    label "Grupo Cliente."
    "A"  AT 60
    tt-param.fin-grupo-cli    NO-LABELS SKIP
    tt-param.ini-nome-cli     label "Nome Cliente.." 
    "A"  AT 60
    tt-param.fin-nome-cli     no-labels SKIP
    tt-param.ini-estado       label "Estado........"
    "A"  AT 60
    tt-param.fin-estado       no-labels SKIP
    tt-param.ini-cidade       LABEL "Cidade........"
    "A"  AT 60
    tt-param.fin-cidade       no-labels SKIP        
    tt-param.ini-cep          LABEL "Cep..........."
    "A"  AT 60                                      
    tt-param.fin-cep          NO-LABELS
    skip(1)
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    emitente.cod-rep      label "Repr"
    emitente.cod-emitente label "Cod."     format ">>>>9"
    emitente.nome-emit    label "Nome"
    emitente.endereco     label "Endereco"
    emitente.cidade       label "Cidade"   format "x(20)"
    emitente.estado       label "UF"
    emitente.telefone[1]  label "Telefone" format "x(12)"
    with no-labels no-box 55 down width 132 STREAM-IO frame f-detalhe.

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
{utp/ut-liter.i Cadastro_de_Clientes * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
VIEW FRAME f-cabecalho.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each emitente where emitente.cod-gr-cli >= tt-param.ini-grupo-cli
                    and emitente.cod-gr-cli <= tt-param.fin-grupo-cli
                    and emitente.cod-rep    >= tt-param.ini-cod-rep
                    and emitente.cod-rep    <= tt-param.fin-cod-rep
                    and emitente.nome-emit  >= tt-param.ini-nome-cli
                    and emitente.nome-emit  <= tt-param.fin-nome-cli
                    and emitente.estado     >= tt-param.ini-estado
                    and emitente.estado     <= tt-param.fin-estado
                    and emitente.cidade     >= tt-param.ini-cidade
                    and emitente.cidade     <= tt-param.fin-cidade
                    AND emitente.cep        >= tt-param.ini-cep
                    AND emitente.cep        <= tt-param.fin-cep
                  no-lock
                  by emitente.nome-emit:
   
   run pi-acompanhar in h-acomp (input string(emitente.cod-emitente)).
   
   display emitente.cod-emitente
           emitente.nome-emit
           emitente.endereco
           emitente.cidade
           emitente.estado
           emitente.telefone[1]
           emitente.cod-rep
           with frame f-detalhe.
   down with frame f-detalhe.
    
end.

IF tt-param.imp-param THEN DO:
   PAGE.
   display tt-param.ini-cod-rep
           tt-param.fin-cod-rep
           tt-param.ini-grupo-cli
           tt-param.fin-grupo-cli
           tt-param.ini-nome-cli
           tt-param.fin-nome-cli
           tt-param.ini-estado
           tt-param.fin-estado
           tt-param.ini-cidade
           tt-param.fin-cidade
           tt-param.ini-cep
           tt-param.fin-cep
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

