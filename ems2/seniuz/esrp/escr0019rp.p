/* Programa: ESCR0019.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Financeiro - Contas a Receber
** Objetivo: Listar o catalogo de Clientes.
** Autor...: Gilvando de Souza Araujo - Setembro/2008.
** Obs.....: Especifico da TEAR TEXTIL IND.COM.LTDA.
**
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESCR0019RP 2.04.00.000}

DEFINE TEMP-TABLE tt-param NO-UNDO
       FIELD destino          AS INTEGER
       FIELD arquivo          AS CHAR format "x(35)"
       FIELD usuario          AS CHAR format "x(12)"
       FIELD data-exec        AS DATE
       FIELD hora-exec        AS INTEGER
       FIELD classifica       AS INTEGER
       FIELD desc-classifica  AS CHAR FORMAT "x(40)"
       FIELD ini-cod-gr-cli   LIKE emitente.cod-gr-cli
       FIELD fin-cod-gr-cli   LIKE emitente.cod-gr-cli
       FIELD ini-cod-port     LIKE emitente.port-prefer
       FIELD fin-cod-port     LIKE emitente.port-prefer
       FIELD cred-normal      AS LOG FORMAT "Sim/Nao"
       FIELD cred-automatico  AS LOG FORMAT "Sim/Nao"
       FIELD cred-so-impl-ped AS LOG FORMAT "Sim/Nao"
       FIELD cred-suspenso    AS LOG FORMAT "Sim/Nao"
       FIELD cred-avista      AS LOG FORMAT "Sim/Nao"
       FIELD imp-param        AS LOG FORMAT "Sim/Nao".

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.
DEF VAR c-ind-cre-cli AS CHAR.

form
    "*-------------- Parƒmetros/Sele‡Æo --------------*" SKIP
    tt-param.ini-cod-gr-cli   label "Grupo de Cliente" AT 1
    "a"  AT 36                
    tt-param.fin-cod-gr-cli   no-labels
    tt-param.ini-cod-port     label "Portador Prefer." AT 1
    "a"  AT 36                
    tt-param.fin-cod-port     no-labels
    tt-param.desc-classifica  LABEL "Classifica‡Æo..." AT 1
    tt-param.cred-normal      LABEL "Cr‚dito Normal.." AT 1
    tt-param.cred-automatico  LABEL "Cr‚dito Automat." AT 1
    tt-param.cred-so-impl-ped LABEL "Cr‚dito S¢ Impl." AT 1
    tt-param.cred-suspenso    LABEL "Cr‚dito Suspenso" AT 1
    tt-param.cred-avista      LABEL "Cr‚dito · Vista." AT 1
    with no-box side-labels width 132 stream-io frame f-param.

form
    emitente.cod-emitente  label "Codigo"        
    emitente.nome-emit     label "RazÆo Social"
    c-ind-cre-cli          LABEL "Situa‡Æo Cr‚dito"
    with no-box NO-LABEL 55 down width 134 STREAM-IO frame f-detalhe.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i MATERIAIS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Cat logo_de_Emitentes * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
VIEW frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

FOR EACH emitente WHERE emitente.identific  <> 2
                    AND emitente.cod-gr-cli  >= tt-param.ini-cod-gr-cli
                    AND emitente.cod-gr-cli  <= tt-param.fin-cod-gr-cli
                    AND emitente.port-prefer >= tt-param.ini-cod-port
                    AND emitente.port-prefer <= tt-param.fin-cod-port
                    AND ((emitente.ind-cre-cli = 1 AND tt-param.cred-normal      = YES) OR
                         (emitente.ind-cre-cli = 2 AND tt-param.cred-automatico  = YES) OR
                         (emitente.ind-cre-cli = 3 AND tt-param.cred-so-impl-ped = YES) OR
                         (emitente.ind-cre-cli = 4 AND tt-param.cred-suspenso    = YES) OR
                         (emitente.ind-cre-cli = 5 AND tt-param.cred-avista      = YES))
                  NO-LOCK
    BY IF tt-param.classifica = 1 THEN STRING(emitente.cod-emitente,"999999999")
                                  ELSE emitente.nome-emit:
    
    RUN pi-acompanhar IN h-acomp (INPUT "Item: " + STRING(emitente.cod-emitente)).
    
    {esinc/i-dsrb.i emitente.ind-cre-cli emitente.ind-cre-cli c-ind-cre-cli} 

    DISPLAY emitente.cod-emitente
            emitente.nome-emit
            c-ind-cre-cli
           WITH FRAME f-detalhe.
    DOWN WITH FRAME f-detalhe.           
END.

IF tt-param.imp-param THEN DO:
   PAGE.
   DISPLAY tt-param.desc-classifica
           tt-param.ini-cod-gr-cli  
           tt-param.fin-cod-gr-cli  
           tt-param.ini-cod-port    
           tt-param.fin-cod-port    
           tt-param.cred-normal     
           tt-param.cred-automatico 
           tt-param.cred-so-impl-ped
           tt-param.cred-suspenso   
           tt-param.cred-avista     
           with frame f-param.
END.                                 
                                     
/* fechamento do output do relat¢rio */
{include/i-rpclo.i}                     
run pi-finalizar in h-acomp.            
return "OK":U.

