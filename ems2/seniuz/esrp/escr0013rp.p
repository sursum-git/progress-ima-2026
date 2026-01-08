/* Programa: ESCR021.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Contas a Receber
** Objetivo: Listar Titulos a Receber por CEP de Cobran‡a.
** Autor...: Gilvando de Souza Araujo - Setembro/2000
** Obs.....: Especifico da TEAR TEXTIL INDUSTRIA E COMERCIO LTDA.
**
** Conversao para EMS 2.04:
**   Programa: ESCR021.P  =>  ESCR0013RP.P
**   Autor...: ProDB - Toninho
**   Data....: 12/02/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESCR0013RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       FIELD ep-codigo        LIKE titulo.ep-codigo
       FIELD cod-estabel-ini  LIKE titulo.cod-estabel
       FIELD cod-estabel-fim  LIKE titulo.cod-estabel
       FIELD cod-esp-ini      LIKE titulo.cod-esp
       FIELD cod-esp-fim      LIKE titulo.cod-esp 
       FIELD cod-port-ini     LIKE titulo.cod-port
       FIELD cod-port-fim     LIKE titulo.cod-port
       FIELD dt-emissao-ini   LIKE titulo.dt-emissao
       FIELD dt-emissao-fim   LIKE titulo.dt-emissao
       FIELD dt-vencimen-ini  LIKE titulo.dt-vencimen
       FIELD dt-vencimen-fim  LIKE titulo.dt-vencimen
       FIELD impr-param       AS LOGICAL.

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

def TEMP-TABLE w-work
    field cep          like emitente.cep-cob
    field cidade       like emitente.cidade-cob
    field estado       like emitente.estado
    field cod-emitente like titulo.cod-emitente
    field nr-docto     like titulo.nr-docto
    field parcela      like titulo.parcela
    field cod-port     like titulo.cod-port
    field vl-saldo     like titulo.vl-saldo
    field dt-emissao   like titulo.dt-emissao
    field dt-vencimen  like titulo.dt-vencimen.

def buffer b-emitente for emitente.
def var c-cep    like emitente.cep.
def var c-cidade like emitente.cidade.
def var c-estado like emitente.estado.
def var i-cont-tit as int format ">>>>9".
def var de-tot-val like titulo.vl-saldo.

form
    tt-param.ep-codigo       LABEL "Empresa"            AT 12
    "-" 
    c-empresa                NO-LABELS
    tt-param.cod-estabel-ini LABEL "Estabelecimento de" AT 1
    "a"                                                 AT 33
    tt-param.cod-estabel-fim NO-LABELS
    tt-param.cod-esp-ini     LABEL "Esp‚cie de"         AT 09
    "a"                                                 AT 33
    tt-param.cod-esp-fim     NO-LABELS 
    tt-param.cod-port-ini    LABEL "Portador de"        AT 08
    "a"                                                 AT 33
    tt-param.cod-port-fim    NO-LABELS 
    tt-param.dt-emissao-ini  LABEL "EmissÆo"            AT 12
    "a"                                                 AT 33
    tt-param.dt-emissao-fim  NO-LABELS 
    tt-param.dt-vencimen-ini LABEL "Vencimento"         AT 09
    "a"                                                 AT 33
    tt-param.dt-vencimen-fim NO-LABELS 
    with no-box side-labels width 132 stream-io frame f-parlis.

form
    w-work.cep             label "CEP"
    w-work.cidade          label "CIDADE"
    w-work.estado          label "UF"
    w-work.cod-emitente    label "CLIENTE"
    w-work.nr-docto        label "TITULO"
    w-work.parcela         label "PA"
    w-work.cod-port        label "PORT"
    w-work.vl-saldo        label "VALOR"
    w-work.dt-emissao      label "EMISSAO"
    w-work.dt-vencimen     label "VENCTO."
    with no-box 55 down width 132 no-labels STREAM-IO frame f-detalhe.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i FINANCEIRO * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i T¡tulos_a_Receber_por_CEP_de_Cobranca * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each w-work.
    delete w-work.
end.

FOR each titulo where
         titulo.ep-codigo   =  tt-param.ep-codigo       and
         titulo.cod-estabel >= tt-param.cod-estabel-ini and
         titulo.cod-estabel <= tt-param.cod-estabel-fim and
         titulo.cod-esp     >= tt-param.cod-esp-ini     and
         titulo.cod-esp     <= tt-param.cod-esp-fim     and
         titulo.cod-port    >= tt-param.cod-port-ini    and
         titulo.cod-port    <= tt-param.cod-port-fim    and
         titulo.dt-emissao  >= tt-param.dt-emissao-ini  and
         titulo.dt-emissao  <= tt-param.dt-emissao-fim  and
         titulo.dt-vencimen >= tt-param.dt-vencimen-ini and
         titulo.dt-vencimen <= tt-param.dt-vencimen-fim no-lock.

    run pi-acompanhar in h-acomp (input titulo.nr-docto).

    find emitente where
         emitente.cod-emitente = titulo.cod-emitente no-lock no-error.

    if emitente.cod-emit <> emitente.end-cobranca then do:
       find b-emitente WHERE
            b-emitente.cod-emitente = emitente.end-cobranca no-lock no-error.
       assign c-cep    = b-emitente.cep
              c-cidade = b-emitente.cidade
              c-estado = b-emitente.estado.
    end.
    else do:
       if emitente.endereco-cob = " " then do:
          assign c-cep    = emitente.cep
                 c-cidade = emitente.cidade
                 c-estado = emitente.estado.
       end.
       else do:
          assign c-cep    = emitente.cep-cob
                 c-cidade = emitente.cidade-cob
                 c-estado = emitente.estado-cob.
       end.
    end.

    create w-work.
    assign w-work.cep          = c-cep
           w-work.cidade       = c-cidade
           w-work.estado       = c-estado
           w-work.cod-emitente = emitente.cod-emitente
           w-work.nr-docto     = titulo.nr-docto
           w-work.parcela      = titulo.parcela
           w-work.cod-port     = titulo.cod-port
           w-work.vl-saldo     = titulo.vl-saldo
           w-work.dt-emissao   = titulo.dt-emissao
           w-work.dt-vencimen  = titulo.dt-vencimen.
END.

for each w-work by w-work.cep
                by w-work.cidade
                by w-work.estado
                by w-work.cod-emitente
                by w-work.nr-docto
                by w-work.parcela:

    display w-work.cep
            w-work.cidade
            w-work.estado
            w-work.cod-emitente
            w-work.nr-docto
            w-work.parcela
            w-work.cod-port
            w-work.vl-saldo
            w-work.dt-emissao
            w-work.dt-vencimen
            with frame f-detalhe.

    down with frame f-detalhe.
    assign de-tot-val = de-tot-val + w-work.vl-saldo
           i-cont-tit = i-cont-tit + 1.
end.

display "Total"    @ w-work.cep
        i-cont-tit @ w-work.nr-docto
        de-tot-val @ w-work.vl-saldo
        with frame f-detalhe.

assign i-cont-tit = 0 de-tot-val = 0.

IF tt-param.impr-param THEN DO.
   PAGE.

   find first empresa
        where empresa.ep-codigo = tt-param.ep-codigo no-lock no-error. 

   assign c-empresa = (if avail empresa then empresa.razao-social else "").

   PUT "*****----- PAR¶METROS ------*****"
       SKIP(1).
    
   display tt-param.ep-codigo
           c-empresa           
           tt-param.cod-estabel-ini
           tt-param.cod-estabel-fim
           tt-param.cod-esp-ini    
           tt-param.cod-esp-fim    
           tt-param.cod-port-ini   
           tt-param.cod-port-fim   
           tt-param.dt-emissao-ini 
           tt-param.dt-emissao-fim 
           tt-param.dt-vencimen-ini
           tt-param.dt-vencimen-fim
           with frame f-parlis.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

