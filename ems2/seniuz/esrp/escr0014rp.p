/* Programa: ESCR022.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Contas a Receber
** Objetivo: Listar Titulos a Receber por Cidade de Cobran‡a.
** Autor...: Gilvando de Souza Araujo - Junho/2001
** Obs.....: Especifico da TEAR TEXTIL INDUSTRIA E COMERCIO LTDA.
**
** Conversao para EMS 2.04:
**   Programa: ESCR022.P  =>  ESCR0014RP.P
**   Autor...: ProDB - Toninho
**   Data....: 12/02/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESCR0014RP 2.04.00.000}

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
       FIELD tp-cidade        AS   CHAR 
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

def buffer b-emitente for emitente.
def var l-capital  as log.
def var c-cidade   like emitente.cidade.
def var c-estado   like emitente.estado.
def var c-lis-cid  as char.
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
    tt-param.tp-cidade       LABEL "Tipo Cidade"        AT 08
    with no-box side-labels width 132 stream-io frame f-parlis.

form
    emitente.cod-emitente  label "CLIENTE"
    emitente.nome-abrev    label "NOME ABREV."
    titulo.nr-docto        label "TITULO"
    titulo.parcela         label "PA"
    titulo.cod-port        label "PORT"
    titulo.vl-saldo        label "VALOR"
    titulo.dt-emissao      label "EMISSAO"
    titulo.dt-vencimen     label "VENCTO."
    c-cidade               label "CIDADE"
    c-estado               label "UF"
    with no-box 55 down width 132 no-labels STREAM-IO frame f-detalhe.

/* include para remover acentua‡Æo de strings */
{include/i-freeac.i}

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
{utp/ut-liter.i T¡tulos_a_Receber_por_Cidade_de_Cobran‡a * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

assign c-lis-cid = "rio branco,maceio,macapa,manaus,salvador," +
                   "fortaleza,brasilia,vitoria,goiania,sao luis," +
                   "campo grande,cuiaba,belo horizonte,belem," +
                   "joao pessoa,curitiba,recife,teresina," +
                   "rio de janeiro,natal,porto alegre,porto velho," +
                   "boa vista,florianopolis,sao paulo,aracaju," +
                   "palmas".

FOR EACH titulo WHERE 
         titulo.ep-codigo   =  tt-param.ep-codigo       AND 
         titulo.cod-estabel >= tt-param.cod-estabel-ini AND 
         titulo.cod-estabel <= tt-param.cod-estabel-fim AND 
         titulo.cod-esp     >= tt-param.cod-esp-ini     AND 
         titulo.cod-esp     <= tt-param.cod-esp-fim     AND 
         titulo.cod-port    >= tt-param.cod-port-ini    AND 
         titulo.cod-port    <= tt-param.cod-port-fim    AND 
         titulo.dt-emissao  >= tt-param.dt-emissao-ini  AND 
         titulo.dt-emissao  <= tt-param.dt-emissao-fim  AND 
         titulo.dt-vencimen >= tt-param.dt-vencimen-ini AND 
         titulo.dt-vencimen <= tt-param.dt-vencimen-fim AND  
         titulo.vl-saldo    >  0 NO-LOCK BY titulo.dt-vencimen
                                         BY titulo.nr-docto:
                 
    run pi-acompanhar in h-acomp (input titulo.nr-docto).

    find emitente WHERE
         emitente.cod-emitente = titulo.cod-emitente no-lock no-error.
    if emitente.cod-emit <> emitente.end-cobranca then do:
       find b-emitente where
            b-emitente.cod-emitente = emitente.end-cobranca no-lock no-error.
       assign c-cidade = b-emitente.cidade
              c-estado = b-emitente.estado.
    end.
    else do:
       if emitente.endereco-cob = " " THEN
          assign c-cidade = fn-free-accent(emitente.cidade)
                 c-estado = emitente.estado.
       ELSE
          assign c-cidade = fn-free-accent(emitente.cidade-cob)
                 c-estado = emitente.estado-cob.
    END.
    
    IF LOOKUP(c-cidade,c-lis-cid) <> 0 THEN 
       ASSIGN l-capital = YES.
    ELSE 
       ASSIGN l-capital = NO.
    
    IF (l-capital = YES AND tt-param.tp-cidade = "I") OR
       (l-capital = NO  AND tt-param.tp-cidade = "C") THEN NEXT.
       
    DISPLAY emitente.cod-emitente
            emitente.nome-abrev
            titulo.nr-docto
            titulo.parcela
            titulo.cod-port
            titulo.vl-saldo
            titulo.dt-emissao
            titulo.dt-vencimen
            c-cidade
            c-estado
            WITH FRAME f-detalhe.

    DOWN WITH FRAME f-detalhe.
    ASSIGN de-tot-val = de-tot-val + titulo.vl-saldo
           i-cont-tit = i-cont-tit + 1.
END.

display "Total"    @ emitente.cod-emitente
        i-cont-tit @ titulo.nr-docto
        de-tot-val @ titulo.vl-saldo
        with frame f-detalhe.

assign i-cont-tit = 0 de-tot-val = 0.

IF tt-param.impr-param THEN DO.
   PAGE.

   find first empresa
        where empresa.ep-codigo = tt-param.ep-codigo no-lock no-error. 

   assign c-empresa = (if avail empresa then empresa.razao-social else "")
          tt-param.tp-cidade = IF tt-param.tp-cidade = "C"
                               THEN "Capital" 
                               ELSE IF tt-param.tp-cidade = "I"
                                    THEN "Interior"
                                    ELSE "Todas".

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
           tt-param.tp-cidade
           with frame f-parlis.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

