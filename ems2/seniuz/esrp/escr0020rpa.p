/* Programa: ESCR017.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Contas a Receber
** Objetivo: Gerar o relatorio Carta Notificando Titulos em Aberto
** Autor...: Sandro Wiest/Gilvando de Souza Araujo - Fevereiro/97
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL / SANTA ELISABETH
**
** Conversao para EMS 2.04:
**   Programa: ESCR017.P  =>  ESCR0011RP.P
**   Autor...: Prodb - Toninho
**   Data....: 13/02/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESCR0011RP 2.04.00.000}

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
    FIELD repres-ini       LIKE titulo.cod-rep
    FIELD repres-fim       LIKE titulo.cod-rep
    FIELD cod-port-ini     LIKE titulo.cod-port
    FIELD cod-port-fim     LIKE titulo.cod-port
    FIELD cliente-ini      LIKE titulo.cod-emitente
    FIELD cliente-fim      LIKE titulo.cod-emitente
    FIELD dt-emissao-ini   LIKE titulo.dt-emissao
    FIELD dt-emissao-fim   LIKE titulo.dt-emissao
    FIELD dt-vencimen-ini  LIKE titulo.dt-vencimen
    FIELD dt-vencimen-fim  LIKE titulo.dt-vencimen
    FIELD cod-mensagem     LIKE mensagem.cod-mensagem
    FIELD e-mail           AS LOG
    FIELD subject          AS CHAR FORMAT "x(40)"
    FIELD texto            AS CHAR FORMAT "x(2000)".

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/**************/
/** Includes **/
/**************/

{include/tt-edit.i}
{include/pi-edit.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

def buffer b-emitente for emitente.
DEF VAR c-arq-texto AS CHAR.
DEF VAR c-arq-bat   AS CHAR.
DEF VAR c-comando AS CHAR.
def var l-passou as log.
def var de-tot-cli as dec.
def var i-atraso   as int format "99".
def var c-cidade as char format "x(60)".
def var c-endereco like emitente.endereco.
def var c-cidade-emit like emitente.cidade.
def var c-bairro like emitente.bairro.
def var c-cep    like emitente.cep.
def var c-estado like emitente.estado.
DEF VAR c-meses AS CHAR EXTENT 12 INIT
    [" DE JANEIRO DE "," DE FEVEREIRO DE "," DE MARCO DE "," DE ABRIL DE ",
     " DE MAIO DE "," DE JUNHO DE "," DE JULHO DE "," DE AGOSTO DE ",
     " DE SETEMBRO DE "," DE OUTUBRO DE "," DE NOVEMBRO DE "," DE DEZEMBRO DE "].

DEF STREAM str-email.

form
    skip(3)
    c-cidade              at 5
    skip(3)
    emitente.nome-emit    at 5
    emitente.cod-emit
    "-" emitente.cod-rep  
    skip(3)
    c-endereco            at 5
    "-" 
    c-bairro              
    skip
    c-cep                 at 5
    c-cidade-emit  "-"
    c-estado              
    skip(3)
    "A/C Contas a Pagar"  at 5
    skip(3)
    "Assunto: Extrato de titulos em aberto." at 5
"--------------------------------------------------------------------------------"
    at 5
    "NUMERO PA   EMISSAO  VENCIMENTO ATRASO VALOR TITULO TITULO-BANCO VLR.A PAGAR"
    at 5
    skip(1)
    with no-labels width 85 STREAM-IO frame f-cab-cli no-box.

FORM
    titulo.nr-docto     at 5 FORMAT "x(6)" 
    titulo.parcela      
    titulo.dt-emissao
    titulo.dt-vencimen
    i-atraso            format ">>>9"
    titulo.vl-saldo     format ">>>,>>>,>>9.99"
    titulo.titulo-banco format "x(12)"
    titulo.pedido-rep   format "x(12)"
    with no-labels width 85 STREAM-IO frame f-detalhe no-box.

IF NOT tt-param.e-mail THEN
   /* include padrÆo para output de relat¢rios */
   {include/i-rpout.i &STREAM="stream str-rp"}

/* bloco principal do programa */
FIND FIRST param-global NO-LOCK NO-ERROR.
FIND FIRST empresa
     WHERE empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Imprimindo *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

FOR EACH titulo WHERE 
         titulo.ep-codigo     = tt-param.ep-codigo       AND 
         titulo.cod-estab    >= tt-param.cod-estabel-ini AND 
         titulo.cod-estab    <= tt-param.cod-estabel-fim AND 
         titulo.dt-emissao   >= tt-param.dt-emissao-ini  AND 
         titulo.dt-emissao   <= tt-param.dt-emissao-fim  AND 
         titulo.dt-vencimen  >= tt-param.dt-vencimen-ini AND 
         titulo.dt-vencimen  <= tt-param.dt-vencimen-fim AND 
         titulo.cod-esp      >= tt-param.cod-esp-ini     AND 
         titulo.cod-esp      <= tt-param.cod-esp-fim     AND 
         titulo.cod-port     >= tt-param.cod-port-ini    AND 
         titulo.cod-port     <= tt-param.cod-port-fim    AND 
         titulo.cod-rep      >= tt-param.repres-ini      AND 
         titulo.cod-rep      <= tt-param.repres-fim      AND 
         titulo.cod-emitente >= tt-param.cliente-ini     AND 
         titulo.cod-emitente <= tt-param.cliente-fim     AND 
         titulo.vl-saldo     <> 0 NO-LOCK 
         break by titulo.cod-emit
               by titulo.dt-vencimen:

    run pi-acompanhar in h-acomp (input titulo.nr-docto).

    IF tt-param.e-mail THEN DO.
       ASSIGN c-arq-texto = SESSION:TEMP-DIRECTORY + STRING(titulo.cod-emitente) + ".txt".
       OUTPUT STREAM str-email TO VALUE(c-arq-texto).
    END.
    
    FIND FIRST emitente WHERE 
               emitente.cod-emit  =  titulo.cod-emit AND 
               emitente.identific <> 2 NO-LOCK NO-ERROR.

    ASSIGN c-endereco    = emitente.endereco
           c-bairro      = emitente.bairro
           c-cidade-emit = emitente.cidade
           c-estado      = emitente.estado
           c-cep         = emitente.cep.
    IF emitente.end-cobranca <> 0 AND 
       emitente.end-cobranca <> emitente.cod-emit THEN DO:

       FIND FIRST b-emitente WHERE 
                  b-emitente.cod-emit  =  emitente.end-cobranca AND 
                  b-emitente.identific <> 2 NO-LOCK NO-ERROR.

       ASSIGN c-endereco    = b-emitente.endereco
              c-bairro      = b-emitente.bairro
              c-cidade-emit = b-emitente.cidade
              c-estado      = b-emitente.estado
              c-cep         = b-emitente.cep.
    END.
    ELSE 
       IF emitente.endereco-cob <> "" THEN 
          ASSIGN c-endereco    = emitente.endereco-cob
                 c-bairro      = emitente.bairro-cob
                 c-cidade-emit = emitente.cidade-cob
                 c-estado      = emitente.estado-cob
                 c-cep         = emitente.cep-cob.

    ASSIGN c-cidade = empresa.cidade + "," + " " + STRING(DAY(TODAY)) +
                      c-meses[MONTH(TODAY)] + STRING(YEAR(TODAY),"9999") + ".".

    FIND esp-doc WHERE 
         esp-doc.cod-esp = titulo.cod-esp NO-LOCK NO-ERROR.

    FIND mensagem WHERE 
         mensagem.cod-mensag = tt-param.cod-mensagem NO-LOCK NO-ERROR.

    IF l-passou = NO THEN DO:
       DISPLAY STREAM str-rp
               c-cidade
               emitente.nome-emit
               emitente.cod-emit
               emitente.cod-rep
               c-endereco
               c-bairro
               c-cidade-emit
               c-estado
               c-cep
               WITH FRAME f-cab-cli.
        DOWN stream str-rp WITH FRAME f-cab-cli.
        ASSIGN l-passou = YES. 
    END.

    IF titulo.dt-vencimen < TODAY THEN 
       ASSIGN i-atraso = TODAY - titulo.dt-vencimen.
    ELSE 
       ASSIGN i-atraso = 0.
       
    DISPLAY STREAM str-rp
            titulo.nr-docto
            titulo.parcela
            titulo.dt-emissao
            titulo.dt-vencimen
            i-atraso             WHEN i-atraso > 0
            titulo.vl-saldo
            titulo.titulo-banco
            titulo.pedido-rep
             when (tt-param.cod-mensagem >= 921 and tt-param.cod-mensagem <= 924)
            with frame f-detalhe.

    DOWN STREAM str-rp WITH FRAME f-detalhe.
    ASSIGN de-tot-cli = de-tot-cli + titulo.vl-saldo.

    if LAST-OF(titulo.cod-emit) THEN DO.
       IF de-tot-cli <> 0 THEN DO:
          DISPLAY STREAM str-rp
                  "Total ->"                       @ titulo.dt-vencimen
                  de-tot-cli format ">,>>>,>>9.99" @ titulo.vl-saldo
                  WITH FRAME f-detalhe.
          DOWN STREAM str-rp WITH FRAME f-detalhe.
    
          PUT STREAM str-rp
              FILL("-",78) FORMAT "x(78)" 
              SKIP.
    
          RUN pi-print-editor(INPUT tt-param.texto, INPUT 77).
          FOR EACH tt-editor:
              PUT STREAM str-rp
                  tt-editor.conteudo
                  SKIP.
          END.
    
          ASSIGN l-passou   = NO 
                 de-tot-cli = 0.

          PAGE STREAM str-rp.

          IF tt-param.e-mail THEN DO.
             OUTPUT STREAM str-email CLOSE.

             run pi-acompanhar in h-acomp (input "Enviando E-mail...").

             ASSIGN c-comando = 'c:\postie\postie.exe -host:www.teartextil.com.br -from:' + 'gilvando@teartextil.com.br' +
                                ' -to:' + emitente.e-mail + ' -s:"' + tt-param.subject + '" -file:"' + c-arq-texto + '"' +
                                ' -dsn -mdn '. 

             ASSIGN c-arq-bat = SESSION:TEMP-DIRECTORY + STRING(titulo.cod-emitente) + ".bat".
             OUTPUT TO VALUE(c-arq-bat).
                PUT c-comando FORMAT "x(300)".
             OUTPUT CLOSE.

/*             OS-COMMAND SILENT VALUE(c-arq-bat) NO-WAIT. 
             OS-DELETE VALUE(c-arq-bat) value(c-arq-texto). */
          END.
       END.
       IF tt-param.e-mail THEN DO.
          OUTPUT STREAM str-email CLOSE.
          OS-DELETE VALUE(c-arq-texto). 
       END.
    END.
end.

/* fechamento do output do relat¢rio  */
IF NOT tt-param.e-mail THEN
   {include/i-rpclo.i &STREAM="stream str-rp"}

RUN pi-finalizar in h-acomp.
RETURN "OK":U.

