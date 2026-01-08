/* Programa: ESCR010.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Contas a Receber
** Objetivo: Listar a Carta de Declara‡Æo de Quita‡Æo de T¡tulos.
** Autor...: Gilvando de Souza Araujo - Agosto/95
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL.
**
** Conversao para EMS 2.04:
**   Programa: ESCR010.P  =>  ESCR0004RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 07/02/2005
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESCR0004RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       FIELD ep-codigo        LIKE titulo.ep-codigo
       FIELD cod-estabel      LIKE titulo.cod-estabel
       FIELD cod-esp          LIKE titulo.cod-esp
       FIELD serie            LIKE titulo.serie 
       FIELD nr-docto-ini     LIKE titulo.nr-docto
       FIELD nr-docto-fin     LIKE titulo.nr-docto
       FIELD parcela-ini      LIKE titulo.parcela 
       FIELD parcela-fin      LIKE titulo.parcela
       FIELD e-mail           AS LOG
       FIELD subject          AS CHAR FORMAT "x(40)"
       FIELD texto            AS CHAR FORMAT "x(2000)".

define temp-table tt-digita no-undo
       field ep-codigo        LIKE titulo.ep-codigo
       field cod-estabel      LIKE titulo.cod-estabel
       FIELD cod-esp          LIKE titulo.cod-esp
       FIELD serie            LIKE titulo.serie
       FIELD nr-docto         LIKE titulo.nr-docto
       FIELD parcela          LIKE titulo.parcela
       index id ep-codigo
                cod-estabel
                cod-esp
                serie
                nr-docto
                parcela.

define temp-table tt-raw-digita 
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

FOR EACH tt-raw-digita.
    CREATE tt-digita.
    RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
END.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para tt-editor  */
{include/tt-edit.i}
{include/pi-edit.i}

/* defini‡Æo de vari veis  */
define temp-table tt-titulos no-undo
       field ep-codigo        LIKE titulo.ep-codigo
       field cod-estabel      LIKE titulo.cod-estabel
       FIELD cod-esp          LIKE titulo.cod-esp
       FIELD serie            LIKE titulo.serie
       FIELD nr-docto         LIKE titulo.nr-docto
       FIELD parcela          LIKE titulo.parcela
       FIELD cod-emitente     LIKE titulo.cod-emitente
       FIELD dt-emissao       LIKE titulo.dt-emissao
       FIELD dt-vencimen      LIKE titulo.dt-vencimen
       FIELD dt-ult-pagto     LIKE titulo.dt-ult-pagto
       FIELD vl-baixa         LIKE titulo.vl-liquido.

def buffer b-emitente for emitente.

def var h-acomp as handle no-undo.

def var de-vl-baixa   LIKE titulo.vl-liquido.
def var de-saldo      LIKE titulo.vl-saldo.
def var c-cidade      as char format "x(60)".
def var c-endereco    like emitente.endereco.
def var c-cidade-emit like emitente.cidade.
def var c-bairro      like emitente.bairro.
def var c-cep         like emitente.cep.
def var c-estado      like emitente.estado.
DEF VAR c-meses       AS CHAR EXTENT 12 INIT
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
    "Assunto: Declara‡Æo de quita‡Æo de t¡tulos." at 5
    "------------------------------------------------------------------------------"
    at 5
    "NUMERO  PA   EMISSAO  VENCIMENTO  PAGAMENTO         VALOR PAGO"
    at 5
    skip(1)
    with no-labels width 85 STREAM-IO frame f-cab-cli no-box.

FORM
    tt-titulos.nr-docto     at 5 FORMAT "x(7)" 
    tt-titulos.parcela      
    tt-titulos.dt-emissao
    tt-titulos.dt-vencimen
    tt-titulos.dt-ult-pagto
    de-vl-baixa         
    with no-labels width 85 STREAM-IO frame f-detalhe no-box.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i &STREAM="stream str-rp"}

/* bloco principal do programa */
FIND FIRST param-global NO-LOCK NO-ERROR.
FIND FIRST empresa
     WHERE empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Imprimindo *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

ASSIGN c-cidade = empresa.cidade + "," + " " + STRING(DAY(TODAY)) +
                  c-meses[MONTH(TODAY)] + STRING(YEAR(TODAY),"9999") + ".".

FIND FIRST tt-digita NO-LOCK NO-ERROR.
IF AVAIL tt-digita THEN DO: /* Houve digita‡Æo */
   FOR EACH tt-digita.
       FIND titulo WHERE titulo.ep-codigo   = tt-digita.ep-codigo
                     AND titulo.cod-estabel = tt-digita.cod-estabel
                     AND titulo.cod-esp     = tt-digita.cod-esp
                     AND titulo.serie       = tt-digita.serie
                     AND titulo.nr-docto    = tt-digita.nr-docto
                     AND titulo.parcela     = tt-digita.parcela
                   NO-LOCK NO-ERROR.
       assign de-saldo    = titulo.vl-saldo
              de-vl-baixa = 0.
       for each mov-tit where mov-tit.ep-codigo   = titulo.ep-codigo
                          and mov-tit.cod-estabel = titulo.cod-estabel
                          and mov-tit.cod-esp     = titulo.cod-esp
                          AND mov-tit.serie       = titulo.serie
                          and mov-tit.nr-docto    = titulo.nr-docto
                          and mov-tit.parcela     = titulo.parcela
                        no-lock:

           find esp-doc where esp-doc.cod-esp = titulo.cod-esp
                        no-lock no-error.
           if AVAIL esp-doc AND esp-doc.tipo = 2 then next. /* Antecip */

           if mov-tit.transacao = 14 then
              assign de-saldo = de-saldo - mov-tit.vl-original.
           if mov-tit.transacao = 2 then
              assign de-saldo    = de-saldo + mov-tit.vl-baixa
                     de-vl-baixa = de-vl-baixa + mov-tit.vl-baixa.
           if mov-tit.transacao = 3 then
              assign de-saldo    = de-saldo + mov-tit.vl-baixa
                     de-vl-baixa = de-vl-baixa + mov-tit.vl-baixa.
           if mov-tit.transacao = 13 and mov-tit.lancamento = 1 then
              assign de-saldo = de-saldo + mov-tit.vl-baixa
                     de-vl-baixa = de-vl-baixa + mov-tit.vl-baixa.
           if mov-tit.transacao = 13 and mov-tit.lancamento = 2 then
              assign de-saldo = de-saldo - mov-tit.vl-baixa
                     de-vl-baixa = de-vl-baixa - mov-tit.vl-baixa.
       end.

       IF AVAIL titulo AND de-saldo = 0 THEN DO:
          CREATE tt-titulos.
          ASSIGN tt-titulos.cod-emitente = titulo.cod-emitente
                 tt-titulos.ep-codigo    = titulo.ep-codigo    
                 tt-titulos.cod-estabel  = titulo.cod-estabel 
                 tt-titulos.cod-esp      = titulo.cod-esp     
                 tt-titulos.serie        = titulo.serie       
                 tt-titulos.nr-docto     = titulo.nr-docto     
                 tt-titulos.parcela      = titulo.parcela
                 tt-titulos.dt-emissao   = titulo.dt-emissao   
                 tt-titulos.dt-vencimen  = titulo.dt-vencimen  
                 tt-titulos.dt-ult-pagto = titulo.dt-ult-pagto 
                 tt-titulos.vl-baixa     = de-vl-baixa.
       END.
   END.
END.
ELSE DO: /* Houve sele‡Æo */
   FOR EACH titulo WHERE titulo.ep-codigo   =  tt-param.ep-codigo
                     AND titulo.cod-estabel =  tt-param.cod-estabel
                     AND titulo.cod-esp     =  tt-param.cod-esp
                     AND titulo.serie       =  tt-param.serie
                     AND titulo.nr-docto    >= tt-param.nr-docto-ini
                     AND titulo.nr-docto    <= tt-param.nr-docto-fin
                     AND titulo.parcela     >= tt-param.parcela-ini
                     AND titulo.parcela     <= tt-param.parcela-fin
                   NO-LOCK:
   
       assign de-saldo    = titulo.vl-saldo
              de-vl-baixa = 0.
       for each mov-tit where mov-tit.ep-codigo   = titulo.ep-codigo
                          and mov-tit.cod-estabel = titulo.cod-estabel
                          and mov-tit.cod-esp     = titulo.cod-esp
                          AND mov-tit.serie       = titulo.serie
                          and mov-tit.nr-docto    = titulo.nr-docto
                          and mov-tit.parcela     = titulo.parcela
                        no-lock:
           find esp-doc where esp-doc.cod-esp = titulo.cod-esp
                              no-lock no-error.
           if esp-doc.tipo = 2 then next.
           if mov-tit.transacao = 14 then
              assign de-saldo = de-saldo - mov-tit.vl-original.
           if mov-tit.transacao = 2 then
              assign de-saldo    = de-saldo + mov-tit.vl-baixa
                     de-vl-baixa = de-vl-baixa + mov-tit.vl-baixa.
           if mov-tit.transacao = 3 then
              assign de-saldo    = de-saldo + mov-tit.vl-baixa
                     de-vl-baixa = de-vl-baixa + mov-tit.vl-baixa.
           if mov-tit.transacao = 13 and mov-tit.lancamento = 1 then
              assign de-saldo = de-saldo + mov-tit.vl-baixa
                     de-vl-baixa = de-vl-baixa + mov-tit.vl-baixa.
           if mov-tit.transacao = 13 and mov-tit.lancamento = 2 then
              assign de-saldo = de-saldo - mov-tit.vl-baixa
                     de-vl-baixa = de-vl-baixa - mov-tit.vl-baixa.
       end.
       
       IF AVAIL titulo AND de-saldo = 0 THEN DO:
          CREATE tt-titulos.
          ASSIGN tt-titulos.cod-emitente = titulo.cod-emitente
                 tt-titulos.ep-codigo    = titulo.ep-codigo    
                 tt-titulos.cod-estabel  = titulo.cod-estabel 
                 tt-titulos.cod-esp      = titulo.cod-esp     
                 tt-titulos.serie        = titulo.serie       
                 tt-titulos.nr-docto     = titulo.nr-docto     
                 tt-titulos.parcela      = titulo.parcela
                 tt-titulos.dt-emissao   = titulo.dt-emissao   
                 tt-titulos.dt-vencimen  = titulo.dt-vencimen  
                 tt-titulos.dt-ult-pagto = titulo.dt-ult-pagto 
                 tt-titulos.vl-baixa     = de-vl-baixa.
       END.
   END.
END.

FOR EACH tt-titulos BREAK BY tt-titulos.cod-emitente
                          BY tt-titulos.ep-codigo
                          BY tt-titulos.cod-estabel
                          BY tt-titulos.cod-esp
                          BY tt-titulos.serie
                          BY tt-titulos.nr-docto
                          BY tt-titulos.parcela:
    IF FIRST-OF(tt-titulos.cod-emitente) THEN DO:
       FIND emitente WHERE emitente.cod-emitente = tt-titulos.cod-emitente
                     NO-LOCK NO-ERROR.
       
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
    END.
    
    DISPLAY STREAM str-rp
            tt-titulos.nr-docto
            tt-titulos.parcela
            tt-titulos.dt-emissao
            tt-titulos.dt-vencimen
            tt-titulos.dt-ult-pagto
            de-vl-baixa
            WITH FRAME f-detalhe.

    DOWN STREAM str-rp WITH FRAME f-detalhe.

    if LAST-OF(tt-titulos.cod-emitente) THEN DO:
       PUT STREAM str-rp
           FILL("-",78) FORMAT "x(78)" AT 5
           SKIP.
 
       RUN pi-print-editor(INPUT tt-param.texto, INPUT 78).
       FOR EACH tt-editor:
           PUT STREAM str-rp
               tt-editor.conteudo AT 5
               SKIP.
       END.
       PAGE STREAM str-rp.
    END.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i &STREAM="stream str-rp"}

RUN pi-finalizar in h-acomp.
RETURN "OK":U.

