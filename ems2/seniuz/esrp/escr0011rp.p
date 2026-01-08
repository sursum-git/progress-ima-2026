/* Programa: ESCR017.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Contas a Receber
** Objetivo: Gerar o relatorio Carta de Notifi‡Æo de Titulos em Aberto
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
       FIELD serie-ini        LIKE titulo.serie
       FIELD serie-fim        LIKE titulo.serie
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
DEF VAR h-acomp AS HANDLE NO-UNDO.

DEF BUFFER b-emitente FOR emitente.
DEF BUFFER b-titulo   FOR titulo.

DEF VAR c-arq-texto    AS CHAR.
DEF VAR c-arq-bat      AS CHAR.
DEF VAR c-comando      AS CHAR.
DEF VAR l-passou       AS LOG.
DEF VAR de-tot-cli     AS DEC.
DEF VAR dt-emissao-dup LIKE titulo.dt-emissao.
DEF VAR dt-vencto-dup  LIKE titulo.dt-vencimen.
DEF VAR de-valor-dup   LIKE titulo.vl-saldo.
DEF VAR i-atraso       AS INT FORMAT "99".
DEF VAR c-cidade       AS CHAR FORMAT "x(60)".
DEF VAR c-endereco     LIKE emitente.endereco.
DEF VAR c-cidade-emit  LIKE emitente.cidade.
DEF VAR c-bairro       LIKE emitente.bairro.
DEF VAR c-cep          LIKE emitente.cep.
DEF VAR c-estado       LIKE emitente.estado.
DEF VAR c-meses        AS CHAR EXTENT 12 INIT
    [" DE JANEIRO DE "," DE FEVEREIRO DE "," DE MARCO DE "," DE ABRIL DE ",
     " DE MAIO DE "," DE JUNHO DE "," DE JULHO DE "," DE AGOSTO DE ",
     " DE SETEMBRO DE "," DE OUTUBRO DE "," DE NOVEMBRO DE "," DE DEZEMBRO DE "].

DEF STREAM str-email.

form
    empresa.razao-social
    SKIP(1)
    c-cidade              
    skip(2)
    emitente.nome-emit    
    emitente.cod-emit
    "-" emitente.cod-rep  
    skip(1)
    c-endereco            
    "-" 
    c-bairro              
    skip
    c-cep                 
    c-cidade-emit  "-"
    c-estado              
    skip(1)
    "A/C Contas a Pagar"  
    skip(1)
    "Assunto: Extrato de titulos em aberto." 
    "------------------------------------------------------------------------------------" 
    "NUMERO  PA VLR-TITULO EMISSAO  VENCIMEN VLR-NotaDebt TITULO-BANCO        VLR.A PAGAR" 
    skip(1)                                    
    with no-labels width 95 STREAM-IO frame f-cab-cli no-box.

FORM
    titulo.nr-docto     FORMAT "x(7)" 
    titulo.parcela      
    de-valor-dup        FORMAT ">>>,>>9.99"
    dt-emissao-dup      FORMAT "99/99/99"
    dt-vencto-dup       FORMAT "99/99/99"
    titulo.vl-saldo     format ">>>,>>9.99"
    titulo.titulo-banco format "x(19)"
    titulo.pedido-rep   format "x(12)"
    with no-labels width 95 STREAM-IO frame f-detalhe no-box.

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
         titulo.serie        >= tt-param.serie-ini       AND
         titulo.serie        <= tt-param.serie-fim       AND
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
               empresa.razao-social
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
     
    IF titulo.cod-esp = "nd" THEN DO:
       FIND mov-tit WHERE mov-tit.ep-codigo   = titulo.ep-codigo   
                      AND mov-tit.cod-estabel = titulo.cod-estabel 
                      AND mov-tit.nr-docto    = titulo.nr-docto 
                      AND mov-tit.serie       = titulo.serie       
                      AND mov-tit.parcela     = titulo.parcela
                      AND mov-tit.cod-esp     = titulo.cod-esp 
                      AND mov-tit.transacao   = 14 /* IMP */
                    NO-LOCK NO-ERROR.
       FIND b-titulo WHERE b-titulo.ep-codigo   = titulo.ep-codigo
                       AND b-titulo.cod-estabel = titulo.cod-estabel
                       AND b-titulo.nr-docto    = titulo.doc-antecip
                       AND b-titulo.serie       = titulo.serie
                       AND b-titulo.parcela     = titulo.parc-antecip
                       AND b-titulo.cod-esp     = titulo.esp-antecip
                     NO-LOCK NO-ERROR.                         
       IF AVAIL b-titulo THEN
          ASSIGN de-valor-dup   = b-titulo.vl-original
                 dt-emissao-dup = mov-tit.dt-trans /*b-titulo.dt-ult-pagto*/
                 dt-vencto-dup  = b-titulo.dt-vencimen
                 i-atraso       = dt-emissao-dup - dt-vencto-dup.
       ELSE
          ASSIGN de-valor-dup   = 0
                 dt-emissao-dup = ?
                 dt-vencto-dup  = ?
                 i-atraso       = 0.
    END.
    ELSE
        ASSIGN de-valor-dup   = titulo.vl-saldo
               dt-emissao-dup = titulo.dt-emissao
               dt-vencto-dup  = titulo.dt-vencimen
               i-atraso       = TODAY - titulo.dt-vencimen.
       
    DISPLAY STREAM str-rp
            titulo.nr-docto
            titulo.parcela
            de-valor-dup
            dt-emissao-dup
            dt-vencto-dup
            titulo.vl-saldo
            titulo.titulo-banco
            titulo.pedido-rep
             WHEN (tt-param.cod-mensagem >= 921 AND tt-param.cod-mensagem <= 924)
            WITH FRAME f-detalhe.

    DOWN STREAM str-rp WITH FRAME f-detalhe.
    ASSIGN de-tot-cli = de-tot-cli + titulo.vl-saldo.

    if LAST-OF(titulo.cod-emit) THEN DO.
       IF de-tot-cli <> 0 THEN DO:
          DISPLAY STREAM str-rp
                  "Total->"                        @ titulo.nr-docto
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
       END.
    END.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i &STREAM="stream str-rp"}

RUN pi-finalizar in h-acomp.
RETURN "OK":U.

