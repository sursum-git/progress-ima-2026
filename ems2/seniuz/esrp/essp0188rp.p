/* Programa: ESSP0188.W
** Modulo..: CRIVO FINANCEIRO       
** Objetivo: Fazer Reavalia‡Æo do Credito de pedidos (Abertos e Atendido Pacial) 
**           cujo CRDITO Esteja Aprovado Situacao (2, 3) e submeter o pedido ao
**           crivo da API0180, se o parametro RESTRI€ÇO, devolvido pela for YES
**           entÆo DESAPROVAR O CREDITO.
** Autor...: F bio Coelho Lanza - MAIO/2009
** Obs.....: Especifico da TEAR TEXTIL IND.COM.LTDA.
**
*/
/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0188RP 2.04.00.000}

DEFINE TEMP-TABLE tt-param   NO-UNDO
   FIELD destino            AS INTEGER
   FIELD arquivo            AS CHAR FORMAT "x(35)"
   FIELD usuario            AS CHAR FORMAT "x(12)"
   FIELD data-exec          AS DATE
   FIELD hora-exec          AS INTEGER
   FIELD classifica         AS INTEGER
   FIELD desc-classifica    AS CHAR FORMAT "x(40)"
   FIELD nr-pedcli-ini      LIKE ped-venda.nr-pedcli
   FIELD nr-pedcli-fin      LIKE ped-venda.nr-pedcli 
   FIELD nome-abrev-ini     LIKE ped-venda.nome-abrev
   FIELD nome-abrev-fin     LIKE ped-venda.nome-abrev 
   FIELD no-ab-reppri-ini   LIKE ped-venda.no-ab-reppri
   FIELD no-ab-reppri-fin   LIKE ped-venda.no-ab-reppri 
   FIELD imp-param          AS LOG.

DEFINE TEMP-TABLE tt-raw-digita
       FIELD raw-digita AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i}

/* defini‡Æo de vari veis  */
DEF VAR h-acomp        AS HANDLE NO-UNDO.
DEF VAR c-destinatario AS CHAR.

/* Aprova‡Æo/Reprova‡Æo de Cr‚dito */
DEF VAR c-motivo       LIKE ped-venda.motivo.       
DEF VAR l-restricao    AS LOG.
DEF VAR c-texto-log    AS CHAR.
DEF VAR c-ped-proc     AS CHAR.
DEF VAR i-lin          AS INT INITIAL 99.
DEF VAR i-pag          AS INT INITIAL  1.
DEF VAR c-mensagem     AS CHAR.


/* bloco principal do programa */
FIND FIRST param-global NO-LOCK NO-ERROR.
FIND FIRST empresa
     WHERE empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 

ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Imprimindo *}
RUN pi-inicializar in h-acomp (INPUT RETURN-VALUE).

FIND FIRST param-dis NO-LOCK NO-ERROR. 
ASSIGN c-destinatario = param-dis.emails-fin.
 
ASSIGN c-ped-proc = SESSION:TEMP-DIRECTORY + "Pedidos Desaprovados.txt".
OUTPUT TO VALUE(c-ped-proc) CONVERT SOURCE "ibm850".

FOR EACH ped-venda WHERE
         LOOKUP(STRING(ped-venda.cod-sit-ped),"1,2") > 0     AND
         ped-venda.nr-pedcli    >= tt-param.nr-pedcli-ini    AND
         ped-venda.nr-pedcli    <= tt-param.nr-pedcli-fin    AND
         ped-venda.nome-abrev   >= tt-param.nome-abrev-ini   AND
         ped-venda.nome-abrev   <= tt-param.nome-abrev-fin   AND
         ped-venda.no-ab-reppri >= tt-param.no-ab-reppri-ini AND
         ped-venda.no-ab-reppri <= tt-param.no-ab-reppri-fin 
         SHARE-LOCK
      BY ped-venda.no-ab-reppri
      BY ped-venda.nome-abrev.


    RUN pi-acompanhar IN h-acomp (INPUT "Pedido : "  + ped-venda.nr-pedcli + 
                                        " Cliente: " + ped-venda.nome-abrev).

    FIND ped-venda-ext WHERE
         ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.
    IF NOT AVAIL ped-venda-ext THEN NEXT.

    IF ped-venda-ext.tp-pedido = "· Vista"            OR
       ped-venda-ext.tp-pedido = "Exporta‡Æo"         OR 
       ped-venda-ext.tp-pedido = "Amostra"            OR
       ped-venda-ext.tp-pedido = "Amostra Exporta‡Æo" OR
       ped-venda-ext.tp-pedido = "Bonifica‡Æo"        OR
       ped-venda-ext.tp-pedido = "Doa‡Æo" 
       THEN NEXT.

    FIND emitente WHERE
         emitente.nome-abrev = ped-venda.nome-abrev NO-LOCK NO-ERROR.

    IF emitente.ind-cre-cli = 2 THEN NEXT.  /* Cr‚dito Autom tico */

    /* Credito AVALIADO ou APROVADO */
    IF LOOKUP(STRING(ped-venda.cod-sit-aval),"2,3") = 0 THEN NEXT.

    RUN esapi/esapi0180.p (INPUT emitente.cod-emitente, 
                           OUTPUT l-restricao,
                           OUTPUT c-motivo).

    IF l-restricao = NO THEN NEXT.
    
    ASSIGN ped-venda.cod-sit-aval = 4.

    /* Grava LOG */
    ASSIGN c-texto-log = "Cr‚dito Desaprovado: " + c-motivo.
    RUN esapi/cria-log-pedvenda.p (INPUT ped-venda.nr-pedcli,
                                   INPUT ped-venda.nome-abrev,
                                   INPUT c-texto-log,
                                   INPUT NO).

    /* Gerar Arquivo com os Pedidos Processados */
    IF i-lin > 61 THEN DO:
       RUN pi-imp-cabec.
       ASSIGN i-lin = 7.
    END.

    PUT ped-venda.nr-pedcli    FORMAT "x(6)"  AT   1
        ped-venda.nome-abrev   FORMAT "x(12)" AT   8
        ped-venda.no-ab-reppri FORMAT "x(12)" AT  21 
        c-motivo               FORMAT "x(97)" AT  36.
    PUT SKIP.
    ASSIGN i-lin = i-lin + 1.
   
END.
OUTPUT CLOSE.

IF i-lin <> 99 THEN DO: 
   ASSIGN c-mensagem = "Segue anexo rela‡Æo de pedidos, que tiveram seu credito DESAPROVADO." +
          CHR(10) +  CHR(10) +
          "Grato, " + CHR(13) + CHR(13) +
          "Crivo Autom tico" + CHR(13) +
          "TEAR TEXTIL INDUSTRIA E COMERCIO LTDA".

   RUN esapi/esapi002.p (INPUT "teartextil@teartextil.com.br", /* e-mail remetente */
                         INPUT c-destinatario, /* e-mail destinat rio */ 
                         INPUT "Crivo Financeiro AUTOMµTICO" , /* Assunto */
                         INPUT c-mensagem, /* Mensagem */
                         INPUT c-ped-proc, /*arquivo anexo*/
                         INPUT YES). /* Mostra Erros */
END. 

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
RUN pi-finalizar IN h-acomp.
RETURN "OK":U.



/* P  R  O  C  E  D  I  M  E  N  T  O  S */
/* ------------------------------------- */
PROCEDURE pi-imp-cabec.

  PUT "TEAR TEXTIL INDUSTRIA E COMERCIO LTDA."  AT   1
      "DATA: "                                  AT  62
      STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  68
      "HORA: "                                  AT  89
      STRING(TIME,"hh:mm:ss")                   AT  95
      "PAG:"                                    AT 125
      i-pag FORMAT ">>>"                        AT 130
      SKIP(1).
  PUT "CRIVO FINANCEIRO AUTOMATICO" AT 58 SKIP(1).


  PUT "Pedido Cliente      Representante  Motivo" AT 1.
  PUT "------ ------------ -------------  -------------------------------------------------------------------------------------------------" AT 1.
  ASSIGN i-pag = i-pag + 1.                                                                              

END PROCEDURE.
