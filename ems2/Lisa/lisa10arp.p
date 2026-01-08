 /* include de controle de vers’o */
{include/i-prgvrs.i lisa10arp 2.06.00.001}
DEFINE VARIABLE h-acomp         AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoDadosRetorno AS HANDLE      NO-UNDO.

{lisa/lisa10a.i}

DEF TEMP-TABLE tt-raw-digita
   	FIELD raw-digita	      AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

/* include padr’o para variÿveis de relat½rio  */
{include/i-rpvar.i}


{include/i-rpout.i &STREAM="stream str-rp" }

/* include com a defini»’o da frame de cabe»alho e rodap² */
{include/i-rpcab.i &STREAM="str-rp"}

/* bloco principal do programa */
FIND FIRST param-global NO-LOCK NO-ERROR.
FIND first ems2cad.empresa
     WHERE empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 

ASSIGN c-programa = "lisa10aRP"
       c-versao	  = "2.06"
       c-revisao  = ".00.001"
       c-empresa  = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

ASSIGN c-sistema = 'ERP'.

ASSIGN c-titulo-relat = 'Pedidos LISA - Integra‡Æo'.

VIEW STREAM str-rp FRAME f-cabec.
VIEW STREAM str-rp FRAME f-rodape.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

RUN pi-inicializar in h-acomp (input "Importando Dados Pedido LISA").
RUN pi-acompanhar IN h-acomp('Buscando Dados').
RUN esbo/boDadosRetorno.p PERSIST SET hBoDadosRetorno.
RUN iniciar       IN hBoDadosRetorno.
RUN setAcomp      IN hBoDadosRetorno(YES,?).
RUN setValsIni    IN hBoDadosRetorno.
RUN setProp       IN hBoDadosRetorno('dtEmisNota',1,tt-param.dt_ini_nf).
RUN setProp       IN hBoDadosRetorno('dtEmisNota',2,tt-param.dt_fim_nf).
RUN setProp       IN hBoDadosRetorno('nrPedido',1,STRING(tt-param.nr_pedido_ini)).
RUN setProp       IN hBoDadosRetorno('nrPedido',2,STRING(tt-param.nr_pedido_fim)).
RUN exec          IN hBoDadosRetorno.
//RUN getTTResult   IN hBoDadosRetorno(OUTPUT TABLE ttResult).
//RUN finalizar     IN hBoDadosRetorno. verificar o porque de dar erro no finalizar
//{esp/exportarEAbrirTabelaCsv3.i ttResult " " " " "ttResult"}
IF VALID-HANDLE(hBoDadosRetorno) THEN
 DELETE PROCEDURE hBoDAdosRetorno.


{include/i-rpclo.i &STREAM="stream str-rp"}
RUN pi-finalizar in h-acomp.
RETURN "Ok":U.

