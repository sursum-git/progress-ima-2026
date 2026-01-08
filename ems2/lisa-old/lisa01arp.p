 {esp/USING_json.i}
 /* include de controle de vers’o */
{include/i-prgvrs.i lisa01arp 2.06.00.001}
DEFINE VARIABLE h-acomp         AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoDadosRetorno AS HANDLE      NO-UNDO.

{esp/utilJson.i}
{esapi/analisarJsonObject2.i}
DEFINE VARIABLE cDiretorio      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lSelecionouDir  AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cArquivo        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE oJsonRetorno    AS jsonObject  NO-UNDO.

DEFINE VARIABLE cNovoArquivo    AS CHARACTER   NO-UNDO.

DEFINE VARIABLE logJaProcessado AS LOGICAL     NO-UNDO.



{esapi/extrairDadosNFRetorno.i}
{lisa/lisa01a.i}
{esapi/getArqsDir.i}
{esp/util.i}

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

ASSIGN c-programa = "lisa01aRP"
       c-versao	  = "2.06"
       c-revisao  = ".00.001"
       c-empresa  = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

ASSIGN c-sistema = 'ERP'.

ASSIGN c-titulo-relat = 'Retornos LISA - Importa‡Æo'.

VIEW STREAM str-rp FRAME f-cabec.
VIEW STREAM str-rp FRAME f-rodape.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

RUN pi-inicializar IN h-acomp("Analise de JSON de Retorno").
RUN pi-acompanhar IN h-acomp("Buscando Arquivos no diret¢rio:" + tt-param.diretorioJson).

PUT STREAM str-rp  UNFORM "Inicio:" NOW SKIP.
RUN esapi/getArqsDir.p(tt-param.diretorioJson, NO, INPUT-OUTPUT TABLE ttArquivo ).
FOR EACH ttArquivo:
      ASSIGN cArquivo = ttArquivo.diretorio + getSeparadorDir(ttArquivo.diretorio) +
                        ttArquivo.arquivo.
      RUN pi-acompanhar IN h-acomp("Arquivo:" + ttArquivo.arquivo).
      PUT STREAM str-rp UNFORM  ttArquivo.Arquivo "-" NOW SKIP.
      IF SEARCH(cArquivo) <> ? AND getExtensaoArq(cArquivo) = 'json' THEN DO:
         FILE-INFO:FILE-NAME = cArquivo.
         RUN convFileJson2JsonObject(cArquivo,OUTPUT oJsonRetorno).
         RUN esapi/analisarJsonObject2.p(oJsonRetorno,OUTPUT TABLE ttJson) .
         RUN insertTtJson('','nome_arquivo',cArquivo,0,NO).
         RUN insertTtJson('','dt_hr_recbto',string(FILE-INFO:FILE-CREATE-DATE) + " " + STRING(FILE-INFO:FILE-CREATE-TIME,'hh:mm:ss'),0,NO).
         RUN esapi/extrairDadosNFRetorno.p(INPUT TABLE ttJson, OUTPUT logJaProcessado,OUTPUT TABLE ttRomaneioErro) .
         ASSIGN  ttArquivo.resultado = "Arquivo Analisado com Sucesso".
         IF logJaProcessado THEN DO:
            ASSIGN ttArquivo.resultado = ttArquivo.resultado + "|Pedido J  registrado e movido. Ser  deletado ".
            OS-DELETE value(cArquivo).
            RUN pi-acompanhar IN h-acomp("Deletando Arquivo Duplicado:" + ttArquivo.arquivo).
            NEXT.
         END.    
      END.                                                                 
      ELSE DO:
         ASSIGN ttArquivo.logErro   = TRUE
                ttArquivo.resultado = "Arquivo:" + cArquivo + " NÆo encontrado ou diferente de json" . 
      END.
      IF ttArquivo.logErro = FALSE THEN DO:
         RUN pi-acompanhar IN h-acomp("Movendo Arquivo:" + ttArquivo.arquivo).
         RUN esapi/moverArqRetornoRegistrado.p(cArquivo, OUTPUT cNovoArquivo).
         ASSIGN ttArquivo.resultado = ttArquivo.resultado + "|Pedido foi movido para o arquivo:" + cNovoArquivo.
      END.
END.



PUT STREAM str-rp  UNFORM "Fim:" NOW SKIP.
{include/i-rpclo.i &STREAM="stream str-rp"}
{esp/exportarTabelacsv3.i ttArquivo " " " " "  "ttArquivosRetorno" }
{esp/exportarTabelacsv3.i ttRomaneioErro " " " " "  "ttRomaneioErro" }
RUN pi-finalizar in h-acomp.
RETURN "Ok":U.

