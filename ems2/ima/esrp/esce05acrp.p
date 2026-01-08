/* PROGRAMA: esimft05abrp.P                                                   **
** DATA    : 09/2024                                                          **
** AUTOR   : Tadeu Silva                                                      **
** OBJETIVO: Extraá∆o de Dados de Saldos Periodos de Estoque para analise     **
             gerencial                                                        **
******************************************************************************/

/* Programa de controle de versao e seguranªa do Datasul EMS */
{include/i-prgvrs.i esce05abrp 2.06.00.001}
{esp/util.i}

/* Definicao de variaveis globais comuns a todos os programas do EMS */
{utp/ut-glob.i}
//DEF VAR h-acomp     AS HANDLE  NO-UNDO.
DEFINE VARIABLE hBoSaldo  AS HANDLE      NO-UNDO.
DEFINE VARIABLE cArqExcel AS CHARACTER   NO-UNDO.

/*RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Processando..").*/


{esrp/esimft05ac.i}
{esbo/boSlItPer.i ttresult sl-it-per}

    
/*---------------------------------------------------*/
DEFINE NEW SHARED TEMP-TABLE tt-digita NO-UNDO
    FIELD ordem              AS INTEGER   FORMAT ">>>>9"
    FIELD nr-pedcli          AS CHAR
    FIELD nome-abrev         AS CHAR
    FIELD l-status           AS LOG.

/* Parametros de entrada logica obrigatoria */
DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita                   AS RAW.

DEF BUFFER empresa FOR mgcad.empresa.
DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.   


CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

FOR EACH tt-raw-digita :
    CREATE tt-digita.
    RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
END.

{include/i-rpvar.i}   

/* ABERTURA DO ARQUIVO DE SA÷DA (ARQUIVO/IMPRESSORA) CORREPONDE A INCLUDE CDP/CD9520.I (MAGNUS) */
{include/i-rpout.i}
{include/i-rpcab.i}

FORM HEADER "Per°odo entre"          AT 01
            tt-param.fi-dt-trans-ini AT 15 FORMAT "99/99/9999"
            "e"                      AT 26
            tt-param.fi-dt-trans-fim AT 28 FORMAT "99/99/9999"
            SKIP
            FILL("-",132)           AT 01 FORMAT "x(132)" 
            WITH FRAME f-cabec. 

/* LOCALIZACAO DO NOME DA EMPRESA PARA COMPOR O CABECALHO PADRAO */
FIND empresa NO-LOCK WHERE empresa.ep-codigo = i-ep-codigo-usuario NO-ERROR.
IF NOT AVAIL empresa THEN
   RETURN "ADM-ERROR":U.

/* VALORIZACAO DAS OUTRAS VARIAVEIS QUE COMPOEM O CABECALHO PADRAO */
ASSIGN c-programa     = "esce05acrp":U
       c-versao       = "2.06":U
       c-revisao      = ".00.001":U
       c-empresa      = empresa.razao-social
       c-sistema      = "EMS":U.

ASSIGN c-titulo-relat = "Relat¢rio de Saldo de Produtos por Per°odo  ":U.


VIEW FRAME f-cabec.
VIEW FRAME f-rodape.
PUT  "inicio:" NOW SKIP.
RUN esbo/boSlItPer.p PERSIST SET hBoSaldo.
RUN iniciar     IN hBoSaldo.
RUN setValsIni  IN hBoSaldo.
RUN setAcomp IN hBoSaldo(YES,?).
FIND FIRST tt-param NO-ERROR.
IF AVAIL tt-param THEN DO:
   RUN setProp IN hBoSaldo('itCodigo',1, tt-param.fi-it-codigo-ini).
   RUN setProp IN hBoSaldo('itCodigo',2,tt-param.fi-it-codigo-fim).
   RUN setProp IN hBoSaldo('periodo',1,tt-param.fi-dt-trans-ini).
   RUN setProp IN hBoSaldo('periodo',2,tt-param.fi-dt-trans-fim).
   RUN setProp IN hBoSaldo('codEstabel',1,tt-param.cod-estabel-ini).
   RUN setProp IN hBoSaldo('codEstabel',2,tt-param.cod-estabel-fim).
   RUN setProp IN hBoSaldo('geCodigo',1,fi-ge-codigo-ini).
   RUN setProp IN hBoSaldo('geCodigo',2,fi-ge-codigo-fim).
   RUN setProp IN hBoSaldo('logApenasFat',0,log-so-faturaveis). 
END.
ELSE 
   RETURN 'nok'.

RUN exec             IN hBoSaldo.
RUN exportarttResult IN hBoSaldo('saldo_estoque.txt',"|").
RUN finalizar   IN hBoSaldo.

ASSIGN cArqExcel = SEARCH('excel/saldoEstoqueProdPeriodo.xlsx').
IF cArqExcel <> ? THEN
  OS-COMMAND VALUE('start excel /t ' + cArqExcel ).
ELSE 
  MESSAGE 'Arquivo modelo do Excel n∆o encontrado'
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

PUT  "fim:" NOW SKIP.








/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
RETURN "OK":U.
