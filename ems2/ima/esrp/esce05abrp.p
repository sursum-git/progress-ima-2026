/* PROGRAMA: esimft05abrp.P                                                   **
** DATA    : 09/2024                                                          **
** AUTOR   : Tadeu Silva                                                      **
** OBJETIVO: Extraá∆o de Dados de Movimentaá∆o de Estoque para analise        **
             gerencial                                                        **
******************************************************************************/

/* Programa de controle de versao e seguranªa do Datasul EMS */
{include/i-prgvrs.i esimft05abrp 2.06.00.001}
{esp/util.i}

/* Definicao de variaveis globais comuns a todos os programas do EMS */
{utp/ut-glob.i}
DEF VAR h-acomp                  AS HANDLE  NO-UNDO.
DEF VAR da-dt-calc               AS DATE    FORMAT "99/99/9999".
DEFINE VARIABLE hBoAnaliseEstoq  AS HANDLE      NO-UNDO.
DEFINE VARIABLE cNivel           AS CHARACTER   NO-UNDO.
{esrp/esce05ab.i}
{esbo/boAnaliseEstoq.i}

    



/*------------------- Excel-------------------------*/



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
ASSIGN c-programa     = "esce05abrp":U
       c-versao       = "2.06":U
       c-revisao      = ".00.001":U
       c-empresa      = empresa.razao-social
       c-sistema      = "EMS":U.

ASSIGN c-titulo-relat = "RELAT‡RIO DE ANALISE DE MOVIMENTAÄ«O DE ESTOQUE NO TEMPO":U.



VIEW FRAME f-cabec.
VIEW FRAME f-rodape.
PUT  "inicio:" NOW SKIP.
/******************************* Calcula Movimentos *******************************/
RUN esbo/boAnaliseEstoq.p PERSIST SET hBoAnaliseEstoq.
RUN iniciar     IN hBoAnaliseEstoq.
RUN setValsIni  IN hBoAnaliseEstoq.
RUN setAcomp    IN hBoAnaliseEstoq(YES,?).
FIND FIRST tt-param NO-ERROR.
IF AVAIL tt-param THEN DO:
   RUN setProp IN hBoAnaliseEstoq('itCodigo'    ,1, tt-param.fi-it-codigo-ini).
   RUN setProp IN hBoAnaliseEstoq('itCodigo'    ,2,tt-param.fi-it-codigo-fim).
   RUN setProp IN hBoAnaliseEstoq('codRefer'    ,1,tt-param.fi-cod-refer-ini).
   RUN setProp IN hBoAnaliseEstoq('codRefer'    ,2,tt-param.fi-cod-refer-fim).
   RUN setProp IN hBoAnaliseEstoq('dtTrans'     ,1,tt-param.fi-dt-trans-ini).
   RUN setProp IN hBoAnaliseEstoq('dtTrans'     ,2,tt-param.fi-dt-trans-fim).
   RUN setProp IN hBoAnaliseEstoq('codEstabel'  ,1,tt-param.cod-estabel-ini).
   RUN setProp IN hBoAnaliseEstoq('codEstabel'  ,2,tt-param.cod-estabel-fim).
   RUN setProp IN hBoAnaliseEstoq('geCodigo'    ,1,fi-ge-codigo-ini).
   RUN setProp IN hBoAnaliseEstoq('geCodigo'    ,2,fi-ge-codigo-fim).
   RUN setProp IN hBoAnaliseEstoq('logApenasFat',0,log-so-faturaveis). 
END.
ELSE 
   RETURN 'nok'.
RUN exec                        IN hBoAnaliseEstoq.

CASE tt-param.nivelDetalhe:
    WHEN 1 THEN DO:
        ASSIGN cNivel = 'container'.        
    END.
    WHEN 2 THEN DO:
       ASSIGN cNivel = 'etiqueta'.        
    END.
END CASE.

IF cNivel <> '' THEN DO:
   RUN getTTMovtoContainer IN hBoAnaliseEstoq(cNivel).    
END.




RUN exportarttResult            IN hBoAnaliseEstoq('movto_estoque.txt',"|").
RUN exportarEstatistica         IN hBoAnaliseEstoq('estatistica_prod_ref.txt',"|").
RUN exportarEstatisticaProduto  IN hBoAnaliseEstoq('estatistica_prod.txt',"|").
RUN exportarTTSaldo             IN hBoAnaliseEstoq('saldo_calculado.txt',"|").
RUN exportarTTData              IN hBoAnaliseEstoq('saldo_data.txt',"|").
RUN exportarTTComp              IN hBoAnaliseEstoq('saldo_comp.txt',"|").   
RUN expttMsg                    IN hBoAnaliseEstoq(SESSION:TEMP-DIRECTORY + 'log_ce05abrp-boAnaliseEstoq.txt').
RUN finalizar                   IN hBoAnaliseEstoq.

RUN esapi/abrirExcel.p(SEARCH('excel/movtoestoq.xltx') ).

PUT  "Fim:" NOW SKIP.
RETURN "OK":U.

