/********************************************************************************************
PADRAO DE BO DE CONSULTA
Programa    : esbo/boDadosRetornoLisa
Autor       : Tadeu Silva Parreiras
Objetivo    : A Partir dos Pedidos Faturados para os estabelecimentos que retornam da procedure 
              getEstabsIntegraLISA, busca os dados dos pre-pedidos finalizados
Data        : 01/2024 
Modificacoes:
*********************************************************************************************/


&SCOPED-DEFINE ttParam  ttParam
&SCOPED-DEFINE boMsg    HBoMsg
&SCOPED-DEFINE ttResult ttResult
{esapi/analisarJsonObject2.i}

DEFINE TEMP-TABLE {&ttparam}  NO-UNDO
       FIELD dtEmisNota     AS DATE EXTENT 2
       FIELD serie          AS CHAR EXTENT 2
       FIELD nrNotaFis      AS CHAR EXTENT 2
       FIELD nrPedido       AS INT  EXTENT 2
       FIELD nrPrePedido    AS CHAR EXTENT 2
       .
DEFINE VARIABLE {&boMsg}        AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoConsParam    AS HANDLE      NO-UNDO.
DEFINE VARIABLE cSitsPedLisa    AS CHARACTER   NO-UNDO INIT "finalizado".

{esbo/boDadosRetorno.i {&ttResult} }


{esp/util.i}
{esp/setProp.i  {&ttparam} }
{lisa/extrairTtJsonPrePedido.i}

DEFINE VARIABLE hAcomp AS HANDLE      NO-UNDO.


PROCEDURE iniciar:

    RUN esbo/boMsg.p PERSIST SET {&boMsg}.
    CREATE {&ttparam}.

    RUN esbo/boConsParam.p PERSIST SET hBoConsParam.

    RUN esbo/boAcomp.p PERSIST SET hAcomp.
    
    
    
END PROCEDURE.

PROCEDURE finalizar:

    IF VALID-HANDLE({&boMsg}) THEN
       DELETE PROCEDURE {&boMsg}.

    IF VALID-HANDLE(hAcomp) THEN
    RUN finalizar IN hAcomp.

    DELETE PROCEDURE THIS-PROCEDURE.


END PROCEDURE.

PROCEDURE setSitsPedLisa:

    DEFINE INPUT  PARAMETER pSitsPedLisa AS CHARACTER   NO-UNDO.
    ASSIGN cSitsPedLisa = pSitsPedLisa .

END PROCEDURE.

PROCEDURE setAcomp:

    DEFINE INPUT  PARAMETER logHabilita AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pHAComp     AS HANDLE      NO-UNDO.

    RUN setHabilita IN hAcomp(logHabilita).
    IF valid-handle(phAcomp) THEN DO:
       RUN setHandle IN hAcomp(phAComp).
    END.
    ELSE DO:
       RUN setTitulo IN hAcomp('Extraá∆o Dados Ped.LISA').
    END.

    

END PROCEDURE.


PROCEDURE setBoMsg:


    DEFINE INPUT  PARAMETER pHBoMsg AS HANDLE      NO-UNDO.
    ASSIGN {&boMsg} = pHBoMsg.

END PROCEDURE.



PROCEDURE setValsIni:

    FIND FIRST {&ttParam} NO-ERROR.
    IF AVAIL {&ttParam} THEN DO:
       ASSIGN {&ttParam}.dtEmisNota[1]      = 01.01.2001
              {&ttParam}.dtEmisNota[2]      = 01.01.2999
              {&ttParam}.serie[1]           = ''
              {&ttParam}.serie[2]           = 'zzzzz'
              {&ttParam}.nrNotaFis[1]       = ''
              {&ttParam}.nrNotaFis[2]       = 'zzzzzzzzzz'
              {&ttParam}.nrPedido[1]        = 0
              {&ttParam}.nrPedido[2]        = 99999999 
              {&ttParam}.nrPrePedido[1]     = ''
              {&ttParam}.nrPrePedido[2]     = 'zzzzzzzzzzz'.
        RUN setSitsPedLisa('FINALIZADO').

              

    END.
    //setar os valores iniciais


END PROCEDURE.

PROCEDURE setTTParam:

    DEFINE INPUT PARAMETER TABLE FOR {&ttParam}.

END PROCEDURE.


PROCEDURE exec:
    DEFINE VARIABLE cListaEstab AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE logExiste   AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE cErros      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE hExtrair    AS HANDLE      NO-UNDO.
    DEFINE VARIABLE hImportar   AS HANDLE      NO-UNDO.
    DEFINE VARIABLE hConsultar  AS HANDLE      NO-UNDO.
    DEFINE VARIABLE hVerificar  AS HANDLE      NO-UNDO.
    RUN limparTTMsg IN {&boMsg}.
    FIND FIRST {&ttparam} NO-ERROR.
    IF NOT AVAIL {&ttParam} THEN DO:
       RUN setMsg IN {&bomsg}(1,'N∆o foram passados parametros','erro').
       RETURN 'nok'.
    END.
    RUN getEstabsIntegraLISA IN hBoConsParam(OUTPUT cListaEstab).
    FOR EACH nota-fiscal NO-LOCK
        WHERE lookup(nota-fiscal.cod-estabel,cListaEstab) > 0
        AND nota-fiscal.serie          >= {&ttParam}.serie[1]
        AND nota-fiscal.serie          <= {&ttParam}.serie[2]
        AND nota-fiscal.nr-nota-fis    >= {&ttParam}.nrNotaFis[1]
        AND nota-fiscal.nr-nota-fis    <= {&ttParam}.NrNotaFis[2]
        AND nota-fiscal.dt-emis-nota   >= {&ttParam}.dtEmisNota[1]
        AND nota-fiscal.dt-emis-nota   <= {&ttParam}.dtEmisNota[2]
        AND nota-fiscal.dt-cancela = ?
        ,
        /*
        EACH natur-oper OF nota-fiscal NO-LOCK
        WHERE natur-oper.tp-rec-desp = 1,*/
        EACH ped-venda NO-LOCK
        WHERE ped-venda.nome-abrev     = nota-fiscal.nome-ab-cli
        AND  ped-venda.nr-pedcli       = nota-fiscal.nr-pedcli
        AND  ped-venda.nr-pedido      >= {&ttParam}.nrPedido[1] 
        AND  ped-venda.nr-pedido      <= {&ttParam}.nrPedido[2]   ,
        EACH ped-venda-ext
            WHERE ped-venda-ext.cod-estabel = ped-venda.cod-estabel
            AND   ped-venda-ext.nr-pedido   = ped-venda.nr-pedido
            AND   ped-venda-ext.nr-pedext  >= {&ttParam}.nrPrePedido[1] 
            AND   ped-venda-ext.nr-pedext  <= {&ttParam}.nrPrePedido[2]
            NO-LOCK .
       
        RUN acomp IN hAcomp('NF:' + nota-fiscal.nr-nota-fis + ' - Verificando').

        CREATE {&ttResult}.
        ASSIGN {&ttResult}.nrNotaFis     =  nota-fiscal.nr-nota-fis
               {&ttResult}.nrPedido      =  ped-venda.nr-pedido
               {&ttResult}.nrPrePedido   =  ped-venda-ext.nr-pedext
               .

        RUN esapi/verifExistPedidoLisa.p PERSISTENT SET hVerificar(ped-venda.cod-estabel,
                                         ped-venda.nr-pedido,
                                         OUTPUT logExiste).
        IF VALID-HANDLE(hVerificar) THEN
           DELETE PROCEDURE hVerificar.

        IF logExiste THEN DO: 
           ASSIGN {&ttResult}.erros  =  'J† Importado'
                  {&ttResult}.logfinalizado = YES.
           NEXT.

        END.
        RUN acomp IN hAcomp('NF:' + nota-fiscal.nr-nota-fis + ' - Consultando na LISA').
        RUN lisa/consultarPedVenda.p PERSISTENT SET hConsultar(ped-venda-ext.nr-pedExt,
                                    OUTPUT cErros,
                                    OUTPUT TABLE ttJson ).
        IF VALID-HANDLE(hConsultar) THEN
           DELETE PROCEDURE hConsultar.
        ASSIGN {&ttResult}.erros = cErros.
        RUN acomp IN hAcomp('NF:' + nota-fiscal.nr-nota-fis + ' - Extraindo').
        RUN lisa/extrairTtJsonPrePedido.p PERSIST SET hExtrair(INPUT TABLE ttJson,  
                                          OUTPUT TABLE ttPedido,
                                          OUTPUT TABLE ttPedItem,
                                          OUTPUT TABLE ttPedItemFat,                      
                                          OUTPUT TABLE ttPedItemEtq). 
        IF VALID-HANDLE(hExtrair) THEN
           DELETE PROCEDURE hExtrair.
        RUN acomp IN hAcomp('NF:' + nota-fiscal.nr-nota-fis + ' - Incluindo Ped.Lisa').
        FIND FIRST ttPedido NO-ERROR.
        IF AVAIL ttPedido THEN DO:
           IF cSitsPedLisa = '' THEN
              ASSIGN cSitsPedLisa = 'FINALIZADO'.
           ASSIGN {&ttResult}.logFinalizado = lookup(ttPedido.situacaoPed,cSitsPedLisa) > 0
                  ttPedido.codEstabel       = nota-fiscal.cod-estabel. 
        END.
        ELSE DO:
           ASSIGN {&ttResult}.erros = "N∆o encontrado na base da LISA".
        END.
        

        //apenas pedidos finalizados na lisa ou que tenham recebido arquivo de retorno da lisa  ter∆o os seus dados importados, pois nesta situaá∆o n∆o ter∆o mais alteraá‰es de dados.
        IF {&ttResult}.logFinalizado or
           CAN-FIND(FIRST retornos_lisa WHERE int(retornos_lisa.nr_pedido) = ped-venda.nr-pedido)THEN DO:
           /* MESSAGE   '  entrei para importar os dados'
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
           RUN esapi/importarDadosPedidoLisa.p PERSIST SET hImportar(
                                                INPUT TABLE ttPedido,
                                                INPUT TABLE ttPedItem,
                                                INPUT TABLE ttPedItemFat,
                                                INPUT TABLE ttPedItemEtq).
        END.
            
         IF VALID-HANDLE(hImportar) THEN
            DELETE PROCEDURE hImportar.
    END.
    RUN finalizar IN hAcomp.


END PROCEDURE.


PROCEDURE exportarTTParam:

OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY +  'PARAMETROS_' + PROGRAM-NAME(1) + '.txt').
    FOR EACH {&ttParam}.
        DISP {&ttParam} WITH 1 COL WIDTH 550.
    END.
OUTPUT CLOSE.


END PROCEDURE.

PROCEDURE getErro:

    DEFINE OUTPUT PARAMETER pErro AS CHARACTER   NO-UNDO.
    RUN getErro IN {&boMsg}.

END PROCEDURE.

PROCEDURE getTTResult:

    DEFINE OUTPUT PARAMETER TABLE FOR ttResult.

END PROCEDURE.
