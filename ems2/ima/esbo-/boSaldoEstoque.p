/*
programa: esbo/boSaldoEstoque.p
objetivo: Buscar o saldo em estoque dos produtos passados
por parametro considerando as vendas ainda nÆo integradas
pelo portal e a quantidade j  vendida e ainda nÆo faturada
Desenv: Tadeu
data: 11/2021
*/

DEFINE VARIABLE cItemIni            AS CHARACTER   NO-UNDO INIT '' .
DEFINE VARIABLE cItemFim            AS CHARACTER   NO-UNDO INIT 'zzzzzzzzzzz'.
DEFINE VARIABLE cRefIni             AS CHARACTER   NO-UNDO INIT ''.
DEFINE VARIABLE cRefFim             AS CHARACTER   NO-UNDO INIT 'zzzzzz'.
DEFINE VARIABLE cEstabIni           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEstabFim           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLoginCorrente      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lConsiderarCarrinho AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lConsiderarNegativo AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lSaldoPorItem       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lSaldoIncr          AS LOGICAL     NO-UNDO.

DEFINE TEMP-TABLE ttSaldo
    FIELD itCodigo        AS CHAR
    FIELD codRefer        AS CHAR
    FIELD qtSaldo         AS DECIMAL
    FIELD qtPedida        AS DECIMAL
    FIELD qtCarrinho      AS DECIMAL
    FIELD qtCarrinhoLogin AS DECIMAL
    INDEX ind AS PRIMARY UNIQUE itCodigo codRefer.

        

PROCEDURE setFiltro:
    DEFINE INPUT  PARAMETER cChave AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER cValor AS CHARACTER   NO-UNDO.

    CASE cChave:
        WHEN 'it-codigo' THEN DO:
            ASSIGN cItemIni = cValor
                   cItemFIm = cValor .       
        END.

        WHEN 'cod-refer' THEN DO:
            ASSIGN cRefIni = cValor
                   cRefFim = cValor .       
        END.

        WHEN 'cod-estabel' THEN DO:
            ASSIGN cEstabIni = cValor
                   cEstabFim = cValor .       
        END.
    END CASE.


END PROCEDURE.

PROCEDURE setLoginCorrente:

    DEFINE INPUT  PARAMETER pLogin AS CHARACTER   NO-UNDO.
    ASSIGN cLoginCorrente = pLogin .

END PROCEDURE.

PROCEDURE  setConsiderarCarrinho:

    DEFINE INPUT  PARAMETER pConsiderarCarrinho AS LOGICAL     NO-UNDO.
    ASSIGN lConsiderarCarrinho =  pConsiderarCarrinho .


END PROCEDURE.

PROCEDURE setConsiderarNegativo:
    DEFINE INPUT  PARAMETER pConsiderarNegativo AS LOGICAL     NO-UNDO.
    ASSIGN lConsiderarNegativo = pConsiderarNegativo.
END PROCEDURE.

PROCEDURE setCodEstab:


END PROCEDURE.

PROCEDURE getCarrinho:

END PROCEDURE.

PROCEDURE getCarrinhoLoginCorrente:

END PROCEDURE.


PROCEDURE setSaldoPorItem:
    DEFINE INPUT  PARAMETER pSaldoPorItem AS LOGICAL     NO-UNDO.
    ASSIGN lSaldoPorItem = pSaldoPorItem.

END PROCEDURE.

PROCEDURE getSaldoEstoque:
    DEFINE VARIABLE dTotItem AS DECIMAL     NO-UNDO.
    ASSIGN dTotItem = 0.
    RUN setSaldoIncremental(NO).
    FOR EACH saldo-estoq  NO-LOCK
        WHERE saldo-estoq.it-codigo     >= cItemIni
        AND   saldo-estoq.it-codigo     <= cItemFim
        AND   saldo-estoq.cod-estabel   >= cEstabIni
        AND   saldo-estoq.cod-estabel   <= cEstabFim
        AND   saldo-estoq.cod-refer     >= cRefIni
        AND   saldo-estoq.cod-refer     <= cRefFim
        AND   saldo-estoq.cod-depos     = 'arm' 
        BREAK BY saldo-estoq.it-codigo.
        IF lSaldoPorItem THEN DO:
           IF lConsiderarNegativo THEN
               ASSIGN dTotItem = dTotItem  + saldo-estoq.qtidade-atu.
           ELSE 
              ASSIGN dTotItem  = dTotItem + IF saldo-estoq.qtidade-atu < 0 THEN 
                                            0 ELSE saldo-estoq.qtidade-atu. 
           IF LAST-OF(saldo-estoq.it-codigo) THEN DO:
               RUN sincrTTSaldo(saldo-estoq.it-codigo,'*','qt_saldo',dTotItem).
               ASSIGN dTotItem = 0.
           END.
        END.
        ELSE DO:
           RUN sincrTTSaldo(saldo-estoq.it-codigo,saldo-estoq.cod-refer,'qt_saldo',saldo-estoq.qtidade-atu).     
        END.    
    END.
END PROCEDURE.

PROCEDURE setSaldoIncremental.

    DEFINE INPUT  PARAMETER pSaldoIncr AS LOGICAL     NO-UNDO.
    ASSIGN lSaldoIncr = pSaldoIncr.

END PROCEDURE.
        
PROCEDURE sincrTtSaldo.
    DEFINE INPUT  PARAMETER pItCodigo AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pCodRefer AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pCampo    AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pQt       AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dSaldoIncr AS DECIMAL     NO-UNDO.

    FIND ttSaldo
        WHERE ttSaldo.itCodigo = pItCodigo
        AND   ttSaldo.codRefer = pCodRefer
        NO-ERROR.
    IF NOT AVAIL ttSaldo THEN DO:
       CREATE ttSaldo.
       ASSIGN ttSaldo.itCodigo = pItCodigo
              ttSaldo.codRefer = pCodRefer.
    END.

    CASE pCampo:
        WHEN 'qt_saldo' THEN  DO:
            IF lSaldoIncr THEN
               ASSIGN ttSaldo.qtSaldo = ttSaldo.qtSaldo + pQt .
            ELSE
              ASSIGN ttSaldo.qtSaldo =  pQt .
            
        END.
        WHEN 'qt_pedida' THEN DO:
            IF lSaldoIncr THEN
               ASSIGN ttSaldo.qtPedida = ttSaldo.qtPedida + pQt .
            ELSE
              ASSIGN ttSaldo.qtPedida =  pQt .
        END.
            
        WHEN 'qt_carrinho' THEN DO:
            IF lSaldoIncr THEN
               ASSIGN ttSaldo.qtCarrinho = ttSaldo.qtCarrinho + pQt .
            ELSE
              ASSIGN ttSaldo.qtCarrinho =  pQt .
        END.
        WHEN 'qt_carrinho_login' THEN DO:
            IF lSaldoIncr THEN
               ASSIGN ttSaldo.qtCarrinhoLogin = ttSaldo.qtCarrinhoLogin + pQt.
            ELSE
              ASSIGN ttSaldo.qtCarrinhoLogin =  pQt.
        END.
    END CASE.


END PROCEDURE.
