/**********************************************************************************************
programa: esbo/boSaldo.p
objetivo: Buscar o saldo em estoque e/ou programados dos produtos passados
por parametro considerando as vendas ainda n∆o integradas
pelo portal e a quantidade j† vendida e ainda n∆o faturada
Desenv: Tadeu
data: 11/2021 - 03/2022
**********************************************************************************************/
{esp/util.i}
DEFINE VARIABLE cItem               AS CHARACTER   NO-UNDO INIT '' .
//DEFINE VARIABLE cItemIni            AS CHARACTER   NO-UNDO INIT '' .
//DEFINE VARIABLE cItemFim            AS CHARACTER   NO-UNDO INIT 'zzzzzzzzzzz'.
DEFINE VARIABLE cRef                AS CHARACTER   NO-UNDO INIT ''.
DEFINE VARIABLE qtMinKg             AS DECIMAL     NO-UNDO INIT 0.
DEFINE VARIABLE qtMaxKg             AS DECIMAL     NO-UNDO INIT 99999999.
DEFINE VARIABLE qtMinMt             AS DECIMAL     NO-UNDO INIT 0.
DEFINE VARIABLE qtMaxMt             AS DECIMAL     NO-UNDO INIT 99999999.
DEFINE VARIABLE lFiltroQt           AS LOGICAL     NO-UNDO INIT NO.
/*DEFINE VARIABLE lPE                 AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lPI                 AS LOGICAL     NO-UNDO.*/


//DEFINE VARIABLE cRefIni             AS CHARACTER   NO-UNDO INIT ''.
//DEFINE VARIABLE cRefFim             AS CHARACTER   NO-UNDO INIT 'zzzzzz'.
DEFINE VARIABLE nrContainerIni      AS INTEGER NO-UNDO.
DEFINE VARIABLE nrContainerFim      AS INTEGER NO-UNDO.

DEFINE VARIABLE cListaPermContainer AS CHARACTER   NO-UNDO.

/*DEFINE VARIABLE cEstabIni           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEstabFim           AS CHARACTER   NO-UNDO.*/
DEFINE VARIABLE cLoginCorrente          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lConsiderarCarrinho     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lConsiderarNegativo     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lSaldoPorItem           AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lSaldoIncr              AS LOGICAL     NO-UNDO.
DEFINE VARIABLE hBoRepres               AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoPermisRepres         AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoLocaisEstoq          AS HANDLE      NO-UNDO.

DEFINE TEMP-TABLE ttLocais NO-UNDO LIKE locais_estoq_portal .

{esbo/boSaldo.i}
/*DEFINE TEMP-TABLE ttPermisContainer
    FIELD nrContainer AS INT. */

PROCEDURE iniciarBos:

    IF NOT VALID-HANDLE(hBoRepres) THEN
       RUN esbo/boRepres.p PERSIST SET hBoRepres.
    RUN iniciarBos IN hBoRepres.
    IF NOT VALID-HANDLE(hBoPermisRepres) THEN
       RUN esbo/boPermisRepres.p PERSIST SET hBoPermisRepres .
    IF NOT VALID-HANDLE(hBoLocaisEstoq) THEN DO:
       RUN esbo/boWeb100.p PERSIST SET hBoLocaisEstoq.
       RUN iniciar IN hBoLocaisEstoq.
    END.



END PROCEDURE.

PROCEDURE finalizarBos:

    IF VALID-HANDLE(hBoRepres) THEN DO:
       DELETE PROCEDURE hBoRepres.
    END.

    IF VALID-HANDLE(hBoPermisRepres) THEN DO:
       DELETE PROCEDURE hBoPermisRepres.
    END.

     IF VALID-HANDLE(hBoLocaisEstoq) THEN DO:
      RUN finalizar IN hBoLocaisEstoq.
    END.

END PROCEDURE.

PROCEDURE limparTtS:

    EMPTY TEMP-TABLE ttSaldo.
    EMPTY TEMP-TABLE ttPedidosEmAberto.
    EMPTY TEMP-TABLE ttEstabDepos.
    //EMPTY TEMP-TABLE ttPermisContainer.

END PROCEDURE.




PROCEDURE inserirEstabDepos:

    DEFINE INPUT  PARAMETER pEstab AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pDepos AS CHARACTER   NO-UNDO.
    CREATE ttEstabDepos.
    ASSIGN ttEstabDepos.codEstab = pEstab
           ttEstabDepos.codDepos = pDepos .


END PROCEDURE.

PROCEDURE getTtEstabDepos.

    DEFINE OUTPUT PARAMETER  TABLE FOR ttEstabDepos.

END PROCEDURE.

PROCEDURE getPermContainerRepres:

    RUN getContainersRepres IN hBoPermisRepres(OUTPUT cListaPermContainer).
    

END PROCEDURE.

PROCEDURE setFiltro:
    DEFINE INPUT  PARAMETER cChave AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER cValor AS CHARACTER   NO-UNDO.

    CASE cChave:
        WHEN 'it-codigo' THEN DO:
            ASSIGN cItem = cValor.
                /*cItemIni = cValor
                   cItemFIm = cValor .       */
        END.

        WHEN 'cod-refer' THEN DO:
            ASSIGN  cRef = cValor.
                    /*cRefIni = cValor
                   cRefFim = cValor .  */     
        END.
        WHEN 'qt_min_kg' THEN DO:
            ASSIGN qtMinKg = DECIMAL(cValor).
            IF DECIMAL(cValor) <> 0 THEN
               ASSIGN lFiltroQt = YES.
        END.
        WHEN 'qt_max_kg' THEN DO:
            ASSIGN qtMaxKg = DECIMAL(cValor).
            IF DECIMAL(cValor) <> 99999 THEN
               ASSIGN lFiltroQt = YES.

        END.
        WHEN 'qt_min_mt' THEN DO:
            ASSIGN qtMinMt = DECIMAL(cValor).
            IF DECIMAL(cValor) <> 0 THEN
                   ASSIGN lFiltroQt = YES.

        END.
        WHEN 'qt_max_mt' THEN DO:
            ASSIGN qtMaxMt = DECIMAL(cValor).
            IF DECIMAL(cValor) <> 99999 THEN
               ASSIGN lFiltroQt = YES.

        END.
        /*WHEN 'buscar_pe' THEN DO:
            ASSIGN lPE = LOGICAL(cValor).
        END.
        WHEN 'buscar_pi' THEN DO:
            ASSIGN lPI = LOGICAL(cValor).
        END.*/



     END CASE.


END PROCEDURE.

PROCEDURE setQtMinBook:

    DEFINE VARIABLE qtMinKG AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE qtMinMt AS DECIMAL     NO-UNDO.

    
    DEFINE VARIABLE hBo  AS HANDLE      NO-UNDO.

    RUN esbo/BoConsParam.p PERSIST SET hBo .
    RUN getQtMinKgBook IN hBo(OUTPUT qtMinKg).
    RUN getQtMinMtBook IN hBo(OUTPUT qtMinMt).

    RUN setFiltro('qt_min_kg',STRING(qtMinKg)).
    RUN setFiltro('qt_min_mt',STRING(qtMinMt)).


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

/*PROCEDURE getParamsLocaisEstoque:
    DEFINE VARIABLE cParam           AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cListaEstabs     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cListaDepos      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cConsDepFechado  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cEstabDepFechado AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cDeposDepFechado AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iCont            AS INTEGER     NO-UNDO.
    //preencher tabela tempor†ria de estabdepos com parametros gerais
    RUN getVlParametro('cods_estab_estoque', OUTPUT cListaEstabs).
    RUN getVlParametro('cods_depos_estoque', OUTPUT cListaDepos).
    RUN getVlParametro('considerar_deposito_fechado', OUTPUT cConsDepFechado).
    RUN getVlParametro('considerar_deposito_fechado', OUTPUT cConsDepFechado).
    RUN getVlParametro('cod_estab_deposito_fechado', OUTPUT cEstabDepFechado).
    RUN getVlParametro('cod_depos_deposito_fechado', OUTPUT cDeposDepFechado).

    REPEAT iCont = 1 TO NUM-ENTRIES(cListaEstabs).
        RUN inserirEstabDepos(ENTRY(iCont, cListaEstabs),ENTRY(iCont, cListaDepos)).
    END.
    
    IF cConsDepFechado = '1' THEN DO:
       RUN inserirEstabDepos(cEstabDepFechado,cDeposDepFechado).
    END.  

END PROCEDURE.*/

PROCEDURE setParamsLocaisEstoque:

    RUN setValsIni  IN hBoLocaisEstoq.
    RUN exec        IN hBoLocaisEstoq.
    RUN getTtResult IN hBoLocaisEstoq(OUTPUT TABLE ttLocais).
    EMPTY TEMP-TABLE ttEstabDepos.
    FOR EACH ttLocais:
        RUN inserirEstabDepos(ttLocais.cod_estab,ttLocais.cod_depos).
    END.

END PROCEDURE.


PROCEDURE getPermisRepres:
    DEFINE VARIABLE iCodRep AS INTEGER   NO-UNDO.
    RUN setNomeAbrev IN hBoRepres(cLoginCorrente).
    RUN getCodRep(OUTPUT iCodRep).
    

END PROCEDURE.


/*PROCEDURE getCarrinho:
    DEFINE OUTPUT PARAMETER dQt AS DECIMAL     NO-UNDO.
    ASSIGN dQt = qtCarrinho.

END PROCEDURE.

PROCEDURE getCarrinhoLoginCorrente:
    DEFINE OUTPUT PARAMETER dQt AS DECIMAL     NO-UNDO.
    ASSIGN dQt = qtCarrinhoLoginCorrente.

END PROCEDURE.*/


PROCEDURE setSaldoPorItem:
    DEFINE INPUT  PARAMETER pSaldoPorItem AS LOGICAL     NO-UNDO.
    ASSIGN lSaldoPorItem = pSaldoPorItem.

END PROCEDURE.

PROCEDURE getSaldoProgramado:
    DEFINE VARIABLE dSaldo        AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dSaldoSemPerc AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dPedida       AS DECIMAL     NO-UNDO.
    IF lSaldoPorItem THEN DO:
        FOR EACH pp-it-container NO-LOCK
             WHERE pp-it-container.it-codigo = cItem,
             EACH pp-container OF pp-it-container NO-LOCK
             WHERE pp-container.situacao = 1 
             AND (
                 (LOOKUP(cListaPermContainer,string(pp-container.nr-container)) > 0 AND pp-container.exclusivo = YES) OR pp-container.exclusivo = NO
                 ).
             ASSIGN dSaldo           = dSaldo        +  (pp-it-container.qt-pedida * pp-it-container.perc-dsp-venda) 
                    dSaldoSemPerc    = dSaldoSemPerc +  pp-it-container.qt-pedida
                    dPedida          = dPedida       +  pp-it-container.qt-vendida .
         END.
         RUN sincrTTSaldo(cItem,'*','qt_saldo_pi',dSaldo).     
         RUN sincrTTSaldo(cItem,'*','qt_saldo_pi_sem_perc',dSaldoSemPerc).     
         RUN sincrTTSaldo(cItem,'*','qt_pedida_pi',dPedida). 

    END.
    ELSE DO:
        FOR EACH pp-it-container NO-LOCK
            WHERE pp-it-container.it-codigo = cItem
            AND   pp-it-container.cod-refer = cRef ,
            EACH pp-container OF pp-it-container NO-LOCK
            WHERE pp-container.situacao = 1 
            AND (
                (LOOKUP(cListaPermContainer,string(pp-container.nr-container)) > 0 AND pp-container.exclusivo = YES) OR pp-container.exclusivo = NO
                ).
            ASSIGN dSaldo           = dSaldo        +  (pp-it-container.qt-pedida * pp-it-container.perc-dsp-venda) 
                   dSaldoSemPerc    = dSaldoSemPerc +  pp-it-container.qt-pedida
                   dPedida          = dPedida       + pp-it-container.qt-vendida .
        END.
        RUN sincrTTSaldo(cItem,cRef,'qt_saldo_pi',dSaldo).     
        RUN sincrTTSaldo(cItem,cRef,'qt_saldo_pi_sem_perc',dSaldoSemPerc).     
        RUN sincrTTSaldo(cItem,cRef,'qt_pedida_pi',dPedida).     

    END.
    
END PROCEDURE.



PROCEDURE getSaldoEstoque:
    DEFINE OUTPUT PARAMETER dQt AS DECIMAL     NO-UNDO.
    //acrescentar l¢gica para deposito fechado.
    DEFINE VARIABLE dTotItem    AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dTotItemRef AS DECIMAL     NO-UNDO.
    ASSIGN dTotItem = 0.
    RUN setSaldoIncremental(NO).
    IF lSaldoPorItem THEN DO:
       FOR EACH ttEstabDepos :
           FOR EACH saldo-estoq  NO-LOCK
               WHERE saldo-estoq.cod-estab = ttEstabDepos.codEstab
               AND   saldo-estoq.cod-depos = ttEstabDepos.codDepos 
               AND   saldo-estoq.it-codigo = cItem 
               AND   saldo-estoq.lote = saldo-estoq.cod-refer
               USE-INDEX dt-vali-lote.
               IF lConsiderarNegativo THEN
                  ASSIGN dTotItem = dTotItem  + saldo-estoq.qtidade-atu.
               ELSE 
                  ASSIGN dTotItem  = dTotItem + IF saldo-estoq.qtidade-atu < 0 THEN 
                                                0 ELSE saldo-estoq.qtidade-atu. 
           END.                                                                 
       END.  
       RUN sincrTTSaldo(cItem,'*','qt_saldo_pe',dTotItem).
       ASSIGN dQt = dtotItem.
    END.
    ELSE DO:
        FOR EACH ttEstabDepos :
            /*MESSAGE 'oi'
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
           FOR EACH saldo-estoq  NO-LOCK
               WHERE saldo-estoq.cod-estab = ttEstabDepos.codEstab
               AND   saldo-estoq.cod-depos = ttEstabDepos.codDepos 
               AND   saldo-estoq.it-codigo = cItem 
               AND   saldo-estoq.cod-refer = cRef
               AND   saldo-estoq.lote = saldo-estoq.cod-refer
               USE-INDEX dt-vali-lote BREAK BY saldo-estoq.cod-refer.
               ASSIGN dTotItemRef = dTotItemRef  + saldo-estoq.qtidade-atu.
               /*MESSAGE dTotItemRef 
                   VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
               IF LAST-OF(saldo-estoq.cod-refer) THEN DO:
                  RUN sincrTTSaldo(saldo-estoq.it-codigo,saldo-estoq.cod-refer,'qt_saldo_pe',dTotItemRef).     
               END.
           END.                                                                 
        END.  
        ASSIGN dQt = dtotItemRef.
    END.
END PROCEDURE.

PROCEDURE filtrarQt:
    IF lFiltroQt THEN DO:
       FOR EACH ttSaldo:
            FIND ITEM 
                WHERE ITEM.it-codigo =  ttSaldo.itCodigo NO-LOCK NO-ERROR.
            IF AVAIL ITEM THEN DO:
               CASE ITEM.un:
                   WHEN 'kg' THEN DO:
                      IF ttSaldo.qtSaldoPE + ttSaldo.qtSaldoPI < qtMinKg OR 
                         ttSaldo.qtSaldoPE + ttSaldo.qtSaldoPI > qtMaxKg THEN
                         DELETE ttSaldo.
                   END.
                   WHEN 'mt' OR WHEN 'mts' THEN DO:
                       IF ttSaldo.qtSaldoPE + ttSaldo.qtSaldoPI  < qtMinMt OR 
                          ttSaldo.qtSaldoPE +  ttSaldo.qtSaldoPI > qtMaxMt THEN
                         DELETE ttSaldo.
                   END.
               END CASE.
            END.         
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
        WHEN 'qt_saldo_pe' THEN  DO:
            IF lSaldoIncr THEN
               ASSIGN ttSaldo.qtSaldoPE = ttSaldo.qtSaldoPE + pQt .
            ELSE
              ASSIGN ttSaldo.qtSaldoPE =  pQt .
            
        END.
        WHEN 'qt_saldo_pi' THEN  DO:
            IF lSaldoIncr THEN
               ASSIGN ttSaldo.qtSaldoPI = ttSaldo.qtSaldoPI + pQt .
            ELSE
              ASSIGN ttSaldo.qtSaldoPI =  pQt .
            
        END.

        WHEN 'qt_saldo_pi_sem_perc' THEN  DO:
            IF lSaldoIncr THEN
               ASSIGN ttSaldo.qtSaldoPISemPerc = ttSaldo.qtSaldoPISemPerc + pQt .
            ELSE
              ASSIGN ttSaldo.qtSaldoPISemPerc =  pQt .
            
        END.

        WHEN 'qt_pedida_pe' THEN DO:
            IF lSaldoIncr THEN
               ASSIGN ttSaldo.qtPedidaPE = ttSaldo.qtPedidaPE + pQt .
            ELSE
              ASSIGN ttSaldo.qtPedidaPE =  pQt .
        END.

        WHEN 'qt_pedida_pi' THEN DO:
            IF lSaldoIncr THEN
               ASSIGN ttSaldo.qtPedidaPI = ttSaldo.qtPedidaPI + pQt .
            ELSE
              ASSIGN ttSaldo.qtPedidaPI =  pQt .
        END.
            
        WHEN 'qt_carrinho_pe' THEN DO:
            IF lSaldoIncr THEN
               ASSIGN ttSaldo.qtCarrinhoPE = ttSaldo.qtCarrinhoPE + pQt .
            ELSE
              ASSIGN ttSaldo.qtCarrinhoPE =  pQt .
        END.

        WHEN 'qt_carrinho_pi' THEN DO:
            IF lSaldoIncr THEN
               ASSIGN ttSaldo.qtCarrinhoPI = ttSaldo.qtCarrinhoPI + pQt .
            ELSE
              ASSIGN ttSaldo.qtCarrinhoPI =  pQt .
        END.                             

        WHEN 'qt_carrinho_login_pe' THEN DO:
            IF lSaldoIncr THEN
               ASSIGN ttSaldo.qtCarrinhoLoginPE = ttSaldo.qtCarrinhoLoginPE + pQt.
            ELSE
              ASSIGN ttSaldo.qtCarrinhoLoginPE =  pQt.
        END.


        WHEN 'qt_carrinho_login_pi' THEN DO:
            IF lSaldoIncr THEN
               ASSIGN ttSaldo.qtCarrinhoLoginPI = ttSaldo.qtCarrinhoLoginPI + pQt.
            ELSE
              ASSIGN ttSaldo.qtCarrinhoLoginPI =  pQt.
        END.



    END CASE.


END PROCEDURE.


PROCEDURE getQtPedidosEmAberto:
    DEFINE INPUT  PARAMETER pTipo  AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER dTotal AS DECIMAL     NO-UNDO.
    //fazer logica para gerar tabela temporaria de memoria de calculo e armazenar o valor conforme o tipo de pedido
    FOR EACH ped-venda NO-LOCK
        WHERE ped-venda.cod-sit-ped = 1
        AND ped-venda.tp-pedido = pTipo.
        FIND FIRST peds_web
            WHERE peds_web.nr_pedido_erp = ped-venda.nr-pedido
            NO-LOCK NO-ERROR.

        FOR EACH ped-item OF ped-venda NO-LOCK
            WHERE ped-item.cod-sit-item = 1
            AND   ped-item.it-codigo = cItem
            AND   ped-item.cod-refer = cRef .
            RUN inserirTTPedsEmAberto(INPUT pTipo,
                                      INPUT IF AVAIL peds_web THEN peds_web.ped_web_id ELSE 0,
                                      INPUT ped-venda.cod-estabel,
                                      INPUT ped-venda.nr-pedido,
                                      INPUT ped-item.it-codigo,
                                      INPUT ped-item.cod-refer,
                                      INPUT ped-item.qt-pedida,
                                      INPUT IF AVAIL peds_web THEN peds_web.ind_sit_ped_web ELSE 0     ).
            ASSIGN dTotal = dTotal + ped-item.qt-pedida.
        END.
        IF pTipo = 'pe' THEN DO:
           RUN sincrTTSaldo(cItem,cRef,'qt_pedida_pe',dTotal).      
        END.

    END.

END PROCEDURE.


PROCEDURE getQtPedidosNaoIntegrados:

    DEFINE INPUT  PARAMETER pTipo  AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE dTotal          AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dTotalLogin     AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dTotalCarrinho  AS DECIMAL     NO-UNDO.

    
    FOR EACH itens_ped_web 
        WHERE itens_ped_web.it_codigo = cItem
        AND   itens_ped_web.cod_refer = cRef  NO-LOCK,
        EACH peds_web 
        WHERE itens_ped_web.ped_web_id = peds_web.ped_web_id
        AND peds_web.ind_sit_ped_web <> 4. //n∆o integrado
        RUN inserirTTPedsEmAberto(INPUT pTipo,
                                      INPUT peds_web.ped_web_id,
                                      INPUT peds_web.cod_estab,
                                      INPUT 0,
                                      INPUT itens_ped_web.it_codigo,
                                      INPUT itens_ped_web.cod_refer,
                                      INPUT itens_ped_web.qt_pedida,
                                      INPUT peds_web.ind_sit_ped_web  ).

        ASSIGN dTotal = dTotal + ped-item.qt-pedida.
        IF peds_web.ind_sit_ped_web = 1 // em digitacao 
        OR peds_web.ind_sit_ped_web = 9 // alteraá∆o solic. gerencia
        THEN DO:
            IF peds_web.login     = cLoginCorrente THEN DO:
               ASSIGN dTotalLogin = dTotalLogin + ped-item.qt-pedida.
            END.
            ASSIGN dTotalCarrinho = dTotalCarrinho + ped-item.qt-pedida.
        END.                                                     
    END.
    RUN sincrTTSaldo(cItem,cRef,IF pTipo = 'pe' THEN 'qt_pedida_pe' ELSE 'qt_pedida_pi',
                                dTotal).      
    RUN sincrTTSaldo(cItem,cRef,IF pTipo = 'pe' THEN 'qt_carrinho_pe' ELSE 'qt_carrinho_pi',
                                dTotalCarrinho).      
    RUN sincrTTSaldo(cItem,cRef,IF pTipo = 'pe' THEN 'qt_carrinho_login_pe' ELSE 'qt_carrinho_login_pi',
                                dTotalLogin).      



END PROCEDURE.

PROCEDURE inserirTtPedsEmAberto:

    DEFINE INPUT  PARAMETER ptipoPed        AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pPedWebId       AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pCodEstab       AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pNrPedido       AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pItCodigo       AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pCodRefer       AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pQuant          AS DECIMAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pIndSitPedWeb   AS INTEGER     NO-UNDO.

    CREATE ttPedidosEmAberto.
    ASSIGN ttPedidosEmAberto.pedwebId      = pPedWebId     
           ttPedidosEmAberto.codEstab      = pCodEstab     
           ttPedidosEmAberto.nrPedido      = pNrPedido     
           ttPedidosEmAberto.itCodigo      = pItCodigo     
           ttPedidosEmAberto.codRefer      = pCodRefer     
           ttPedidosEmAberto.quantidade    = pQuant        
           ttPedidosEmAberto.indSitPedweb  = pIndSitPedWeb.   

END PROCEDURE.


PROCEDURE getSaldoItemRef:
    DEFINE INPUT  PARAMETER pSaldoPorItem   AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pBuscarPE       AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pBuscarPI       AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pConsiderarNeg  AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pItem           AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pRef            AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER qtMin           AS DECIMAL     NO-UNDO.
    DEFINE INPUT  PARAMETER qtMax           AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dQt AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE un AS CHARACTER   NO-UNDO.

    
    RUN limparTTs.
    RUN setSaldoPorItem(pSaldoPorItem).
    RUN setParamsLocaisEstoque.
    RUN setFiltro('it-codigo',pItem).
    RUN setFiltro('cod-refer',pRef).
    RUN setConsiderarNegativo(pConsiderarNeg).

    IF pBuscarPE THEN DO:
       RUN getSaldoEstoque(OUTPUT dQt).
    END.
    IF pBuscarPI THEN DO:
       RUN getSaldoProgramado.
    END.
    RUN _setFiltroQts(pItem,qtMin,QtMax).
    RUN filtrarQt.


END PROCEDURE.


PROCEDURE _setFiltroQts:
    DEFINE INPUT  PARAMETER pItem AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER qtMin AS DECIMAL     NO-UNDO.
    DEFINE INPUT  PARAMETER qtMax AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE un AS CHARACTER   NO-UNDO.
    FIND ITEM WHERE ITEM.it-codigo =  pItem NO-LOCK NO-ERROR.
    IF AVAIL ITEM THEN
       ASSIGN un = ITEM.un.
    CASE un:
        WHEN 'kg' THEN DO:
            RUN setFiltro('qt_min_kg',STRING(qtMin)).
            RUN setFiltro('qt_max_kg',STRING(qtMax)).
        END.
        WHEN 'mt' OR WHEN 'mts' THEN DO:
            RUN setFiltro('qt_min_mt',STRING(qtMin)).
            RUN setFiltro('qt_max_mt',STRING(qtMax)).
        END.                        
    END CASE.

END PROCEDURE.


PROCEDURE getTTSaldo:
    DEFINE OUTPUT PARAMETER TABLE FOR ttSaldo.

END PROCEDURE.

PROCEDURE getTTPedidosEmAberto:
    DEFINE OUTPUT PARAMETER TABLE FOR ttPedidosEmAberto.
END PROCEDURE.
