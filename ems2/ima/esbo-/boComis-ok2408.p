/*
* Programa: esbo/boComis.p
* Objetivo: Tratar todas s regras e calculos de comiss∆o customizados
* Autor: Tadeu Silva
* Data: 08/2020
* 27/04/2021 -tadeu - inclusao de tratamento de reduá∆o de comiss∆o 
* conforme tabela de preáo.
*/



DEFINE VARIABLE iCliente        AS INTEGER     NO-UNDO.
DEFINE VARIABLE iClienteTriang  AS INTEGER     NO-UNDO.
DEFINE VARIABLE ufCliente       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ufClienteTriang AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vlMinimoBonus   AS DECIMAL     NO-UNDO.
DEFINE VARIABLE vlTotPed        AS DECIMAL     NO-UNDO.
DEFINE VARIABLE iPrazoMedio     AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCondPagto      AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCodRep         AS INTEGER     NO-UNDO.
DEFINE VARIABLE iTbPreco        AS INTEGER     NO-UNDO.
DEFINE VARIABLE cEstab          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE nrContainer     AS INTEGER     NO-UNDO.
DEFINE VARIABLE nrPedido        AS INTEGER     NO-UNDO.
DEFINE VARIABLE pedWebId        AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMoeda          AS INTEGER     NO-UNDO.

{utp/ut-glob.i}
{esbo\boMsg.i}
{seniuz/esp/espd4000.i}
{esbo/bocomis.i}


DEFINE VARIABLE hBoMsgComis         AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoRepres           AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoCondPagtoPed     AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoPrecosItemRef    AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoTbPreco          AS HANDLE      NO-UNDO.
PROCEDURE limparTts:
 EMPTY TEMP-TABLE ttCondPed.
 EMPTY TEMP-TABLE ttPedItem.
 EMPTY TEMP-TABLE tt-itens-ped.
END PROCEDURE.

PROCEDURE iniciarBos:
    IF NOT VALID-HANDLE(hBoMsgComis) THEN
       RUN esbo/boMsg.p PERSISTENT SET hBoMsgComis.
    IF NOT VALID-HANDLE(hBoRepres) THEN do:
       RUN esbo/boRepres.p PERSISTENT SET hBoRepres.
       RUN iniciarBos IN hBoRepres.
    END.
    IF NOT VALID-HANDLE(hBoPrecosItemRef) THEN do:
       RUN esbo/boPrecosItemRef.p PERSISTENT SET hBoPrecosItemRef.
       RUN iniciarBos IN hBoPrecosItemRef.
    END.

    IF NOT VALID-HANDLE(hBoCondPagtoPed) THEN do:
       RUN esbo/boCondPagtoPed.p PERSISTENT SET hBoCondPagtoPed.
    END.

    IF NOT VALID-HANDLE(hBoTbPreco) THEN DO:
       RUN esbo/boTbPreco.p PERSISTENT SET hBoTbPreco.
    END.
END PROCEDURE.


PROCEDURE finalizarBos:
    IF VALID-HANDLE(hBoMsgComis) THEN
       DELETE PROCEDURE hBoMsgComis.

    IF VALID-HANDLE(hBoRepres) THEN do:
       RUN finalizarBos IN hBoRepres.
       DELETE PROCEDURE hBoRepres.
    END.
    IF VALID-HANDLE(hBoPrecosItemRef) THEN do:
       RUN finalizarBos IN hBoPrecosItemRef.
       DELETE PROCEDURE hBoPrecosItemRef.
    END.

    IF VALID-HANDLE(hBoCondPagtoPed) THEN do:
       DELETE PROCEDURE hBoCondPagtoPed.
    END.

    IF VALID-HANDLE(hBoTbPreco) THEN do:
       DELETE PROCEDURE hBoTbPreco.
    END.

END PROCEDURE.

PROCEDURE setTbPreco:
    DEFINE INPUT  PARAMETER pTb AS INTEGER     NO-UNDO.
    ASSIGN iTbPreco  = pTb.
END PROCEDURE.

PROCEDURE setNrPedido:
    DEFINE INPUT  PARAMETER pNrPedido AS INTEGER     NO-UNDO.
    ASSIGN nrPedido = pNrPedido.

END PROCEDURE.

PROCEDURE setMoeda:
    DEFINE INPUT  PARAMETER pMoeda AS INTEGER     NO-UNDO.
    ASSIGN iMoeda = pMoeda.


END.

PROCEDURE setPedWebId:
    DEFINE INPUT  PARAMETER pPedWebId AS INTEGER     NO-UNDO.
    ASSIGN pedwebid = pPedwebId.
END.


PROCEDURE setContainer:
DEFINE INPUT  PARAMETER pNrContainer AS INTEGER     NO-UNDO.
ASSIGN nrContainer = pNrContainer.

END PROCEDURE.
PROCEDURE setCliente:
    DEFINE INPUT  PARAMETER pcliente AS INTEGER     NO-UNDO.
    ASSIGN icliente =  pCliente.

END PROCEDURE.

PROCEDURE setClienteTriang:
    DEFINE INPUT  PARAMETER pClienteTriang AS INTEGER     NO-UNDO.
    ASSIGN iclienteTriang =  pClienteTriang.

END PROCEDURE.

PROCEDURE setCondPagto.
    DEFINE INPUT  PARAMETER pcondPagto AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iCont AS INT  NO-UNDO.
    DEFINE VARIABLE dSoma AS DECIMAL     NO-UNDO.
    ASSIGN icondPagto = pCondPagto.
    IF icondPagto <> 0 THEN DO:
       FIND cond-pagto
           WHERE cond-pagto.cod-cond-pag = iCondPagto
           NO-LOCK NO-ERROR.
       IF AVAIL cond-pagto THEN DO:
          ASSIGN iPrazoMedio =  cond-pagto.qtd-dias-prazo-medio .
       END.
       ELSE DO:
          ASSIGN iPrazoMedio = 0.
          RUN SetMsg IN hBoMsgComis(1,'Cond.Pagto n∆o encontrada','erro').
       END.        
    END.

END PROCEDURE.

PROCEDURE setTTCondPed:
    DEFINE INPUT PARAMETER TABLE FOR ttCondPed.
    DEFINE VARIABLE lSemParcelas AS LOGICAL     NO-UNDO.
    RUN setTipoCalc IN hBoCondPagtoPed(3).
    RUN setTTCondPed IN hBoCondPagtoPed(TABLE ttCondPed).
    RUN calcularPrazoMedio IN hBoCondPagtoPed.
    RUN getPrazoMedio IN hBoCondPagtoPed(OUTPUT iPrazoMedio).
    RUN getSemParcelas IN hBoCondPagtoPed(OUTPUT lSemParcelas).
    IF lSemParcelas THEN
       RUN SetMsg IN hBoMsgComis(2,'Cond. Pagto Especial sem parcelas informadas','erro').
    

END PROCEDURE.

PROCEDURE setDiasCondPagto:
    DEFINE INPUT  PARAMETER cDias AS CHARACTER  FORMAT 'x(200)' NO-UNDO.
    DEFINE VARIABLE iCont       AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iQtdias     AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iSomaDias   AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iDia        AS INTEGER     NO-UNDO.
    ASSIGN iQtdias = NUM-ENTRIES(cDias,',').
    REPEAT iCont = 1 TO iQtdias.
        ASSIGN iDia = int(ENTRY(iCont,cDias,","))
               iSomaDias = iSomaDias + iDia.
    END.
    ASSIGN iPrazoMedio = iSomaDias / iQtDias.

END PROCEDURE.


PROCEDURE setCodRep.
    DEFINE INPUT  PARAMETER pCodRep AS INTEGER     NO-UNDO.
    ASSIGN iCodrep = pCodrep.


END PROCEDURE.

PROCEDURE setEstab:
    DEFINE OUTPUT PARAMETER pEstab AS CHARACTER   NO-UNDO.
    ASSIGN cEstab = pEstab.

END PROCEDURE.


PROCEDURE setTtItensEspd4000:
    DEFINE INPUT PARAMETER TABLE FOR tt-itens-ped.
    FOR EACH tt-itens-ped
        WHERE tt-itens-ped.cod-sit-item = 1.
       /* MESSAGE tt-Itens-ped.it-codigo SKIP
                tt-itens-ped.cod-refer SKIP
                tt-itens-ped.vl-pre-calc SKIP

            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
        CREATE ttPedItem.
        ASSIGN ttPedItem.itCodigo  = tt-itens-ped.it-codigo
               ttPedItem.codRefer  = tt-itens-ped.cod-refer 
               ttPedItem.vlTb      = tt-itens-ped.vl-pre-calc
               ttPedItem.logOutlet = tt-itens-ped.outlet
               ttPedItem.vlInf     = tt-itens-ped.vl-preori
               ttPedItem.qtPedida  = tt-itens-ped.qt-pedida .
    END.


END PROCEDURE.

PROCEDURE setTtItensPedWeb:
    DEFINE BUFFER bfPedVenda FOR ped-venda.
    DEFINE BUFFER bfPedItem  FOR ped-item.
    DEFINE INPUT  PARAMETER rRowidPedWeb AS ROWID       NO-UNDO.
    FIND peds_web NO-LOCK
        WHERE rowid(peds_web) = rRowidPedWeb NO-ERROR.
    FIND bfPedVenda
        WHERE bfPedVenda.nr-pedido = nrPedido
        NO-LOCK NO-ERROR.
 
    IF AVAIL peds_web THEN DO:
       FOR EACH itens_ped_web 
           WHERE itens_ped_web.ped_web_id =  peds_web.ped_web_id NO-LOCK.
           CREATE ttPedItem.
           ASSIGN ttPedItem.itCodigo  = itens_ped_web.it_codigo
                  ttPedItem.codRefer  = itens_ped_web.cod_refer 
                  ttPedItem.vlTb      = itens_ped_web.vl_unit_tabela
                  ttPedItem.logOutlet = itens_ped_web.num_id_liquida_ima <> '0' 
                  ttPedItem.vlInf     = itens_ped_web.vl_informado
                  ttPedItem.qtPedida  = itens_ped_web.qt_pedida .
           IF ttPedItem.vlInf = 0 THEN DO:
              FIND bfPedItem  OF bfPedVenda
                  WHERE bfPedItem.it-codigo = ttPedItem.itcodigo
                  AND   bfPedItem.cod-refer = ttPedItem.codrefer
                  NO-LOCK NO-ERROR.
              IF AVAIL bfPedItem THEN DO:
                 ASSIGN ttPedItem.vlInf = bfPedItem.vl-preori. 
              END.

           END.
       END.
    END.


END PROCEDURE.

PROCEDURE getVlMinimoBonus:
    DEFINE OUTPUT PARAMETER vlMinimo AS DECIMAL     NO-UNDO.
    DEFINE VAR   vlParam LIKE im-param.val-param   NO-UNDO.  
    RUN esapi/getImParam.p('vl_minimo_bonus_comissao', OUTPUT vlParam).
    IF vlParam = '' THEN
       ASSIGN vlMinimo = 2500.
    ELSE 
      ASSIGN vlMinimo = DEC(vlParam).
END PROCEDURE.

PROCEDURE getHabilitarBonusComis:
    DEFINE OUTPUT PARAMETER lhabilitar AS LOGICAL     NO-UNDO.
    DEFINE VAR  vlParam LIKE im-param.val-param   NO-UNDO.    
    RUN esapi/getImParam.p('habilita_bonus_comissao', OUTPUT vlParam).
    ASSIGN lHabilitar = IF vlParam <> '1' THEN NO ELSE YES.
END PROCEDURE.


PROCEDURE getPercBonus:
    DEFINE OUTPUT PARAMETER percBonus AS DECIMAL  NO-UNDO.
    DEFINE VAR  vlParam LIKE im-param.val-param   NO-UNDO.    
    RUN esapi/getImParam.p('perc_bonus_comis', OUTPUT vlParam).
    IF vlParam = '' THEN
       ASSIGN vlParam = '25'.
    ASSIGN percBonus = DEC(vlParam).
END PROCEDURE.

PROCEDURE setNrContainer:
    DEFINE INPUT  PARAMETER pNrContainer AS INTEGER     NO-UNDO.
    ASSIGN nrContainer = pNrContainer.
    

END PROCEDURE.


PROCEDURE getPrazoMedio:
    DEFINE OUTPUT PARAMETER pPrazoMedio AS INTEGER     NO-UNDO.
    ASSIGN pprazoMedio = iPrazoMedio.
END PROCEDURE.

PROCEDURE getPrecoAbaixoTb:
    DEFINE OUTPUT PARAMETER lAbaixo AS LOGICAL     NO-UNDO INIT NO.

    DEFINE VARIABLE lAchou AS LOGICAL     NO-UNDO INIT NO.
    DEFINE VARIABLE dVlReal     AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dVlDolar    AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE cTipoPreco AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iControlePreco AS INTEGER.

    ASSIGN vlTotPed = 0.
    FOR EACH ttPedItem:
        /*MESSAGE 'informado:' ttPedItem.vlInf SKIP
                'tabela:' ttPedItem.vlTb SKIP
                'outlet:' ttpedItem.logOutlet SKIP
                'qt.pedida:' ttPeditem.qtPedida
            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
        ASSIGN lAchou = YES.
        IF ttPedItem.logOutlet THEN DO:
           IF nrContainer <> 0 THEN DO:
               RUN setTipoBusca    IN hBoPrecosItemRef(2). //pi
               RUN setNrContainer  IN hBoPrecosItemRef(nrContainer).
               ASSIGN cTipoPreco = 'pi'.

           END.
           ELSE DO:
              RUN setTipoBusca IN hBoPrecosItemRef(1). // pe
              ASSIGN cTipoPreco = 'pe'.
           END.                                             
           RUN SetPrazoMedio   IN hBoPrecosItemRef(iPrazoMedio).
           RUN setUfCliente    IN hBoPrecosItemRef( IF ufClienteTriang = '' THEN ufCliente ELSE ufClienteTriang).
           RUN setITem         IN hBoPrecosItemRef(ttPedItem.itCodigo).
           RUN setRef          IN hBoPrecosItemRef(ttPedItem.codRefer).
           RUN buscarPrecos    IN hBoPrecosItemRef.
           RUN getPrecoPrazo   IN hBoPrecosItemRef(INPUT cTipoPreco, 
                                                   OUTPUT dVlReal,
                                                   OUTPUT dVlDolar,
                                                   OUTPUT iControlePreco).
           ASSIGN ttPedItem.vlTb = IF iMoeda <> 0 THEN dVlDolar ELSE dVlReal.  
           RUN expttMsg        IN hBoPrecosItemRef('preco_item_ref'). 
        END.


        /*  comentado, pois, o item pode ser outlet e ser vendido pela tabela.
            IF ttPedItem.logOutlet THEN DO:
           ASSIGN lAbaixo = YES
                  vltotPed = 0.
           LEAVE.
        END.*/ 

        IF ROUND(ttPedItem.vlTb,2)= 0 THEN
           RUN setMsg IN HBoMsgComis(15,'o item:' +  ttPedItem.ItCodigo + " - ref:" + ttPedItem.codRefer + 
                                     ' tem o preáo de tabela zerado ','aviso').
        IF round(ttPedItem.vlInf,2) < round(ttPedItem.vlTb,2) THEN DO:
           ASSIGN lAbaixo = YES
                  vlTotPed = 0.
           RUN setMsg IN HBoMsgComis(15,'o item:' +  ttPedItem.ItCodigo + " - ref:" + ttPedItem.codRefer + 
                                     ' tem o preáo informado:' + string(round(ttPedItem.vlInf,2)) +
                                     ' menor que o preáo de tabela:' + STRING(round(ttPedItem.vlTb,2)),'aviso').
           LEAVE.
        END. 
        /*MESSAGE 'qt.pedida:' ttPedItem.qtPedida SKIP
                'vl.inf:' ttPedItem.vlInf
            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
        ASSIGN vltotPed = vlTotPed + (ttPedItem.qtPedida * ttPedItem.vlInf).
    END.
    IF lAchou = NO THEN
       RUN SetMsg IN hBoMsgComis(3,'N∆o foram encontrados itens','aviso').
END PROCEDURE.

PROCEDURE calcPercComis:
    DEFINE OUTPUT PARAMETER percFinal   AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE lBonus              AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE lPrecoAbaixoTb      AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE percPad             AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE percBonus           AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE percReducComis      AS DECIMAL     NO-UNDO.
    RUN limparTTMsg IN hboMsgComis.
    RUN getHabilitarBonusComis(OUTPUT lBonus).
    RUN setTbPreco IN hBoTbPreco(iTbPreco).
    RUN setMsg IN HBoMsgComis(9,'Tabela de Preáo' + STRING(iTbPreco) ,'aviso').   
    // caso o parametro de bonus esteja ligado, verifica se a tabela calcula bonus
    IF lBonus = YES THEN DO:
       RUN setMsg IN HBoMsgComis(12,'Parametro geral - calcula bonus' ,'aviso').   
       RUN getCalcBonus IN hBoTbPreco(OUTPUT lBonus).
       RUN setMsg IN HBoMsgComis(11,'Tabela de Preáo calcula Bonus?' + STRING(lBonus) ,'aviso').   
    END.
    ELSE  DO:
       RUN setMsg IN HBoMsgComis(112,'Parametro geral - NAO calcula bonus' ,'aviso').   
    END. 

    RUN setCodRep    IN hBoRepres(icodrep).
    RUN setEmpresa   IN hboRepres(i-ep-codigo-usuario).
    RUN getPercComisPad IN hBoRepres(OUTPUT percPad).
    RUN setMsg IN HBoMsgComis(110,'Comiss∆o padr∆o Representante:' + STRING(percPad),'aviso').
    /*MESSAGE 'comissao padrao:' percpad
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    ASSIGN percFinal = percPad.
    IF lBonus = NO THEN DO:
       RUN setMsg IN HBoMsgComis(4,'Bonus de Comiss∆o inativo','aviso').
    END.
    ELSE DO:
        RUN setMsg IN HBoMsgComis(104,'Bonus de Comiss∆o Ativo','aviso').
        IF iPrazoMedio > 90  THEN DO:
           RUN setMsg IN HBoMsgComis(5,'Prazo mÇdio maior de 90 dias','aviso').
           ASSIGN lBonus = NO.
        END.
        ELSE DO:
            RUN setMsg IN HBoMsgComis(105,'Prazo mÇdio menor, igual a 90 dias:' + STRING(iPrazoMedio),'aviso').
        END.
           
        RUN getPrecoAbaixoTb(OUTPUT lPrecoAbaixoTb).
        IF lPrecoAbaixoTb THEN DO:
           RUN setMsg IN HBoMsgComis(6,'Encontrado Preáo informado abaixo da Tabela','aviso').
           ASSIGN lBonus = NO.
        END.
        ELSE DO:
           RUN setMsg IN HBoMsgComis(106,'NAO Encontrado Preáo informado abaixo da Tabela','aviso').
        END.
           
        RUN getVlMinimoBonus( OUTPUT vlMinimoBonus).
        /*MESSAGE 'total pedido:' vlTotPed SKIP
               'vl.minimo:'   vlMinimoBonus
            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
        IF vlTotPed < vlMinimoBonus THEN DO:
           RUN setMsg IN HBoMsgComis(7,'Total do pedido:' + STRING(vlTotPed) + ' ABAIXO do valor minimo para bonus:'
                                    + string(vlMinimoBonus),'aviso').
           ASSIGN lBonus = NO.     
        END.
        ELSE DO:
          RUN setMsg IN HBoMsgComis(107,'Total do pedido:' + STRING(vlTotPed) + ' igual ou acima do valor minimo para bonus:'
                                    + string(vlMinimoBonus) ,'aviso').   
        END.
           
    END.
    IF lBonus THEN DO:
        /*MESSAGE 'entrei no calculo final do bonus'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
       RUN getPercBonus(OUTPUT percBonus).
       ASSIGN percFinal = percPad * (1 + percBonus / 100).
       
       RUN setMsg IN HBoMsgComis(100,'percentual de bonus aplicado:' + STRING(percBonus) + "perc.reduc." + STRING(percReducComis) + "% - resultado final:"
                                 + string(percFinal),'aviso').
    END.

    RUN getPercReducComis IN hBoTbPreco(OUTPUT percReducComis).
    IF percReducComis > 0 THEN DO:
       ASSIGN percFinal = percFinal * ( 1 - percReducComis / 100).
       RUN setMsg IN HBoMsgComis(8,'Percentual e reduá∆o da tabela de Preáo maior que zero:' + STRING(percReducComis) ,'aviso').   
    END.
    ELSE DO:
       RUN setMsg IN HBoMsgComis(108,'Percentual de reduá∆o da tabela de Preáo igual a zero','aviso').   
    END.

    RUN expTTmsg IN hboMsgComis(SESSION:TEMP-DIRECTORY + "\log_bo_comis_" + STRING(pedWebId) + "_" + STRING(nrPedido)).
END PROCEDURE.










