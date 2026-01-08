/*
* Programa: esbo/boComis.p
* Objetivo: Tratar todas s regras e calculos de comissÆo customizados
* Autor: Tadeu Silva
* Data: 08/2020
* 27/04/2021 -tadeu - inclusao de tratamento de redu‡Æo de comissÆo 
* conforme tabela de pre‡o.
* 13/05/2021 - tadeu - mudan‡a do calculo da comissÆo para considerar
* os percentuais de comissÆo especificos de uma campanha de pre‡o e 
* para calcular comissÆo de um segundo repres/vend quando a comissÆo for dividida
18/01/2021 - tadeu -  acrescimo de procedimento setPropsPedsWeb  que recebe o id do pedido web passado no procedimento setPedWebId e chama os procedimentos da bo
necess rios para o correto calculo da comissÆo
19/01/2022 - tadeu - acrescimo de procedimento setFormaPagto e acrescimo da condi‡Æo
de que a venda com cartÆo de cr‚dito nÆo tem bonus de comissÆo.
(conforme e-mail da Ana Fl via enviado dia 19/01/2022 as 17:54 )
- acrescimo da include {esbo/boComis.i2}  onde ‚ colocado um procedimento
deParaFormaPagtoERP para possibilitar o intercambio da forma de pagamento
do portal com o ERP.
09/02/2022 = Tadeu -acrescimo de procedimento para recuperar os c¢digos de representantes
referentes a comissao -> getCodsRepres
*/
{esbo/boComis.i2}
{esbo/boPedsWeb.i}

DEFINE VARIABLE iCliente         AS INTEGER     NO-UNDO.
DEFINE VARIABLE iClienteTriang   AS INTEGER     NO-UNDO.
DEFINE VARIABLE ufCliente        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ufClienteTriang  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vlMinimoBonus    AS DECIMAL     NO-UNDO.
DEFINE VARIABLE vlTotPed         AS DECIMAL     NO-UNDO.
DEFINE VARIABLE iPrazoMedio      AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCondPagto       AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCodRep          AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCodRepCliente   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iTbPreco         AS INTEGER     NO-UNDO.
DEFINE VARIABLE cEstab           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE nrContainer      AS INTEGER     NO-UNDO.
DEFINE VARIABLE nrPedido         AS INTEGER     NO-UNDO.
DEFINE VARIABLE pedWebId         AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMoeda           AS INTEGER     NO-UNDO.
DEFINE VARIABLE logDivideComis   AS LOGICAL     NO-UNDO.
DEFINE VARIABLE dPercComisVend   AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dPercComisRepres AS DECIMAL     NO-UNDO.
DEFINE VARIABLE lBonus           AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lComisEspCamp    AS LOGICAL     NO-UNDO INIT YES.
DEFINE VARIABLE percBonus        AS DECIMAL     NO-UNDO.
DEFINE VARIABLE percReducComis   AS DECIMAL     NO-UNDO.
DEFINE VARIABLE percFinalCli     AS DECIMAL     NO-UNDO.
DEFINE VARIABLE lTemOutlet       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE iTransacao       AS INTEGER     NO-UNDO.
DEFINE VARIABLE iFormaPagto      AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCalculo         AS INTEGER     NO-UNDO.

{utp/ut-glob.i}
{esbo\boMsg.i}
{seniuz/esp/espd4000.i}
{esbo/bocomis.i}



DEFINE VARIABLE hBoMsgComis         AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoRepres           AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoCondPagtoPed     AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoPrecosItemRef    AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoTbPreco          AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoEmitente         AS HANDLE      NO-UNDO.
DEFINE VARIABLE cLoginPortal        AS CHARACTER   NO-UNDO.

//DEFINE TEMP-TABLE ttPedsWeb LIKE peds_web.



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

    IF NOT VALID-HANDLE(hBoEmitente) THEN DO:
       RUN esbo/boEmitente.p PERSISTENT SET hBoEmitente.
       RUN iniciarBos IN hBoEmitente.
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

    IF VALID-HANDLE(hBoEmitente) THEN DO:
        RUN finalizarBos IN hBoEmitente.
       DELETE PROCEDURE hBoEmitente.
    END.

END PROCEDURE.

PROCEDURE setLoginPortal:

   DEFINE INPUT  PARAMETER pLogin AS CHARACTER   NO-UNDO.
   ASSIGN cLoginPortal = pLogin.

END PROCEDURE.

PROCEDURE setTransacao:

    DEFINE INPUT  PARAMETER ptransacao AS INTEGER     NO-UNDO.
    ASSIGN iTransacao = pTransacao.

END PROCEDURE.


PROCEDURE setDivideComis:

    DEFINE INPUT  PARAMETER lParam AS LOGICAL     NO-UNDO.
    ASSIGN logDivideComis = lParam .


END PROCEDURE.


PROCEDURE setPercComisVend:

    DEFINE INPUT  PARAMETER dParam AS DECIMAL     NO-UNDO.
    ASSIGN dPercComisVend = dParam.


END PROCEDURE.


PROCEDURE setPercComisRepres:  
    DEFINE INPUT  PARAMETER dParam AS DECIMAL     NO-UNDO.
    ASSIGN dPercComisRepres = dParam.
END PROCEDURE.
                  
PROCEDURE setFormaPagto:
    DEFINE INPUT  PARAMETER pFormaPagto AS INTEGER     NO-UNDO.
    ASSIGN iFormaPagto  = pFormaPagto .
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

PROCEDURE setNrContainer:
    DEFINE INPUT  PARAMETER pNrContainer AS INTEGER     NO-UNDO.
    ASSIGN nrContainer = pNrContainer.
END PROCEDURE.

PROCEDURE setCliente:
    DEFINE INPUT  PARAMETER pcliente AS INTEGER     NO-UNDO.
    ASSIGN icliente =  pCliente.

    RUN setCodEmitente IN hBoEmitente(iCliente).
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
          RUN SetMsg IN hBoMsgComis(1,'Cond.Pagto nÆo encontrada','erro').
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
    DEFINE INPUT PARAMETER pEstab AS CHARACTER   NO-UNDO.
    ASSIGN cEstab = pEstab.

END PROCEDURE.


PROCEDURE setTtItensEspd4000:
    DEFINE INPUT PARAMETER TABLE FOR tt-itens-ped.
    ASSIGN lTemOutlet  = YES.
    FOR EACH tt-itens-ped
        WHERE tt-itens-ped.cod-sit-item = 1.
        /*MESSAGE tt-Itens-ped.it-codigo SKIP
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
        IF tt-itens-ped.outlet = NO THEN
           ASSIGN lTemOutlet = tt-itens-ped.outlet .
    END.


END PROCEDURE.

PROCEDURE setTtItensPedWeb:
    DEFINE INPUT  PARAMETER rRowidPedWeb AS ROWID       NO-UNDO.
    DEFINE BUFFER bfPedVenda FOR ped-venda.
    DEFINE BUFFER bfPedItem  FOR ped-item.    
    ASSIGN lTemOutlet  = YES.

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

           IF ttPedItem.logOutlet = NO  THEN
              ASSIGN lTemOutlet = ttPedItem.logOutlet .

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

PROCEDURE getPrazoMedio:

    DEFINE OUTPUT PARAMETER pPrazoMedio AS INTEGER     NO-UNDO.
    ASSIGN pprazoMedio = iPrazoMedio.

END PROCEDURE.

PROCEDURE getPrecoAbaixoTb:

    DEFINE OUTPUT PARAMETER lAbaixo       AS LOGICAL     NO-UNDO INIT NO.
    DEFINE OUTPUT PARAMETER lAbaixoOutlet AS LOGICAL     NO-UNDO INIT NO.
    DEFINE VARIABLE lAchou                AS LOGICAL     NO-UNDO INIT NO.
    DEFINE VARIABLE dVlReal               AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dVlDolar              AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dVlRealOutLet         AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dVlDolarOutlet        AS DECIMAL     NO-UNDO.

    DEFINE VARIABLE cTipoPreco AS CHARACTER   NO-UNDO.
	DEFINE VARIABLE iControlePreco AS INTEGER.
    /*MESSAGE 'dentro getPrecoAbaixoTb'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

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
           RUN getPrecoPrazo   IN hBoPrecosItemRef(INPUT 'outlet', 
		                                           OUTPUT dVlRealOutlet, 
												   OUTPUT dVlDolarOutlet,
												   OUTPUT iControlePreco).

           ASSIGN ttPedItem.vlTb = IF iMoeda <> 0 THEN dVlDolar ELSE dVlReal
                  ttPedItem.vlOutlet = dVlRealOutlet. 
           //RUN expttMsg        IN hBoPrecosItemRef('t:\especificos\logs\preco_item_ref'). 
           /*    
           comentado, pois, o item pode ser outlet e ser vendido pela tabela.
           IF ttPedItem.logOutlet THEN DO:
           ASSIGN lAbaixo = YES
                  vltotPed = 0.
           LEAVE. */
        END. 

        IF ROUND(ttPedItem.vlTb,2)= 0 THEN
           RUN setMsg IN HBoMsgComis(15,'o item:' +  ttPedItem.ItCodigo + " - ref:" + ttPedItem.codRefer + 
                                     ' tem o pre‡o de tabela zerado ','aviso').
        IF round(ttPedItem.vlInf,2) < round(ttPedItem.vlTb,2) AND lAbaixo = NO THEN DO:
           ASSIGN lAbaixo = YES
                  //vlTotPed = 0
               .
           RUN setMsg IN HBoMsgComis(15,'o item:' +  ttPedItem.ItCodigo + " - ref:" + ttPedItem.codRefer + 
                                     ' tem o pre‡o informado:' + string(round(ttPedItem.vlInf,2)) +
                                     ' menor que o pre‡o de tabela:' + STRING(round(ttPedItem.vlTb,2)),'aviso').
        END. 

         IF round(ttPedItem.vlInf,2) < round(ttPedItem.vlOutlet,2) AND lAbaixoOutlet = NO THEN DO:
           ASSIGN lAbaixoOutlet = YES
                  //vlTotPed = 0
               .
           RUN setMsg IN HBoMsgComis(15,'o item:' +  ttPedItem.ItCodigo + " - ref:" + ttPedItem.codRefer + 
                                     ' tem o pre‡o informado:' + string(round(ttPedItem.vlInf,2)) +
                                     ' menor que o pre‡o de outlet:' + STRING(round(ttPedItem.vlOutlet,2)),'aviso').
        END. 



        /*MESSAGE 'qt.pedida:' ttPedItem.qtPedida SKIP
                'vl.inf:' ttPedItem.vlInf
            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
        ASSIGN vltotPed = vlTotPed + (ttPedItem.qtPedida * ttPedItem.vlInf).
    END.
    IF lAchou = NO THEN
       RUN SetMsg IN hBoMsgComis(3,'NÆo foram encontrados itens','aviso').

END PROCEDURE.


/*PROCEDURE avaliarRepresCliente.

    DEFINE VARIABLE iRepresCli AS INTEGER     NO-UNDO.
    RUN getCodRepresEmitente(iCliente, OUTPUT iRepresCli).

END PROCEDURE.*/

/*
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
    RUN setMsg IN HBoMsgComis(9,'Tabela de Pre‡o' + STRING(iTbPreco) ,'aviso').   
    // caso o parametro de bonus esteja ligado, verifica se a tabela calcula bonus
    IF lBonus = YES THEN DO:
       RUN setMsg IN HBoMsgComis(12,'Parametro geral - calcula bonus' ,'aviso').   
       RUN getCalcBonus IN hBoTbPreco(OUTPUT lBonus).
       RUN setMsg IN HBoMsgComis(11,'Tabela de Pre‡o calcula Bonus?' + STRING(lBonus) ,'aviso').   
    END.
    ELSE  DO:
       RUN setMsg IN HBoMsgComis(112,'Parametro geral - NAO calcula bonus' ,'aviso').   
    END. 

    RUN setCodRep    IN hBoRepres(icodrep).
    RUN setEmpresa   IN hboRepres(i-ep-codigo-usuario).
    RUN getPercComisPad IN hBoRepres(OUTPUT percPad).
    RUN setMsg IN HBoMsgComis(110,'ComissÆo padrÆo Representante:' + STRING(percPad),'aviso').
    /*MESSAGE 'comissao padrao:' percpad
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    ASSIGN percFinal = percPad.
    IF lBonus = NO THEN DO:
       RUN setMsg IN HBoMsgComis(4,'Bonus de ComissÆo inativo','aviso').
    END.
    ELSE DO:
        RUN setMsg IN HBoMsgComis(104,'Bonus de ComissÆo Ativo','aviso').
        IF iPrazoMedio > 90  THEN DO:
           RUN setMsg IN HBoMsgComis(5,'Prazo m‚dio maior de 90 dias','aviso').
           ASSIGN lBonus = NO.
        END.
        ELSE DO:
            RUN setMsg IN HBoMsgComis(105,'Prazo m‚dio menor, igual a 90 dias:' + STRING(iPrazoMedio),'aviso').
        END.
           
        RUN getPrecoAbaixoTb(OUTPUT lPrecoAbaixoTb).
        IF lPrecoAbaixoTb THEN DO:
           RUN setMsg IN HBoMsgComis(6,'Encontrado Pre‡o informado abaixo da Tabela','aviso').
           ASSIGN lBonus = NO.
        END.
        ELSE DO:
           RUN setMsg IN HBoMsgComis(106,'NAO Encontrado Pre‡o informado abaixo da Tabela','aviso').
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
       RUN setMsg IN HBoMsgComis(8,'Percentual e redu‡Æo da tabela de Pre‡o maior que zero:' + STRING(percReducComis) ,'aviso').   
    END.
    ELSE DO:
       RUN setMsg IN HBoMsgComis(108,'Percentual de redu‡Æo da tabela de Pre‡o igual a zero','aviso').   
    END.

    RUN expTTmsg IN hboMsgComis(SESSION:TEMP-DIRECTORY + "\log_bo_comis_" + STRING(pedWebId) + "_" + STRING(nrPedido)).
END PROCEDURE.
*/

PROCEDURE calcPercComis:

    DEFINE OUTPUT PARAMETER percFinal   AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE lPrecoAbaixoTb      AS LOGICAL     NO-UNDO INIT NO.
    DEFINE VARIABLE lPrecoAbaixoOutlet  AS LOGICAL     NO-UNDO INIT NO.
    DEFINE VARIABLE percDiv             AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE cTipoVendCli        AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cTipoVend           AS CHARACTER   NO-UNDO.
    ASSIGN percFinalCli = 0
           lComisEspCamp = YES.

    //RUN avaliarRepresCliente.   
    /*MESSAGE 'antes de limpar tts' SKIP
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    RUN limparTTMsg IN hboMsgComis.
    RUN setLoginPortal IN hBoPrecosItemRef(cLoginPortal).
    RUN getHabilitarBonusComis(OUTPUT lBonus).
    RUN setTbPreco IN hBoTbPreco(iTbPreco).
    RUN setMsg IN HBoMsgComis(9,'Tabela de Pre‡o' + STRING(iTbPreco) ,'aviso').   
    // caso o parametro de bonus esteja ligado, verifica se a tabela calcula bonus
    /*
     * acrescimo de condicao de que o tipo de pagamento 3(cartÆo de cr‚dito) nÆo em bonus de comissÆo
     * conforme e-mail 19/01/2022 enviado pela Ana Fl via as 17:24 
    */
    IF  iFormaPagto = 4 THEN DO:
        ASSIGN lBonus = FALSE.
        RUN setMsg IN HBoMsgComis(122,'Forma de Pagamento igual a cartÆo de cr‚dito - Bonus setado como FALSE' ,'aviso').   
    END.
        
    IF lBonus = YES  THEN DO:
       RUN setMsg IN HBoMsgComis(12,'Parametro geral - calcula bonus' ,'aviso').   
       RUN getCalcBonus IN hBoTbPreco(OUTPUT lBonus).
       RUN setMsg IN HBoMsgComis(11,'Tabela de Pre‡o calcula Bonus?' + STRING(lBonus) ,'aviso').   
    END.
    ELSE DO:
       RUN setMsg IN HBoMsgComis(112,'Parametro geral - NAO calcula bonus' ,'aviso').   
    END. 

    IF lBonus = NO THEN DO:
       RUN setMsg IN HBoMsgComis(4,'Bonus de ComissÆo inativo','aviso').
    END.
    ELSE DO:
        RUN setMsg IN HBoMsgComis(104,'Bonus de ComissÆo Ativo','aviso').
        IF iPrazoMedio > 90  THEN DO:
           RUN setMsg IN HBoMsgComis(5,'Prazo m‚dio maior de 90 dias','aviso').
           ASSIGN lBonus        = NO
                  lComisEspCamp = NO.
        END.
        ELSE DO:
            RUN setMsg IN HBoMsgComis(105,'Prazo m‚dio menor, igual a 90 dias:' + STRING(iPrazoMedio),'aviso').
        END.
           
        RUN getPrecoAbaixoTb(OUTPUT lPrecoAbaixoTb, OUTPUT lPrecoAbaixoOutlet).
        
        IF lPrecoAbaixoTb THEN DO:
           RUN setMsg IN HBoMsgComis(6,'Encontrado Pre‡o informado abaixo da Tabela','aviso').
           ASSIGN lBonus = NO.
        END.
        ELSE DO:
           RUN setMsg IN HBoMsgComis(106,'NAO Encontrado Pre‡o informado abaixo da Tabela','aviso').
        END.

        IF lPrecoAbaixoOutlet THEN DO:
           RUN setMsg IN HBoMsgComis(30,'Encontrado Pre‡o informado abaixo do Pre‡o Outlet','aviso').
           ASSIGN lComisEspCamp = NO.
        END.
        ELSE DO:
           RUN setMsg IN HBoMsgComis(130,'NAO Encontrado Pre‡o informado abaixo da Pre‡o Outlet','aviso').
        END.
           
        RUN getVlMinimoBonus( OUTPUT vlMinimoBonus).
        /*MESSAGE 'total pedido:' vlTotPed SKIP
               'vl.minimo:'   vlMinimoBonus
            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
        IF vlTotPed < vlMinimoBonus THEN DO:
           RUN setMsg IN HBoMsgComis(7,'Total do pedido:' + STRING(vlTotPed) + ' ABAIXO do valor minimo para bonus:'
                                    + string(vlMinimoBonus),'aviso').
           ASSIGN lBonus        = NO
                  lComisEspCamp = NO.     
        END.
        ELSE DO:
          RUN setMsg IN HBoMsgComis(107,'Total do pedido:' + STRING(vlTotPed) + ' igual ou acima do valor minimo para bonus:'
                                    + string(vlMinimoBonus) ,'aviso').   
        END.
    END.

    RUN setDesconsiderarInativo IN hBoEmitente(INPUT YES).
    RUN getCodRepresCliente IN hBoEmitente(OUTPUT iCodRepCliente).

    IF logDivideComis THEN DO:
       IF iCodRep <> iCodRepCliente AND 
          iCodRep <> 0 AND
          iCodRepCliente <> 0 THEN DO:                         

          RUN setMsg IN HBoMsgComis(210,'Divide ComissÆo? SIM' ,'aviso').   
          RUN setMsg IN HBoMsgComis(220,'Repres ' + STRING(iCodRepCliente) + ' do Cliente ' + ' ‚ Diferente do Vendedor ' + STRING(iCodRep) + ' ? SIM' ,'aviso').   
          RUN GetPercComisTipoVend(INPUT iCodRepCliente, OUTPUT percFinalCli,OUTPUT cTipoVendCli).
          RUN setMsg IN HBoMsgComis(215,'% Comis Rep Cli:' + STRING(percFinalCli) ,'aviso').
          RUN GetPercComisTipoVend(INPUT iCodRep, OUTPUT percFinal, OUTPUT cTipoVend).
          RUN setMsg IN HBoMsgComis(215,'% Vend:' + STRING(percFinal) ,'aviso').
          
          IF cTipoVendCli = 'interno' AND cTipoVend = 'externo' THEN DO:
             RUN setMsg IN HBoMsgComis(251,'repres cliente interno e vendedor externo - metade da comissÆo padrÆo de cada tipo de repres.','aviso').   
             ASSIGN percFinalCli = percFinalCli / 2
                    percFinal    = percFinal / 2.
          END.
          ELSE DO:
            RUN setMsg IN HBoMsgComis(251,'comissÆo divida gual a metade da maior comissÆo para cada tipo de vendedor','aviso').   
            IF percFinal > percFinalCli THEN
                ASSIGN percDiv = percFinal.  
            ELSE 
                ASSIGN percDiv = percFinalCli.
            RUN setMsg IN HBoMsgComis(250,'% ComissÆo a ser dividido:' + STRING(percDiv) ,'aviso').   

            ASSIGN percFinal = percDiv / 2 
                   percFinalCli = percFinal.
          END.
       END.
       ELSE DO:                                  
          RUN setMsg IN HBoMsgComis(220,'Repres Cliente Diferente Vendedor? NÇO' ,'aviso').   
          RUN GetPercComisTipoVend(INPUT iCodRep, OUTPUT percFinal,OUTPUT cTipoVendCli).
       END.
    END.
    ELSE DO:
       IF iCodRepCliente <> 0 THEN DO.
          IF iCodRep <> iCodRepCliente THEN DO:
             RUN setMsg IN HBoMsgComis(320,'Repres Cliente Diferente Vendedor? SIM' ,'aviso').   
             RUN GetPercComisTipoVend(INPUT iCodRepCliente, OUTPUT percFinalCli,OUTPUT cTipoVendCli).
             RUN setMsg IN HBoMsgComis(315,'% Comis Rep Cli:' + STRING(percFinalCli) ,'aviso').
             RUN GetPercComisTipoVend(INPUT iCodRep, OUTPUT percFinal, OUTPUT cTipoVend).
             RUN setMsg IN HBoMsgComis(315,'% Vend:' + STRING(percFinal) ,'aviso').
             RUN setMsg IN HBoMsgComis(351,'repres cliente diferente do represetnante do pedido - metade da comissÆo padrÆo de cada tipo de repres.','aviso').   
             ASSIGN percFinalCli = percFinalCli / 2
                    percFinal    = percFinal / 2.
          END.
          ELSE
             RUN GetPercComisTipoVend(INPUT iCodRepCliente, OUTPUT percFinal, OUTPUT cTipoVendCli).
       END.
       ELSE 
          RUN GetPercComisTipoVend(INPUT iCodRep, OUTPUT percFinal, OUTPUT cTipoVendCli).

       RUN setMsg IN HBoMsgComis(211,'Divide ComissÆo? NÇO' ,'aviso').   
    END.

    RUN setTransacaoLogCalculo IN hBoMsgComis(iTransacao).



    RUN gravarLogCalculo IN hBoMsgComis(IF iCalculo = 0 THEN 3 ELSE iCalculo ). //calculo comissao

    //RUN expTTmsg IN hboMsgComis(SESSION:TEMP-DIRECTORY + "\log_bo_comis_" + STRING(pedWebId) + "_" + STRING(nrPedido)).
END PROCEDURE.

PROCEDURE setCalculo:
    DEFINE INPUT  PARAMETER pCalculo AS INTEGER     NO-UNDO.
    ASSIGN iCalculo = pCalculo.

END PROCEDURE.


PROCEDURE getPercComisTipoVend:
    
    DEFINE INPUT  PARAMETER pRepres         AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER percFinal       AS DECIMAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER pTipoVendedor   AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE percPad                 AS DECIMAL     NO-UNDO.

    RUN setCodRep    IN hBoRepres(pRepres).
    RUN setEmpresa   IN hboRepres(i-ep-codigo-usuario).
    RUN getPercComisPad IN hBoRepres(OUTPUT percPad).
    RUN setMsg IN HBoMsgComis(110,'ComissÆo padrÆo Representante:' + STRING(percPad),'aviso').
    ASSIGN percFinal = percPad.

    IF lBonus THEN
       RUN getPercBonus(OUTPUT percBonus).

    ASSIGN percFinal = percPad * (1 + percBonus / 100).

    RUN setMsg IN HBoMsgComis(100,'percentual de bonus aplicado:' + STRING(percBonus) + "perc.reduc." + STRING(percReducComis) + "% - resultado final:" + string(percFinal),'aviso').

    IF lComisEspCamp THEN DO:
       RUN setMsg IN HBoMsgComis(300,'Cumprida as condi‡äes para comissÆo especial da campanha','aviso').    
       RUN setCodRep IN hBoRepres(pRepres).
       RUN getTipoVendedor IN hBoRepres(OUTPUT pTipoVendedor).
       /*MESSAGE 'vendedor:' pRepres SKIP
               'tipo vend:' cTipoVendedor  SKIP
               'perc.final:' percFinal SKIP
               '% comis vend:' dPercComisVend SKIP
               '% comis repres:' dPercComisRepres
           VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
       CASE pTipoVendedor:
         WHEN 'interno' THEN
             ASSIGN percFinal = IF dPercComisVend   = 0 THEN percFinal ELSE dPercComisVend.
         WHEN 'externo' THEN
             ASSIGN percFinal = IF dPercComisRepres = 0 THEN percFinal ELSE dPercComisRepres.
         OTHERWISE
             RUN setMsg IN HBoMsgComis(300,'Vendedor sem classe definida ou definida fora do escopo interno/externo','erro').    
       END CASE.
    END.
    ELSE DO :
        RUN setMsg IN HBoMsgComis(300,'NAO Cumprida as condi‡äes para comissÆo especial da campanha','aviso').    
    END.

    RUN getPercReducComis IN hBoTbPreco(OUTPUT percReducComis).
    IF percReducComis > 0 THEN DO:
       ASSIGN percFinal = percFinal * ( 1 - percReducComis / 100).
       RUN setMsg IN HBoMsgComis(8,'Percentual e redu‡Æo da tabela de Pre‡o maior que zero:' + STRING(percReducComis) ,'aviso').   
    END.
    ELSE DO:
       RUN setMsg IN HBoMsgComis(108,'Percentual de redu‡Æo da tabela de Pre‡o igual a zero','aviso').   
    END.
 END PROCEDURE.


PROCEDURE getPercComis2:

    DEFINE OUTPUT PARAMETER pPercComis AS DECIMAL     NO-UNDO.
    ASSIGN pPercComis = percFinalCli.


             
END PROCEDURE.


PROCEDURE setPropsPedsWeb:

 DEFINE VARIABLE iMoeda     AS INTEGER     NO-UNDO.
 


 FIND peds_web NO-LOCK
     WHERE peds_web.ped_web_id = pedWebId NO-ERROR.
 IF AVAIL peds_web THEN DO:
    RUN setPedWebId(pedWebId).
    RUN getDeParaMoedaPedWeb(peds_web.cod_moeda,OUTPUT iMoeda).
    RUN setMoeda(iMoeda).
    RUN setDivideComis(peds_web.LOG_divide_comis).
    RUN setPercComisVend(peds_web.perc_comis_vend).
    RUN setPercComisRepres(peds_web.perc_comis_repres).
    RUN setTbPreco(peds_web.tb_preco_id).
    RUN setNrContainer(peds_web.nr_container).
    RUN setCliente(peds_web.cliente_id).
    RUN setClienteTriang(peds_web.cliente_triang_id).
    RUN setDiasCondPagto(peds_web.dias_cond_pagto_esp).
    RUN setCodRep(peds_web.repres_id).
    RUN setEstab(peds_web.cod_estab).
    RUN setTtItensPedWeb(ROWID(peds_web)).
                        
 END.


  





END PROCEDURE.

PROCEDURE getCodsRepres:

    DEFINE OUTPUT PARAMETER iRepres     AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER iRepres2    AS INTEGER     NO-UNDO.

    ASSIGN iRepres      = iCodRepCliente
            iRepres2    = iCodRep .

END PROCEDURE.







