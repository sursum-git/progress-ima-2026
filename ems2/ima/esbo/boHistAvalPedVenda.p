/*************************************************************************************************************************************************
programa: esbo/boHistAvalPedVenda.p
objetivo: manter a tabela hist_aval_ped_venda e todas as regras de negocio existentes para gera‡Æo das pendencias de avalia‡Æo do pedido de venda.
data: 10/2021
*************************************************************************************************************************************************/
 //DEFINE VARIABLE iIdHist        AS INTEGER     NO-UNDO.
 DEFINE VARIABLE iPedWebId          AS INTEGER     NO-UNDO.
 DEFINE VARIABLE cEstab             AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE iNrPedido          AS INTEGER     NO-UNDO.
 DEFINE VARIABLE iCodTipo           AS INTEGER     NO-UNDO.
 DEFINE VARIABLE iSituacao          AS INTEGER     NO-UNDO.
 DEFINE VARIABLE iOrigem            AS INTEGER     NO-UNDO.
 DEFINE VARIABLE iMotivo            AS INTEGER     NO-UNDO.
 DEFINE VARIABLE cDescMotivo        AS CHARACTER   NO-UNDO.

 DEFINE VARIABLE dtIniReg           AS DATE        NO-UNDO.
 DEFINE VARIABLE dtFimReg           AS DATE        NO-UNDO.
 DEFINE VARIABLE dtIniAVal          AS DATE        NO-UNDO.
 DEFINE VARIABLE dtFimAval          AS DATE        NO-UNDO.
 DEFINE VARIABLE codUsuario         AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE dtHrRefer          AS DATETIME    NO-UNDO.
 DEFINE VARIABLE dtHrVencUltValid   AS DATETIME    NO-UNDO.
 DEFINE VARIABLE idUltValid         AS INT64       NO-UNDO.
 DEFINE VARIABLE indSitUltValid     AS INTEGER     NO-UNDO.
 {esp/util.i}
 {esbo/boHistAvalPedVenda.i}
 {utp/ut-glob.i}

  DEFINE VARIABLE hBoPedWeb      AS HANDLE      NO-UNDO.
  DEFINE VARIABLE hBoMsgHistAval AS HANDLE      NO-UNDO.
  DEFINE VARIABLE hBoConsParam   AS HANDLE      NO-UNDO.
  DEFINE VARIABLE hBoCalendGlob  AS HANDLE      NO-UNDO.
  DEFINE VARIABLE hBoRepres      AS HANDLE      NO-UNDO.

PROCEDURE iniciarBOs:

    IF NOT VALID-HANDLE(hBoPedWeb) THEN
       RUN esbo/boPedsweb.p PERSISTENT SET hBoPedWeb.
    IF NOT VALID-HANDLE(hBoMsgHistAval) THEN
       RUN esbo/boMsg.p PERSISTENT SET hBoMsgHistAval.
    IF NOT VALID-HANDLE(hBoConsParam) THEN  
       RUN esbo/boConsParam.p PERSISTENT SET hBoConsParam.
    IF NOT VALID-HANDLE(hBoCalendGlob) THEN  
       RUN esbo/boCalendGlob.p PERSISTENT SET hBoCalendGlob.
    IF NOT VALID-HANDLE(hBoRepres) THEN
       RUN esbo/boRepres.p PERSISTENT SET hBoRepres.



END PROCEDURE.

PROCEDURE finalizarBOs:
    IF VALID-HANDLE(hBoPedWeb) THEN
       DELETE OBJECT hBoPedWeb.

    IF VALID-HANDLE(hBoMsgHistAval) THEN
       DELETE OBJECT hBoMsgHistAval.

    IF  VALID-HANDLE(hBoConsParam) THEN  
       DELETE OBJECT hBoConsParam.
    IF VALID-HANDLE(hBoCalendGlob) THEN  
       DELETE OBJECT hBoCalendGlob.

    IF VALID-HANDLE(hBoRepres) THEN
       DELETE OBJECT hBoRepres.       
       

END PROCEDURE.


PROCEDURE setChavePedido:
   DEFINE INPUT  PARAMETER pPedWebId AS INTEGER     NO-UNDO.
   DEFINE INPUT  PARAMETER pEstab    AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER pNrPedido AS INTEGER     NO-UNDO.
   ASSIGN iPedWebId = pPedWebId
          cEstab    = pEstab
          iNrPedido = pNrPedido .   
   IF iPedWebId > 0 THEN
      RUN setPedWebId IN hBoPedWeb(pPedwebId).

END PROCEDURE.

PROCEDURE setCodTipo:
    DEFINE INPUT  PARAMETER pCodTipo AS INTEGER     NO-UNDO.
    ASSIGN iCodTipo = pCodTipo .


END PROCEDURE.

PROCEDURE setSituacao:

    DEFINE INPUT  PARAMETER pSituacao AS INTEGER     NO-UNDO.
    ASSIGN iSituacao = pSituacao.

END PROCEDURE.

PROCEDURE setMotivo:

    DEFINE INPUT  PARAMETER pMotivo AS INTEGER     NO-UNDO.
    ASSIGN iMotivo = pMotivo.

END PROCEDURE.


PROCEDURE setOrigem:

    DEFINE INPUT  PARAMETER pOrigem AS INTEGER     NO-UNDO.
    ASSIGN iOrigem = pOrigem.


END PROCEDURE.


PROCEDURE setDescMotivo:

    DEFINE INPUT  PARAMETER pDescMotivo LIKE hist_aval_ped_venda.DESC_motivo   NO-UNDO.
    ASSIGN cDescMotivo = pDescMotivo.

END PROCEDURE.

PROCEDURE setDtsReg:

    DEFINE INPUT  PARAMETER pDtIni AS DATE        NO-UNDO.
    DEFINE INPUT  PARAMETER pDtFim AS DATE        NO-UNDO.
    ASSIGN dtIniReg = pDtIni
           dtFimReg = pDtFim .


END PROCEDURE.

PROCEDURE setDtsAval:

    DEFINE INPUT  PARAMETER pDtIni AS DATE        NO-UNDO.
    DEFINE INPUT  PARAMETER pDtFim AS DATE        NO-UNDO.
    ASSIGN dtIniAval = pDtIni
           dtFimAval = pDtFim .



END PROCEDURE.

PROCEDURE setCodUsuario:

    DEFINE INPUT  PARAMETER pCodUsuario AS CHARACTER   NO-UNDO.
    ASSIGN codUsuario = pCodUsuario.


END PROCEDURE.


PROCEDURE vencerHistAvalPedVendaValido:
    DEFINE INPUT  PARAMETER pIdHist AS INTEGER     NO-UNDO.
    //DEFINE VARIABLE idHist AS INTEGER     NO-UNDO.
    FIND hist_aval_ped_venda
        WHERE hist_aval_ped_venda.hist_aval_ped_venda_id = idUltValid NO-LOCK NO-ERROR.
    IF AVAIL hist_aval_ped_venda THEN DO:
       FIND CURRENT hist_aval_ped_Venda EXCLUSIVE-LOCK NO-ERROR.
       ASSIGN hist_aval_ped_venda.hist_aval_ped_venda_relac_id = pIdHist  .
       FIND CURRENT hist_aval_ped_Venda NO-LOCK NO-ERROR.
       RUN setMsg IN hBoMsgHistAval(110, 'vencido hist aval valido:' + STRING(pIdHist),'log').
    END.
    ELSE DO:                                                         
       RUN setMsg IN hBoMsgHistAval(111, 'NAO ENCONTRADO e POR ISTO NAO vencido hist aval valido:' + STRING(pIdhist),'log').
    END.
END PROCEDURE.


PROCEDURE getIdValidHistAvalPedVenda:
   /*
   ‚ necess rio definir o tipo de aprova‡Æo, e a chave do pedido pelas procedures proprias antes de chamar esta metodo procedure
   */

   DEFINE OUTPUT PARAMETER pId AS INTEGER     NO-UNDO.
  //busca o id do historico de avalia‡Æo do pedido corrente pendente de avalia‡Æo
   FIND LAST hist_aval_ped_venda
       WHERE hist_aval_ped_venda.ped_web_id                     = iPedWebId
       AND   hist_aval_ped_venda.cod_Estab                      = cEstab
       AND   hist_aval_ped_venda.nr_pedido                      = iNrPedido
       AND   hist_aval_ped_venda.hist_aval_ped_venda_relac_id   = 0
       AND   hist_aval_ped_venda.cod_tipo_aprov                 = iCodTipo
       NO-LOCK NO-ERROR.
   IF AVAIL hist_aval_ped_venda THEN
      ASSIGN pid                = hist_aval_ped_venda.hist_aval_ped_venda_id.
   
END PROCEDURE.

PROCEDURE getIdVencidoHistAvalPedVenda:

   /*
   ‚ necess rio definir o tipo de aprova‡Æo, e a chave do pedido pelas procedures proprias antes de chamar esta metodo procedure
   */

   DEFINE OUTPUT PARAMETER pId AS INTEGER     NO-UNDO.
  //busca o id do historico de avalia‡Æo do pedido corrente pendente de avalia‡Æo
   FIND LAST hist_aval_ped_venda
       WHERE hist_aval_ped_venda.ped_web_id                     = iPedWebId
       AND   hist_aval_ped_venda.cod_Estab                      = cEstab
       AND   hist_aval_ped_venda.nr_pedido                      = iNrPedido
       AND   hist_aval_ped_venda.hist_aval_ped_venda_relac_id   = 0
       AND   hist_aval_ped_venda.dt_hr_limite_aval              < NOW
       AND   hist_aval_ped_Venda.ind_situacao                   <> 1
       AND   hist_aval_ped_venda.cod_tipo_aprov                 = iCodTipo
       NO-LOCK NO-ERROR.
   IF AVAIL hist_aval_ped_venda THEN
      ASSIGN pid  = hist_aval_ped_venda.hist_aval_ped_venda_id.
   
END PROCEDURE.



PROCEDURE getDadosValidHistAvalPedVenda:
    //‚ necess rio definir o tipo de aprova‡Æo, e a chave do pedido pelas procedures proprias antes de chamar esta metodo procedure
   //busca os dados do historico de avalia‡Æo do pedido corrente pendente de avalia‡Æo
    DEFINE BUFFER bHist FOR hist_aval_ped_venda.           
    /*IF c-seg-usuario = 'SUPER' THEN
    MESSAGE iPedWebId
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
    //RUN setMsg IN hBoMsgHistAval(999,'ped.Web:' + STRING(iPedWebId)  ,'aviso').
    //RUN setMsg IN hBoMsgHistAval(999,'estab:' + cEstab    ,'aviso').
    //RUN setMsg IN hBoMsgHistAval(999,'pedidoERP:' + string(iNrPedido)  ,'aviso'). 
    //RUN setMsg IN hBoMsgHistAval(999, 'tipo:' + string(iCodTipo)   ,'aviso').
    FIND LAST bhist
       WHERE bHist.ped_web_id                     = iPedWebId
       AND   bHist.cod_Estab                      = cEstab
       AND   bHist.nr_pedido                      = iNrPedido
       AND   bHist.hist_aval_ped_venda_relac_id   = 0
       AND   bHist.cod_tipo_aprov                 = iCodTipo
       NO-LOCK NO-ERROR.
    IF AVAIL bHist THEN DO:
       //RUN setMsg IN hBoMsgHistAval(999, 'Achou o historico'   ,'aviso').
       ASSIGN idUltValid         = bHist.hist_aval_ped_venda_id
             dtHrVencUltValid   = bHist.dt_hr_limite_aval 
             indSitUltValid     = bHist.ind_situacao .
    END.
    ELSE DO:
      //RUN setMsg IN hBoMsgHistAval(999, 'NAO Achou o historico'   ,'aviso').
      ASSIGN idUltValid        = 0
             dtHrVencUltValid   = ?
             indSitUltValid     = ? .
    END.
       

   
END PROCEDURE.

PROCEDURE verificarExistPend:
    DEFINE OUTPUT PARAMETER lTemPend AS LOGICAL     NO-UNDO.
    DEFINE BUFFER bf FOR hist_aval_ped_venda .
    FIND FIRST  bf
        WHERE bf.ped_web_id                    = iPedWebId
        AND   bf.cod_Estab                     = cEstab
        AND   bf.nr_pedido                     = iNrPedido
        AND   bf.hist_aval_ped_venda_relac_id  = 0
        AND   bf.cod_tipo_aprov                = iCodTipo
        AND   bf.ind_situacao                  = 0
        NO-LOCK NO-ERROR.
    ASSIGN ltemPend = AVAIL bf .


END PROCEDURE.


PROCEDURE inserirHistAvalPedVenda:
    //‚ necess rio definir o tipo de aprova‡Æo,chave do pedido, origem, motivo e descri‡Æo(quando necess rio) pelas procedures proprias antes de chamar esta metodo procedure
    DEFINE VARIABLE qtMinutos AS INTEGER     NO-UNDO.
    DEFINE VARIABLE dtHr      AS DATETIME    NO-UNDO.
    DEFINE VARIABLE idHist    AS INT64       NO-UNDO. 
    DEFINE VARIABLE lTemPend  AS LOGICAL     NO-UNDO.

    RUN  getIdValidHistAvalPedVenda(OUTPUT idHist). // id duplicado na variavel idUltValid da BO
    RUN setMsg IN hBoMsgHistAval(106, 'ID Hist. aval.:' + STRING(idhist),'log').
    RUN setMsg IN hBoMsgHistAval(107, 'Situa‡Æo:' + STRING(iSituacao),'log').
    CASE iSituacao:
        WHEN 0  THEN DO:    //calcula a data e hora de vencimento pelo tipo de aprova‡Æo
            RUN verificarExistPend(OUTPUT lTemPend).
            RUN setMsg IN hBoMsgHistAval(108, 'existe pendencia?' + STRING(lTemPend),'log').
            IF lTemPend THEN RETURN.
            RUN calcTempoVenctoTipoHist(iCodTipo, OUTPUT dtHr).
            RUN setMsg IN hBoMsgHistAval(109, 'tempo calculado:' + STRING(dtHr),'log').

        END.
       WHEN 4 THEN DO: // busca o tempo de prorroga‡Æo e soma a data hora limite do registro anterior
           RUN getQtMinutosVenctoAprovGerProrrog(OUTPUT qtMinutos). 
           ASSIGN dthr = ADD-INTERVAL(dtHrVencUltValid,qtMinutos,'minutes').
       END.
        WHEN 5 THEN DO: //solicitaca de alteracao
           ASSIGN dtHr = 01.01.2999. // neste caso o vencimento ser  pelo prazo da altera‡Æo do pedido e por este motivo esta data fica em aberto para nÆo vencer
        END.
        OTHERWISE  DO:
            // Para 1-aprova‡Æo, 2-reprova‡Æo e 3-analise mant‚m a data e hora limite do registro anterior
            ASSIGN dtHr = dtHrVencUltValid.
        END.
   END CASE.
   
   /*MESSAGE 'dt hr antes tratamento datas nao uteis' dtHr
       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/

   RUN tratarDatasNaoUteis(INPUT-OUTPUT dtHr).
   

   /*MESSAGE 'depois'
       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/

   CREATE hist_aval_ped_venda.
   ASSIGN hist_aval_ped_Venda.hist_aval_ped_venda_id   = NEXT-VALUE(seq_hist_aval_ped_venda)
          hist_aval_ped_venda.ped_Web_id               = iPedwebId
          hist_aval_ped_venda.cod_estab                = cEstab
          hist_aval_ped_venda.nr_pedido                = iNrPedido
          hist_aval_ped_venda.cod_usuario              = IF iSituacao = 0 THEN  '' ELSE codUsuario
          hist_aval_ped_venda.dt_hr_reg                = NOW
          hist_aval_ped_venda.dt_hr_aval               = IF iSituacao > 0 THEN hist_aval_ped_venda.dt_hr_reg ELSE ?
          hist_aval_ped_venda.cod_tipo                 = iCodTipo
          hist_aval_ped_Venda.dt_hr_limite_aval        = dtHr
          hist_aval_ped_Venda.ind_situacao             = iSituacao
          hist_aval_ped_venda.desc_motivo              = cDescMotivo 
          hist_aval_ped_venda.origem_id                = iOrigem 
          hist_aval_ped_venda.motivo_id                = iMotivo. 
   RUN vencerHistAvalPedVendaValido(hist_aval_ped_Venda.hist_aval_ped_venda_id).
   IF iPedwebId > 0 THEN DO:
      RUN atuSitPedWebPorSitAvalGer.
      
   END.
   ELSE DO:
       RUN setMsg IN hBoMsgHistAval(112, 'id pedweb zerado','log').
   END.
END PROCEDURE.

PROCEDURE setDthrReferCalctempo:
    DEFINE INPUT  PARAMETER pDtHr AS DATETIME    NO-UNDO.
    ASSIGN dtHrRefer = pDtHr . 

END PROCEDURE.

PROCEDURE calcTempoVenctoTipoHist:

    DEFINE INPUT  PARAMETER iTipo AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER dtHr  AS DATETIME    NO-UNDO.
    DEFINE VARIABLE cRetorno AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE qtMinutos     AS INTEGER     NO-UNDO.

    RUN getQtMinutosVenctoAprovGer(OUTPUT qtMinutos).
    IF dtHrRefer = ? THEN
       ASSIGN dthrRefer = NOW.

    /*MESSAGE 'itipo:' iTipo SKIP
             'dtHrRefer' dthrRefer SKIP
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
    CASE iTipo:
        WHEN 6 THEN // pend. aprov.gerencia
             ASSIGN dtHrRefer = ADD-INTERVAL (NOW,qtMinutos,'minutes').
        OTHERWISE
             ASSIGN dtHrRefer = 01.01.2999.

    END CASE.

    ASSIGN dtHr = dtHrRefer.


END PROCEDURE.


/*PROCEDURE getQtMinutosVenctoAprovGer:

    DEFINE OUTPUT PARAMETER qtMinutos AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cRetorno    AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cTipoVend   AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE hBoRepres   AS HANDLE      NO-UNDO.
    DEFINE VARIABLE iQtMinPad   AS INTEGER     NO-UNDO.
    
    DEFINE BUFFER bf FOR peds_web .
    FIND bf
        WHERE bf.ped_web_id = iPedWebId NO-LOCK NO-ERROR.
    IF AVAIL bf THEN DO:
       RUN esbo/boRepres.p PERSIST SET hBoRepres.
       RUN iniciarBos.
       RUN setCodRep IN hBoRepres(INPUT bf.repres_id).
       RUN getTipoVendedor IN  hBoRepres(OUTPUT cTipoVend).
       RUN finalizarBos.
       IF VALID-HANDLE(hBoRepres) THEN
          DELETE PROCEDURE hBoRepres.
    END.
    CASE cTipoVend:
        WHEN 'interno' THEN DO:
            IF bf.cod_tipo_pedido = 'pe' THEN DO:
               RUN getVlParametro('qt_min_venc_aprov_ger_loja_pe',OUTPUT cRetorno).
               ASSIGN iQtMinPad = 1440.
            END.
            ELSE DO: //pi
                RUN getVlParametro('qt_min_venc_aprov_ger_loja_pi',OUTPUT cRetorno).
               ASSIGN iQtMinPad = 60.
            END.  
        END.
        WHEN 'externo' THEN DO:
            IF bf.cod_tipo_pedido = 'pe' THEN DO:
               RUN getVlParametro('qt_min_venc_aprov_ger_pe',OUTPUT cRetorno).
               ASSIGN iQtMinPad = 120.
            END.
            ELSE DO: //pi
                RUN getVlParametro('qt_min_venc_aprov_ger_pi',OUTPUT cRetorno).
               ASSIGN iQtMinPad = 60.
            END.  
        END.
    END CASE.

    IF cRetorno <> '' THEN
       ASSIGN qtMinutos = INT(cRetorno).
    ELSE
       ASSIGN qtMinutos = iQtMinPad .

    

END PROCEDURE.*/

PROCEDURE getQtMinutosVenctoAprovGer:

    DEFINE OUTPUT PARAMETER qtMinutos AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cRetorno AS CHARACTER   NO-UNDO.
    RUN getVlParametro('qt_minutos_vencto_aprov_ger',OUTPUT cRetorno).
    IF cRetorno <> '' THEN
       ASSIGN qtMinutos = INT(cRetorno).
    ELSE
      ASSIGN qtMinutos = 1440 .


END PROCEDURE.

PROCEDURE getQtMinutosVenctoAprovGerProrrog:

    DEFINE OUTPUT PARAMETER qtMinutos AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cRet AS CHARACTER   NO-UNDO.
    RUN getVlParametro('qt_minutos_vencto_aprov_ger_prorrog',OUTPUT cRet).
    IF cRet <> '' THEN
       ASSIGN qtMinutos = INT(cRet).
    ELSE
      ASSIGN qtMinutos = 1440 .

END PROCEDURE.

PROCEDURE vencerPedsWebSemAprovGer:
    DEFINE VARIABLE id                  AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cListaIdsPendAprov  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iCont               AS INTEGER     NO-UNDO.
    DEFINE VARIABLE idVencido           AS INT64       NO-UNDO.
    RUN getIdsPedWebPorSituacao IN  hBoPedWeb(INPUT 8, //pend. aprov.ger
                                OUTPUT cListaIdsPendAprov).
    /*MESSAGE cListaIdsPendAprov
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
    REPEAT iCont = 1 TO NUM-ENTRIES(cListaIdsPendAprov,',').
        ASSIGN id  =  int(ENTRY(iCont,cListaIdsPendAprov,',')).
        RUN setChavePedido(id,'',0).
        RUN getIdVencidoHistAvalPedVenda(OUTPUT idVencido).
        /*MESSAGE 'id passado' id SKIP
                'id vencido' idVencido SKIP
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
        IF idVencido > 0 THEN
           RUN vencerPedWebPorId IN hBoPedWeb(id).
    END.                            
END PROCEDURE.

PROCEDURE getHistsPorSituacao:


END PROCEDURE.


PROCEDURE completarDescTT:


END PROCEDURE.

PROCEDURE avaliarListaPedsWeb : 
    /*
    ‚ necessario passar o cod_usuario,a ind_situacao e o tipo de aprova‡Æo pelas procedures proprias antes de chcmar este metodo.
    */
    DEFINE INPUT  PARAMETER cListaPed AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iCont             AS INTEGER     NO-UNDO.
    DEFINE VARIABLE id                AS INTEGER     NO-UNDO.
    DEFINE VARIABLE lErro             AS LOGICAL     NO-UNDO.

    REPEAT iCont = 1 TO NUM-ENTRIES(cListaPed,',').
        ASSIGN id  =  int(ENTRY(iCont,cListaPed,','))
               lErro = NO .
        RUN setMsg IN hBoMsgHistAval(100,'id pedido web:' + STRING(id),'log').
        RUN setChavePedido(id,'',0).
        RUN getDadosValidHistAvalPedVenda. // dados sÆo jogados em variaveis de escopo global dentro da BO
        //RUN setMsg IN hBoMsgHistAval(999, 'Situacao:' + STRING(iSituacao),'log').
        //RUN setMsg IN hBoMsgHistAval(999, 'Ult.Id valid:' + STRING(idUltValid),'log').
        
        IF idUltValid = 0 AND iSituacao > 0 THEN DO: // nÆo encontrou hist_aval para o pedido e  nÆo ‚ uma situa‡Æo inicial de pendencia(0)
           //'NÆo foi encontrado registro de avalia‡Æo para o Pedido de Venda web:'
           RUN setMsg IN hBoMsgHistAval(1, 'NÆo foi encontrado registro de avalia‡Æo para o Pedido de Venda web:' + STRING(id),'erro').
           ASSIGN lErro = YES.
        END. 
        ELSE DO:
           IF idUltValid > 0 THEN
              RUN setMsg IN hBoMsgHistAval(1, 'FOI encontrado registro de avalia‡Æo para o Pedido de Venda web:' + STRING(id),'log').
           ELSE
              RUN setMsg IN hBoMsgHistAval(1, 'Gera‡Æo de Pendencia Inicial:' + STRING(id),'log').

           IF indSitUltValid = 1 THEN do: //ja aprovado  
              RUN setMsg IN hBoMsgHistAval(2, 'Pendencia de Avalia‡Æo j  Aprovada - Pedido:' + STRING(id),'erro').
              
              //provisorio at‚ achar problema da situa‡Æo do pedido nÆo estar voltando
              FIND peds_web
                  WHERE peds_web.ped_web_id = id
                  AND   peds_web.ped_web_id = 8 //pend. aprov.ger.
                  NO-ERROR.
              IF AVAIL peds_web THEN
                 ASSIGN peds_web.ind_sit_ped_web = 2. // efetivado   

              ASSIGN lErro = YES.
           END.
           ELSE DO:
               RUN setMsg IN hBoMsgHistAval(102, 'Pendencia de Avalia‡Æo NÆo Aprovada - Pedido:' + STRING(id),'log').
               IF dtHrVencUltValid < NOW THEN DO:
                  //'Pendencia de Avalia‡Æo j  Vencida'
                  RUN setMsg IN hBoMsgHistAval(3,'Pendencia de Avalia‡Æo j  Vencida - Pedido:' + STRING(id),'erro').
                  ASSIGN lErro = YES.
               END.
               ELSE DO:
                RUN setMsg IN hBoMsgHistAval(103,'Pendencia de Avalia‡Æo ainda nÆo Vencida - Pedido:' + STRING(id),'log').
               END.
           END.
        END.

        IF lErro = NO THEN DO:
           RUN setMsg IN hBoMsgHistAval(104, 'NÆo houve erro','log').
           RUN inserirHistAvalPedVenda. 
           //'Avalia‡Æo do Pedido  feita com sucesso' 
           RUN setMsg IN hBoMsgHistAval(0,  string(id) ,'aviso').
        END.
        ELSE DO:
           RUN setMsg IN hBoMsgHistAval(105, 'houve erro','erro').
        END.
    END.
    RUN gravarLogCalculo IN hBoMsgHistAval(6).
END PROCEDURE.

PROCEDURE getHandleMsg:
    DEFINE OUTPUT PARAMETER pHandle AS HANDLE      NO-UNDO.
    ASSIGN pHandle = hBoMsgHistAval.


END PROCEDURE.


PROCEDURE getAprovPedido:

    DEFINE OUTPUT PARAMETER cAprov AS CHARACTER   NO-UNDO.
    //‚ necess rio definir o tipo de aprova‡Æo, e a chave do pedido pelas procedures proprias antes de chamar esta metodo procedure
    //busca os dados do historico de avalia‡Æo do pedido corrente pendente de avalia‡Æo
    DEFINE BUFFER bHist FOR hist_aval_ped_venda.               
    FIND LAST bhist
       WHERE bHist.ped_web_id                     = iPedWebId
       AND   bHist.cod_Estab                      = cEstab
       AND   bHist.nr_pedido                      = iNrPedido
       AND   bHist.hist_aval_ped_venda_relac_id   = 0
       AND   bHist.cod_tipo_aprov                 = iCodTipo
       AND   bHist.ind_situacao                   = 1 //aprovado
       NO-LOCK NO-ERROR.
    IF AVAIL bHist THEN
       ASSIGN cAprov = bhist.cod_usuario .
          
END PROCEDURE.



PROCEDURE sincrHistAvalPedVendaGer:

    DEFINE INPUT        PARAMETER pEstab       AS CHARACTER   NO-UNDO.
    DEFINE INPUT        PARAMETER pNrPedido    AS INTEGER     NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER pAprovador   AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iPedido AS INTEGER     NO-UNDO.

    FIND FIRST peds_web
        WHERE  peds_web.cod_estabel   = pEstab
        AND    peds_web.nr_pedido_erp = pNrPedido
        NO-LOCK NO-ERROR.
    IF AVAIL peds_web THEN DO:
       ASSIGN iPedido   =  peds_web.ped_web_id 
              pNrPedido = 0
              pEstab    = ''.
    END.
     

    RUN setChavePedido( iPedido,pEstab,pnrPedido).
    RUN setCodTipo(6). // aprov. gerencial
    
    IF pAprovador = '' THEN DO:
       RUN getAprovPedido(OUTPUT pAprovador).
    END.
    ELSE DO:
        RUN setCodTipo(6).   // aprov. gerencial
        RUN setSituacao(0).  //pendencia      
        RUN setMotivo(3).   //digita‡Æo        
        RUN setOrigem(3).   // digita‡Æo     

        RUN inserirHistAvalPedVenda.         
        RUN setcodUsuario(pAprovador).      
        RUN setSituacao(1). //aprovado       
        RUN inserirHistAvalPedVenda.  
    END.                              
END PROCEDURE.

PROCEDURE tratarDatasNaoUteis:

    DEFINE INPUT-OUTPUT PARAMETER pDtHr AS DATETIME    NO-UNDO.
    DEFINE VARIABLE dtHr                AS DATETIME    NO-UNDO.
    DEFINE VARIABLE codCalend           AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE lDiaUtil            AS LOGICAL     NO-UNDO.

    RUN getCalendAprovGer               IN hBoConsParam(OUTPUT codCalend).
    RUN setCalend                       IN hBoCalendGlob(codCalend).
    RUN setData                         IN hBoCalendGlob(DATE(pDtHr)).
    RUN verifDiaUtil                    IN hBoCalendGlob(OUTPUT lDiaUtil).
    /*MESSAGE pdthr SKIP
        ldiautil
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
    ASSIGN dtHr = pDtHr.
    IF NOT lDiaUtil AND dtHr <> ? AND date(dtHr) <> 01.01.2999 THEN DO:
       
       REPEAT:
          ASSIGN dtHr = ADD-INTERVAL(dtHr,1,'days').
          RUN setData IN hBoCalendGlob(DATE(dtHr)).
          RUN verifDiaUtil IN hBoCalendGlob(OUTPUT lDiaUtil).
          IF lDiaUtil THEN LEAVE.
       END.
    END.           
    ASSIGN pDtHr = dtHr.
END PROCEDURE.



PROCEDURE atuSitPedWebPorSitAvalGer:
    
    
    //DEFINE INPUT  PARAMETER cListaAvals AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE dtHr        AS DATETIME    NO-UNDO.
    DEFINE VARIABLE qtMinutos   AS INTEGER     NO-UNDO.
    
    FIND peds_web
        WHERE peds_web.ped_web_id = iPedWebId NO-LOCK NO-ERROR.
    IF AVAIL peds_web THEN DO:
       RUN setMsg IN hBoMsgHistAval(113, 'sit aval:'  + string(iSituacao) +  '- achei pedido de venda web:' + string(peds_web.ped_web_id) ,'log').
       
       IF Peds_web.cod_tipo_pedido = 'PE' THEN
          RUN getQtMinutosAltPedidoPE(OUTPUT qtMinutos).
       ELSE 
          RUN getQtMinutosAltPedidoPI(OUTPUT qtMinutos).
    
       ASSIGN dtHr = NOW
              dtHr = ADD-INTERVAL(dtHr,qtMinutos,'minutes').  
       RUN setSitPedWebGer(rowid(peds_web), dthr).
           
       
    END.
    ELSE DO:
        RUN setMsg IN hBoMsgHistAval(414, 'NAO achei pedido de venda web' ,'log').
    END.
    OUTPUT CLOSE.
END PROCEDURE.

PROCEDURE setSitPedWebGer:

    DEFINE INPUT  PARAMETER pRowid          AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER dtHr            AS DATETIME    NO-UNDO.
    FIND peds_web
        WHERE rowid(peds_web) = pRowid SHARE-LOCK.
    IF AVAIL peds_web THEN DO:
       IF peds_web.ind_sit_ped_web = 8 THEN DO:
           RUN setMsg IN hBoMsgHistAval(440,
                     'entrei na op‡Æo pend. aprov.ger.(8)' ,
                     'log').
           IF iSituacao = 1 THEN DO:
              ASSIGN peds_web.ind_sit_ped_web = 2. //efetivado
              RUN setMsg IN hBoMsgHistAval(441,
                                               'Situa‡Æo de avalia‡Æo 1 , mudei o pedido para efetivado' ,
                                               'log').
           END.
           IF iSituacao = 2 THEN DO:
              ASSIGN peds_web.ind_sit_ped_web = 3
                     peds_web.descr_rejei    = 'Cancelado por reprova‡Æo da Gerˆncia'. // cancelado
              RUN setMsg IN hBoMsgHistAval(442,
                                         'Situa‡Æo de avalia‡Æo 2 , mudei o pedido para Cancelado' ,
                                          'log').
           END.  
       END.

       IF peds_web.ind_sit_ped_web = 9 THEN DO: //1-em digita‡Æo se aprovado vai para 2-efetivado
          RUN setMsg IN hBoMsgHistAval(130, 
                                      'entrei na op‡Æo em digita‡Æo(9)' ,
                                      'log').
          IF iSituacao = 1 THEN DO:
             ASSIGN peds_web.ind_sit_ped_web = 2. //efetivado
             RUN setMsg IN hBoMsgHistAval(131,
                                         'Situa‡Æo de avalia‡Æo 1 , mudei o pedido para efetivado' ,
                                         'log').
          END.  
          IF iSituacao = 2 THEN DO:
              ASSIGN peds_web.ind_sit_ped_web = 3
                      peds_web.descr_rejei    = 'Cancelado por reprova‡Æo da Gerˆncia'. // cancelado
              RUN setMsg IN hBoMsgHistAval(132, 
                                          'Situa‡Æo de avalia‡Æo 2 , mudei o pedido para Cancelado' ,
                                          'log').
          END.
       END.
       FIND CURRENT peds_web NO-LOCK NO-ERROR.
    END.



END PROCEDURE.



PROCEDURE getQtMinutosAltPedidoPE:
    
    DEFINE OUTPUT PARAMETER qtMinutos   AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cRet                AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cParam              AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE qtMinutosPadrao     AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cTipoVend           AS CHARACTER   NO-UNDO.
    RUN setCodRep        IN hboRepres(peds_web.repres_id).
    RUN getTipoVendedor  IN hboRepres(OUTPUT cTipoVend).
    IF cTipoVend = 'externo' THEN
       ASSIGN cParam = 'qt_minutos_alt_pedido_pe'
              qtMinutosPadrao = 120.
    ELSE
       ASSIGN cParam = 'qt_minutos_alt_pedido_pe_loja'
              qtMinutosPadrao = 1440.

    RUN getVlParametro(cParam,OUTPUT cRet).
    IF cRet <> '' THEN
       ASSIGN qtMinutos = INT(cRet).
    ELSE
      ASSIGN qtMinutos = qtMinutosPadrao.

END PROCEDURE.

PROCEDURE getQtMinutosAltPedidoPI:
    
    DEFINE OUTPUT PARAMETER qtMinutos AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cRet AS CHARACTER   NO-UNDO.
    RUN getVlParametro('qt_minutos_alt_pedido_pi',OUTPUT cRet).
    IF cRet <> '' THEN
       ASSIGN qtMinutos = INT(cRet).
    ELSE
      ASSIGN qtMinutos = 120 .           

END PROCEDURE.

