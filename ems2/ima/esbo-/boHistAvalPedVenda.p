/*************************************************************************************************************************************************
programa: esbo/boHistAvalPedVenda.p
objetivo: manter a tabela hist_aval_ped_venda e todas as regras de negocio existentes para geraá∆o das pendencias de avaliaá∆o do pedido de venda.
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
  DEFINE VARIABLE hBoCalendGlob   AS HANDLE      NO-UNDO.

PROCEDURE iniciarBOs:

    IF NOT VALID-HANDLE(hBoPedWeb) THEN
       RUN esbo/boPedsweb.p PERSISTENT SET hBoPedWeb.
    IF NOT VALID-HANDLE(hBoMsgHistAval) THEN
       RUN esbo/boMsg.p PERSISTENT SET hBoMsgHistAval.
    IF NOT VALID-HANDLE(hBoConsParam) THEN  
       RUN esbo/boConsParam.p PERSISTENT SET hBoConsParam.
    IF NOT VALID-HANDLE(hBoCalendGlob) THEN  
       RUN esbo/boCalendGlob.p PERSISTENT SET hBoCalendGlob.


END PROCEDURE.

PROCEDURE finalizarBOs:
    IF VALID-HANDLE(hBoPedWeb) THEN
       DELETE PROCEDURE hBoPedWeb.

    IF VALID-HANDLE(hBoMsgHistAval) THEN
       DELETE PROCEDURE hBoMsgHistAval.

    IF  VALID-HANDLE(hBoConsParam) THEN  
       DELETE PROCEDURE hBoConsParam.
    IF NOT VALID-HANDLE(hBoCalendGlob) THEN  
       DELETE PROCEDURE hBoCalendGlob.

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
   Ç necess†rio definir o tipo de aprovaá∆o, e a chave do pedido pelas procedures proprias antes de chamar esta metodo procedure
   */

   DEFINE OUTPUT PARAMETER pId AS INTEGER     NO-UNDO.
  //busca o id do historico de avaliaá∆o do pedido corrente pendente de avaliaá∆o
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
   Ç necess†rio definir o tipo de aprovaá∆o, e a chave do pedido pelas procedures proprias antes de chamar esta metodo procedure
   */

   DEFINE OUTPUT PARAMETER pId AS INTEGER     NO-UNDO.
  //busca o id do historico de avaliaá∆o do pedido corrente pendente de avaliaá∆o
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
    //Ç necess†rio definir o tipo de aprovaá∆o, e a chave do pedido pelas procedures proprias antes de chamar esta metodo procedure
   //busca os dados do historico de avaliaá∆o do pedido corrente pendente de avaliaá∆o
    DEFINE BUFFER bHist FOR hist_aval_ped_venda.               
    //RUN setMsg IN hBoMsgHistAval(999,'pedidoWeb:' + STRING(iPedWebId) + '-estab:' + cEstab + '-pedidoERP:' + string(iNrPedido) +    'tipo:' + string(iCodTipo)   ,'aviso').
    FIND LAST bhist
       WHERE bHist.ped_web_id                     = iPedWebId
       AND   bHist.cod_Estab                      = cEstab
       AND   bHist.nr_pedido                      = iNrPedido
       AND   bHist.hist_aval_ped_venda_relac_id   = 0
       AND   bHist.cod_tipo_aprov                 = iCodTipo
       NO-LOCK NO-ERROR.
    IF AVAIL bHist THEN
      ASSIGN idUltValid         = bHist.hist_aval_ped_venda_id
             dtHrVencUltValid   = bHist.dt_hr_limite_aval 
             indSitUltValid     = bHist.ind_situacao .
    ELSE
       ASSIGN idUltValid        = 0
             dtHrVencUltValid   = ?
             indSitUltValid     = ? .

   
END PROCEDURE.

PROCEDURE verificarExistPend:
    DEFINE OUTPUT PARAMETER lTemPend AS LOGICAL     NO-UNDO.
    DEFINE BUFFER bf FOR hist_aval_ped_venda .
    FIND FIRST  bf
        WHERE bf.hist_aval_ped_venda_id        = iPedWebId
        AND   bf.cod_Estab                     = cEstab
        AND   bf.nr_pedido                     = iNrPedido
        AND   bf.hist_aval_ped_venda_relac_id  = 0
        AND   bf.cod_tipo_aprov                = iCodTipo
        AND   bf.ind_situacao                  = 0
        NO-LOCK NO-ERROR.
    ASSIGN ltemPend = AVAIL bf .


END PROCEDURE.


PROCEDURE inserirHistAvalPedVenda:
    //Ç necess†rio definir o tipo de aprovaá∆o,chave do pedido, origem, motivo e descriá∆o(quando necess†rio) pelas procedures proprias antes de chamar esta metodo procedure
    DEFINE VARIABLE qtMinutos AS INTEGER     NO-UNDO.
    DEFINE VARIABLE dtHr      AS DATETIME    NO-UNDO.
    DEFINE VARIABLE idHist    AS INT64       NO-UNDO. 
    DEFINE VARIABLE lTemPend  AS LOGICAL     NO-UNDO.
    RUN  getIdValidHistAvalPedVenda(OUTPUT idHist). // id duplicado na variavel idUltValid da BO
    RUN setMsg IN hBoMsgHistAval(106, 'ID Hist. aval.:' + STRING(idhist),'log').
    RUN setMsg IN hBoMsgHistAval(107, 'Situaá∆o:' + STRING(iSituacao),'log').
    CASE iSituacao:
        WHEN 0  THEN DO:    //calcula a data e hora de vencimento pelo tipo de aprovaá∆o
            RUN verificarExistPend(OUTPUT lTemPend).
            RUN setMsg IN hBoMsgHistAval(108, 'existe pendencia?' + STRING(lTemPend),'log').
            IF lTemPend THEN RETURN.
            RUN calcTempoVenctoTipoHist(iCodTipo, OUTPUT dtHr).
            RUN setMsg IN hBoMsgHistAval(109, 'tempo calculado:' + STRING(dtHr),'log').

        END.
       WHEN 4 THEN DO: // busca o tempo de prorrogaá∆o e soma a data hora limite do registro anterior
           RUN getQtMinutosVenctoAprovGerProrrog(OUTPUT qtMinutos). 
           ASSIGN dthr = ADD-INTERVAL(dtHrVencUltValid,qtMinutos,'minutes').
       END.
        WHEN 5 THEN DO: //solicitaca de alteracao
           ASSIGN dtHr = 01.01.2999. // neste caso o vencimento ser† pelo prazo da alteraá∆o do pedido e por este motivo esta data fica em aberto para n∆o vencer
        END.
        OTHERWISE  DO:
            // Para 1-aprovaá∆o, 2-reprovaá∆o e 3-analise mantÇm a data e hora limite do registro anterior
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
      RUN setHandleMsgExterno IN hBoPedWeb(hBoMsgHistAval).
      RUN atuSitPedWebPorSitAvalGer IN hBoPedWeb(iSituacao).
   END.
   ELSE
     RUN setMsg IN hBoMsgHistAval(112, 'id pedweb zerado','log').

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
    Ç necessario passar o cod_usuario,a ind_situacao e o tipo de aprovaá∆o pelas procedures proprias antes de chcmar este metodo.
    */
    DEFINE INPUT  PARAMETER cListaPed AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iCont             AS INTEGER     NO-UNDO.
    DEFINE VARIABLE id                AS INTEGER     NO-UNDO.
    DEFINE VARIABLE lErro             AS LOGICAL      NO-UNDO.

    REPEAT iCont = 1 TO NUM-ENTRIES(cListaPed,',').
        ASSIGN id  =  int(ENTRY(iCont,cListaPed,','))
               lErro = NO .
        RUN setMsg IN hBoMsgHistAval(100,'id pedido web:' + STRING(id),'log').
        RUN setChavePedido(id,'',0).
        RUN getDadosValidHistAvalPedVenda. // dados s∆o jogados em variaveis de escopo global dentro da BO
        IF idUltValid = 0 AND iSituacao > 0 THEN DO: // n∆o encontrou hist_aval para o pedido e  n∆o Ç uma situaá∆o inicial de pendencia(0)
           //'N∆o foi encontrado registro de avaliaá∆o para o Pedido de Venda web:'
           RUN setMsg IN hBoMsgHistAval(1, 'N∆o foi encontrado registro de avaliaá∆o para o Pedido de Venda web:' + STRING(id),'erro').
           ASSIGN lErro = YES.
        END. 
        ELSE DO:
           RUN setMsg IN hBoMsgHistAval(1, 'FOI encontrado registro de avaliaá∆o para o Pedido de Venda web:' + STRING(id),'log').
           IF indSitUltValid = 1 THEN do: //ja aprovado
              // 
              RUN setMsg IN hBoMsgHistAval(2, 'Pendencia de Avaliaá∆o j† Aprovada - Pedido:' + STRING(id),'erro').
              ASSIGN lErro = YES.
           END.
           ELSE DO:
               RUN setMsg IN hBoMsgHistAval(102, 'Pendencia de Avaliaá∆o N∆o Aprovada - Pedido:' + STRING(id),'log').
               IF dtHrVencUltValid < NOW THEN DO:
                  //'Pendencia de Avaliaá∆o j† Vencida'
                  RUN setMsg IN hBoMsgHistAval(3,'Pendencia de Avaliaá∆o j† Vencida - Pedido:' + STRING(id),'erro').
                  ASSIGN lErro = YES.
               END.
               ELSE DO:
                RUN setMsg IN hBoMsgHistAval(103,'Pendencia de Avaliaá∆o ainda n∆o Vencida - Pedido:' + STRING(id),'log').
               END.
           END.
        END.

        IF lErro = NO THEN DO:
           RUN setMsg IN hBoMsgHistAval(104, 'N∆o houve erro','log').
           RUN inserirHistAvalPedVenda. 
           //'Avaliaá∆o do Pedido  feita com sucesso' 
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
    //Ç necess†rio definir o tipo de aprovaá∆o, e a chave do pedido pelas procedures proprias antes de chamar esta metodo procedure
    //busca os dados do historico de avaliaá∆o do pedido corrente pendente de avaliaá∆o
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
        RUN setMotivo(3).   //digitaá∆o        
        RUN setOrigem(3).   // digitaá∆o     

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



