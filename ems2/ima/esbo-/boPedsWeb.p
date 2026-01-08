/* Programa: boPedsWeb
** Modulo..: Pedido Web
** Objetivo: Manutená∆o da tabela peds_web
** Autor...: Tadeu - 08/2020
11/2021 - incorporaá∆o de vencimento na BO j† considerando o processo de aprovaá∆o gerencial
*/
DEFINE VARIABLE rRowid      AS ROWID       NO-UNDO.
DEFINE VARIABLE cTipoVend   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hBoLogTxt   AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoMsgExterno AS HANDLE      NO-UNDO.
{esp/util.i}

PROCEDURE setHandleMsgExterno:
DEFINE INPUT  PARAMETER hParam AS HANDLE      NO-UNDO.
ASSIGN hBoMsgExterno = hParam.

END PROCEDURE.

PROCEDURE setPedWebId:

    DEFINE INPUT  PARAMETER Pid AS INTEGER     NO-UNDO.
    FIND peds_web
        WHERE peds_web.ped_web_id = Pid
        NO-LOCK NO-ERROR.
    ASSIGN rRowid = rowid(peds_web).

END PROCEDURE.

PROCEDURE setRowid:

    DEFINE INPUT  PARAMETER prowid AS ROWID       NO-UNDO.
    ASSIGN rRowid = pRowid.

END PROCEDURE.


PROCEDURE setIndSitPedWeb:
    DEFINE INPUT  PARAMETER iIndSitPedWeb AS INTEGER     NO-UNDO.
    RUN setPropPedWeb('ind_sit_ped_web',STRING(iIndSitPedweb)).
    
END PROCEDURE.

PROCEDURE setDescrRejeicao:
    DEFINE INPUT  PARAMETER cDescr LIKE peds_web.descr_rejei    NO-UNDO.
    RUN setPropPedWeb('descr_rejei',cDescr).

END PROCEDURE.

PROCEDURE setNrPedidoERP:

    DEFINE INPUT  PARAMETER iNrPedido AS INTEGER NO-UNDO.
    RUN setPropPedWeb('nr_pedido_erp',string(iNrPedido)).
END PROCEDURE.


PROCEDURE setTranspId:

    DEFINE INPUT  PARAMETER iTransp AS INTEGER NO-UNDO.
    RUN setPropPedWeb('transp_id',string(iTransp)).

END PROCEDURE.

PROCEDURE setDtHrVencto:
    
    DEFINE INPUT  PARAMETER dtHrVencto  AS DATETIME    NO-UNDO.
    RUN setPropPedWeb('dt_hr_vencto',string(dtHrVencto)).  


END PROCEDURE.



PROCEDURE setPropPedWeb:

    DEFINE INPUT  PARAMETER cCampo AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER cValor AS CHARACTER NO-UNDO.
    FIND peds_web
        WHERE ROWID(peds_web) = rRowid SHARE-LOCK.
    IF AVAIL peds_web THEN DO:

       CASE cCampo:
           WHEN 'transp_id' THEN
              ASSIGN peds_web.transp_id = INT(cValor).
           WHEN 'ind_sit_ped_web' THEN
              ASSIGN peds_web.ind_sit_ped_web = INT(cValor).
           WHEN 'descr_rejei' THEN
              ASSIGN peds_web.descr_rejei = cValor.
           WHEN 'dt_hr_vencto' THEN
              ASSIGN peds_web.dt_hr_vencto = DATETIME(cValor).
           WHEN 'nr_pedido_erp' THEN
              ASSIGN peds_web.nr_pedido_erp = INT(cValor).
       END CASE. 
       FIND CURRENT peds_web NO-LOCK NO-ERROR.
    END.

END PROCEDURE.


PROCEDURE atuSitPedWebPorSitAvalGer:

    DEFINE INPUT  PARAMETER iSitAvalGer AS INTEGER     NO-UNDO.
    //DEFINE INPUT  PARAMETER cListaAvals AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE dtHr        AS DATETIME    NO-UNDO.
    DEFINE VARIABLE qtMinutos   AS INTEGER     NO-UNDO.
    DEFINE VARIABLE hBoRepres   AS HANDLE      NO-UNDO.
    FIND peds_web
        WHERE ROWID(peds_web) = rRowid SHARE-LOCK.
    IF AVAIL peds_web THEN DO:
       RUN setMsg IN hBoMsgExterno(113, 'sit aval:'  + string(iSitAvalGer) +  '- achei pedido de venda web:' + string(peds_web.ped_web_id) ,'log').
       
       RUN esbo/boRepres.p PERSISTENT SET hBoRepres.
       RUN setCodRep        IN hboRepres(peds_web.repres_id).
       RUN getTipoVendedor  IN hboRepres(OUTPUT cTipoVend).
       DELETE PROCEDURE hBoRepres.

       IF Peds_web.cod_tipo_pedido = 'PE' THEN
          RUN getQtMinutosAltPedidoPE(OUTPUT qtMinutos).
       ELSE 
          RUN getQtMinutosAltPedidoPI(OUTPUT qtMinutos).

       ASSIGN dtHr = NOW
              dtHr = ADD-INTERVAL(dtHr,qtMinutos,'minutes').

       CASE peds_web.ind_sit_ped_web:
           WHEN 9 THEN DO: //1-em digitaá∆o se aprovado vai para 2-efetivado
               RUN setMsg IN hBoMsgExterno(130, 'entrei na opá∆o em digitaá∆o(9)' ,'log').
               IF iSitAvalGer = 1 THEN DO:
                  ASSIGN peds_web.ind_sit_ped_web = 2. //efetivado
                  RUN setMsg IN hBoMsgExterno(131, 'Situaá∆o de avaliaá∆o 1 , mudei o pedido para efetivado' ,'log').
               END.
                  
               IF iSitAvalGer = 2 THEN DO:
                   ASSIGN peds_web.ind_sit_ped_web = 3
                           peds_web.descr_rejei    = 'Cancelado por reprovaá∆o da Gerància'. // cancelado
                   RUN setMsg IN hBoMsgExterno(132, 'Situaá∆o de avaliaá∆o 2 , mudei o pedido para Cancelado' ,'log').

               END.
           END.  
           WHEN 8 THEN DO:
               RUN setMsg IN hBoMsgExterno(140, 'entrei na opá∆o pend. aprov.ger.(8)' ,'log').
               IF iSitAvalGer = 1 THEN DO:
                   ASSIGN peds_web.ind_sit_ped_web = 2. //efetivado
                   RUN setMsg IN hBoMsgExterno(141, 'Situaá∆o de avaliaá∆o 1 , mudei o pedido para efetivado' ,'log').

               END.
               IF iSitAvalGer = 2 THEN DO:
                  ASSIGN peds_web.ind_sit_ped_web = 3
                          peds_web.descr_rejei    = 'Cancelado por reprovaá∆o da Gerància'. // cancelado
                  RUN setMsg IN hBoMsgExterno(142, 'Situaá∆o de avaliaá∆o 2 , mudei o pedido para Cancelado' ,'log').
               END.
                   
               IF iSitAvalGer = 5 THEN DO:
                  ASSIGN peds_web.ind_sit_ped_web = 9 // em digitaá∆o   
                         peds_web.dt_hr_vencto =   dtHr .
                  RUN setMsg IN hBoMsgExterno(143, 'Situaá∆o de avaliaá∆o 5 , mudei o pedido para Em Digitaá∆o' ,'log').
               END.
           END.
           OTHERWISE DO:
               RUN setMsg IN hBoMsgExterno(150, 'Situaá∆o do pedido web(' + string(peds_web.ind_sit_ped_web) + ') n∆o tratada.' ,'log').


           END.
       END CASE.
       FIND CURRENT peds_web NO-LOCK NO-ERROR.
    END.
    ELSE DO:
        RUN setMsg IN hBoMsgExterno(114, 'NAO achei pedido de venda web' ,'log').
    END.
END PROCEDURE.



PROCEDURE getQtMinutosAltPedidoPE:

    DEFINE OUTPUT PARAMETER qtMinutos   AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cRet                AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cParam              AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE qtMinutosPadrao     AS INTEGER     NO-UNDO.
    IF cTipoVend = 'externo' THEN
       ASSIGN cParam = 'qt_minutos_alt_pedido_pe'
              qtMinutosPadrao = 1440.
    ELSE
       ASSIGN cParam = 'qt_minutos_alt_pedido_pe_loja'
              qtMinutosPadrao = 120.

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

PROCEDURE getIdsPedWebPorSituacao:
    DEFINE INPUT  PARAMETER pSituacao AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER cLista    AS CHARACTER   NO-UNDO.
    FOR EACH peds_web
        WHERE peds_web.ind_sit_ped_web = pSituacao NO-LOCK.
        RUN incrValor(INPUT-OUTPUT cLista, 
                      INPUT string(peds_web.ped_web_id),
                      INPUT  ",").
    END.

END PROCEDURE.


PROCEDURE vencerPedWebPorId:
    DEFINE INPUT  PARAMETER idPedWeb AS INTEGER     NO-UNDO.
    DEFINE BUFFER bfPedWeb FOR peds_web.
        /*MESSAGE 'id a ser vencido' idPedWeb SKIP
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
    FIND bfPedWeb
        WHERE bfPedWeb.ped_web_id = idPedweb
        NO-LOCK NO-ERROR.
    IF AVAIL bfPedWeb THEN DO:
       FIND CURRENT bfPedWeb EXCLUSIVE-LOCK NO-ERROR.
       /*MESSAGE 'entrei para vencer o pedido'
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
       ASSIGN bfPedWeb.ind_sit_ped_web = 6 //vencido.
              bfPedWeb.dt_hr_vencto_real = NOW.

       FIND CURRENT bfPedWeb NO-LOCK NO-ERROR.   
    END.
END PROCEDURE.

PROCEDURE setBoLogTxt:

    DEFINE INPUT  PARAMETER phBoLogTxt AS HANDLE      NO-UNDO.
    ASSIGN hboLogTxt = phBoLogTxt.


END PROCEDURE.

PROCEDURE getBoLogTxt:

    DEFINE OUTPUT  PARAMETER phBoLogTxt AS HANDLE      NO-UNDO.
    ASSIGN phboLogTxt = hBoLogTxt.

END PROCEDURE.

PROCEDURE vencerPedsWeb:

    DEFINE BUFFER bfPW FOR peds_web.
    DEFINE VARIABLE codParam AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vlParam  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE dAgora   AS DATETIME    NO-UNDO.
    DEFINE VARIABLE cDescLog AS CHARACTER   NO-UNDO.

    ASSIGN dAgora = NOW.
       
    RUN incrSeqDados IN hBologTxt.
    RUN setDados IN hBologtxt('bloco','vencerPedidos').
    RUN setDados IN hBologtxt('acao','INICIO').
    RUN setDados IN hBologtxt('descricao','').
    RUN gerarLogs IN hBoLogtxt.

    //vence os pedidos que n∆o foram efetivados a tempo
    FOR EACH bfPW WHERE
             bfPW.ind_sit_ped_web = 1 NO-LOCK.

        IF bfPW.cod_tipo_pedido = 'PI' THEN
           ASSIGN codParam = 'PORTAL_MINUTOS_DESALOCAR_PI'.
        ELSE 
          ASSIGN codParam = 'PORTAL_MINUTOS_DESALOCAR_PE'.
                
        RUN getVlParametro(INPUT codParam,
                          OUTPUT vlParam).

        IF vlParam = '' THEN DO: // n∆o encontrado parametro
           ASSIGN cDesclog = cDescLog + " - PARAMETRO NAO ENCONTRADO ".
           RUN setDados IN hBologtxt('severidade','ERRO').
        END.
        ELSE DO:
           ASSIGN cDescLog = cDescLog + " - valor -> " + vlParam .
            RUN setDados IN hBologtxt('severidade','INF'). 
        END.
        RUN setDados IN hBologtxt('descricao',cDescLog ).
        RUN gerarLogs IN hBoLogtxt.

        RUN incrSeqDados IN hboLogtxt.
        RUN setDados IN hBologtxt('data_hora',NOW).
        RUN setDados IN hBologtxt('acao','vencerPedido').
        RUN setDados IN hBologtxt('severidade','INF').
        RUN setDados IN hBologtxt('descricao','').

        IF bfPW.dt_hr_vencto <> ? THEN DO.
           IF dAgora > bfPW.dt_hr_vencto THEN DO:
              RUN vencerPedWebPorId(INPUT bfPW.ped_web_id).  
              ASSIGN cDescLog = 'pedido setado como vencido e com data/hora de vencimento anterior a hora inicial de processando:' 
                                        + string(bfPW.dt_hr_vencto,'99/99/9999 hh:mm:ss.sss ') .

           END.
           ELSE DO:
              ASSIGN cDescLog = 'pedido NAO setado como vencido por ter data/hora de vencimento posterior a hora inicial de processando:' 
                                       + string(bfPW.dt_hr_vencto,'99/99/9999 hh:mm:ss.sss ') .
           END.
        END.
        ELSE DO:
            IF ADD-INTERVAL(dAgora,- INT(vlParam),"minutes") > bfPW.dt_hr_registro THEN DO:
               RUN vencerPedWebPorId(INPUT bfPW.ped_web_id).  
               ASSIGN cDescLog = 'pedido setado como vencido sem data/hora de vencimento informado no portal. Vencido entre a efetivaá∆o e o inicio do processamento desta integraá∆o' .
            END.
            ELSE DO:
               ASSIGN cDescLog = 'pedido NAO setado como vencido por nao ter data/hora de vencimento informado no portal e por n∆o ter vencido entre a efetivaá∆o e o inicio do processamento desta integraá∆o' .   
            END.
        END.
        RUN setDados  IN hBoLogtxt('descricao',cDescLog).
        RUN gerarLogs IN hBoLogtxt.
    END.

    RUN setDados IN hBologtxt('bloco','vencerPedidos').
    RUN setDados IN hBologtxt('acao','FIM').
    RUN setDados IN hBologtxt('descricao','').
    RUN gerarLogs IN hBoLogtxt.




END PROCEDURE.

