/*
* Programa: esbo/boPedVenda.p
* Objetivo: Tratar todas s regras e calculos referentes ao pedido de venda customizados
* Autor: Tadeu Silva
* Data: 08/2020
*/                                   
 
DEFINE TEMP-TABLE ttPedVenda        LIKE ped-venda.
DEFINE TEMP-TABLE ttPedVendaExt     LIKE ped-venda-ext.
DEFINE VARIABLE nrPedido            AS INTEGER     NO-UNDO.
DEFINE VARIABLE nrPedCli            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE nomeAbrev           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTpPagtoAtual       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE rowidPedVenda       AS ROWID       NO-UNDO.
DEFINE VARIABLE rowidPedVendaExt    AS ROWID       NO-UNDO.

{utp/ut-glob.i}
{esbo\boMsg.i} 

DEFINE VARIABLE hBoMsgPedVenda      AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoRepres           AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoCondPagtoPed     AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoPrecosItemRef    AS HANDLE      NO-UNDO.
PROCEDURE limparTts:
    EMPTY TEMP-TABLE ttPedVenda.
    EMPTY TEMP-TABLE ttPedVendaExt.
END PROCEDURE.

PROCEDURE setNrPedido:
  DEFINE INPUT  PARAMETER pNrPedido AS INTEGER     NO-UNDO.
  ASSIGN nrPedido = pNrPedido.
  FIND ped-venda NO-LOCK
      WHERE ped-venda.nr-pedido = nrPedido NO-ERROR.
  IF AVAIL ped-venda THEN DO:
     ASSIGN nrPedcli  = ped-venda.nr-pedcli 
            nomeAbrev = ped-venda.nome-abrev .
  END.
  ELSE DO:
     ASSIGN nrPedcli = ''
            nomeAbrev = ''.
  END.
  FIND ped-venda-ext NO-LOCK
      WHERE ped-venda-ext.nr-pedido = nrPedido NO-ERROR.
  ASSIGN rowidPedVenda = ROWID(ped-venda)
         rowidPedVendaExt = ROWID(ped-venda-ext).
END PROCEDURE.


PROCEDURE getTtPedVenda:
     EMPTY TEMP-TABLE ttPedVenda.
     FIND CURRENT ped-venda NO-LOCK NO-ERROR.
     IF AVAIL ped-venda THEN DO:
        CREATE ttPedVenda.
        BUFFER-COPY ped-venda TO ttPedVenda.
     END.

END PROCEDURE.

PROCEDURE getTtPedVendaExt:
     EMPTY TEMP-TABLE ttPedVendaExt.
     FIND CURRENT ped-venda NO-LOCK NO-ERROR.
     IF AVAIL ped-venda-ext THEN DO:
        CREATE ttPedVendaExt.
        BUFFER-COPY ped-venda-ext TO ttPedVendaExt.
     END.

END PROCEDURE.

PROCEDURE getTpPagto:
    DEFINE OUTPUT PARAMETER cTpPagto AS CHARACTER   NO-UNDO.
    
    RUN getTtPedVendaExt.
    FIND FIRST ttPedVendaExt NO-ERROR.
    IF AVAIL ttPedVendaExt THEN DO:
       ASSIGN cTpPagto = ttPedVendaExt.tp-pagto.
    END.


END PROCEDURE.



PROCEDURE getMoeda:
    DEFINE OUTPUT PARAMETER iMoeda AS INT  NO-UNDO.
    RUN getTtPedVenda.
    FIND FIRST ttPedVenda NO-ERROR.
    IF AVAIL ttPedVenda THEN DO:
       ASSIGN iMoeda = ttPedVenda.mo-codigo.
    END.


END PROCEDURE.


PROCEDURE setTpPagtoAtual:
    DEFINE INPUT  PARAMETER pTipo AS CHARACTER   NO-UNDO.

END PROCEDURE.

PROCEDURE validarMudancaTpPagto:
    DEFINE VARIABLE cTpPagto AS CHARACTER   NO-UNDO.
    RUN getTpPagto(OUTPUT cTpPagto).
    IF cTpPagtoAtual <> cTpPagto THEN DO: // tipo de pagto alterado
       IF lookup(cTpPagto,'CartÆo de Cr‚dito,CartÆo de D‚bito,Caixa') > 0 THEN DO: // tipo de pagto que nÆo ‚ boleto
            IF cTpPagtoAtual = 'normal' THEN DO:
               RUN setCodSitAval(1).
               RUN esapi/cria-log-pedvenda.p (nrPedcli,nomeAbrev,INPUT "Necess ria nova aprova‡Æo de cr‚dito. Tipo de Pagto Modificado de:" + 
                                              cTpPagto  + " para " + 
                                              cTpPagtoAtual,INPUT NO). 

            END.
       END.
    END.
END PROCEDURE.

PROCEDURE setCodSitAval:

    DEFINE INPUT  PARAMETER pCodSitAval AS INTEGER     NO-UNDO.
    FIND CURRENT ped-venda EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL ped-venda THEN DO:
       ASSIGN ped-venda.cod-sit-aval = pCodSitAval.
       FIND CURRENT ped-venda NO-LOCK NO-ERROR.
    END.


END PROCEDURE.




PROCEDURE iniciarBos:
    IF NOT VALID-HANDLE(hBoMsgPedVenda) THEN
       RUN esbo/boMsg.p PERSISTENT SET hBoMsgPedVenda.
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
END PROCEDURE.


PROCEDURE finalizarBos:
    IF VALID-HANDLE(hBoMsgPedVenda) THEN
       DELETE PROCEDURE hBoMsgPedVenda.

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

END PROCEDURE.











