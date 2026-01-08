{esp/utilApi.i2} //tabelas tempor rias utilApi.i
DEFINE INPUT  PARAMETER pCodPrograma AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pListaPed    AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pTipo        AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pOrigem      AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pMotivo      AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pSituacao    AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pCodUsuario  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pDescMotivo  AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttRetorno.
DEFINE OUTPUT PARAMETER TABLE FOR ttChave.           


DEFINE VARIABLE hBoTransacao        AS HANDLE           NO-UNDO.
DEFINE VARIABLE hBoHistAvalPedVenda AS HANDLE           NO-UNDO.
DEFINE VARIABLE hMsg                AS HANDLE           NO-UNDO.
DEFINE VARIABLE iTrans              AS INTEGER          NO-UNDO.
DEFINE VARIABLE cChave              AS CHARACTER        NO-UNDO.



//chamada das BOS
RUN esbo/boTransacoes.p PERSISTENT SET hBoTransacao.
RUN esbo/boHistAvalPedVenda.p PERSISTENT SET hBoHistAvalPedVenda.
RUN iniciarBos IN hBoHistAvalPedVenda.

//inicia transacao
DO TRANSACTION:
   RUN getChave(pListaPed,OUTPUT cChave).
   RUN setChave            IN hBoTransacao(cChave).
   RUN setCodPrograma      IN hBoTransacao(pCodPrograma).
   RUN setLogin            IN hBoTransacao(pCodUsuario).
   RUN iniciarTransacao    IN hBoTransacao.
   RUN getIDTransCorrente  IN hBoTransacao(OUTPUT iTrans).
   RUN setCodtipo          IN hBoHistAvalPedVenda(pTipo). 
   RUN setMotivo           IN hBoHistAvalPedVenda(pMotivo).
   RUN setOrigem           IN hBoHistAvalPedVenda(pOrigem).
   RUN setSituacao         IN hBoHistAvalPedVenda(pSituacao).
   RUN setCodUsuario       IN hBoHistAvalPedVenda(pCodUsuario).
   RUN setDescMotivo       IN hBoHistAvalPedVenda(pDescMotivo).
   
   
   //pega mensagens da BO e insere na temp-table ttRetorno
   RUN getHandleMsg        IN hBoHistAvalPedVenda(OUTPUT hMsg).
   RUN setTransacaoLogCalculo IN hMsg(INPUT iTrans). 
   
   RUN avaliarListaPedsWeb IN hBoHistAvalPedVenda(pListaPed).
   RUN inserirMsgTTRet(hMsg,''). // em branco traz erros e avisos
   
   RUN finalizarTransacao IN hBoTransacao(1).


END.

IF VALID-HANDLE(hBoHistAvalPedVenda) THEN DO:
   RUN finalizarBos IN hBoHistAvalPedVenda.
   DELETE PROCEDURE hBoHistAvalPedVenda.
END.
IF VALID-HANDLE(hBoTransacao) THEN
   DELETE PROCEDURE hBoTransacao.

PROCEDURE getChave:

    //substituir pela bo de calculos buscando a chave na tabela

    DEFINE INPUT  PARAMETER pPedWeb AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER pChave  AS CHARACTER   NO-UNDO.
    
    ASSIGN pChave = "PW-GER-" + pPedWeb.

END PROCEDURE.
