/*
programa:esbo/boIntegrarCliente.p
Objetivo: Fazer a integraá∆o dos dados do cliente junto ERP.
Os dados do cliente s∆o esperados de duas formas:
1- Arquivo Json
2- Requisiá∆o HTTP POST
Os conte£do Ç convertido em um objeto JSON e os dados s∆o extra°dos
e integrados com a BO da TOTVS. 
Foi criado um calculo de n£mero 17 para que seja poss°vel armazenar um log 
de todas as transaá‰es de integraá∆o.
data:11/2022
autor: Tadeu Silva
*/
USING Progress.Json.ObjectModel.JsonObject.
{esbo\boMsg.i}
DEFINE VARIABLE hBoTransacao            AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoCalculo              AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoMsgIntCli            AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoCliConvJson          AS HANDLE      NO-UNDO.
DEFINE VARIABLE jsonAux                 AS jsonObject  NO-UNDO.
DEFINE VARIABLE cErro                   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE idIntegracao            AS CHARACTER   NO-UNDO. // representa a transaá∆o externa e que ser† gravada no campo  'chave' da tabela transaá∆o do progress

{esbo/ttErroCliente.i}


{esapi/clienteConvJson.i}
{esbo/errosBo.i}
DEFINE TEMP-TABLE ttAux LIKE emitente
    FIELD r-rowid AS ROWID.

DEFINE VARIABLE iCodEmitente  AS INTEGER     NO-UNDO.




PROCEDURE iniciarBos:
    IF NOT VALID-HANDLE(hBoMsgIntcli) THEN
       RUN esbo/boMsg.p PERSISTENT SET hBoMsgIntCli.

    IF NOT VALID-HANDLE(hBoCalculo) THEN
       RUN esbo/boCalculos.p PERSISTENT SET hBoCalculo.

    IF NOT VALID-HANDLE(hBoTransacao) THEN
       RUN esbo/boTransacoes.p PERSISTENT SET hBoTransacao.

    IF NOT VALID-HANDLE(hBoCliConvJson) THEN
       RUN esbo/boCriarClienteConvJson.p PERSISTENT SET hBoCliConvJson.

END PROCEDURE.


PROCEDURE finalizarBos:
    IF VALID-HANDLE(hBoMsgIntCli) THEN
       DELETE PROCEDURE hBoMsgIntCli.

    IF VALID-HANDLE(hBoCalculo) THEN
       DELETE PROCEDURE hBoCalculo.

    IF VALID-HANDLE(hBoTransacao) THEN
       DELETE PROCEDURE hBoTransacao.

    IF VALID-HANDLE(hBoCliConvJson) THEN
       DELETE PROCEDURE hBoCliConvJson .


END PROCEDURE.

PROCEDURE setIdIntegracao:
    DEFINE INPUT  PARAMETER pId AS CHARACTER   NO-UNDO.
    ASSIGN idIntegracao = pId .


END PROCEDURE.


PROCEDURE setObjJson:

    DEFINE INPUT PARAMETER poJsonObject AS jsonObject.
    DEFINE INPUT  PARAMETER pNivelPrinc AS CHARACTER   NO-UNDO.
    RUN setObjJson IN hboCliConvJson(poJsonObject,pNivelPrinc).
    

END PROCEDURE.

PROCEDURE getObjJson:

    DEFINE OUTPUT PARAM pJson AS jsonobject .              
    RUN getObjetoJsonPrinc IN hBoCliConvJson(OUTPUT pJson).

END PROCEDURE.

PROCEDURE setArqJson:
    DEFINE INPUT  PARAMETER pArquivo AS CHARACTER   NO-UNDO.
    RUN setArqJson IN hboCliConvJson(pArquivo).

END PROCEDURE.


PROCEDURE getUltCodEmitente:
    
    DEFINE OUTPUT PARAMETER iEmit AS INTEGER     NO-UNDO.
    
    FIND  FIRST  tab-ocor USE-INDEX  descricao 
        WHERE tab-ocor.cod-tab   = 098 
        AND   tab-ocor.descricao = "Inclui Emit" NO-ERROR.
    
    ASSIGN iEmit = IF AVAIL tab-ocor THEN tab-ocor.i-campo[1] ELSE 0.

END PROCEDURE.

PROCEDURE setUltCodEmitente:
    
    DEFINE INPUT  PARAMETER iEmit AS INTEGER     NO-UNDO.
    
    FIND  FIRST  tab-ocor USE-INDEX  descricao 
        WHERE tab-ocor.cod-tab   = 098 
        AND   tab-ocor.descricao = "Inclui Emit" NO-ERROR.
    IF AVAIL tab-ocor THEN
       ASSIGN tab-ocor.i-campo[1] = iEmit .

END PROCEDURE.




PROCEDURE integrar:
    DEFINE INPUT  PARAMETER pNivel AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cSufixo        AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iTransacao     AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iCalculo       AS INTEGER     NO-UNDO INIT 17. //cadastro cliente novo
    DEFINE VARIABLE iCodAnt        AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iUltCod        AS INTEGER     NO-UNDO.

    RUN gerarTransacao IN hboTransacao(
        INPUT 'bl_integracoes_clientes',
        INPUT '',
        iCalculo,
        idIntegracao,
        OUTPUT iTransacao
        ).               
    RUN setMsg              IN hBoMsgIntCli(0,'Id de Integraá∆o:' + idIntegracao, 'log').
    RUN setHandleBoMsg      IN hboCliConvJson(hBoMsgIntCli).
    //RUN setNivel            IN hBoCliConvJson(pNivel).
    

    RUN getDados            IN hBoCliConvJson.
    RUN getTtEmitente       IN hBoCliConvJson( OUTPUT TABLE ttEmitente).
    RUN getTtEmitenteExt    IN hBoCliConvJson( OUTPUT TABLE ttEmitenteExt).
    RUN getTtAtividades     IN hBoCliConvJson( OUTPUT TABLE ttAtividades).
    RUN exportTTEmitente    IN hBoCliConvJson('c:\temp\jsonCliente_' + STRING(TIME) + '.txt').
    //RUN getObjetoJsonPrinc  IN hBoCliConvJson(OUTPUT jsonAux ).
    //RUN extrairJsonParaArquivo  IN hBoCliConvJson('c:\temp\result_' + STRING(TIME) + '.json', jsonaux, '').
    
    FOR EACH ttEmitente
        WHERE ttEmitente.LOG_integrado = NO :
        //DISP ttEmitente.cod-emitente.
        //numero do ultimo cliente cadastrado(cd0101) + 1 
        /*MESSAGE ttEmitente.nome-emit
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
        ASSIGN iCodAnt = ttEmitente.cod-emitente.
        RUN getUltCodEmitente( OUTPUT iUltCod).
        /*MESSAGE 'ultimo codigo recuperado:' iUltCod
                 VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
        ASSIGN ttEmitente.cod-emitente = iUltCod + 1 .
        //atualiza os codigos de emitente nas outras tabelas
        RUN alterarCodTbsRelac(iCodAnt,ttEmitente.cod-emitente).  

        /*MESSAGE 'anterior' iCodAnt SKIP
                'atual' ttEmitente.cod-emitente SKIP
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
        EMPTY TEMP-TABLE ttAux.
        CREATE ttAux.
        BUFFER-COPY ttEmitente TO ttAux.
        //DISP ttAux.cod-emitente ttAux.nome-abrev.
        RUN esapi/criarCliente.p(INPUT TABLE ttAux,INPUT TABLE ttEmitenteExt,INPUT TABLE ttAtividades, OUTPUT TABLE rowErrors).
        FOR EACH rowErrors:
            ASSIGN cErro = string(rowErrors.ErrorSequence) + "-" + string(rowErrors.ErrorNumber) + "-" +  rowErrors.ErrorDescription +  
                IF rowErrors.ErrorParameters = ? THEN  "" ELSE  "-" +  rowErrors.ErrorParameters .
            /*MESSAGE 'cliente:' ttEmitente.cod-emitente SKIP
                     ttEmitente.nome-abrev   SKIP
                    'erro:' cerro
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
            RUN _inserirErroCliente(ttEmitente.cgc,ttEmitente.nome-abrev, ttEmitente.nome-emit,cErro).
        END.
        FIND FIRST rowErrors NO-ERROR.
        IF NOT AVAIL rowErrors THEN DO:
           RUN setUltCodEmitente(ttEmitente.cod-emitente).
           ASSIGN ttEmitente.LOG_integrado = YES.
        END.
        ELSE DO:                                    
           RUN alterarCodTbsRelac(ttEmitente.cod-emitente,iCodAnt).  
           ASSIGN ttEmitente.cod-emitente = icodAnt.
        END.
           

    END.

    RUN gravarLogCalculo IN hBoMsgIntCli(iCalculo).
    RUN finalizarTransacao IN hBoTransacao(2).

END PROCEDURE.

PROCEDURE _inserirErroCliente:
    DEFINE INPUT  PARAMETER pCNPJ        AS CHAR        NO-UNDO.
    DEFINE INPUT  PARAMETER pNomeAbrev   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pNomeEmit    AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pErro        AS CHARACTER   NO-UNDO.

    CREATE ttErroCliente.
    ASSIGN ttErrocliente.cnpj        = pCNPJ
           ttErroCliente.nomeAbrev   = pNomeAbrev
           ttErroCliente.nomeEmit    = pNomeEmit
           ttErroCliente.erro        = pErro .

END PROCEDURE.


PROCEDURE getTTErroCliente.
    DEFINE OUTPUT PARAMETER TABLE FOR ttErroCliente .

END PROCEDURE.

PROCEDURE alterarCodTbsRelac:

    DEFINE INPUT  PARAMETER pCod  AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pNovo AS INTEGER     NO-UNDO.

    
    FIND ttEmitenteExt
         WHERE ttEmitenteExt.cod-emitente = pCod
         NO-ERROR.
    IF AVAIL ttEmitenteExt THEN
       ASSIGN ttEmitenteExt.cod-emitente = pNovo.

   FOR EACH ttAtividades
       WHERE ttAtividades.cod_emitente = pCod.
      /* MESSAGE 'alterando cod atividade para :' pNovo
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
       ASSIGN ttAtividades.cod_emitente  = pNovo.
   END.




END PROCEDURE.


PROCEDURE limpatTTs:

    EMPTY TEMP-TABLE ttEmitente.
    EMPTY TEMP-TABLE ttEmitenteExt.
    EMPTY TEMP-TABLE ttAtividades.
    EMPTY TEMP-TABLE ttErroCliente.


END PROCEDURE.
