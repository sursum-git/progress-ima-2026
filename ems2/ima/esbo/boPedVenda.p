/*
* Programa: esbo/boPedVenda.p
* Objetivo: Tratar todas s regras e calculos referentes ao pedido de venda customizados
* Autor: Tadeu Silva
* Data: 08/2020
05/2021 - tadeu -  acrescimo de validacao de agrupamento - tsp01 e inclus∆o do calculo de  comiss∆o - tsp02
10/2021 - tadeu - acrescimo de transacao e suporte a logs de calulos no banco de dados atravÇs da bomsg.p 
01/2022 - tadeu - acrescimo de propriedade cod_forma_pagto */                                  

DEFINE TEMP-TABLE ttPedVenda        LIKE ped-venda.
DEFINE TEMP-TABLE ttPedVendaExt     LIKE ped-venda-ext.
DEFINE VARIABLE nrPedido            AS INTEGER     NO-UNDO.
DEFINE VARIABLE nrPedCli            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE nomeAbrev           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTpPagtoAtual       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE rowidPedVenda       AS ROWID       NO-UNDO.
DEFINE VARIABLE rowidPedVendaExt    AS ROWID       NO-UNDO.
DEFINE VARIABLE iTransacao          AS INTEGER     NO-UNDO.

//variaveis para validacao do agrup.(tabela,moeda, container,divide comissao,perc.Vend,perc.Repres) do pedido de venda
DEFINE VARIABLE tbPrecoId           AS INTEGER     NO-UNDO.
DEFINE VARIABLE codMoeda            AS INTEGER     NO-UNDO.
DEFINE VARIABLE nrContainer         AS INTEGER     NO-UNDO.
DEFINE VARIABLE lDivideComis        AS LOGICAL     NO-UNDO FORMAT 'Sim/N∆o'.
DEFINE VARIABLE dPercComisVend      AS INTEGER     NO-UNDO.
DEFINE VARIABLE dPercComisRepres    AS INTEGER     NO-UNDO.
DEFINE VARIABLE lAgrupDefinido      AS LOGICAL     NO-UNDO INIT NO.

DEFINE VARIABLE pedWebId            AS INTEGER     NO-UNDO.
DEFINE VARIABLE codEmitente         AS INTEGER     NO-UNDO.
DEFINE VARIABLE codEmitenteTriang   AS INTEGER     NO-UNDO.
DEFINE VARIABLE codCondPagto        AS INTEGER     NO-UNDO.
DEFINE VARIABLE codEstabel          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE codRep              AS INTEGER     NO-UNDO.
DEFINE VARIABLE diasCondPagto       AS CHARACTER   NO-UNDO FORMAT 'x(500)'.
DEFINE VARIABLE rRowidPedWeb        AS ROWID       NO-UNDO.
DEFINE VARIABLE cPrograma           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cChave              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFormaPagto         AS CHARACTER   NO-UNDO. //forma de pagto ERP
DEFINE VARIABLE iFormaPagto         AS INTEGER     NO-UNDO.  //forma de pagto peds_web
/*DEFINE TEMP-TABLE ttPrecos no-undo
    FIELD itCodigo          LIKE ped-item.it-codigo
    FIELD codRefer          LIKE ped-item.cod-refer
    FIELD idPreco           AS INT
    FIELD iPrecoOut         AS INT
    FIELD logDivideComis    AS LOGICAL 
    FIELD percComisVend     AS INT
    FIELD percComisRepres   AS INT.*/




{utp/ut-glob.i}
{esbo\boMsg.i} 
{seniuz/esp/espd4000.i}
{esbo/bocomis.i}

DEFINE VARIABLE hBoMsgPedVenda      AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoRepres           AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoCondPagtoPed     AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoPrecosItemRef    AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoComis            AS HANDLE      NO-UNDO.
DEFINE VARIABLE hboTransacoes       AS HANDLE      NO-UNDO.


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

    IF NOT VALID-HANDLE(hBoComis) THEN do:
       RUN esbo/boComis.p PERSISTENT SET hBoComis .
       RUN iniciarBos IN hBoComis.
    END.

    IF NOT valid-handle(hBoTransacoes) THEN DO:
      RUN esbo/boTransacoes.p PERSISTENT SET hBoTransacoes.
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
    IF VALID-HANDLE(hBoComis) THEN DO:
       RUN finalizarBos IN hBoComis.
       DELETE PROCEDURE hBoComis.
    END.

    IF VALID-HANDLE(hBoTransacoes) THEN DO:
      DELETE PROCEDURE hBoTransacoes.
    END.


END PROCEDURE.


PROCEDURE setCodPrograma:

    DEFINE INPUT  PARAMETER pPrograma AS CHARACTER   NO-UNDO.
    ASSIGN cPrograma = pPrograma.
    RUN setCodPrograma     IN hBoTransacoes(pPrograma).
    

END PROCEDURE.

PROCEDURE setChave:

    DEFINE INPUT PARAMETER pChave AS CHARACTER   NO-UNDO.
    ASSIGN cChave = pChave.
    RUN setChave            IN hBoTransacoes(pChave).


END PROCEDURE.

PROCEDURE iniciarTransacao:
    RUN iniciarTransacao    IN hBoTransacoes.
    RUN getIDTransCorrente  IN hBoTransacoes (OUTPUT iTransacao).
    RUN setTransacao        IN hBoComis(iTransacao).
END PROCEDURE.

PROCEDURE getIDTransCorrente:
   DEFINE OUTPUT PARAMETER pTrans AS INTEGER     NO-UNDO.
   ASSIGN pTrans  = iTransacao .
     
END PROCEDURE.

PROCEDURE finalizarTransacao.
    DEFINE INPUT  PARAMETER iSit AS INTEGER     NO-UNDO.
    RUN finalizarTransacao IN hBoTransacoes(iSit).
    ASSIGN iTransacao = 0 .


END PROCEDURE.

PROCEDURE limparTts:
    EMPTY TEMP-TABLE ttPedVenda.                           
    EMPTY TEMP-TABLE ttPedVendaExt.
END PROCEDURE.

PROCEDURE setNrPedido:

  DEFINE INPUT  PARAMETER pNrPedido AS INTEGER     NO-UNDO.
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



PROCEDURE setProp:

    DEFINE INPUT  PARAMETER prop    AS CHAR FORMAT 'x(30)'   NO-UNDO.
    DEFINE INPUT  PARAMETER vlProp  AS CHAR FORMAT 'x(200)'   NO-UNDO.
    CASE prop:
        WHEN 'tb_preco_id' THEN DO:
            ASSIGN tbPrecoId = INT(vlProp).
            RUN setTbPreco IN hBoComis(tbPrecoId).
        END.
        WHEN 'cod_moeda' THEN DO:
            ASSIGN codMoeda = INT(vlProp).
            RUN setMoeda IN hBoComis(codMoeda).
        END.
        WHEN 'nr_container' THEN DO:
            ASSIGN nrContainer = INT(vlProp).
            RUN setNrContainer(nrContainer).
        END.
        WHEN 'log_divide_comis' THEN DO:
            ASSIGN lDivideComis = LOGICAL(vlProp).
           
        END.
        WHEN 'perc_comis_vend' THEN DO:
            ASSIGN dPercComisVend = DEC(vlProp).
            
        END.
        WHEN 'perc_comis_repres' THEN DO:
            ASSIGN dPercComisRepres = DEC(vlProp).
            
        END. 
        WHEN 'nr_pedido' THEN DO:
            ASSIGN nrPedido = INT(vlProp).
            RUN setNrPedido(nrPedido).
            RUN setNrPedido IN hBoComis(nrPedido).
        END.
        WHEN 'ped_web_id' THEN DO:
            ASSIGN pedWebId = INT(vlProp).
            RUN setPedWebId IN hBoComis(pedWebId).
        END.
        WHEN 'cod_emitente' THEN DO:
            ASSIGN codEmitente = INT(vlProp).
            RUN setCliente IN hBoComis(codEmitente).
        END.
        WHEN 'cod_emitente_triang' THEN DO:
            ASSIGN codEmitenteTriang = INT(vlProp).
            RUN setClienteTriang IN hBoComis(codEmitenteTriang).
        END.
        WHEN 'cod_cond_pagto' THEN DO:
            ASSIGN codCondPagto = INT(vlProp).
            RUN setCondPagto IN hBoComis(codCondPagto).
            
        END.
        WHEN 'cod_rep' THEN DO:
            ASSIGN codRep = INT(vlProp).
            RUN setCodRep IN hBoComis(codRep).
        END.
        WHEN 'cod_estabel' THEN DO:
            ASSIGN codEstabel = vlProp.
            RUN codEstab IN hBoComis(codEstabel).
        END.
        WHEN 'dias_cond_pagto' THEN DO:
           ASSIGN diasCondPagto = vlProp.
           RUN setDiasCondPagto IN hBoComis(diasCondPagto).
        END.
        WHEN 'cod_forma_pagto' THEN DO:
             ASSIGN cFormaPagto = vlProp.
             RUN deParaFormaPagtoERP IN  hBoComis(cFormaPagto, OUTPUT iFormaPagto).
             RUN SETformaPagto IN hBoComis(iFormaPagto).
        END.


    END CASE.
END PROCEDURE.                      


PROCEDURE setVarsAgrupComis:
    /*MESSAGE lDivideComis SKIP
            dPercComisVend SKIP
            dPercComisRepres
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/
    RUN setDivideComis      IN hBoComis(lDivideComis).
    RUN setPercComisVend    IN hBoComis(dPercComisVend).
    RUN setPercComisRepres  IN hBoComis(dPercComisRepres).


END PROCEDURE.

PROCEDURE setTTCondPed: //tsp02
    DEFINE INPUT PARAMETER TABLE FOR ttCondPed.
    RUN  setTTCondPed IN hBoComis(TABLE ttCondPed).
     

END PROCEDURE.

PROCEDURE limparTtsComis:

    RUN limparTTs IN hBoComis.

END PROCEDURE.



PROCEDURE setTtItensEspd4000: //tsp02
    DEFINE INPUT PARAMETER TABLE FOR tt-itens-ped.

    RUN setTtItensEspd4000 IN hBoComis(TABLE tt-itens-ped).
END PROCEDURE.

PROCEDURE setTtItensPedWeb: //tsp02
    DEFINE INPUT  PARAMETER pRowidPedWeb AS ROWID       NO-UNDO.
    ASSIGN rRowidPedWeb = pRowidPedWeb.
    RUN setTtItensPedWeb IN hBoComis(rRowidPedWeb).


END PROCEDURE.




PROCEDURE validarVarsAgrup: //tsp01

    DEFINE INPUT  PARAMETER pDivideComis     AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pPercComisVend   AS DECIMAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pPercComisRepres AS DECIMAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER cMsg             AS CHARACTER   NO-UNDO.
    
    
    /*MESSAGE 'agrup.definido' lAgrupDefinido SKIP
            'divide comis ped.:' lDivideComis SKIP
            'divide comis.:' pDivideComis   SKIP
            'comis.vend.ped.:' dPercComisVend   SKIP
            'comis.vend.:'   pPercComisVend SKIP
            'comis.repres.ped:' dPercComisRepres SKIP  
            'comis.repres.'  ppercComisRepres SKIP
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    


    IF lAgrupDefinido THEN DO:
       IF pDivideComis <> lDivideComis THEN DO:
          ASSIGN cMsg = 'Pedido Divide Comiss∆o:' + STRING(lDivideComis,'sim/n∆o') + ' - Item Divide Comiss∆o:' 
                        + STRING(pDivideComis, 'sim/n∆o') .
       END.
       IF pPercComisVend <> dPercComisVend THEN DO:
          ASSIGN cMsg = cMsg + CHR(10) + CHR(13) +
                 'Pedido - % Comis.Vendedor:' + STRING(dPercComisVend)  + ' - Item % Comis.Vendedor:' 
                 + STRING(pPercComisVend).
       END.
       IF pPercComisRepres <> dPercComisRepres THEN DO:
          ASSIGN cMsg  = cMsg + CHR(10) + CHR(13) +
                         'Pedido - % Comis.Representante:' + STRING(dPercComisRepres) + ' - Item % Comis.Representante:' 
                         + STRING(pPercComisRepres).
       END.
       IF cMsg <> '' THEN DO:
            ASSIGN cMsg = "Item diferente das condiá‰es de preáo do Pedido. Diferenáas:" + CHR(10) + CHR(13) + cMsg.
       END.
    END.
    ELSE DO:
        ASSIGN lDivideComis      = pDivideComis
               dPercComisVend    = pPercComisVend
               dPercComisRepres  = pPercComisRepres
               lAgrupDefinido    = YES.
    END. 

    /*MESSAGE 'mensagem' SKIP
            cmsg
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
END PROCEDURE.

PROCEDURE setAgrupIndefinido:
    
    ASSIGN lAgrupDefinido = NO.

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
       IF lookup(cTpPagto,'Cart∆o de CrÇdito,Cart∆o de DÇbito,Caixa') > 0 THEN DO: // tipo de pagto que nío ≤ boleto
            IF cTpPagtoAtual = 'normal' THEN DO:
               RUN setCodSitAval(1).
               RUN esapi/cria-log-pedvenda.p (nrPedcli,nomeAbrev,INPUT "Necessaria nova aprovaá∆o de crÇdito. Tipo de Pagto Modificado de:" + 
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


PROCEDURE calcPercComis: //tsp02
    DEFINE OUTPUT PARAMETER pPerc AS DECIMAL     NO-UNDO.
    RUN calcPercComis IN hBoComis(OUTPUT pPerc).

END PROCEDURE.

PROCEDURE getPercComis2:  //tsp02
    DEFINE OUTPUT PARAMETER pPerc AS DECIMAL     NO-UNDO.
    RUN getPercComis2 IN hBoComis(OUTPUT pPerc).

END PROCEDURE.












