
/* Programa: ESPD001
** Modulo..: Controle do Portal
** Objetivo: Analisa pedidos do portal, se os mesmos estiverem
**           status OK, a importaá∆o ser† efetuada.
** Autor...: Toninho - DEZEMBRO/2018
** alteraá∆o - tadeu - inclus∆o de arquivo de log - 07/2020
                     - alterado para enviar e-mail mesmo se for execuá∆o manual
                     - alterado para que a comiss∆o fique sempre aprovada, pois, o portal n∆o permite alteraá∆o
                       de percentual de comiss∆o por parte do repres/vendedor. 
             - Toninho - Fev/2021
                       - Tratar o Representante com venda casada (RUBI) 
             - Tadeu - 08/2023 - Retirada da atribuiá∆o de transportadora, respeitando o que veio do portal
                               - Retirada da restriá∆o de hor†rio para envio de e-mail por n∆o estar funcionando correntamente.
                               - Atribuiá∆o do valor destacado de frete calculado pelo portal para o o campo val-frete do pedido no ERP Datasul.
             - tsp01 - Tadeu - 26/11/2024 - ap¢s a virada do progress 12 troca do diretorio de logs para buscar de parametro. antes estava fixo.                   
                                 
*/

{esp/espd4000.i}

DEFINE TEMP-TABLE tt-param  NO-UNDO
    FIELD destino           AS INTEGER
    FIELD arquivo           AS CHAR FORMAT "x(35)"
    FIELD usuario           AS CHAR FORMAT "x(12)"
    FIELD data-exec         AS DATE
    FIELD hora-exec         AS INTEGER
    FIELD enviar-e-mail     AS LOG FORMAT "Sim/N∆o"
    FIELD destinatarios     AS CHAR
    FIELD l-batch           AS LOG.

DEFINE TEMP-TABLE tt-digita NO-UNDO 
    FIELD ordem            AS INTEGER   FORMAT ">>>>9"
    FIELD exemplo          AS CHARACTER FORMAT "x(30)"
    INDEX id ordem.

DEFINE TEMP-TABLE tt-erros-local
    FIELD cod-erro  As INTEGER 
    FIELD desc-erro As Character FORMAT "x(50)"
    FIELD desc-arq  As Character.

DEFINE TEMP-TABLE ttFila
    FIELD arquivo AS CHAR FORMAT 'x(300)'.

DEF BUFFER b-emitente FOR emitente.
DEF BUFFER b-repres FOR repres.
DEF BUFFER b-transporte FOR transporte.
DEF BUFFER moeda FOR mgcad.moeda.



DEFINE TEMP-TABLE tt-raw-digita
       FIELD raw-digita AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.  
 
CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

/* definiá∆o de vari†veis  */
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF VAR h-acomp            AS HANDLE NO-UNDO.
DEF VAR h-bodi018          AS HANDLE.
DEF VAR h-bonat001         AS HANDLE.
DEF VAR hBoLogTxt          AS HANDLE NO-UNDO.
DEF VAR hBoPedsWeb         AS HANDLE NO-UNDO.
DEF VAR h-bo-preco-item    AS HANDLE    NO-UNDO.

DEF VAR c-mens-erro        AS CHAR.
DEF VAR c-mens-erro-it     AS CHAR.
DEF VAR c-mensagem         AS CHAR.
DEF VAR c-remetente        AS CHAR INITIAL "imatextil@imatextil.com.br".
DEF VAR c-destinatario     LIKE param-dis.destinatario.
DEF VAR l-erro             AS LOGICAL.
DEF VAR i-cont             AS INT.
DEF VAR i-cod-rep          LIKE peds_web.repres_id.
DEF VAR i-cod-vend         LIKE peds_web.repres_id.
DEF VAR i-tp-frete         AS INT.
DEF VAR i-prazo-medio      AS INT.
DEF VAR i-ct               AS INT.
DEF VAR i-nr-sequencia     AS INT.
DEF VAR da-dt-now          AS DATETIME.
DEF VAR de-perc-comis-ven  AS DECIMAL.
DEF VAR de-perc-comis-rep  AS DECIMAL.
DEF VAR de-vl-preori       AS DECIMAL.
DEF VAR c-tpped-cred-aut   AS CHAR INIT "∑ Vista,Exportaá∆o,Amostra,Amostra Exportaá∆o,Bonificaá∆o,Doaá∆o".
DEF VAR c-natur-oper       AS CHAR.
DEF VAR c-pct-desconto     AS CHAR.
DEF VAR i-fin-nat          AS INTEGER.
DEF VAR i-moeda            AS INTEGER.
DEF VAR c-cnae             AS CHAR.
DEF VAR c-erro-nat         AS CHAR.
DEF VAR c-arq-gerado-pdf   AS CHAR.
DEF VAR i-param-nat        AS INTEGER.
DEF VAR l-ok               AS LOGICAL.
DEF VAR c-base             AS CHAR.

DEF VAR lBloqueado         AS LOGICAL.
DEF VAR hBOEmitente        AS HANDLE.

//DEF VAR codParam           AS CHARACTER   NO-UNDO FORMAT 'x(50)'.
DEF VAR cDesclog           AS CHARACTER   NO-UNDO FORMAT 'x(500)'.
DEF VAR cDesclog2          AS CHARACTER   NO-UNDO FORMAT 'x(500)'.
DEF VAR cSever             AS CHARACTER   NO-UNDO.
DEF VAR cSever1            AS CHARACTER   NO-UNDO.
DEF VAR cAdcLogPed         AS CHARACTER   NO-UNDO INIT " Adicionado ao Log do Pedido de venda".
DEF VAR cPastaLog          AS CHARACTER   NO-UNDO.
DEF VAR lOkNatOper         AS LOGICAL     NO-UNDO.
DEF VAR lOkComisRep        AS LOGICAL     NO-UNDO.
DEF VAR lEnviarEmail       AS LOGICAL     NO-UNDO INIT YES.
DEF VAR cAgora             AS CHARACTER   NO-UNDO.
DEF VAR de-qt-vend         AS DEC.


DEF VAR de-vlReal          AS DECIMAL.
DEF VAR de-vlDolar         AS DECIMAL.
DEF VAR de-vlRealOut       AS DECIMAL.
DEF VAR de-vlDolarOut      AS DECIMAL.
DEF VAR i-ControlePreco    AS INTEGER.
DEF VAR i-controleprecoOut AS INTEGER.

DEFINE VARIABLE cDirLogEspd001 AS CHARACTER   NO-UNDO.

DEFINE VARIABLE hBoHistAvalPedVenda AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoConsParam        AS HANDLE      NO-UNDO.

//tsp01
RUN esbo/boConsParam.p PERSIST SET hBoConsParam.


// Definiá‰es para e-mail HTML
{utp/utapi019.i}

DEF TEMP-TABLE tt-aux 
    FIELD c-linha AS CHAR.
RUN esapi/busca-base.p (OUTPUT c-base).

IF VALID-HANDLE(hBologtxt) THEN
   DELETE PROCEDURE hBoLogTxt.
RUN esbo/bologtxt.p PERSISTENT SET hBoLogtxt.

IF VALID-HANDLE(hBoPedsWeb) THEN
   DELETE PROCEDURE hBoPedsWeb.
RUN esbo/boPedsWeb.p PERSISTENT SET hBoPedsWeb.

IF VALID-HANDLE(hBOEmitente) THEN
   DELETE PROCEDURE hBOEmitente.
RUN esbo/boEmitente.p PERSISTENT SET hBOEmitente.

IF VALID-HANDLE(h-bo-preco-item) THEN
   DELETE PROCEDURE h-bo-preco-item.

IF VALID-HANDLE(hBoHistAvalPedVenda) THEN DO:
   RUN finalizarBos IN HBoHistAvalPedVenda.
   DELETE PROCEDURE hBoHistAvalPedVenda.   
END.
   
RUN esbo/boHistAvalPedVenda.p PERSISTENT SET hBoHistAvalPedVenda.
RUN iniciarBos IN hBoHistAvalPedVenda .

RUN esbo/boPrecosItemRef.p PERSISTENT SET h-bo-preco-item.
RUN iniciarBos      IN h-bo-preco-item.
RUN limparTTPreco   IN h-bo-preco-item.
RUN limparTTMsg     IN h-bo-preco-item.


//Definicoes gerais do log
RUN esapi/getCAgora.p(OUTPUT cAgora).
RUN setPrefixoArquivoLog in hbologtxt('integracao_portal_' + cAgora + '_' ).
RUN setExtArquivoLog in hbologtxt('txt').

//tsp01
RUN getDirLogEspd001 in hBoConsParam(OUTPUT cDirLogEspd001).
ASSIGN cPastaLog = cDirLogEspd001.


RUN setDiretorioLog IN hBoLogTxt(cPastaLog ).

/** definiá∆o dos niveis de log **/
RUN setNivel IN hBoLogtxt('data_hora',1,30,'Data/Hora').
RUN setNivel IN hBoLogtxt('usuario',2,20,'Usu†rio').
RUN setNivel IN hBoLogtxt('nr_pedido',3,12,'Pedido').
RUN setNivel IN hBoLogtxt('severidade',4,12,'Severidade').
RUN setNivel IN hBoLogtxt('bloco',5,30,'Bloco').
RUN setNivel IN hBoLogtxt('acao',6,50,'Aá∆o').
RUN setNivel IN hBoLogtxt('descricao',7,500,'Descriá∆o').

/* Code placed here will execute AFTER standard behavior.    */
FIND FIRST param-global NO-LOCK NO-ERROR.
FIND FIRST param-dis NO-LOCK NO-ERROR.

//ASSIGN tt-param.enviar-e-mail = tt-param.l-batch.
// avaliaá∆o de envio de e-mail do pedido conforme marcaá∆o do usu†rio ou por execiá∆o batch
IF tt-param.l-batch THEN
   ASSIGN tt-param.enviar-e-mail = YES.

IF tt-param.l-batch = NO THEN DO:
   RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
   {utp/ut-liter.i Imprimindo *}
   RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
END.
   

IF NOT VALID-HANDLE(h-bodi018) OR 
   h-bodi018:TYPE      <> "PROCEDURE":U OR
   h-bodi018:FILE-NAME <> "dibo/bodi018.p":U THEN
   RUN dibo/bodi018.p PERSISTENT SET h-bodi018.

IF NOT VALID-HANDLE(h-bonat001) THEN
   RUN esbo/bonat001.p PERSISTENT SET h-bonat001.

RUN incrSeqDados IN hBologTxt.
RUN setDados IN hBologtxt('data_hora',NOW).
RUN setDados IN hBologtxt('usuario',c-seg-usuario).
RUN setDados IN hBologtxt('nr_pedido','N/A').
RUN setDados IN hBologtxt('Severidade','INF').
RUN setDados IN hBologtxt('bloco','INICIO').
RUN setDados IN hBologtxt('acao','buscarBase').
RUN setDados IN hBologtxt('descricao',"Base " + c-base + IF tt-param.l-batch THEN 'Processamento BATCH' ELSE 'Processamento MANUAL' ).
RUN gerarLogs IN hBoLogtxt.

// avaliaá∆o de hor†rio e dia para envio de e-mails de erro.
//RUN getAvalEnviarEmail. retirado em 08/2023

// Acerta Saldo dos Containers
RUN pi-act-sld-container.
RUN limparIntegracoesErradas.


IF tt-param.l-batch = NO THEN
    RUN pi-acompanhar IN h-acomp(INPUT 'Corrigindo situaá‰es os pedidos web').
RUN corrigirSitPedsWebAproGer IN hBoPedsWeb.


IF tt-param.l-batch = NO THEN
   RUN pi-acompanhar IN h-acomp (INPUT "Inicio - Vencendo Pedidos...." ).  
RUN vencerPedidos.         
IF tt-param.l-batch = NO THEN
   RUN pi-acompanhar IN h-acomp (INPUT "Final - Vencendo Pedidos...." ).  


DO TRANSACTION:

   RUN pi-integrarPedidos.
   RUN enviarFilaEmails.

   CATCH oneError AS Progress.Lang.SysError:
       RUN incluirLogGeral('tratarErros','errosSistema', oneError:GetMessage(1),'FATAL').
       RUN pi-envio-email-erro(oneError:GetMessage(1)).
   END CATCH.

   CATCH eSysError AS Progress.Lang.ProError:
       RUN incluirLogGeral('tratarErros','errosProgress', eSysError:GetMessage(1),'FATAL').
       RUN pi-envio-email-erro(eSysError:GetMessage(1)).
   END CATCH.

   CATCH eAsysexcpError AS System.Exception:
      RUN incluirLogGeral('tratarErros','excessaoSistema', eAsysexcpError:GetMessage(1),'FATAL').
      RUN pi-envio-email-erro(eAsysexcpError:GetMessage(1)).
   END CATCH.

   FINALLY:
      RUN finalizarBos.
      //RUN incluirLogGeral('FIM','finally','final do log ','INF').
   END.
END.

IF tt-param.l-batch = NO THEN
   RUN pi-finalizar IN h-acomp.



/***funcoes**/

FUNCTION getPrioridadeERP RETURNS CHAR(numPrioridade AS INT):
    DEFINE VARIABLE cRetorno AS CHARACTER   NO-UNDO.
    CASE numPrioridade.
        WHEN 15 THEN 
           ASSIGN cRetorno = '50'.
        WHEN 16 THEN 
           ASSIGN cRetorno = '40'.
        WHEN 17 THEN 
           ASSIGN cRetorno = '30'.
        WHEN 18 THEN 
           ASSIGN cRetorno = '20'.
        OTHERWISE 
          ASSIGN cRetorno = ''.
    END CASE.

    RETURN  cRetorno.
END FUNCTION.

FUNCTION getTipoFreteERP RETURNS CHAR(numTipoFrete AS INT):
    DEFINE VARIABLE cRetorno AS CHARACTER   NO-UNDO.
    CASE numTipoFrete.
        WHEN 1 THEN  ASSIGN cRetorno = "Cif Total".
        WHEN 2 THEN ASSIGN cRetorno = "Cif atÇ Redesp".
        WHEN 3 THEN ASSIGN cRetorno = "Fob Total".
    END CASE.

    RETURN  cRetorno.
END FUNCTION.

FUNCTION getTpPagto RETURNS CHAR (numFormaPagto AS INT):
    DEFINE VARIABLE cRetorno AS CHARACTER   NO-UNDO.
    CASE numFormaPagto:
        WHEN 2 THEN DO: // a vista
            ASSIGN cRetorno = 'CAIXA'.
        END.

        WHEN 3 THEN DO: //debito
            ASSIGN cRetorno = 'CART«O DE DêBITO'.

        END.
        WHEN 4 THEN DO: //credito
            ASSIGN cRetorno = 'CART«O DE CRêDITO'.

        END.
        OTHERWISE
            ASSIGN cRetorno = 'NORMAL'.

    END CASE.
    RUN incluirLogPed('setTpPagto',
                      'Forma de Pagto Portal WEb =' + string(numformaPagto) + ', tp.pagto ->' + cRetorno,
                      'INF').
    RETURN cRetorno.
END FUNCTION .


//-------------- Procedures ---------------------

PROCEDURE  finalizarBos.

    IF VALID-HANDLE(h-bodi018) THEN
         DELETE PROCEDURE h-bodi018.
      IF VALID-HANDLE(h-bonat001) THEN
         DELETE PROCEDURE h-bonat001.
      IF VALID-HANDLE(hboLogTxt) THEN
         DELETE PROCEDURE hBoLogTxt.
      IF VALID-HANDLE(hBoPedsWeb) THEN
         DELETE PROCEDURE hboPedsWeb.
      IF VALID-HANDLE(hBoHistAvalPedVenda) THEN DO:
         RUN finalizarBos IN HBoHistAvalPedVenda.
         DELETE PROCEDURE hBoHistAvalPedVenda.   
      END.                                       


END PROCEDURE.

PROCEDURE pi-integrarPedidos:
    
    // Integra Pedidos Efetivados
            
    RUN incrSeqDados IN hBologTxt.
    RUN setDados IN hBologtxt('Severidade','INF').
    RUN setDados IN hBologtxt('bloco','integrarPedido').
    RUN setDados IN hBologtxt('acao','INICIO').
    RUN setDados IN hBologtxt('descricao','Envia Email?' + IF tt-param.enviar-e-mail THEN 'SIM' ELSE 'NAO').
        
    
    // 1-Em Digitaá∆o   2-Efetivado   3-Cancelado   4-Integrado   5-Rejeitado   6-Vencido  7-Aprovado
    FOR EACH peds_web WHERE
             peds_web.ind_sit_ped_web = 2 NO-LOCK.
        RUN incrSeqDados IN hBologTxt. 
        RUN setDados IN hBologtxt('data_hora',NOW).
        RUN setDados IN hBologtxt('nr_pedido', peds_web.ped_web_id).
        RUN setDados IN hBologtxt('acao','INICIO').
        RUN setDados IN hBologtxt('descricao','' ).
        RUN gerarLogs IN hBoLogtxt.
    
        FIND FIRST itens_ped_web WHERE
                   itens_ped_web.ped_web_id = peds_web.ped_web_id AND 
                   itens_ped_web.ind_sit_itens_ped_web <> 3 NO-LOCK NO-ERROR.
        
        IF NOT AVAIL itens_ped_web THEN DO.
           ASSIGN c-mens-erro = "Pedido WEB Efetiviado sem Itens, Imposs°vel importar Pedido... " + CHR(10) +
                                'ID: ' + STRING(peds_web.cliente_id)
                  cDescLog    = c-mens-erro
                  cSever      = 'ERRO' .
           RUN enviarErro(c-mens-erro,tt-param.l-batch,tt-param.enviar-e-mail). 
           RUN incluirLogPed('validarPedidoSemItens',
                             cDescLog2,
                             'INF').
           RUN setRowid IN hBoPedsWeb(ROWID(peds_web)).
           RUN setIndsitPedweb IN hBoPedsWeb(5). 
           RUN setDescrRejeicao IN hBoPedsWeb(c-mens-erro).
           RUN incluirLogPed('validarPedidoSemItens',
                             cDescLog,
                             cSever).
           RUN finalizarLogPed.
           NEXT.
        END.
        ELSE DO:
            ASSIGN cDescLog = "Pedido Possui Itens"
                   cSever   = 'INF'.
            RUN incluirLogPed('validarPedidoSemItens',
                              cDescLog,
                              cSever).
        END.
        

        // Verificar Cliente
        IF peds_web.log_novo_cliente THEN DO:
           RUN incluirLogPed('validarNovoCliente',
                             'Pedido com cliente novo informado'  ,
                             'INF'). 
           FIND emitente WHERE
                emitente.cgc = peds_web.cnpj_novo_cliente NO-LOCK NO-ERROR.
        END.
        ELSE DO:
           RUN incluirLogPed('validarNovoCliente',
                             'Pedido SEM cliente novo informado'  ,
                             'INF'). 
           FIND emitente WHERE  
       		    emitente.cod-emit = peds_web.cliente_id NO-LOCK NO-ERROR.
        END.
    
        IF NOT AVAIL emitente THEN DO.
           ASSIGN c-mens-erro = "Cliente n∆o foi Encontrado no Sistema, Imposs°vel importar Pedido... (2)" + CHR(10) +
                                'CNPJ: ' + peds_web.cnpj_novo_cliente + CHR(10) +
                                'ID: ' + STRING(peds_web.cliente_id).
           RUN rejeitarPedido(c-mens-erro,'validarExistCliente').
           NEXT.
        END.
        ELSE DO:
          RUN incluirLogPed('validarExistCliente',
                            'Encontrado Cliente - ID:' + STRING(peds_web.cliente_id) + ' CNPJ:' + peds_web.cnpj_novo_cliente, 
                            'INF').   
        END.
    
        // Representante/Vendedor
        ASSIGN i-cod-rep = peds_web.repres_id
               de-perc-comis-rep = peds_web.perc_comis.

        IF peds_web.repres_2_id <> 0 THEN
           ASSIGN i-cod-rep = peds_web.repres_2_id
                  de-perc-comis-rep = peds_web.perc_comis_2
                  i-cod-vend = peds_web.repres_id
                  de-perc-comis-ven = peds_web.perc_comis.

        FIND repres WHERE 
             repres.cod-rep = i-cod-rep NO-LOCK NO-ERROR.
        IF NOT AVAIL repres THEN DO: 
           RUN incluirLogPed('validarRepresPed',
                             'Representante NAO encontrado:' + STRING(i-cod-rep) ,
                             'ERRO').
           RUN finalizarLogPed.
           NEXT.
        END.
        ELSE DO:
           RUN incluirLogPed('validarRepresPed',
                             'Representante econtrado:' + string(i-cod-rep)  ,
                             'INF').
        END.
    
        IF i-cod-vend <> 0 THEN DO.
           FIND b-repres WHERE 
                b-repres.cod-rep = i-cod-vend NO-LOCK NO-ERROR.
           IF NOT AVAIL b-repres THEN DO: 
              RUN incluirLogPed('validarRepresPed',
                                'Vendedor NAO encontrado:' + string(i-cod-vend) ,
                                'ERRO').
              RUN finalizarLogPed.
              NEXT.
           END.
           ELSE DO:
              FIND b-repres WHERE
                  b-repres.cod-rep = 0 NO-LOCK NO-ERROR.
              RUN incluirLogPed('validarRepresPed',
                                'Vendedor econtrado:' + string(i-cod-vend)  ,
                                'INF').
           END.
        END.

        /*
        // Se o Representante for IMA, ou se for Cliente novo/inativo ou se 
        // Vendedor Ç o mesmo Representante considerar o Representante enviado 
        // pelo Portal

        FIND ext-emitente OF emitente NO-LOCK NO-ERROR.

        ASSIGN l-venda-casada = NO.
        IF emitente.cod-rep = 1 OR
           ext-emitente.situacao = 2 OR // Inativo
           peds_web.log_novo_cliente = YES OR
           peds_web.repres_id = emitente.cod-rep THEN DO.

           FIND repres WHERE 
                repres.cod-rep = peds_web.repres_id NO-LOCK NO-ERROR.
           IF NOT AVAIL repres THEN DO: 
              RUN incluirLogPed('validarRepresPed','Representante NAO encontrado:' + string(peds_web.repres_id) ,'ERRO').
              RUN finalizarLogPed.
              NEXT.
           END.
           ELSE DO:
              RUN incluirLogPed('validarRepresPed','Representante econtrado:' + string(peds_web.repres_id)  ,'INF').
           END.
        END.
        ELSE DO.  
           // Considerar o Representante do Cadastro do Cliente  
           ASSIGN l-venda-casada = YES.
            
           FIND repres WHERE 
                repres.cod-rep = emitente.cod-rep NO-LOCK NO-ERROR.
           IF NOT AVAIL repres THEN DO: 
              RUN incluirLogPed('validarRepresPed','Representante NAO encontrado:' + string(emitente.cod-rep) ,'ERRO').
              RUN finalizarLogPed.
              NEXT.
           END.
           ELSE DO:
              RUN incluirLogPed('validarRepresPed','Representante econtrado:' + string(emitente.cod-rep)  ,'INF').
           END.

           // Vendedor quem digitou o Pedido (eniado pelo portal como repres_id)
           FIND b-repres WHERE 
                b-repres.cod-rep = peds_web.repres_id NO-LOCK NO-ERROR.
           IF NOT AVAIL b-repres THEN DO: 
              RUN incluirLogPed('validarRepresPed','Vendedor NAO encontrado:' + string(peds_web.repres_id) ,'ERRO').
              RUN finalizarLogPed.
              NEXT.
           END.
           ELSE DO:
              RUN incluirLogPed('validarRepresPed','Vendedor econtrado:' + string(peds_web.repres_id)  ,'INF').
           END.
        END.
        */

        IF tt-param.l-batch = NO THEN
           RUN pi-acompanhar IN h-acomp (INPUT "Pedido: " + STRING(peds_web.ped_web_id) ).
    
        IF peds_web.nr_pedido_erp <> 0 THEN DO.
           FIND ped-venda WHERE
                ped-venda.nr-pedido = peds_web.nr_pedido_erp NO-LOCK NO-ERROR.
           IF AVAIL ped-venda THEN DO.
              RUN setRowid IN hBoPedsWeb(ROWID(peds_web)).
              RUN setIndsitPedweb IN hBoPedsWeb(4). 
              RUN incluirLogPed('validarExistPedERP',
                                'Pedido web atualizado para Integrado por encontrar Pedido ERP:' + string(peds_web.nr_pedido_erp),
                                'AVISO').
              RUN finalizarLogPed.
              NEXT.
           END.
           ELSE DO:
              RUN incluirLogPed('validarExistPedERP',
                                'Pedido ERP:' +  string(peds_web.nr_pedido_erp)  + ' n∆o encontrado - Fluxo n∆o interrompido',
                                'INF').
           END.
        END.
        
        FIND FIRST ped-venda-ext WHERE
                   ped-venda-ext.cod-estab = peds_web.cod_estabel AND
                   ped-venda-ext.ped_web_id = peds_web.ped_web_id NO-LOCK NO-ERROR.
        IF AVAIL ped-venda-ext THEN DO.
           FIND ped-venda WHERE
                ped-venda.nr-pedido = ped-venda-ext.nr-pedido NO-LOCK NO-ERROR.
           IF AVAIL ped-venda THEN DO.
              RUN setRowid IN hBoPedsWeb(ROWID(peds_web)).
              RUN setIndsitPedweb IN hBoPedsWeb(4). 
              RUN setNrPedidoERP IN hBoPedsweb(ped-venda-ext.nr-pedido).
              RUN incluirLogPed('validarExistPedEXT',
                                'Pedido EXT  e pedido ERP encontrados - pedido setado como integrado'
                                ,'AVISO').
              RUN finalizarLogPed.
              NEXT.
           END.
    
           FIND CURRENT ped-venda-ext EXCLUSIVE-LOCK NO-ERROR.
           DELETE ped-venda-ext.
           RUN incluirLogPed('validarExistPedEXT',
                             'Pedido EXT encontrado e APAGADO' ,
                             'AVISO').
        END.
        ELSE DO:
           RUN incluirLogPed('validarExistPedEXT',
                             'Pedido EXT NAO encontrado ' ,
                             'INF'). 
        END.
    
        IF peds_web.nr_container <> 0 THEN DO.
           FIND pp-container WHERE
                pp-container.nr-container = peds_web.nr_container NO-LOCK NO-ERROR.
           IF NOT AVAIL pp-container THEN DO.
              ASSIGN c-mens-erro = "Container Informado n∆o foi Encontrado no Sistema, Imposs°vel importar Pedido..." + CHR(10) +
                                   'CONTAINER: ' + STRING(peds_web.nr_container) + CHR(10) +
                                   'ID: ' + STRING(peds_web.cliente_id).
              RUN rejeitarPedido(c-mens-erro,'validarExistContainer').
              NEXT.
           END.
           ELSE DO:
             RUN incluirLogPed('validarExistContainer',
                               'Container Encontrado:' + string(peds_web.nr_container)   ,
                               'INF'). 
           END.
    
           IF pp-container.situacao <> 1 THEN DO.
              ASSIGN c-mens-erro = "Situaá∆o do Container Informado n∆o est† Dispon°vel, Imposs°vel importar Pedido..." + CHR(10) +
                                   'CONTAINER: ' + STRING(peds_web.nr_container) + CHR(10) +
                                   'ID: ' + STRING(peds_web.cliente_id).
              RUN rejeitarPedido(c-mens-erro,'validarSitContainer').
              NEXT.
           END.
           ELSE DO:
             RUN incluirLogPed('validarSitContainer',
                               'Container Disponivel'  ,
                               'INF'). 
           END.
        END.
        
    
        IF peds_web.login_preposto <> '' THEN DO.
           FIND user-web WHERE
                user-web.usuario = repres.nome-abrev AND
                user-web.tp-usuario = 5 AND 
                user-web.login = peds_web.login_preposto NO-LOCK NO-ERROR.
           IF NOT AVAIL user-web THEN DO.
              ASSIGN c-mens-erro = "Preposto n∆o foi Encontrado no Sistema, Imposs°vel importar Pedido..." + CHR(10) +
                                   'PREPOSTO: ' + peds_web.login_preposto + CHR(10) +
                                   'ID: ' + STRING(peds_web.cliente_id).
              RUN rejeitarPedido(c-mens-erro,'validarPreposto').
              NEXT.
           END.
           ELSE DO:
            RUN incluirLogPed('validarPreposto',
                              'Preposto encontrado:' + peds_web.login_preposto  ,
                              'INF'). 
           END.
        END.
        ELSE DO:
            RUN incluirLogPed('validarPreposto',
                              'Preposto nao informado'  ,
                              'INF'). 
        END.
    

        // Validar Situaá∆o Comercial do Cliente
        RUN setCodEmitente IN hBOEmitente (INPUT emitente.cod-emitente).
        RUN getSitCliAdmVendas IN hBOEmitente (OUTPUT lBloqueado).
        IF lBloqueado THEN DO.
           ASSIGN c-mens-erro = "Cliente est† Bloqueado pela Adm de Vendas, Imposs°vel importar Pedido..." + CHR(10) +
                                'CNPJ: ' + peds_web.cnpj_novo_cliente + CHR(10) +
                                'ID: ' + STRING(peds_web.cliente_id).
           RUN rejeitarPedido(c-mens-erro,'validarExistCliente').
           NEXT.
        END.


        IF peds_web.log_operac_triang THEN DO.
           RUN incluirLogPed('validarOpTriang',
                             'Pedido COM Operacao Triangular' ,
                             'INF').   
           IF peds_web.log_novo_cliente_triang THEN DO:
              RUN incluirLogPed('validarNovoClienteTriang',
                                'Pedido COM cliente triangular novo informado'  ,
                                'INF').
              FIND b-emitente WHERE
                   b-emitente.cgc = peds_web.cnpj_novo_cliente_triang NO-LOCK NO-ERROR.
           END.
              
           ELSE DO:
              RUN incluirLogPed('validarNovoClienteTriang',
                                'Pedido SEM cliente triangular novo informado'  ,
                                'INF').
              FIND b-emitente WHERE
                   b-emitente.cod-emit = peds_web.cliente_triang_id NO-LOCK NO-ERROR.
           END.
              
    
           IF NOT AVAIL b-emitente THEN DO.
              ASSIGN c-mens-erro = "Cliente Triangular n∆o foi Encontrado no Sistema, Imposs°vel importar Pedido... (3)" + CHR(10) +
                                   'CNPJ: ' + peds_web.cnpj_novo_cliente_triang + CHR(10) +
                                   'ID: ' + STRING(peds_web.cliente_triang_id).
              RUN rejeitarPedido(c-mens-erro,'validarExistClienteTriang').
              NEXT.
           END.
           ELSE DO:
               RUN incluirLogPed('validarExistCliente',
                                 'Encontrado Cliente - ID:' + STRING(peds_web.cliente_triang_id) + ' CNPJ:' + peds_web.cnpj_novo_cliente_triang ,
                                 'INF').
           END.
        END.
        ELSE DO:
            RUN incluirLogPed('validarOpTriang',
                              'Pedido SEM Operacao Triangular' ,
                              'INF').   
        END.
    
       
        // Transportadora
        IF peds_web.transp_id = 0 THEN DO:
           RUN setRowid IN hBoPedsWeb(ROWID(peds_web)).
           RUN setTranspId   IN hBoPedsWeb(50).
           RUN incluirLogPed('validarTransp',
                             'Transportador com c¢digo 0 modificado para 50' ,
                             'INF').   
    
        END.
    
        FIND transporte WHERE 
             transporte.cod-transp = peds_web.transp_id NO-LOCK NO-ERROR.
        IF NOT AVAIL transporte THEN DO: 
           ASSIGN c-mens-erro = 'Transportador n∆o encontrado:' + STRING(peds_web.transp_id).
           RUN rejeitarPedido(c-mens-erro,'validarTransp').
           NEXT.
    
        END.
        ELSE DO:
          RUN incluirLogPed('validarTransp',
                            'Transportador Encontrado:' + STRING(peds_web.transp_id) ,
                            'INF').   
        END.
    
        IF peds_web.transp_redesp_id <> 0 THEN DO:
           FIND b-transporte WHERE 
                b-transporte.cod-transp = peds_web.transp_redesp_id NO-LOCK NO-ERROR.
           RUN incluirLogPed('validarTranspRedesp',
                             'Transportador de redespacho diferente de zero. Posiconou sem validaá∆o de existencia no transportador:' + STRING(peds_web.transp_redesp_id) 
                             ,'INF').   
        END.
           
        RUN incluirLogPed('transpCliente',
                          'PROVISORIAMENTE: est† mantendo a transportadora do PORTAL sem buscar a do cliente devido a implementaá‰es pendentes com relaá∆o a ITAJAI - transp x estab. '  
                           ,'INF').

        /*
        IF peds_web.cod_tipo_frete <> 3 AND  // N∆o Ç FOB e o cliente n∆o quis um transportador proprio, busca Tansp. do Cliente
           emitente.cod-transp <> 0 AND cod_tipo_frete_cliente <> 3 AND emitente.cod-transp <>  0 THEN DO:
           FIND transporte WHERE 
                transporte.cod-transp = emitente.cod-transp NO-LOCK NO-ERROR.
           RUN incluirLogPed('transpCliente',
                             'Tipo de Frete diferente de FOB e cliente n∆o quis um transportador pr¢prio - buscou transportador do cadastro do cliente por ser diferente de zero ->' +
                             string(emitente.cod-transp) + ' - desconsiderou neste caso o transp. informado no portal '
                                ,'INF').
    
    
        END.
        ELSE DO:
            RUN incluirLogPed('transpCliente',
                              'Nao alterou o transp informado no portal por n∆o atender a condiá∆o de n∆o ser FOB, n∆o ter tipo de frete do cliente como FOB e o cliente c¢digo do transportador do cliente n∆o estar zerado. '   
                              ,'INF').
        END.
        */   
         
    
        // Moeda
        IF peds_web.cod_moeda = 'Real' OR  peds_web.cod_moeda = '1' THEN DO:
           ASSIGN i-moeda = 0.
           RUN incluirLogPed('ConvMoeda',
                             'Moeda igual a REAL ou 1 convertida para o valor 0 ->' + peds_web.cod_moeda,
                             'INF').
    
        END.
        ELSE DO:
           ASSIGN i-moeda = 3.
           RUN incluirLogPed('ConvMoeda',
                             'Moeda DIFERENTE de REAL e de 1 convertida para o valor 3 ->' + peds_web.cod_moeda,
                             'INF').
        END.
           
    
        FIND moeda WHERE
             moeda.mo-codigo = i-moeda NO-LOCK NO-ERROR.
        IF NOT AVAIL moeda THEN do:
            ASSIGN c-mens-erro = 'Moeda n∆o encontrada:' + string(i-moeda).
            RUN rejeitarPedido(c-mens-erro,'validarMoeda').
            NEXT.
        END.
        ELSE DO:
            RUN incluirLogPed('validarMoeda',
                              'Moeda encontrada:' + STRING(i-moeda),
                              'INF').
        END.
        FOR EACH tt-ped-venda.
            DELETE tt-ped-venda.
        END.
        
        CREATE tt-ped-venda.
        ASSIGN tt-ped-venda.cod-estabel = peds_web.cod_estabel
               tt-ped-venda.nr-pedido = IF peds_web.nr_pedido_erp <> 0
                                        THEN peds_web.nr_pedido_erp
                                        ELSE NEXT-VALUE(seq-nr-pedido)
               tt-ped-venda.nr-pedcli = STRING(tt-ped-venda.nr-pedido)
               tt-ped-venda.nome-abrev = emitente.nome-abrev
               tt-ped-venda.cod-portador = emitente.portador
               tt-ped-venda.modalidade = emitente.modalidade
               tt-ped-venda.per-max-canc = 99
               tt-ped-venda.cod-entrega = emitente.cod-entrega
               tt-ped-venda.dt-userimp = TODAY
               tt-ped-venda.val-frete = peds_web.vl_frete_destacado
               tt-ped-venda.observacoes = peds_web.comentario
               tt-ped-venda.char-1 = STRING(TODAY,"99/99/9999") + " " + STRING(TIME,"HH:MM") + ' DIGITAÄ«O PORTAL'.
    
        // Busca Finalizade de Venda e Natureza de Operaá∆o
        RUN limparErros IN h-bonat001.
        RUN retornarfinalidadecliente IN h-bonat001 (INPUT emitente.cod-emitente,
                                                     OUTPUT i-fin-nat, 
                                                     OUTPUT c-cnae).
        RUN retornarerros IN h-bonat001 (OUTPUT c-erro-nat).
        IF c-erro-nat <> '' THEN DO.
           RUN rejeitarPedido(c-erro-nat,'validarFinalidadeCli').
           NEXT.
        END.
        ELSE DO:
            RUN incluirLogPed('validarFinalidadeCli',
                              'Finalidade Encontrada com Sucesso:' + STRING(i-fin-nat)  + ' - CNAE considerado:'  + c-cnae,
                              'INF' ).
        END.
         ASSIGN tt-ped-venda.cidade = emitente.cidade
                tt-ped-venda.estado = emitente.estado.
        
        IF NOT peds_web.log_operac_triang THEN DO.
           RUN pi-calc-natur-oper (INPUT emitente.cod-emit,
                                   INPUT "",
                                   OUTPUT lOkNatOper).
    
           IF lOkNatOper = NO  THEN DO:
               RUN rejeitarPedido('SEM Operacao Triangular - Retornou Erro ao buscar a natureza de operacao -> pi-calc-natur-oper-> emitente:' + STRING(emitente.cod-emit)
                                  + ' ->' + c-erro-nat,'buscarNatOperacao'). 
               NEXT.
           END.
           ELSE DO:
               RUN incluirLogPed('buscarNatOperacao',
                                 'SEM Operacao Triangular - Natureza de Operaá∆o encontrada com sucesso:' + c-natur-oper,
                                 'INF' ).
           END.
        END.
        ELSE DO.
           ASSIGN tt-ped-venda.nome-abrev-tri = b-emitente.nome-abrev.
           RUN pi-calc-natur-oper (INPUT emitente.cod-emit,
                                   INPUT b-emitente.nome-abrev,
                                   OUTPUT lOkNatOper).
    
           IF lOkNatOper = NO THEN DO: 
              RUN rejeitarPedido('COM Operacao Triangular - Retornou Erro ao buscar a natureza de operacao -> pi-calc-natur-oper-> emitente:' 
                                 + STRING(emitente.cod-emit) + ' - nome abrev cli triang.:' + b-emitente.nome-abrev +
                                  ' ->' + c-erro-nat,'buscarNatOperacao').  
              NEXT.
           END.
           ELSE DO:
             RUN incluirLogPed('buscarNatOperacao',
                               'COM Operacao Triangular - Retornou com sucesso  a natureza de operacao -> pi-calc-natur-oper-> emitente:' + STRING(emitente.cod-emit) + ' - nome abrev cli triang.:' + b-emitente.nome-abrev + ' natureza encontrada:' + c-natur-oper,
                               'INF').
           END.
        END.
    
        FIND natur-oper WHERE
             natur-oper.nat-operacao = c-natur-oper NO-LOCK NO-ERROR.
        
        RUN incluirLogPed('buscarNatOperacao',
                          'posicionou sem validaá∆o de existencia na natureza de operacao:' + c-natur-oper,
                          'INF').
        // Desconto e Prioridade
        ASSIGN c-pct-desconto = getPrioridadeERP(peds_web.cod_prioridade).
        RUN incluirLogPed('buscarPrioridade','Prioridade Ped.Web:' + string(peds_web.cod_prioridade) + " retornou ->" + c-pct-desconto,
                          'INF' ).
    
        ASSIGN tt-ped-venda.tp-preco       = 1
               tt-ped-venda.des-pct-descon = c-pct-desconto
               tt-ped-venda.cod-priori     = peds_web.cod_prioridade.
    
        IF peds_web.log_a_vista THEN DO:
           FIND cond-pagto WHERE
                cond-pagto.cod-cond-pag = 1 NO-LOCK NO-ERROR.
           RUN incluirLogPed('tratarCondPagto',
                             'Pedido Web com condiá∆o Ö Vista, setado condiá∆o do pedido ERP igual a 1',
                             'INF' ).
        END.
        ELSE DO:
            FIND cond-pagto WHERE
                 cond-pagto.cod-cond-pag = peds_web.cond_pagto_id NO-LOCK NO-ERROR.
            RUN incluirLogPed('tratarCondPagto',
                              'Pedido Web com condiá∆o DIFERENTE de  Ö Vista, busca na tabela de condicao de pagamento-> ' + STRING(peds_web.cond_pagto_id),
                              'INF' ).
    
        END.
           
    
        // Demais campos
        ASSIGN tt-ped-venda.cod-mensagem   = natur-oper.cod-mensagem
               tt-ped-venda.cod-cond-pag   = 0
               tt-ped-venda.mo-codigo      = moeda.mo-codigo
               tt-ped-venda.no-ab-reppri   = repres.nome-abrev
               tt-ped-venda.cod-cond-pag   = IF AVAIL cond-pagto
                                             THEN cond-pagto.cod-cond-pag ELSE 0
               tt-ped-venda.nr-tab-finan   = IF AVAIL cond-pagto 
                                             THEN cond-pagto.nr-tab-fin ELSE 1
               tt-ped-venda.nr-ind-finan   = IF AVAIL cond-pagto
                                             THEN cond-pagto.nr-ind-fin ELSE 1
               tt-ped-venda.tp-pedido      = peds_web.cod_tipo_pedido
               tt-ped-venda.nome-transp    = transporte.nome-abrev
               tt-ped-venda.nome-tr-red    = IF peds_web.transp_redesp_id <> 0 AND AVAIL b-transporte
                                             THEN b-transporte.nome-abrev ELSE ""
               tt-ped-venda.ind-fat-par    = NO
               tt-ped-venda.dsp-pre-fat    = YES
               tt-ped-venda.ind-lib-nota   = YES.
        
    
        RUN incluirLogPed('preencherDadosPedidoERP','Preenchimento dos seguintes campos da tabela ped-venda com seus respectivos valores fixos: ' + 'cod-cond-pag = 0 , nr-tab-finan = 1, nr-ind-finan = 1',
                          'INF' ).
    
        IF peds_web.cod_tipo_frete =  1 OR 
           peds_web.cod_tipo_frete =  2 THEN DO:
           ASSIGN tt-ped-venda.cidade-cif = tt-ped-venda.cidade
                  tt-ped-venda.ind-tp-frete = 1.
           RUN incluirLogPed('tratarTipoFrete','Tipo de frete in (1,2) seta ind-tp-frete como 1 -> tipo de frete: ' + STRING(peds_web.cod_tipo_frete),
                             'INF').
        END.
           
        ELSE DO:
           RUN incluirLogPed('tratarTipoFrete',
                             'Tipo de frete NOT in (1,2) seta ind-tp-frete como 2 e cidade CIF como " "(em branco) -> tipo de frete: ' + STRING(peds_web.cod_tipo_frete),
                             'INF').
           ASSIGN tt-ped-venda.cidade-cif = ''
                  tt-ped-venda.ind-tp-frete = 2.
        END.
           
    
       
        // Cria o Pedido
        ASSIGN c-mens-erro = "".
        RUN esapi/cria-pedvenda-portal.p (INPUT TABLE tt-ped-venda,
                                          OUTPUT c-mens-erro).
        IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
           ASSIGN c-mens-erro = 'Erro ao Criar o Pedido...' + CHR(10) + 
                                 c-mens-erro.
    
           RUN rejeitarPedido(c-mens-erro,'criarPedidoERP') .
           NEXT.
        END.
        ELSE DO:
           RUN incluirLogPed('criarPedidoERP',
                             'Pedido de Venda criado com sucesso no ERP ou j† existia no ERP','INF').
        END.
    
        FIND ped-venda WHERE
             ped-venda.nr-pedcli  = tt-ped-venda.nr-pedcli AND
             ped-venda.nome-abrev = tt-ped-venda.nome-abrev SHARE-LOCK NO-ERROR.
    
        ASSIGN cDescLog = "Implantado o Pedido WEB: " + tt-ped-venda.nr-pedcli + "   Cliente:" + tt-ped-venda.nome-abrev .
        RUN esapi/cria-log-pedvenda.p (INPUT tt-ped-venda.nr-pedcli,
                                       INPUT tt-ped-venda.nome-abrev,
                                       INPUT cDescLog,
                                       INPUT NO).
        
        RUN incluirLogPed('logPedVenda', cDescLog + cAdcLogPed,'INF').
    
        // Cria extens∆o do Pedido de Venda
        //IMPORTANTE: Ç necess†rio pegar este c¢digo de extens∆o e compatibilizar com esta parte no espd4000, fazendo uma API e chamando nos dois locais para centralizar a regra.
    
        CREATE ped-venda-ext.
        ASSIGN ped-venda-ext.cod-estabel  = ped-venda.cod-estabel 
               ped-venda-ext.nr-pedido    = ped-venda.nr-pedido      
               ped-venda-ext.nome-abrev   = ped-venda.nome-abrev
               ped-venda-ext.nome-ab-vend = IF AVAIL b-repres
                                            THEN b-repres.nome-abrev
                                            ELSE ""
               ped-venda-ext.l-etiqueta   = NO
               ped-venda-ext.nr-pedcli    = ped-venda.nr-pedcli   
               ped-venda-ext.nr-container = peds_web.nr_container
               ped-venda-ext.ped_web_id   = peds_web.ped_web_id 
               ped-venda-ext.tb_preco_id  = IF peds_web.tb_preco_id = 0
                                            THEN 1 ELSE peds_web.tb_preco_id 
               ped-venda-ext.origem       = 5  // ImaOnline
               ped-venda-ext.tp-pagto     = getTpPagto(peds_web.cod_forma_pagto)
               ped-venda-ext.preposto     = peds_web.login_preposto
               ped-venda-ext.l_comis_neg  = peds_web.log_perc_negoc
               ped-venda-ext.cod_finalidade_venda = i-fin-nat.
    
        RUN incluirLogPed('criarPedVendaExt',
                         'Criaá∆o do registro na tabela ped-venda-ext com os seguintes valores fixos:' 
                         + 'l-etiqueta = no, origem = 5(portal de vendas) '
                          ,'INF').     
    
        
        ASSIGN ped-venda-ext.tp-pedido = 'Normal'.
        IF peds_web.log_a_vista OR
           peds_web.dias_cond_pagto_esp = ''  OR
           peds_web.dias_cond_pagto_esp = '0' OR
           peds_web.dias_cond_pagto_esp = '1' THEN DO:

           //ASSIGN ped-venda-ext.tp-pedido = "∑ Vista".
           RUN incluirLogPed('tratarPedAVista','Campo peds_web.log_a vista igual a 1 ','INF' ).     
    
           ASSIGN ped-venda.cod-portador = 99
                  ped-venda.modalidade = 6.
    
           RUN incluirLogPed('tratarPedAVista','Campo peds_web.log_a vista igual a 1, modificado os  seguintes campos da tabela ped-venda:' + 
                             'cod-portador = 99, modalidade = 6 ','INF' ).     
    
        END.
        ELSE DO:
           RUN incluirLogPed('tratarPedAVista','Campo peds_web.log_a vista diferente de 1, pedido n∆o Ç a vista e o cod-portador e a modalidade da tabela ped-venda' + 
                             ' e o tp-pedido da tabela ped-venda-ext mantiveram-se no valor anterior',
                             'INF' ).        
        END.
    
        ASSIGN ped-venda-ext.tp-frete = getTipoFreteERP(peds_web.cod_tipo_frete).
    
        RUN incluirLogPed('converterTipoFrete','Tipo de Frete Pedido Web:' + STRING(peds_web.cod_tipo_frete) +
                           " convertido para:" + ped-venda-ext.tp-frete + 
                           " e setado para o campo ped-venda-ext.tp-frete ",'INF' ).
    
        IF ped-venda.nome-abrev-tri <> "" THEN DO:
           ASSIGN ped-venda-ext.tp-pedido = "Operaá∆o Triangular".
            RUN incluirLogPed('setOperTriangPedExt',
                              'Nome Abrev.Tri. diferente de branco, campo ped-venda-ext.tp-pedido setado como "operaá∆o triangular"',
                              'INF' ).
        END.
        ELSE DO:
            RUN incluirLogPed('setOperTriangPedExt','Nome Abrev.Tri. EM branco, n∆o Ç uma operaá∆o triangular','INF' ).
        END.
           
    
        // Trata Itens do Pedido
        ASSIGN c-mens-erro = ""
               l-erro = NO.
    
        ASSIGN i-nr-sequencia = 0.
        RUN incluirLogPed('itensPedido','INICIO','INF' ).
    
        EMPTY TEMP-TABLE tt-itens-ped.

        FOR EACH itens_ped_web WHERE
                 itens_ped_web.ped_web_id = peds_web.ped_web_id AND 
                 itens_ped_web.ind_sit_itens_ped_web <> 3 NO-LOCK.
             
            ASSIGN i-nr-sequencia = i-nr-sequencia + 10.
            RUN incluirLogPed('itensPedido','INICIO-' + string(itens_ped_web.ITEM_ped_web_id) +
                              ' - atribuida a sequencia' + STRING(i-nr-sequencia) ,'INF' ).  
    
    
            /*IMPORTANTE: Ap¢s acabar o log Ç necess†rio rever esta regra de vl.unit*/
    
            IF itens_ped_web.vl_informado <> 0 THEN DO:
               ASSIGN de-vl-preori = itens_ped_web.vl_informado.
               RUN incluirLogPed('itensPedido_atribuirVlUnit','valor informado diferente de zero assumido como vl.unit:' + 
                             string(itens_ped_web.vl_informado),'INF').
            END.
               
            ELSE IF itens_ped_web.vl_unit_tabela <> 0 THEN DO: 
                ASSIGN de-vl-preori = itens_ped_web.vl_unit_tabela.
                RUN incluirLogPed('itensPedido_atribuirVlUnit','valor informado iguala zero e vl_unit_tabela dif. zero assumido como vl.unit:' + 
                             string(itens_ped_web.vl_unit_tabela),'INF').
            END.
            ELSE IF itens_ped_web.vl_unit_final <> 0 THEN DO:
                ASSIGN de-vl-preori = itens_ped_web.vl_unit_final.
                 RUN incluirLogPed('itensPedido_atribuirVlUnit','valor informado igual a zero , v_unit_tabela igual a zero e  vl_unit_final dif. zero assumido como vl.unit:' + 
                             string(itens_ped_web.vl_unit_final),'INF').
            END.
               
    
            IF de-vl-preori = 0 THEN DO.
    
               IF c-mens-erro = '' THEN
                  ASSIGN c-mens-erro = 'Item: ' + itens_ped_web.it_codigo + 'Ref.: ' + itens_ped_web.cod_refer + ' Sem Preáo Informado'.
               ELSE
                  ASSIGN c-mens-erro = c-mens-erro + CHR(10) +
                                       'Item: ' + itens_ped_web.it_codigo + 'Ref.: ' + itens_ped_web.cod_refer + ' Sem Preáo Informado'.
    
               ASSIGN l-erro = YES.
               RUN incluirLogPed('itensPedido_atribuirVlUnit',c-mens-erro,'ERRO').
               NEXT.
            END.
            ELSE DO:
              RUN incluirLogPed('itensPedido_atribuirVlUnit',
                                'Item: ' + itens_ped_web.it_codigo + 'Ref.: ' + itens_ped_web.cod_refer +
                                 ' Preáo Unit:' + string(de-vl-preori),'INF').
            END.
    
            RUN esapi/cria-peditem-portal.p (INPUT tt-ped-venda.nr-pedcli,
                                             INPUT i-nr-sequencia,
                                             INPUT itens_ped_web.it_codigo,
                                             INPUT itens_ped_web.cod_refer,
                                             INPUT itens_ped_web.qt_pedida,
                                             INPUT de-vl-preori,
                                             OUTPUT c-mens-erro-it).
    
            IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
               ASSIGN c-mens-erro = 'ERRO ao Implantar o Item: ' + itens_ped_web.it_codigo +  ' Ref.: ' + itens_ped_web.cod_refer + CHR(10) + CHR(10) + 
                                    c-mens-erro-it.
               RUN incluirLogPed('itensPedido_criarItemERP',c-mens-erro,'ERRO'). 
               ASSIGN l-erro = YES.
    
               NEXT.
            END.
            ELSE DO:
              RUN incluirLogPed('itensPedido_criarItemERP','Sucessso ao implantar o Item: ' 
                                + itens_ped_web.it_codigo +  ' Ref.: ' + itens_ped_web.cod_refer ,'INF'). 
            END.
    

            // Verofica o Controle Preco
            ASSIGN i-ControlePreco = itens_ped_web.cod_controle_preco.
            IF i-ControlePreco = 0 THEN DO.
               // Busca Prazo MÇdio do Pedido
               RUN pi-prazo-medio.

               ASSIGN de-vlReal = 0        
                      de-vlDolar = 0.

               // Busca o ID do Preáo
               IF ped-venda-ext.tp-pedido = 'PI' THEN
                  // Mostrar Preáo autorizado para Venda
                  RUN pi-busca-preco (INPUT  itens_ped_web.it_codigo,
                                      INPUT  itens_ped_web.cod_refer,
                                      INPUT  "", // Campanha
                                      OUTPUT de-vlReal,  
                                      OUTPUT de-vlDolar,
                                      OUTPUT i-ControlePreco).
               ELSE
                  RUN pi-busca-preco (INPUT  itens_ped_web.it_codigo,
                                      INPUT  itens_ped_web.cod_refer,
                                      INPUT  "", // Campanha
                                      OUTPUT de-vlReal,  
                                      OUTPUT de-vlDolar,
                                      OUTPUT i-ControlePreco).
            END.

            // Verifica o Controle Preco OutLet
            ASSIGN i-ControlePrecoOut = INTEGER(itens_ped_web.num_id_liquida_ima).
            IF i-ControlePrecoOut = 0 THEN DO.
               // Busca Prazo MÇdio do Pedido
               RUN pi-prazo-medio.

               ASSIGN de-vlRealOut = 0     
                      de-vlDolarOut = 0.

               // Busca o ID do Preáo OutLet
               RUN pi-busca-preco (INPUT  itens_ped_web.it_codigo,
                                   INPUT  itens_ped_web.cod_refer,
                                   INPUT  "OUTLET", // Campanha
                                   OUTPUT de-vlRealOut,  
                                   OUTPUT de-vlDolarOut,
                                   OUTPUT i-ControlePrecoOut).
            END.

            // Cria Dados Complementares do Item
            FIND ped-item-ext WHERE
                 ped-item-ext.cod-estabel = tt-ped-venda.cod-estabel AND
                 ped-item-ext.nome-abrev = tt-ped-item.nome-abrev AND
                 ped-item-ext.nr-pedcli = tt-ped-item.nr-pedcli AND
                 ped-item-ext.nr-sequencia = i-nr-sequencia NO-LOCK NO-ERROR.
            IF NOT AVAIL ped-item-ext THEN DO.
               CREATE ped-item-ext.
               ASSIGN ped-item-ext.cod-estabel   = tt-ped-venda.cod-estabel 
                      ped-item-ext.nome-abrev    = tt-ped-venda.nome-abrev
                      ped-item-ext.nr-pedcli     = tt-ped-venda.nr-pedcli
                      ped-item-ext.nr-sequencia  = i-nr-sequencia
                      ped-item-ext.it-codigo     = itens_ped_web.it_codigo
                      ped-item-ext.cod-refer     = itens_ped_web.cod_refer
                      ped-item-ext.retirar-corte = NO
                      ped-item-ext.reservado     = NO
                      ped-item-ext.lote          = 'RP' + itens_ped_web.cod_refer.

               ASSIGN ped-item-ext.cod_controle_preco = i-ControlePreco.
    
               IF i-ControlePrecoOut <> 0 THEN 
                  ASSIGN ped-item-ext.liquida-ima = YES
                         ped-item-ext.num-id-liquida-ima = STRING(i-ControlePrecoOut).
            END.

            // Cria tt-itens-ped para Calculo da Comiss∆o
            //CREATE tt-itens-ped.
            //BUFFER-COPY ped-item TO tt-itens-ped
            //      ASSIGN tt-itens-ped.cod-estabel  = ped-venda.cod-estabel.

        END.  // Criou os Itens

        RUN incluirLogPed('itensPedido','FIM ','INF' ).
    
        IF l-erro = YES THEN DO.
    
           ASSIGN cDescLog = 'Existiram erros no processo de criaá∆o dos itens: ' + c-mens-erro.
           RUN incluirLogPed('itensPedido_criacaoERP',c-mens-erro,
                             'ERRO' ).                            
           RUN excluirPedido('itensPedido_tratarErro',cDescLog).
           NEXT. 
    
        END.
        ELSE DO:
            RUN incluirLogPed('itensPedido_criacaoERP',
                             'Todos os itens foram criados com sucesso',
                             'INF' ).  
        END.
    
    
        /* Cria Condiá∆o de Pagamento Especial */
    
        EMPTY TEMP-TABLE tt-cond-ped.
        IF NOT AVAIL cond-pagto AND 
           peds_web.cond_pagto_id = 0 AND 
           peds_web.dias_cond_pagto_esp <> '' THEN DO.
           RUN incluirLogPed('converterCondPagto','Condiá∆o de pagto especial e cond.pagto especial informada no portal'
                             ,'INF').
           ASSIGN i-nr-sequencia = 0.
           RUN incluirLogPed('converterCondPagto','INICIO - criacao','INF').
           DO i-cont = 1 TO NUM-ENTRIES(peds_web.dias_cond_pagto_esp).
              ASSIGN i-nr-sequencia = i-nr-sequencia + 10.
        
              CREATE tt-cond-ped.
              ASSIGN tt-cond-ped.nr-pedido = tt-ped-venda.nr-pedido
                     tt-cond-ped.nr-sequencia = i-nr-sequencia 
                     tt-cond-ped.nr-dias-venc = INTEGER(ENTRY(i-cont,peds_web.dias_cond_pagto_esp)).
              RUN incluirLogPed('converterCondPagto','Criado registro TT-> seq:' + STRING(tt-cond-ped.nr-sequencia) +
                                ' - dias vencto:' + string(tt-cond-ped.nr-dias-venc) ,'INF').
           END.
           RUN incluirLogPed('converterCondPagto','FIM - criacao','INF').
           RUN pi-perc-pagto (INPUT INT(NUM-ENTRIES(peds_web.dias_cond_pagto_esp))).
           RUN incluirLogPed('converterCondPagto','chamou a pi-perc-pagto para colocar %  de pagto igual a todas as parcelas'
                             ,'INF').
        
           ASSIGN c-mens-erro = "".
           FIND FIRST tt-cond-ped NO-ERROR.
           IF AVAIL tt-cond-ped THEN DO.
    
              RUN incluirLogPed('converterCondPagto','Econtrados registros na TT de cond.pagto','INF').
              IF tt-param.l-batch = NO THEN
                 RUN pi-acompanhar IN h-acomp (INPUT "Criando Condiá∆o Especial de Pagamento...").
    
              RUN openQueryStatic IN h-bodi018 (INPUT "Main":U).
              RUN emptyRowErrors IN h-bodi018.
              FOR EACH tt-cond-ped NO-LOCK.
                  FOR EACH wt-cond-ped.
                      DELETE wt-cond-ped.
                  END.
                  CREATE wt-cond-ped.
                  BUFFER-COPY tt-cond-ped TO wt-cond-ped.
        
                  RUN setRecord IN h-bodi018 (INPUT TABLE wt-cond-ped).
                  RUN createRecord IN h-bodi018.
              END.
              RUN getRowErrors IN h-bodi018 (OUTPUT TABLE RowErrors).
              IF CAN-FIND(FIRST RowErrors 
                          WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
    
                 ASSIGN c-mens-erro = "Erro ao Criar Condiá∆o de Pagamento Especial - ERRO: " + CHR(10).
                 FOR EACH rowerrors WHERE
                          RowErrors.ErrorSubType = "ERROR":U:
                     ASSIGN c-mens-erro = c-mens-erro + rowerrors.errordescription.
                 END.
                 ASSIGN c-mens-erro = c-mens-erro + CHR(10) +
                                      'Verifique se o Cliente pode Comprar a Prazo'.
                 RUN excluirPedido('converCondPagto',c-mens-erro).
                 NEXT. 
              END.
              ELSE DO:
                  RUN incluirLogPed('converterCondPagto','Condiá∆o de Pagto Criada sem Erros','INF').
              END.
           END.
        END.
        ELSE DO:
            RUN incluirLogPed('converterCondPagto','Condiá∆o de pagto cadastrada, nenhuma aá∆o est† sendo feita. Ç preciso implementar'
                             ,'INF').
        END.
    
    
        /* Cria Representantes do Pedido */
        ASSIGN c-mens-erro = "".
        RUN pi-cria-repres (OUTPUT lOkComisRep).

        IF lOkComisRep = NO THEN DO: 
           RUN excluirPedido('criarRepresPedido','Erro ao Criar o Representante...' + CHR(10) + c-mens-erro).
           NEXT. 
        END.
        ELSE DO:
            RUN incluirLogPed('criarRepresPedido','Representante do Pedido criado com sucesso ','INF').
        END.
    
    
        // Atualiza Entrega
        FOR EACH ped-ent OF ped-venda SHARE-LOCK.
            ASSIGN ped-ent.tipo-atend = 2.
        END.
        RUN incluirLogPed('atuEntregaPed','Registros da Tabela ped-ent atualizados para tipo-atend = 2 ','INF').
    
    
    
        ASSIGN ped-venda.cod-sit-ped = 4.
        RUN incluirLogPed('atuSitPed','Situaá∆o do Pedido atualizada para 4(pendente) ','INF').
    
    
        IF LOOKUP(ped-venda-ext.tp-pedido,c-tpped-cred-aut) > 0 THEN DO:
           ASSIGN ped-venda.cod-sit-ped = 1.
           RUN incluirLogPed('atuSitPed','Situaá∆o do Pedido atualizada para 1, pois, o tipo de pedido:' + 
                             ped-venda-ext.tp-pedido + 'est† na lista de pedidos com crÇdito automatico:' + 
                              c-tpped-cred-aut,'INF').  
    
        END.
           
        
        IF tt-ped-venda.tp-pedido = 'PI' THEN DO:
           ASSIGN ped-venda-ext.l-nao-aprovar = YES.
           RUN incluirLogPed('atuNaoAprovar','Pedido PI marcado como "n∆o aprovar"','INF').
        END.
        ELSE DO :
           RUN incluirLogPed('atuNaoAprovar','Pedido PE, ser† avaliado pelo processo de aprovaá∆o','INF'). 
        END.
           
        IF peds_web.log_a_vista OR
           peds_web.dias_cond_pagto_esp = ''  OR
           peds_web.dias_cond_pagto_esp = '0' OR
           peds_web.dias_cond_pagto_esp = '1' THEN DO:
           ASSIGN ped-venda-ext.l-bloqueio = YES.
           RUN incluirLogPed('tratarBloqFatPed','Faturamento bloqueado, pois, o pedido Ç a vista', 'INF').
    
        END.
        ELSE DO:
           RUN incluirLogPed('tratarBloqFatPed','Faturamento NAO bloqueado, pois, o pedido NAO Ç a vista', 'INF'). 
        END.
           
        
        RUN gravarparamnaturpedvenda IN h-bonat001 (INPUT i-param-nat,
                                                    INPUT ped-venda.nr-pedido,
                                                    INPUT ped-venda.cod-estabel,
                                                    OUTPUT l-ok).
    
        RUN incluirLogPed('gravarRegraNatOperPed','Gravado no pedido de venda a regra:' + STRING(i-param-nat),'INF').
    
    
    	/* Valida CrÇdito do Pedido*/
        RUN esapi/credito-pedvenda.p.
    
        RUN incluirLogPed('validarCreditoPed','Passou pelo programa esapi/credito-pedvenda.p para validaá∆o do crÇdito','INF').
    
        /* Validar Desconto*/
        IF ped-venda.des-pct-desconto-inform <> "" AND 
           ped-venda.ind-sit-desconto <> 2 THEN DO. /* Tem Desconto e n∆o foi A*/
           ASSIGN ped-venda.ind-sit-desconto = 1.
           ASSIGN cDescLog = "Desconto Informado, Pedido Requer Aprovaá∆o".
           RUN esapi/cria-log-pedvenda.p (INPUT ped-venda.nr-pedcli,
                                          INPUT ped-venda.nome-abrev,
                                          INPUT cDescLog,
                                          INPUT NO).
           RUN incluirLogPed('validarDescontoPed',cDescLog + cAdcLogPed,'INF').
        END.
        ELSE DO:
           RUN incluirLogPed('validarDescontoPed',"Pedido Sem Desconto",'inf'). 
        END.
    
        /* Valida Frete */
        ASSIGN ped-venda.cod-sit-com = 2.
        RUN incluirLogPed('validarFrete','Campo ped-venda.cod-sit-com alterado para 2','INF').
        RUN esapi/valida-frete-cif.p (INPUT ped-venda.nr-pedcli,
                                      OUTPUT l-ok).
        IF l-ok = NO THEN DO.
           ASSIGN cDescLog = "Valor do Pedido Inv†lido para Frete CIF, Pedido Requer Aprovaá∆o".
           RUN esapi/cria-log-pedvenda.p (INPUT ped-venda.nr-pedcli,
                                          INPUT ped-venda.nome-abrev,
                                          INPUT cDescLog,
                                          INPUT NO).
           RUN incluirLogPed('validarFrete',cDescLog + cAdcLogPed, 'INF' ).
        END.
        ELSE DO:
           RUN incluirLogPed('validarFrete',"Frete sem necessidade de aprovaá∆o", 'INF' ). 
        END.
    
        /* Valida Preáo */
        ASSIGN ped-venda.log-ped-bonif-pendente = NO
               ped-venda.cod-sit-preco = 2.
    
        RUN incluirLogPed('validarPreco','setado log-ped-bonif-pendente = no e cod-sit-preco=2','INF').
        IF ped-venda.tp-preco = 1 THEN DO.
           RUN esapi/valida-preco.p (INPUT ped-venda.nr-pedcli,OUTPUT l-ok).
           RUN incluirLogPed('validarPreco','chamada esapi/valida-preco.p' ,'INF').
           IF l-ok = NO THEN DO.
              ASSIGN cDescLog = "Preáo Inv†lido, Pedido Requer Aprovaá∆o".
              RUN esapi/cria-log-pedvenda.p (INPUT ped-venda.nr-pedcli,
                                             INPUT ped-venda.nome-abrev,
                                             INPUT cDescLog,
                                             INPUT NO).
              RUN incluirLogPed('validarPreco',cdescLog + cAdcLogPed ,'INF').
           END.
        END.
        ELSE DO:
            RUN incluirLogPed('validarPreco',"tp-preco <> 1 , preáo n∆o requer aprovaá∆o" ,'INF').
        END.
    
        /* Valida Comiss∆o */
        /* comentado, pois, no portal nesta data 03/07/2020 n∆o existe forma de modificar a comiss∆o
        a n∆o ser por calculo de bonus que pode variar do valor padrao da comissao, mas, que n∆o 
        necessita de aprovaá∆o.*/

        // Retirado o comentario por Toninho em 31/08, para sempre aprovar quando
        // a comiss∆o do Pedido for diferente da comiss∆o do Representante

        FIND ped-repre WHERE
             ped-repre.nr-pedido = ped-venda.nr-pedido AND
             ped-repre.nome-ab-rep = ped-venda.no-ab-reppri SHARE-LOCK NO-ERROR.
        IF AVAIL ped-repre THEN DO.
           IF ped-repre.perc-comis <> repres.comis-direta THEN DO.
              ASSIGN ped-repre.cod-classif = IF ped-repre.cod-classif = ""
                                             THEN "NAO_AVALIADO"
                                             ELSE ped-repre.cod-classif.
    
              RUN esapi/cria-log-pedvenda.p (INPUT tt-ped-venda.nr-pedcli,
                                             INPUT tt-ped-venda.nome-abrev,
                                             INPUT "Comiss∆o Inv†lida para o Representante, Pedido Requer Aprovaá∆o",
                                             INPUT NO).
           END.
           ELSE
              ASSIGN ped-repre.cod-classif = "".
        END.
        /*
        FIND ped-repre WHERE
             ped-repre.nr-pedido = ped-venda.nr-pedido AND
             ped-repre.nome-ab-rep = ped-venda.no-ab-reppri SHARE-LOCK NO-ERROR.
        IF AVAIL ped-repre THEN DO:
           ASSIGN ped-repre.cod-classif = "".
           RUN incluirLogPed('validarComissao',"Comissao do portal sempre aprovada, pois, Ç de atribuiá∆o automatica" ,'INF').
        END.
        ELSE DO:
           RUN incluirLogPed('validarComissao',"Comissao do representante n∆o encontrada" ,'ALERTA').
        END.
        */
    
        IF ped-venda.cod-sit-aval = 1 OR ped-venda.cod-sit-aval = 4 THEN DO:
           ASSIGN ped-venda.cod-sit-ped = 1
                  ped-venda.quem-aprovou = ""
                  ped-venda.dt-apr-cred = ?.
           RUN incluirLogPed('atuCodSitPed','Seta campos quem-aprovou ="" e dt-aprov-cred=?, pois a situaá∆o de avaliaá∆o do pedido est† como:' +
                             {diinc/i03di159.i 4 ped-venda.cod-sit-aval},'INF').
        END.
        ELSE DO:
            RUN incluirLogPed('atuCodSitPed','Situaá∆o n∆o tratada,pois, a situaá∆o de avalidaá∆o o pedido est† como:' +
                             {diinc/i03di159.i 4 ped-venda.cod-sit-aval},'INF').
        END.
        RUN setRowid IN hBoPedsweb(rowid(peds_web)).
        RUN setIndsitPedweb IN hBoPedsWeb(4). 
        RUN setNrPedidoERP IN hBoPedsweb(ped-venda-ext.nr-pedido).
    
        RUN incluirLogPed('integrarPedido','Situaá∆o do pedido web modificada para Integrado e atribuido o n£mero do pedido do ERP:'
                          + string(tt-ped-venda.nr-pedido),'INF').
    
    
         //RUN pi-envia-email-pedido (INPUT "PDF").
        IF tt-param.enviar-e-mail THEN DO:
           RUN pi-envia-email-pedido (INPUT "HTML").
           RUN incluirLogPed('envioEmailPedido','Pedido Enviado por E-mail','INF'). 
        END.
        ELSE DO:
           RUN incluirLogPed('envioEmailPedido','Pedido NAO Enviado por E-mail','INF'). 
        END.
    
        RUN finalizarLogPed.   
    
    END.   // END do FOR EACH peds-web
    
    RUN incrSeqDados IN hBologTxt.
    RUN setDados IN hBologtxt('Severidade','INF').
    RUN setDados IN hBologtxt('bloco','integrarPedido').
    RUN setDados IN hBologtxt('acao','FIM').
    RUN setDados IN hBologtxt('descricao','Envia Email?' + IF tt-param.enviar-e-mail THEN 'SIM' ELSE 'NAO').
    
    
END PROCEDURE.


PROCEDURE pi-calc-natur-oper:
   DEF INPUT PARAMETER p-cod-emitente LIKE emitente.cod-emitente.
   DEF INPUT PARAMETER p-nome-abrev-tri LIKE emitente.nome-abrev.
   DEFINE OUTPUT PARAMETER lOk      AS LOGICAL     NO-UNDO INIT YES.
   
   RUN limparErros IN h-bonat001.
   RUN buscarnatoperacao IN h-bonat001 (INPUT i-fin-nat, 
                                        INPUT tt-ped-venda.cod-estabel,
                                        INPUT p-cod-emitente,
                                        INPUT p-nome-abrev-tri,  
                                        OUTPUT c-natur-oper,
                                        OUTPUT i-param-nat).
   RUN retornarerros IN h-bonat001 (OUTPUT c-erro-nat).
   IF c-erro-nat <> '' THEN DO.
      ASSIGN lOk = NO.
   END.
   ASSIGN tt-ped-venda.nat-operacao = c-natur-oper.

END PROCEDURE.


PROCEDURE pi-cria-repres.
    DEFINE OUTPUT PARAMETER lOk     AS LOGICAL  NO-UNDO INIT YES.

    DEF VAR de-comis-rep AS DEC.
    DEF VAR de-comis-vend AS DEC.
    /*
    IF peds_web.log_perc_negoc = NO THEN
       RUN piCalcComis (OUTPUT de-comis-rep, 
                        OUTPUT de-comis-vend).
    */
    FIND ped-repre WHERE
         ped-repre.nr-pedido   = INT(tt-ped-venda.nr-pedcli) AND
         ped-repre.nome-ab-rep = repres.nome-abrev NO-LOCK NO-ERROR.

    IF NOT AVAIL ped-repre THEN DO.
       RUN esapi/cria-repres2.p (INPUT INT(tt-ped-venda.nr-pedcli),
                                 INPUT repres.nome-abrev,
                                 INPUT de-perc-comis-rep,
							     OUTPUT TABLE tt-erros-local).

	   FIND FIRST tt-erros-local NO-LOCK NO-ERROR.	   
       IF AVAIL tt-erros-local THEN DO.
          FOR EACH tt-erros-local.
              ASSIGN c-mens-erro = 'Erro ao Criar o Representante..' + CHR(10) +
                                   STRING(tt-erros-local.cod-erro) + " " + tt-erros-local.desc-erro
                                    + " " + tt-erros-local.desc-arq.
          END.
          RUN incluirLogPed('pi-cria-repres',c-mens-erro,'ERRO').
       END.
       ELSE DO:
          RUN incluirLogPed ('pi-cria-repres','Comiss∆o foi criada sem erro com o %:' + 
                              STRING(de-perc-comis-rep),'INF' ).
       END.
    END.
    ELSE DO:
       RUN incluirLogPed('pi-cri-repres','Encontrou registro na ped-repre, NAO vai tentar criar a comiss∆o','ALERTA').
    END.

    // Verificar se o Vendedor Ç Diferente do Representante
    IF AVAIL b-repres THEN DO.
       FIND ped-repre WHERE
            ped-repre.nr-pedido = INT(tt-ped-venda.nr-pedcli) AND
            ped-repre.nome-ab-rep = b-repres.nome-abrev NO-LOCK NO-ERROR.

       IF NOT AVAIL ped-repre THEN DO.
          RUN incluirLogPed ( 'pi-cri-repres','N∆o encontrou registro do Vendedor na ped-repre, vai tentar criar a comissío - % portal:' + 
                               STRING(de-perc-comis-ven) + '-pedido web:' + STRING(peds_web.ped_web_id),"INF" ).

          RUN esapi/cria-repres2.p (INPUT INT(tt-ped-venda.nr-pedcli),
                                    INPUT b-repres.nome-abrev,
                                    INPUT de-perc-comis-ven,
                                    OUTPUT TABLE tt-erros-local).

          FIND FIRST tt-erros-local NO-LOCK NO-ERROR.	   
          IF AVAIL tt-erros-local THEN DO.
             FOR EACH tt-erros-local.
                 ASSIGN c-mens-erro = 'Erro ao Criar o Vendedor..' + CHR(10) +
                                      STRING(tt-erros-local.cod-erro) + " " + tt-erros-local.desc-erro
                                       + " " + tt-erros-local.desc-arq.
             END.
             ASSIGN lok = NO.
             RUN incluirLogPed('pi-cria-repres',c-mens-erro,'ERRO').
          END.
          ELSE DO:
             RUN incluirLogPed('pi-cri-repres','Sucesso na criaá∆o da comiss∆o do Vendedor','INF'). 
          END.
       END.
       ELSE DO.
           RUN incluirLogPed('pi-cri-repres','Encontrou registro do Vendedor na ped-repre, NAO vai tentar criar a comiss∆o','ALERTA').
       END.
    END.


    /* Verificar Desconto Informado */ 
    FIND ped-repre WHERE
         ped-repre.nr-pedido = INT(tt-ped-venda.nr-pedcli) AND
         ped-repre.nome-ab-rep = 'Fulano' NO-LOCK NO-ERROR.
    
    IF AVAIL ped-repre THEN DO:
       RUN esapi/elimina-repres.p (INPUT INT(tt-ped-venda.nr-pedcli),
                                   INPUT 'Fulano').
       RUN incluirLogPed('pi-cri-repres','Encontrou registro na ped-repre para FULANO, eliminou a comiss∆o','INF').
    END.
    ELSE DO:
       RUN incluirLogPed('pi-cri-repres','NAO Encontrou registro na ped-repre para FULANO','INF'). 
    END.
       
    IF tt-ped-venda.des-pct-desconto-inform <> "" THEN DO:   /* Cria Fulano */
       FIND ped-repre WHERE
            ped-repre.nr-pedido = INT(tt-ped-venda.nr-pedcli) AND
            ped-repre.nome-ab-rep = 'Fulano' NO-LOCK NO-ERROR.

       RUN esapi/cria-repres.p (INPUT INT(tt-ped-venda.nr-pedcli),
                                INPUT 'Fulano',
                                INPUT DEC(tt-ped-venda.des-pct-desconto-inform) ).

       IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
          ASSIGN lok = NO.
          RUN incluirLogPed('pi-cri-repres','Erro na criaá∆o da comiss∆o do Fulano','ERRO'). 
       END.
       ELSE DO:
          RUN incluirLogPed('pi-cri-repres','Sucesso na criaá∆o da comiss∆o do Fulano','INF'). 
       END.
         
    END.
END PROCEDURE.

PROCEDURE pi-perc-pagto :
    DEF INPUT PARAMETER p-qtd-seq AS INTEGER.

    ASSIGN i-ct = 1.
    FOR EACH tt-cond-ped. 
        ASSIGN tt-cond-ped.perc-pagto = 100 / p-qtd-seq
               i-ct = i-ct + 1.
    END.

    ASSIGN i-ct = i-ct - 1.
    FIND LAST tt-cond-ped NO-ERROR.
    IF AVAIL tt-cond-ped THEN
       IF i-ct * tt-cond-ped.perc-pagto <> 100 THEN
          ASSIGN tt-cond-ped.perc-pagto = tt-cond-ped.perc-pagto + 
                                           (100 - (i-ct * tt-cond-ped.perc-pagto)).
END PROCEDURE.


PROCEDURE pi-envia-email-erro.
   DEF INPUT PARAMETER c-mens-erro AS CHAR.
   

   // e-mail s¢ de 08:00h as 18:00h E DIAS DE SEMANA
   IF lEnviarEmail = NO THEN DO:
      RUN criarFilaEmail(c-mens-erro).
      RUN incluirLogPed('pi-envia-email-erro',
                        'E-mail colocado na fila para envio, pois, n∆o est† dentro dos hor†rios e dias permitidos para envio.'
                        ,'INFO').
      RETURN 'ADM-ERROR'.
   END.
   ELSE DO:
       RUN incluirLogPed('pi-envia-email-erro','Dentro do hor†rios e dias permitidos para envio de e-mail.','INFO').
   END.
   

   FIND im-param WHERE
        im-param.cod-param = 'DEST_ERROS_PEDIDOS_WEB' NO-LOCK NO-ERROR.

   IF c-base = 'base-pro' AND AVAIL im-param THEN DO:
      ASSIGN c-destinatario = im-param.val-param. 

      FIND repres WHERE 
           repres.cod-rep = peds_web.repres_id NO-LOCK NO-ERROR.
      IF repres.e-mail <> '' THEN
         ASSIGN c-destinatario = c-destinatario + "," + repres.e-mail.

      RUN incluirLogPed('pi-envia-email-erro',
                        'Base de produá∆o e parametro encontrado -> Destinatarios para envio buscados do parametro "DEST_ERROS_PEDIDOS_WEB":' + 
                        c-destinatario,'INFO').
   END.
   ELSE DO:
      ASSIGN c-destinatario = "imatextil@imatextil.com.br,tadeu.parreiras@gmail.com". 
      RUN incluirLogPed('pi-envia-email-erro','Base de TESTE ou NAO parametro encontrado -> Destinatarios fixos:' + 
                        c-destinatario,'INFO').
   END.
      

   ASSIGN c-mensagem = "Prezados," + CHR(13) + 
                       "O Pedido " + STRING(peds_web.ped_web_id) + " implantado no portal FOI REJEITADO em " + STRING(TODAY,"99/99/9999") + "." + CHR(13) +
                       "Motivo: " + c-mens-erro + CHR(13) + CHR(13) +
                       "Atenciosamente, " + CHR(13) + CHR(13)  +
                       "Medtextil Importaá∆o e Exportaá∆o Ltda".


   RUN esapi/esapi002.p (INPUT c-remetente,                         /* e-mail remetente */
                        INPUT c-destinatario,                      /* e-mail destinat†rio */
                        INPUT "Rejeiá∆o de PEDIDO - " + STRING(TODAY,"99/99/9999") + " " + STRING(TIME,"HH:MM:SS"), /* Assunto */
                        INPUT c-mensagem,                          /* Mensagem */
                        INPUT "",                                  /* Anexo */
                        INPUT NOT tt-param.l-batch).                                /* Mostra Erros */ 
   RUN incluirLogPed('pi-envia-email-erro','Enviado o e-mail pela api esapi/esapi002 com a mensagem:'
                         + c-mensagem,'INF').
                                
   IF tt-param.l-batch = NO THEN DO:
     RUN incluirLogPed('pi-envia-email-erro','Processamento Nao Ç batch, exibida em tela a mensagem:'
                         + c-mensagem,'INF').
     MESSAGE c-mensagem
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

   END.
END PROCEDURE.


PROCEDURE pi-envia-email-pedido.
    DEF INPUT PARAMETER p-tipo AS CHAR.

    FIND emitente WHERE
         emitente.nome-abrev = ped-venda.nome-abrev NO-LOCK NO-ERROR.
    
    ASSIGN c-destinatario = "".
    IF c-base = 'base-pro' THEN DO.
       FOR EACH ped-repre OF ped-venda NO-LOCK.
           FIND repres WHERE
                repres.nome-abrev = ped-repre.nome-ab-rep NO-LOCK.
           IF repres.e-mail <> '' THEN
              ASSIGN c-destinatario = c-destinatario + "," + repres.e-mail.
       END.

       FIND cont-emit OF emitente WHERE
            cont-emit.area = 'COMERCIAL' NO-LOCK NO-ERROR.
       IF AVAIL cont-emit AND
          cont-emit.e-mail <> '' THEN
          ASSIGN c-destinatario = c-destinatario + "," + cont-emit.e-mail.
    
       IF AVAIL peds_web THEN DO.
          IF peds_web.emails_adicionais <> '' THEN
             ASSIGN c-destinatario = c-destinatario + "," + peds_web.emails_adicionais.
    
          IF peds_web.login_preposto <> '' AND user-web.email <> '' THEN
             ASSIGN c-destinatario = c-destinatario + "," + user-web.email.
       END.
    END.
    ELSE
       ASSIGN c-destinatario = "ti@imatextil.com.br". 

    FIND repres WHERE
         repres.nome-abrev = ped-venda.no-ab-reppri NO-LOCK.

    ASSIGN c-mensagem = "Prezado Cliente," + CHR(13) + 
                        "Foi efetuado o Pedido de Venda " + ped-venda.nr-pedcli + " que segue em Anexo, pelo Representante " + repres.nome + CHR(13) +
                        "Atenciosamente, " + CHR(13) + CHR(13)  +
                        "Medtextil Importaá∆o e Exportaá∆o Ltda".


    IF p-tipo = 'PDF' THEN DO.
       ASSIGN c-arq-gerado-pdf = SESSION:TEMP-DIRECTORY + STRING(ped-venda.nr-pedcli) + ".pdf".
    
       RUN esapi/imp-ped-venda-pdf.p (INPUT ped-venda.nr-pedido,
                                      INPUT 2).

       IF SEARCH(c-arq-gerado-pdf) = ? THEN DO.
          ASSIGN c-mens-erro = "N∆o foi possivel Enviar o e-mail do Pedido (" +
                               ped-venda.nr-pedcli + ")... PDF n∆o Encontrado... ".
    
          IF tt-param.l-batch = NO THEN 
             MESSAGE c-mens-erro
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
    
          IF tt-param.enviar-e-mail THEN
             RUN pi-envia-email-erro (INPUT c-mens-erro).
    
          RETURN 'ADM-ERROR'.
       END.

       RUN esapi/esapi002.p (INPUT c-remetente,                         /* e-mail remetente */
                             INPUT c-destinatario,                      /* e-mail destinat†rio */
                             INPUT "Pedido de Venda - IMATEXIL",        /* Assunto */
                             INPUT c-mensagem,                          /* Mensagem */
                             INPUT c-arq-gerado-pdf,                    /* Anexo */
                             INPUT NOT tt-param.l-batch).               /* Mostra Erros */ 
    END.
    ELSE IF p-tipo = 'HTML' THEN DO.
       RUN esapi/imp-ped-venda-html.p (INPUT ped-venda.nr-pedido,
                                       OUTPUT TABLE tt-mensagem).
                                       
       RUN esapi/esapi003.p (INPUT c-remetente,                         /* e-mail remetente */
                             INPUT c-destinatario,                      /* e-mail destinat†rio */
                             INPUT "Pedido de Venda - IMATEXIL",        /* Assunto */
                             INPUT TABLE tt-mensagem,                   /* Mensagem */
                             INPUT '',                                  /* Anexo */
                             INPUT NOT tt-param.l-batch).               /* Mostra Erros */ 
    END.
END PROCEDURE.

PROCEDURE pi-act-sld-container.
    RUN incrSeqDados IN hBologTxt.
    RUN setDados IN hBoLogTxt('data_hora',NOW).
    RUN setDados IN hBoLogTxt('bloco','pi-ac-sld-container').
    RUN setDados IN hBoLogTxt('nr_pedido','N/A').
    RUN setDados IN hBologtxt('acao','acertar saldo container').
    RUN setDados IN hBologtxt('descricao','INICIO').
    RUN gerarLogs IN hBoLogtxt.
    FOR EACH pp-container WHERE
             pp-container.situacao = 1 NO-LOCK.
    
        FOR EACH pp-it-container OF pp-container SHARE-LOCK.
            ASSIGN de-qt-vend = 0.
            RUN pi-ver-ped.
    
            IF de-qt-vend = pp-it-container.qt-vendida THEN NEXT.
    
            ASSIGN pp-it-container.qt-vendida = de-qt-vend.
        END.
    END.
    RUN incrSeqDados IN hBologTxt.
    RUN setDados IN hBoLogTxt('data_hora',NOW).
    RUN setDados IN hBoLogTxt('bloco','pi-ac-sld-container').
    RUN setDados IN hBoLogTxt('nr_pedido','N/A').
    RUN setDados IN hBologtxt('acao','acertar saldo container').
    RUN setDados IN hBologtxt('descricao','FIM').
    RUN gerarLogs IN hBoLogtxt.
END PROCEDURE.

PROCEDURE pi-ver-ped.
    FOR EACH ped-item WHERE
             ped-item.it-codigo = pp-it-container.it-codigo AND
             ped-item.cod-refer = pp-it-container.cod-refer NO-LOCK.

        IF ped-item.cod-sit-item = 6 THEN NEXT.

        FIND ped-venda OF ped-item NO-LOCK NO-ERROR.
        IF NOT AVAIL ped-venda THEN NEXT.
        IF ped-venda.cod-sit-ped = 6 THEN NEXT.
             
        FIND ped-venda-ext WHERE
             ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND
             ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.
        IF NOT AVAIL ped-venda-ext THEN NEXT.

        IF ped-venda-ext.nr-container <> pp-container.nr-container THEN NEXT.

        ASSIGN de-qt-vend = de-qt-vend + ped-item.qt-pedida.
    END.
END PROCEDURE.

PROCEDURE vencerPedidos:
    RUN setBoLogTxt     IN hBoPedsWeb(hBoLogTxt).
    RUN vencerPedsWeb   IN HBoPedsWeb.
    RUN getBoLogTxt     IN hBopedsWeb(OUTPUT hBoLogTxt).
    RUN setCodtipo      IN hBohistAvalPedVenda(6). // aprov. gerencial
    RUN vencerPedsWebSemAprovGer IN hBoHistAvalPedVenda.

    
END PROCEDURE.

PROCEDURE finalizarLogPed:

    RUN incrSeqDados IN hBologTxt. 
    RUN setDados IN hBologtxt('data_hora',NOW).
    RUN setDados IN hBologtxt('acao','FIM').
    RUN setDados IN hBologtxt('descricao','' ).
    RUN gerarLogs IN hBoLogtxt.

END PROCEDURE.

PROCEDURE incluirLogPed:
    DEFINE INPUT  PARAMETER pAcao       AS CHARACTER FORMAT 'x(50)'  NO-UNDO.
    DEFINE INPUT  PARAMETER pDescricao  AS CHARACTER  FORMAT 'x(500)' NO-UNDO.
    DEFINE INPUT  PARAMETER pSeveridade AS CHARACTER   NO-UNDO.
    
    RUN incrSeqDados IN hBologTxt. 
    RUN setDados IN hBologtxt('acao',pAcao).
    RUN setDados IN hBologtxt('data_hora',NOW).
    RUN setDados IN hBologtxt('descricao',pDescricao  ).
    RUN setDados IN hbologtxt('severidade',pSeveridade).
    RUN setDados IN hbologtxt('nr_pedido',IF AVAIL peds_web THEN string(peds_web.ped_web_id) else '').
    
    RUN gerarLogs IN hBoLogtxt.

END PROCEDURE.


PROCEDURE incluirLogGeral:
    
    DEFINE INPUT  PARAMETER pBloco      AS CHARACTER FORMAT 'x(50)'  NO-UNDO.
    DEFINE INPUT  PARAMETER pAcao       AS CHARACTER FORMAT 'x(50)'  NO-UNDO.
    DEFINE INPUT  PARAMETER pDescricao  AS CHARACTER  FORMAT 'x(500)' NO-UNDO.
    DEFINE INPUT  PARAMETER pSeveridade AS CHARACTER   NO-UNDO.
    
    RUN incrSeqDados IN hBologTxt. 
    RUN setDados IN hBologtxt('bloco',pBloco).
    RUN setDados IN hBologtxt('acao',pAcao).
    RUN setDados IN hBologtxt('data_hora',NOW).
    RUN setDados IN hBologtxt('descricao',pDescricao  ).
    RUN setDados IN hbologtxt('severidade',pSeveridade).
    RUN setDados IN hBoLogTxt('nr_pedido', 'N/A').
    RUN gerarLogs IN hBoLogtxt.

END PROCEDURE.

PROCEDURE enviarErro:

    DEFINE INPUT  PARAMETER cErro       AS CHARACTER  FORMAT 'x(1000)' NO-UNDO.
    DEFINE INPUT  PARAMETER lMostraMsg  AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER lEnviaEmail AS LOGICAL     NO-UNDO.

     IF tt-param.l-batch = NO THEN 
         MESSAGE cErro
             VIEW-AS ALERT-BOX INFO BUTTONS OK.

      IF tt-param.enviar-e-mail THEN
         RUN pi-envia-email-erro (INPUT cErro).


END PROCEDURE.

PROCEDURE rejeitarPedido:

    DEFINE INPUT  PARAMETER cMsg AS CHARACTER   NO-UNDO FORMAT 'x(500)'.
    DEFINE INPUT  PARAMETER cBlocoLog AS CHARACTER   NO-UNDO FORMAT 'x(50)'.
    RUN setRowid IN hBoPedsWeb(ROWID(peds_web)).
    RUN setIndsitPedweb IN hBoPedsWeb(5). 
    RUN setDescrRejeicao IN hBoPedsweb(cMsg).

    RUN incluirLogPed(cBlocoLog,c-mens-erro ,'ERRO'). 
    RUN incluirLogPed(cBlocoLog,'Pedido Setado como rejeitado' ,'ERRO'). 
    RUN enviarErro(cMsg,tt-param.l-batch,tt-param.enviar-e-mail).
    
    RUN finalizarLogPed.

END PROCEDURE.

PROCEDURE excluirPedido:

    DEFINE INPUT  PARAMETER cAcaoLog AS CHARACTER FORMAT 'x(50)'   NO-UNDO.
    DEFINE INPUT  PARAMETER cMesErro AS CHARACTER FORMAT 'X(300)'  NO-UNDO.
    FOR EACH ped-item OF ped-venda.
        RUN esapi/elimina-peditem.p (INPUT ped-venda.nr-pedcli,
                                     INPUT ped-item.nr-sequencia).
    END.
    RUN incluirLogPed('converterCondPagto',
                      'Chamou esapi/elimina-peditem.p para excluir itens Pedido de Venda ',
                      'INF').

    FIND FIRST ped-item OF ped-venda NO-LOCK NO-ERROR.
    IF NOT AVAIL ped-item THEN DO:
       DELETE ped-venda.
       RUN incluirLogPed(cAcaoLog,'Todos os itens do pedido de venda foram exclu°dos e o pedido de venda tambÇm','INF').
    END.
    ELSE DO:
       RUN incluirLogPed(cAcaoLog,'Algum(ns) os itens do pedido de venda NAO foram exclu°dos e o pedido de venda NAO pode ser excluido','ERRO').
    END.
    RUN rejeitarPedido(cMesErro, cAcaoLog).
END PROCEDURE.

PROCEDURE criarFilaEmail:
    DEFINE INPUT  PARAMETER cMensErro AS CHARACTER   NO-UNDO FORMAT 'x(300)'.
    DEFINE VARIABLE cAgora AS CHARACTER   NO-UNDO.
    RUN esapi/getcAgora.p(OUTPUT cAgora).
    OUTPUT TO VALUE(cPastaLog + "\fila_email\" + cAgora + '.txt' ).
    PUT cMensErro SKIP.
    OUTPUT CLOSE.
END PROCEDURE.



PROCEDURE limparIntegracoesErradas:
    RUN incrSeqDados IN hBologTxt.
    RUN setDados IN hBologtxt('bloco','limparIntegracoesErradas').
    RUN setDados IN hBologtxt('acao','INICIO').
    RUN setDados IN hBologtxt('descricao','').
    RUN gerarLogs IN hBoLogtxt.
    FOR EACH peds_web NO-LOCK
        WHERE peds_web.ind_sit_ped_web = 4
        AND DATE(peds_web.dt_hr_registro) >= TODAY - 2 .
        FIND ped-venda NO-LOCK
            WHERE ped-venda.nr-pedido = peds_web.nr_pedido_erp
            NO-ERROR.
        IF NOT AVAIL ped-venda THEN DO:
           RUN setRowid IN hBoPedsWeb(ROWID(peds_web)).
           RUN setIndsitPedweb IN hBoPedsWeb(2).
           RUN setNrPedidoERP IN hBoPedsWeb(0).
    
        END.
         
    
    END.
    RUN setDados IN hBologtxt('bloco','limparIntegracoesErradas').
    RUN setDados IN hBologtxt('acao','FIM').
    RUN setDados IN hBologtxt('descricao','').
    RUN gerarLogs IN hBoLogtxt.
END PROCEDURE.


PROCEDURE enviarFilaEmails:

    /* alterar procedure pi-envia-email-erro para n∆o precisar dos dados do representante e do pedido web quando for chamado a partir desta procedure
    DEFINE VARIABLE cDir AS CHARACTER NO-UNDO . 
    DEFINE VARIABLE cFileStream AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMensErro   AS CHARACTER   NO-UNDO.
    
    ASSIGN cDir = cPastaLog + "\fila_email\".
    RUN incluirLogGeral('EnvioEmailFila','getDiretorio', 'Diretorio de leitura dos arquivos:' + cDir,'INFO').
    EMPTY TEMP-TABLE ttFila.                                                                                
    INPUT FROM OS-DIR (cDir).
    REPEAT:
        IMPORT cFileStream.
        FILE-INFO:FILE-NAME = cDir + cFileStream.
        IF substr(FILE-INFO:FILE-TYPE,1,1) = 'F' THEN DO: //apenas arquivos
           CREATE ttFila.
           ASSIGN ttFila.arquivo = FILE-INFO:FULL-PATHNAME.
           RUN incluirLogGeral('EnvioEmailFila','lerArquivosDir', 'Arquivo encontrado:' + 
                               ttFila.arquivo,'INFO'). 
        END.

        

        /*DISPLAY cFileStream FORMAT "X(18)" LABEL 'name of the file'
                FILE-INFO:FULL-PATHNAME FORMAT "X(21)" LABEL 'FULL-PATHNAME'
                FILE-INFO:PATHNAME FORMAT "X(21)" LABEL 'PATHNAME'
                FILE-INFO:FILE-TYPE FORMAT "X(5)" LABEL 'FILE-TYPE'.*/
    END.

    FOR EACH ttFila:
        INPUT FROM VALUE(ttFila.arquivo).
            IMPORT UNFORMAT cMensErro.
        INPUT CLOSE.
        IF lEnviarEmail THEN DO:
            RUN pi-envia-email-erro(cMensErro).
            OS-DELETE ttFila.arquivo .
            RUN incluirLogGeral('EnvioEmailFila','enviaEmail', 'Enviada a mensagem:' + 
                               cMensErro + ' e apagado o arquivo:' +  ttFila.arquivo,'INFO'). 
        END.
        ELSE DO:
            RUN incluirLogGeral('EnvioEmailFila','enviaEmail', 'Fora dos hor†rios e dias permitidos - NAO Enviada a mensagem:' + 
                               cMensErro + ' e mantido o arquivo:' + ttFila.arquivo,'INFO'). 
        END.
    END.*/


END PROCEDURE.


PROCEDURE getAvalEnviarEmail:
    // S‡ HABILITA ENVIO DE 08:00 AS 18:00 NOS DIAS DE SEMANA
    IF TIME < 28800 OR TIME > 64800 OR 
       WEEKDAY(TODAY) = 7 OR WEEKDAY(TODAY) = 1 THEN DO:
       ASSIGN lEnviarEmail = NO.
       RUN incluirLogGeral('avalEnvioEmailErros','avaliar','E-mails de erro n∆o ser∆o enviados devido a estarem fora do hor†rio comercial,' + 
                           'ser† criada uma pendencia de envio e caso exista algum erro o mesmo ser† enviado ' + 
                           'no primeiro momento do pr¢ximo hor†rio comercial(seg a sexta de 08:00 as 18:00)' ,'INF').
    END.
    ELSE DO:
       RUN incluirLogGeral('avalEnvioEmailErros','avaliar','E-mails de erro ser∆o enviados normalmente devido a estarem dentro do hor†rio comercial.' ,'INF').
       ASSIGN lEnviarEmail = YES.
    END.
END PROCEDURE.

PROCEDURE pi-busca-preco :
    DEF INPUT  PARAMETER p-it-codigo AS CHAR.
    DEF INPUT  PARAMETER p-cod-refer AS CHAR.
    DEF INPUT  PARAMETER p-campanha  AS CHAR.
    DEF OUTPUT PARAMETER p-vlReal    AS DECIMAL NO-UNDO. 
    DEF OUTPUT PARAMETER p-vlDolar   AS DECIMAL NO-UNDO. 
    DEF OUTPUT PARAMETER p-ControlePreco AS CHAR.

    DEF VAR l-divide-comis AS LOGICAL.     
    DEF VAR de-perc-comis-vend AS DECIMAL.
    DEF VAR de-perc-comis-rep AS DECIMAL.
    
    DEF VAR i-tp-busca  AS INT.

    ASSIGN i-tp-busca = 1.  // PE
    IF p-campanha = '' AND  // N∆o Exste campnha para PI
       ped-venda.tp-pedido = 'PI' AND
       ped-venda-ext.nr-container <> 0 THEN 
       ASSIGN i-tp-busca = 2.  // PI
    
    RUN setTbPreco      IN h-bo-preco-item (INPUT ped-venda-ext.tb_preco_id). 
    RUN setItem         IN h-bo-preco-item (INPUT p-it-codigo). 
    RUN setRef          IN h-bo-preco-item (INPUT p-cod-refer). 
    RUN setNrContainer  IN h-bo-preco-item (INPUT ped-venda-ext.nr-container).
    RUN setTipoBusca    IN h-bo-preco-item (INPUT i-tp-busca). 
    RUN setPrazoMedio   IN h-bo-preco-item (INPUT i-prazo-medio).
        
    RUN buscarPrecos    IN h-bo-preco-item.

    IF p-campanha <> '' THEN
       RUN getPrecoPrazo   IN h-bo-preco-item (INPUT p-campanha,
                                               OUTPUT p-vlReal,
                                               OUTPUT p-vlDolar,
                                               OUTPUT p-ControlePreco).
    ELSE
       RUN getPrecoPrazo IN h-bo-preco-item (INPUT ped-venda-ext.tp-pedido,
                                             OUTPUT p-vlReal,
                                             OUTPUT p-vlDolar,
                                             OUTPUT p-ControlePreco).
END PROCEDURE.

PROCEDURE pi-prazo-medio.
    DEF VAR de-tot-prazo LIKE cond-ped.nr-dias-venc.

    FIND cond-pagto WHERE
         cond-pagto.cod-cond-pag = tt-ped-venda.cod-cond-pag NO-LOCK NO-ERROR.
    IF AVAIL cond-pagto THEN 
       ASSIGN i-prazo-medio = cond-pagto.qtd-dias-prazo-medio.
    ELSE DO.
       ASSIGN de-tot-prazo = 0
              i-ct = 0.
       FOR EACH cond-ped WHERE
                cond-ped.nr-pedido = tt-ped-venda.nr-pedido NO-LOCK.
           IF cond-ped.data-pagto <> ? THEN
              ASSIGN de-tot-prazo = de-tot-prazo + (cond-ped.data-pagto - ped-venda.dt-implant).
           ELSE
              ASSIGN de-tot-prazo = de-tot-prazo + cond-ped.nr-dias-venc.

           ASSIGN i-ct = i-ct + 1.
       END.
       ASSIGN i-prazo-medio = de-tot-prazo / i-ct.
    END.
END.


PROCEDURE piCalcComis :
    DEFINE OUTPUT PARAMETER de-percComisRep     AS DECIMAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER de-percComisVend    AS DECIMAL     NO-UNDO.

    DEF VAR hBoPedVenda AS HANDLE NO-UNDO.
    DEF VAR hBoRepres   AS HANDLE NO-UNDO.

    DEFINE VARIABLE iCliente              AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iClienteTriang        AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iCodRep               AS INTEGER     NO-UNDO.

    IF NOT VALID-HANDLE(hBoPedVenda) THEN DO:
       RUN esbo/boPedVenda.p PERSISTENT SET hBoPedVenda.
       RUN iniciarBos IN hBoPedVenda.
    END.

    IF NOT VALID-HANDLE(hBoRepres) THEN DO:
       RUN esbo/boRepres.p PERSISTENT SET hboRepres.
       RUN iniciarBos IN hBoRepres.
    END.

    RUN limparTtsComis IN hBoPedVenda.
    RUN setVarsAgrupComis IN hBoPedVenda.

    RUN setProp IN hBoPedVenda (INPUT 'tb_preco_id',
                                INPUT ped-venda-ext.tb_preco_id).
    RUN setProp IN hBoPedVenda (INPUT 'cod_moeda',
                                INPUT ped-venda.mo-codigo).

    IF ped-venda-ext.nome-ab-vend <> '' THEN
       RUN setNomeAbrev IN hboRepres (INPUT ped-venda-ext.nome-ab-vend).

    RUN setCodRep IN hboRepres (INPUT repres.cod-rep).
    RUN getCodRep IN hBoRepres (OUTPUT iCodRep).
    
    RUN setProp IN hBoPedVenda (INPUT 'cod_rep', INPUT iCodRep).

    IF ped-venda.cod-cond-pag <> 0 THEN
       RUN setProp IN hBoPedVenda (INPUT 'cod_cond_pagto',
                                   INPUT ped-venda.cod-cond-pag).
    ELSE 
       RUN setTTCondPed IN hBoPedVenda (INPUT TABLE tt-cond-ped).
    
    RUN setProp IN hBoPedVenda(INPUT 'nr_pedido',
                               INPUT ped-venda.nr-pedido).

    RUN setNomeAbrev       IN hboEmitente (INPUT ped-venda.nome-abrev).
    RUN getCodEmitente     IN hBoEmitente (OUTPUT iCliente).
    RUN setNomeAbrev       IN hboEmitente (INPUT ped-venda.nome-abrev-tri).
    RUN getCodEmitente     IN hBoEmitente (OUTPUT iClienteTriang).
    
    RUN setProp            IN hBoPedVenda ('cod_emitente',iCliente).
    RUN setProp            IN hBoPedVenda ('cod_emitente_triang',iCliente).
    
    RUN setTtItensEspd4000 IN hBoPedVenda (INPUT TABLE tt-itens-ped).

    RUN calcPercComis      IN hBoPedVenda (OUTPUT de-percComisRep).

    RUN getPercComis2      IN hBoPedVenda (OUTPUT de-percComisVend).
    
    /*RUN finalizarBos IN h-bo-preco-item.
    
    IF VALID-HANDLE(h-bo-preco-item) THEN
       DELETE PROCEDURE h-bo-preco-item.*/
    
END PROCEDURE.

