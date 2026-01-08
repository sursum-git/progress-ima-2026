//LOG-MANAGER:LOGGING-LEVEL = 5.
//LOG-MANAGER:LOGFILE-NAME = 'C:\temp\rolos.txt'.

DEF TEMP-TABLE infPedido SERIALIZE-NAME ""
    FIELD id                AS INTEGER SERIALIZE-HIDDEN
    FIELD codigoCliente     AS CHAR 
    FIELD pedidoCliente     AS CHAR
    FIELD destinatario      AS CHAR
    FIELD cnpjTransportador AS CHAR
    FIELD observacoes       AS CHAR.

DEF TEMP-TABLE infItens SERIALIZE-NAME "itens" 
    FIELD id            AS INTEGER  SERIALIZE-HIDDEN
    FIELD produto       AS CHAR 
    FIELD quantidade    AS DECIMAL
    FIELD lote          AS CHAR
    FIELD variante      AS CHAR.

{esapi/analisarJsonObject2.i}
{esp/params.i}
{lisa/codProdUnif.i}

DEF VAR h-handle                AS HANDLE.
DEF VAR i-id                    AS INT.
DEF VAR c-chave                 AS CHAR.
DEFINE VARIABLE cAVista         AS CHARACTER   NO-UNDO.
DEF INPUT PARAMETER p-row-di159 AS ROWID.

FIND ped-venda WHERE
     ROWID(ped-venda) = p-row-di159 NO-LOCK NO-ERROR.
     
FIND transporte WHERE 
     transporte.nome-abrev = ped-venda.nome-transp NO-LOCK NO-ERROR.

ASSIGN i-id = RANDOM(1,99999).

IF ped-venda.cod-cond-pag = 1 THEN
   ASSIGN cAVista = " - · Vista".
ELSE 
   ASSIGN cAVista = "".

CREATE infPedido.
ASSIGN infPedido.id = i-id
       infPedido.codigoCliente = '86159501'
       infPedido.pedidoCliente = ped-venda.nr-pedcli
       infPedido.destinatario = ped-venda.nome-abrev + cAVista
       infPedido.cnpjTransportador = transporte.cgc
       infPedido.observacoes = ped-venda.observ.

FOR EACH ped-item OF ped-venda WHERE 
         ped-item.cod-sit-item = 1 NO-LOCK
    BY ped-item.nr-sequencia.

    FIND item WHERE
         item.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.

    CREATE infItens.
    ASSIGN infItens.id          = i-id
           //infItens.produto     = ped-item.it-codigo
           infItens.quantidade  = ped-item.qt-pedida
           //infItens.lote        = ped-item.cod-refer
           infItens.variante    = ''.
           
           
    IF lCodigoProdUnificado THEN DO:
       ASSIGN  infItens.produto = ped-item.it-codigo + "-" +  upper(ped-item.cod-refer)
               infItens.lote    = ''.        
    END.
    ELSE DO:
       ASSIGN infItens.produto     = ped-item.it-codigo
              infItens.lote        = ped-item.cod-refer
              .        
        
    END.           
END.

DEF DATASET dsNFe SERIALIZE-NAME '' FOR infPedido, infItens DATA-RELATION relx FOR infPedido, infItens RELATION-FIELDS (id,id) NESTED.
RUN pi-chama-api.
IF RETURN-VALUE = 'ADM-ERROR' THEN
   RETURN 'ADM-ERROR'.

ASSIGN c-chave = ped-venda.nr-pedcli + "|" + ped-venda.nome-abrev.

FOR EACH ttJson.
    IF ttJson.tag = "prePedido" THEN DO.
       
       FIND lisa-integra WHERE
            lisa-integra.cod-trans = "ISF" AND
            lisa-integra.chave = c-chave  SHARE-LOCK NO-ERROR.
       IF AVAIL lisa-integra THEN DO.
          ASSIGN lisa-integra.val-livre-1 = ttJson.valor.

          FIND ped-venda-ext WHERE
               ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND 
               ped-venda-ext.nr-pedido = ped-venda.nr-pedido
               SHARE-LOCK NO-ERROR.
          ASSIGN ped-venda-ext.dt-isf = TODAY
                 ped-venda-ext.nr-pedext = lisa-integra.val-livre-1.
       END.
    END.
END.


RETURN 'ADM-OK'.

//----------- Procedures
PROCEDURE pi-chama-api.
    DEF VAR iSitRet AS INTEGER.
    DEF VAR cErro AS CHARACTER   NO-UNDO.
    DEF VAR h-dataset AS HANDLE.

    ASSIGN h-dataset = DATASET dsNFe:HANDLE.

    /*RUN esbo/boAPIsLisa.p PERSIST SET h-handle.

    RUN iniciar IN h-handle.
    RUN retirarNoRoot IN h-handle(YES).
    RUN enviarPedidoVenda IN h-handle (INPUT h-dataset,
                                       INPUT 'dataset',
                                       INPUT infPedido.pedidoCliente).

    RUN getStatusRetorno IN h-handle (OUTPUT iSitRet). 
    RUN getTTRetorno IN h-handle (OUTPUT TABLE ttJson).
    RUN getErro IN h-handle(OUTPUT cErro).
    RUN finalizar IN h-handle.*/
    RUN lisa/enviarPedVenda.p(INPUT h-dataset,
                              INPUT infPedido.pedidoCliente,
                              OUTPUT cErro,
                              OUTPUT TABLE ttJson).

    IF cErro <> '' THEN DO:
       MESSAGE 'ERRO ao Enviar o ISF para Armazem Geral' SKIP 
               'Comunique … TI os erros a seguir...' SKIP(2)
               cErro
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

       RETURN 'ADM-ERROR'.
    END.
END PROCEDURE.

