{esapi/analisarJsonObject2.i}
{lisa/extrairTtJsonPrePedido.i}
{esp/util.i}
DEFINE INPUT  PARAMETER TABLE FOR ttJson.
DEFINE OUTPUT PARAMETER TABLE FOR ttPedido.
DEFINE OUTPUT PARAMETER TABLE FOR ttPedItem.
DEFINE OUTPUT PARAMETER TABLE FOR ttPedItemFat.
DEFINE OUTPUT PARAMETER TABLE FOR ttPedItemEtq.

DEFINE VARIABLE cRet AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTagItens AS CHARACTER   NO-UNDO .

DEFINE TEMP-TABLE  ttPai LIKE ttJson.


CREATE ttPedido.

RUN lisa/getDescrSitPedLisa.p(getChaveTTJson('items','status'), 
                              OUTPUT ttPedido.DescrSituacao).

ASSIGN ttPedido.pedidoCliente = INT(getChaveTTJson('items','pedidoCliente')).
                              

ASSIGN ttPedido.obs = getChaveTTJson('items','observacoes').

ASSIGN ttPedido.obsSeparacao = getChaveTTJson('items','observacoesSeparacao').

ASSIGN cRet = getChaveTTJson('items','enviadoNfe')
       logNfeEnviada = IF cRet = 'S' THEN YES ELSE NO.


ASSIGN cRet = getChaveTTJson('items','enviadoApi')
       logEnviadoApi = IF cRet = 'S' THEN YES ELSE NO.

ASSIGN ttPedido.pedidoLisa  = int(trim(getChaveTTJson('ASTATUS','CPEDLISA'))).


ASSIGN ttPedido.codRomaneio = getChaveTTJson('astatus','cRomaneio').

ASSIGN cRet = getChaveTTJson('astatus','dataRomaneio')
       ttPedido.dtRomaneio = convDt4A2M2D(cRet) .


ASSIGN cRet = getChaveTTJson('astatus','dataexpedido')
       ttPedido.dtExpedido = convDt4A2M2D(cRet) .

ASSIGN cRet = getChaveTTJson('items','datainclusao')
       ttPedido.dtInclusao = convDt4A2M2D(cRet) .


ASSIGN ttPedido.horaInclusao   = getChaveTTJson('items','horainclusao').
ASSIGN ttPedido.prePedido      = int(getChaveTTJson('items','prePedido')).

ASSIGN ttPedido.horaExpedido   = getChaveTTJson('astatus','horaexpedido').

ASSIGN ttPedido.nfCliente      = getChaveTTJson('astatus','notacliente').

ASSIGN ttPedido.situacaoPed    = getChaveTTJson('astatus','CSTATUS').

ASSIGN cRet = getChaveTTJson('astatus','npesobru')
       ttPedido.pesoBruto = DECIMAL(cRet).

ASSIGN cRet = getChaveTTJson('astatus','npesoliq')
      ttPedido.pesoLiquido = DECIMAL(cRet).

ASSIGN cRet = getChaveTTJson('astatus','nqtdcaixa')
      ttPedido.qtCaixa = INT(cRet).



RUN criarTTPedItem.

RUN criarTTPedItemFat.

RUN criarPedItemEtq.

PROCEDURE criarTTPedItemFat:
    ASSIGN cTagItens = 'ITENS'.
    EMPTY  TEMP-TABLE ttPai.
    FOR EACH ttjson
        WHERE ttjson.tag_Pai = cTagItens
        AND ASC(SUBSTR(ttjson.tag_pai,1,1)) = 73 // I Maiusculo
        AND   ttjson.tag = ''
        AND   ttJson.valor = 'json'.
        CREATE ttPai.
        BUFFER-COPY ttJson TO ttPai.
    END.

    /*
    OUTPUT TO c:\temp\itens_fat_pai.txt.
    FOR EACH ttPai.
        EXPORT DELIMITER "|" ttPai.
    END.

    OUTPUT CLOSE.
    */

    //RUN setAgrupTtJson('ITENS','IDNOTAORIGEM').
    FOR EACH ttPai:

        CREATE ttPedITemFat.
        ASSIGN ttPeDItemFat.agrup  = ttPai.id .

        FOR EACH ttJson
            WHERE ttJson.agrupJson = ttPai.id.
            FIND FIRST ttPedItemFat
            WHERE ttPedItemFat.agrup = ttPai.id NO-ERROR.
            CASE ttJson.tag:                                                                                                                                 
                WHEN 'IDNOTAORIGEM' THEN DO:                                                                                                                 
                     ASSIGN ttPedItemFat.idnfOrigem = ttJson.valor .                                                                                         
                END.                                                                                                                                         
                                                                                                                                                             
                WHEN 'ITEMNOTAORIGEM' THEN DO:                                                                                                               
                    ASSIGN ttPedItemFat.itemNfOrigem = ttJson.valor .                                                                                        
                END.                                                                                                                                         
                                                                                                                                                             
                WHEN 'LOTE' THEN DO:                                                                                                                         
                    ASSIGN ttPedItemFat.codRefer = trim(ttJson.valor) .
                END.                                                                                                                                         
                                                                                                                                                             
                WHEN 'PRODUTO' THEN DO:                                                                                                                      
                    ASSIGN ttPedItemFat.itCodigo = trim(ttJson.valor) .
                END.                                                                                                                                         
                                                                                                                                                             
                WHEN 'NOTA' THEN DO:                                                                                                                         
                    ASSIGN ttPedItemFat.nfRetorno = trim(ttJson.valor) .                                                                                           
                END.                                                                                                                                         
                                                                                                                                                             
                WHEN 'NOTAORIGEM' THEN DO:                                                                                                                   
                    ASSIGN ttPedItemFat.nfOrigem = trim(ttJson.valor) .                                                                                            
                END.                                                                                                                                         
                                                                                                                                                             
                WHEN 'SERIE' THEN DO:                                                                                                                        
                    ASSIGN ttPedItemFat.serieNFRetorno = trim(ttJson.valor) .                                                                                      
                END.                                                                                                                                         
                                                                                                                                                             
                WHEN 'SERIEORIGEM' THEN DO:                                                                                                                  
                    ASSIGN ttPedItemFat.serieNFOrigem = trim(ttJson.valor) .
                END.                                                                                                                                         
                                                                                                                                                             
                WHEN 'QUANTIDADE' THEN DO:                                                                                                                   
                    ASSIGN ttPedItemFat.qtFaturada = dec(replace(ttJson.valor,".",",")).                                                                    
                END.                                                                                                                                         
            END CASE.
        END.
    END.

    /*OUTPUT TO c:\temp\itens_fat.txt.
    FOR EACH ttpedItemfat.
        EXPORT DELIMITER "|" ttPedItemfat.
    END.

    OUTPUT CLOSE.*/

END PROCEDURE.

PROCEDURE criarTtPedItem:

    //RUN setAgrupTtJson('resumoSeparacao','produtolisa').
    ASSIGN cTagItens = 'resumoSeparacao'.
    EMPTY TEMP-TABLE  ttPai.
    FOR EACH ttjson
        WHERE ttjson.tag_Pai = cTagItens //pega apenas a TAg ITENS maiusculas
        AND   ttjson.tag = ''
        AND   ttJson.valor = 'json'.
        CREATE ttPai.
        BUFFER-COPY ttJson TO ttPai.
    END.

    /*OUTPUT TO c:\temp\resumo_separacao_pai.txt.
    FOR EACH ttPai.
        EXPORT DELIMITER "|" ttPai.
    END.

    OUTPUT CLOSE.*/

    FOR EACH ttPai:
        CREATE ttPedITem.
        ASSIGN ttPeDItem.agrup = ttPai.id.

        FOR EACH ttJson
            WHERE ttjson.agrupJson = ttPai.id.
            FIND FIRST ttPedItem
                WHERE ttPedItem.agrup = ttPai.id NO-ERROR.
    
            CASE ttJson.tag:
                WHEN 'produto' THEN DO:
                     ASSIGN ttPedItem.itCodigo = trim(ttJson.valor) .
                END.
    
                WHEN 'descricao' THEN DO:
                    ASSIGN ttPedItem.descricao = ttJson.valor .
                END.
    
                WHEN 'lote' THEN DO:
                    ASSIGN ttPedItem.codRefer = trim(ttJson.valor) .
                END.
    
                WHEN 'coletado' THEN DO:
                    ASSIGN ttPedItem.qtSeparada = dec(replace(ttJson.valor,".",",")) .
                END.
    
                WHEN 'solicitado' THEN DO:
                    ASSIGN ttPedItem.qtSolicitada = dec(replace(ttJson.valor,".",",")) .
                END.
                WHEN 'diferenca' THEN DO:
                    ASSIGN ttPedItem.qtDiferenca  = dec(replace(ttJson.valor,".",",")) .
                END.
    
            END CASE.
        END.
    END.
    /* OUTPUT TO c:\temp\ped_itens.txt.
    FOR EACH ttpedItem.
        EXPORT DELIMITER "|" ttPedItem.
    END.

    OUTPUT CLOSE.*/

    
END PROCEDURE.



PROCEDURE criarPedItemEtq:

    
    ASSIGN cTagItens = 'separacao'.
    EMPTY TEMP-TABLE  ttPai.
    FOR EACH ttjson
        WHERE ttjson.tag_Pai = cTagItens //pega apenas a TAg ITENS maiusculas
        AND   ttjson.tag = ''
        AND   ttJson.valor = 'json'.
        CREATE ttPai.
        BUFFER-COPY ttJson TO ttPai.
    END. 

    FOR EACH ttPai:
        CREATE ttPedITemEtq.
        ASSIGN ttPedItemEtq.agrup = ttPai.id.
        FOR EACH ttJson
            WHERE ttjson.agrupJson = ttPai.id. 
            FIND FIRST ttPedItemEtq
                WHERE ttPedItemEtq.agrup = ttPai.id NO-ERROR.
            CASE ttJson.tag:
                WHEN 'produto' THEN DO:
                     ASSIGN ttPedItemEtq.itCodigo = ttJson.valor .
                END.
                WHEN 'lote' THEN DO:
                    ASSIGN ttPedItemEtq.codRefer = ttJson.valor .
                END.
                WHEN 'rolo' THEN DO:
                    ASSIGN ttPedItemEtq.rolo = ttJson.valor .
                END.
                WHEN 'id' THEN DO:
                    ASSIGN ttPedItemEtq.idLisa = ttJson.valor .
                END.
                WHEN 'endereco' THEN DO:
                    ASSIGN ttPedItemEtq.endereco = ttJson.valor .
                END.
                WHEN 'cntr' THEN DO:
                    ASSIGN ttPedItemEtq.nrContainer = int(ttJson.valor) .
                END.
                WHEN 'data' THEN DO:
                    ASSIGN ttPedItemEtq.data = convDt4a2m2d(ttJson.valor) .
                END.
                WHEN 'hora' THEN DO:
                    ASSIGN ttPedItemEtq.hora = ttJson.valor .
                END.
                WHEN 'item' THEN DO:
                    ASSIGN ttPedItemEtq.nrSeq = ttJson.valor .
                END.
                WHEN 'quantidade' THEN DO:
                    ASSIGN ttPedItemEtq.quantidade = dec(replace(ttJson.valor,".",",")) .
                END.
            END CASE.
        END.
    END.


    /*OUTPUT TO c:\temp\ttpeditemetq.txt.
    FOR EACH ttPedItemETq.
        EXPORT DELIMITER "|" ttPedItemEtq.
    END.
    OUTPUT CLOSE.*/
   

END PROCEDURE.
