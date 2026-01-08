
//LOG-MANAGER:LOGGING-LEVEL = 5.
//LOG-MANAGER:LOGFILE-NAME = 'C:\temp\rolos.txt'.

DEF TEMP-TABLE infNotaEntrada SERIALIZE-NAME ""
    FIELD id              AS INTEGER SERIALIZE-HIDDEN
    FIELD codigoCliente   AS CHAR 
    FIELD nota            AS CHAR 
    FIELD serie           AS CHAR
    FIELD cnpjEmitente    AS CHAR
    FIELD cntr            AS CHAR.

DEF TEMP-TABLE infRolos SERIALIZE-NAME "itens" 
    FIELD id            AS INTEGER  SERIALIZE-HIDDEN
    FIELD produto       AS CHAR 
    FIELD descricao     AS CHAR 
    FIELD codigoRolo    AS CHAR 
    FIELD variante      AS CHAR 
    FIELD lote          AS CHAR
    FIELD quantidade    AS DECIMAL.



DEF VAR hBo AS HANDLE.
DEF VAR i-id AS INT.


DEF INPUT PARAMETER p-row-di135 AS ROWID.

FIND nota-fiscal WHERE
     ROWID(nota-fiscal) = p-row-di135 NO-LOCK NO-ERROR.
     
FIND estabelec OF nota-fiscal NO-LOCK NO-ERROR.
FIND param-nf-estab OF estabelec NO-LOCK NO-ERROR.

ASSIGN i-id = RANDOM(1,99999).

CREATE infNotaEntrada.
ASSIGN infNotaEntrada.id = i-id
       infNotaEntrada.codigoCliente = '86159501'
       infNotaEntrada.nota = nota-fiscal.nr-nota-fis
       infNotaEntrada.serie = nota-fiscal.serie
       infNotaEntrada.cnpjemitente = estabelec.cgc
       infNotaEntrada.cntr = STRING(nota-fiscal.nro-proc-entrada).


// alterar para buscar da nota de importa‡ao...
FIND pp-container WHERE
     pp-container.nr-container = nota-fiscal.nro-proc-entrada NO-LOCK NO-ERROR.

FOR EACH ob-etiqueta WHERE 
         ob-etiqueta.nr-container = pp-container.nr-container SHARE-LOCK
    BY ob-etiqueta.num-rolo-imp.

    FIND item WHERE
         item.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

    CREATE infRolos.
    ASSIGN infRolos.id = i-id
           infRolos.quantidade = ob-etiqueta.quantidade
           infRolos.produto = ob-etiqueta.it-codigo
           infRolos.descricao = item.desc-item
           infRolos.codigoRolo = STRING(ob-etiqueta.num-rolo-imp)
           infRolos.variante =  ''
           infRolos.lote =  ob-etiqueta.cod-refer. 
END.

DEF DATASET dsNFe SERIALIZE-NAME '' FOR infNotaEntrada, infRolos DATA-RELATION relx FOR infNotaEntrada, infRolos RELATION-FIELDS (id,id) NESTED.
RUN pi-chama-api.
IF RETURN-VALUE = 'ADM-ERROR' THEN
   RETURN 'ADM-ERROR'.

//----------- Procedures
PROCEDURE pi-chama-api.
    DEF VAR iSitRet AS INTEGER.
    DEF VAR cErro AS CHARACTER   NO-UNDO.
    DEF VAR h-dataset AS HANDLE.

    ASSIGN h-dataset = DATASET dsNFe:HANDLE.

   

    RUN esbo/boClienteAPI.p PERSISTENT SET hBo .


    RUN iniciar IN hBo.
    RUN retirarNoRoot IN hBo(YES).
    RUN setDominio      IN hBO('sobradocomercio114725.protheus.cloudtotvs.com.br').
    RUN setPorta        IN hBO(4050).
    RUN setPathPrinc    IN hBO('rest/wms/v2/EntradaRolo').
    RUN setMetodo       IN hBO('POST').
    RUN setDirJson      IN hBo('\\pv1\pv2\integracao_lisa').
    RUN setUrlJson      IN hBo('http://imaonline.imatextil.com.br/iol/integracao_lisa/').
    RUN sincrHandle     IN hBo(h-dataset,1,'dataset').
    RUN setHttps        IN hBo(NO).
    RUN setTipoConteudo IN hBo('json/application').
    RUN sincrChaveCab   IN hBo('tenantid','03,08').
    RUN sincrChaveCab   IN hBo('authorization','Basic YXBpd21zOmFwaXdtcyMyNDVA').
    RUN exec            IN hBo(12345,'teste12345').
    RUN finalizar IN hBo.


    

    
END PROCEDURE.

