
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
    FIELD quantidade    AS DECIMAL
    FIELD cntr          AS CHAR.

{esp/params.i}
{lisa/codProdUnif.i}

DEF VAR h-handle AS HANDLE.
DEF VAR i-id AS INT.
DEF VAR c-chave AS CHAR.

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
       infNotaEntrada.cnpjemitente = estabelec.cgc.

// alterar para buscar da nota de importa‡ao...
ASSIGN c-chave = nota-fiscal.cod-estabel + "|" +
                 nota-fiscal.serie + "|" +
                 nota-fiscal.nr-nota-fis.

FOR EACH lisa-integra WHERE
         lisa-integra.cod-trans = 'PackingAvulso' AND
         lisa-integra.chave = c-chave NO-LOCK.
    FIND ob-etiqueta WHERE
         ob-etiqueta.cod-estabel = nota-fiscal.cod-estabel AND
         ob-etiqueta.num-etiqueta = INTEGER(lisa-integra.conteudo)
         NO-LOCK NO-ERROR.

    FIND item WHERE
         item.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

    CREATE infRolos.
    ASSIGN infRolos.id = i-id
           infRolos.quantidade = ob-etiqueta.quantidade
           //infRolos.produto = ob-etiqueta.it-codigo
           infRolos.descricao = item.desc-item
           infRolos.codigoRolo = STRING(ob-etiqueta.num-rolo-imp)
           //infRolos.lote = ob-etiqueta.cod-refer
           infRolos.cntr = STRING(ob-etiqueta.nr-container). 
           
    IF lCodigoProdUnificado THEN DO:
       ASSIGN infRolos.produto = ob-etiqueta.it-codigo + "-" +  upper(ob-etiqueta.cod-refer)
              infRolos.lote = ''.        
    END.
    ELSE DO:
       ASSIGN infRolos.produto = ob-etiqueta.it-codigo 
              infRolos.lote    = ob-etiqueta.cod-refer.        
        
    END.
   
END.

DEF DATASET dsNFe SERIALIZE-NAME '' FOR infNotaEntrada, infRolos DATA-RELATION relx FOR infNotaEntrada, infRolos RELATION-FIELDS (id,id) NESTED.
RUN pi-chama-api.
IF RETURN-VALUE = 'ADM-ERROR' THEN
   RETURN 'ADM-ERROR'.

FOR EACH lisa-integra WHERE
         lisa-integra.cod-trans = 'PackingAvulso' AND
         lisa-integra.chave = c-chave SHARE-LOCK.
    ASSIGN lisa-integra.acao = ''
           lisa-integra.ind-situacao = 2.
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
    RUN enviarDadosRolosNFImportacao IN h-handle (INPUT h-dataset,
                                                  INPUT 'dataset',
                                                  INPUT infNotaEntrada.nota).

    RUN getStatusRetorno IN h-handle (OUTPUT iSitRet). 
    RUN getTTRetorno IN h-handle (OUTPUT TABLE ttJson).
    RUN getErro IN h-handle(OUTPUT cErro).
    RUN finalizar IN h-handle.*/
    RUN lisa/enviarPackList.p(INPUT h-dataset,
                              INPUT infNotaEntrada.nota,
                              OUTPUT cErro).


    IF cErro <> '' THEN DO:
       MESSAGE 'ERRO ao Enviar o Packing List para Armazem Geral' SKIP 
               'Comunique … TI os erros a seguir...' SKIP(2)
               cErro
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

       RETURN 'ADM-ERROR'.
    END.
END PROCEDURE.

