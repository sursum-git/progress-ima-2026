
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



DEF VAR h-handle AS HANDLE.
DEF VAR i-id AS INT.


DEF INPUT PARAMETER p-row-di135 AS ROWID.
{esp/params.i}
{lisa/codProdUnif.i}

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
           //infRolos.produto = ob-etiqueta.it-codigo
           infRolos.descricao = item.desc-item
           infRolos.codigoRolo = STRING(ob-etiqueta.num-rolo-imp)
           infRolos.variante =  ''
           //infRolos.lote =  ob-etiqueta.cod-refer
           . 
           
           
    IF lCodigoProdUnificado THEN DO:
       ASSIGN infRolos.produto = ob-etiqueta.it-codigo + "-" +  upper(ob-etiqueta.cod-refer)
              infRolos.lote = ''.        
    END.
    ELSE DO:
       ASSIGN infRolos.produto = ob-etiqueta.it-codigo 
              infRolos.lote    = ob-etiqueta.cod-refer.        
        
    END.
END.

//limpar item em branco
FOR EACH infRolos
    WHERE infRolos.produto = '':
    DELETE infRolos.
END.



DEF DATASET dsNFe SERIALIZE-NAME '' FOR infNotaEntrada, infRolos DATA-RELATION relx FOR infNotaEntrada, infRolos RELATION-FIELDS (id,id) NESTED.
RUN pi-chama-api.
IF RETURN-VALUE = 'ADM-ERROR' THEN
   RETURN 'ADM-ERROR'.

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

