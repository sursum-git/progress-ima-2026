
//LOG-MANAGER:LOGGING-LEVEL = 5.
//LOG-MANAGER:LOGFILE-NAME = 'C:\temp\nota' + STRING(TIME) + '.txt'.
{esp/params.i}
DEF TEMP-TABLE infNotaEntrada NO-UNDO SERIALIZE-NAME ""
    FIELD nfid            AS INTEGER SERIALIZE-HIDDEN
    FIELD ccodfilial      AS CHAR
    FIELD cnota           AS CHAR
    FIELD cserie          AS CHAR
    FIELD cemissaonf      AS CHAR
    FIELD ccgcfornecedor  AS CHAR
    FIELD ccgc            AS CHAR
    FIELD cchavenfe       AS CHAR
    FIELD cnforiginal     AS CHAR
    FIELD cserieoriginal  AS CHAR
    FIELD citemoriginal   AS CHAR
    FIELD ccodigopostagem AS CHAR
    FIELD cnumdocsap      AS CHAR
    FIELD cxmldanfe       AS CLOB
    //FIELD cxmldanfe64       AS CHAR
    FIELD cpdfdanfe64     AS BLOB.   
    
DEF TEMP-TABLE infItens SERIALIZE-NAME "aitens" 
    FIELD itid                  AS INTEGER  SERIALIZE-HIDDEN
    FIELD nqtd                  AS DECIMAL  FORMAT ">>>,>>9.9999" 
	FIELD nvalorunitario        AS DECIMAL  FORMAT ">>>,>>9.9999999"
	FIELD cproduto              AS CHAR
	FIELD cprodutonofornecedor  AS CHAR
	FIELD clote                 AS CHAR
	FIELD clotevalidade         AS CHAR
	FIELD carmazem              AS CHAR
	FIELD cdescricao            AS CHAR
	FIELD ccodbarras            AS CHAR
	FIELD cncm                  AS CHAR
	FIELD nfatorconvercao       AS INTEGER
	FIELD lcontrolaserie        AS LOGICAL 
	FIELD npesokg               AS INTEGER
	FIELD altura                AS INTEGER
	FIELD largura               AS INTEGER
	FIELD comprimento           AS INTEGER
	FIELD cubagem               AS INTEGER
	FIELD alturaCaixa           AS INTEGER
	FIELD larguraCaixa          AS INTEGER
	FIELD comprimentoCaixa      AS INTEGER
	FIELD cCodClac              AS CHAR
	FIELD cDun14                AS CHAR
	FIELD cUpc                  AS CHAR
	FIELD cubagemCaixa          AS INTEGER
    FIELD gramatura             AS DECIMAL
    FIELD largtecido            AS CHAR
    FIELD regralavagem          AS CHAR
    FIELD composicao            AS CHAR
    FIELD paisOrigem            AS CHAR
    FIELD rendimento            AS CHAR.
	


DEF VAR h-handle  AS HANDLE.
DEF VAR i-id      AS INT.
DEF VAR c-arq-xml AS CHAR.
DEF VAR c-arq-pdf AS CHAR.
DEF VAR lc-xml    AS LONGCHAR.
DEF VAR lc-pdf    AS LONGCHAR.
{lisa/codProdUnif.i}

DEF INPUT PARAMETER p-row-di135 AS ROWID.

FIND nota-fiscal WHERE
     ROWID(nota-fiscal) = p-row-di135 NO-LOCK NO-ERROR.

FIND estabelec OF nota-fiscal NO-LOCK NO-ERROR.
FIND param-nf-estab OF estabelec NO-LOCK NO-ERROR.

ASSIGN i-id = RANDOM(1,99999).

CREATE infNotaEntrada.
ASSIGN infNotaEntrada.nfid = i-id
       infNotaEntrada.ccodfilial = '08'
       infNotaEntrada.cnota = nota-fiscal.nr-nota-fis
       infNotaEntrada.cserie = nota-fiscal.serie
       infNotaEntrada.cemissaonf = STRING(YEAR(nota-fiscal.dt-emis),"9999") +  STRING(MONTH(nota-fiscal.dt-emis),"99") +  STRING(DAY(nota-fiscal.dt-emis),"99") 
       infNotaEntrada.ccgcfornecedor = estabelec.cgc 
       infNotaEntrada.ccgc = estabelec.cgc
       infNotaEntrada.cchavenfe = nota-fiscal.cod-chave-aces-nf-eletro
       infNotaEntrada.cnforiginal = ""   
       infNotaEntrada.cserieoriginal = ""
       infNotaEntrada.citemoriginal = "" 
       infNotaEntrada.ccodigopostagem = ""
       infNotaEntrada.cnumdocsap = "".

ASSIGN c-arq-xml = param-nf-estab.cod-caminho-xml + "\" + nota-fiscal.cod-estabel + FILL("0", (3 - LENGTH(nota-fiscal.serie))) + nota-fiscal.serie + nota-fiscal.nr-nota-fis + ".xml".
FILE-INFO:FILE-NAME = c-arq-xml.
IF FILE-INFO:FULL-PATHNAME = ? THEN DO.
   MESSAGE 'NÆo foi encontrado o XML da nota,' SKIP
           'Nota nÆo poder  ser Enviada'
       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
   RETURN 'ADM-ERROR'.
END.
COPY-LOB FROM FILE c-arq-xml TO infNotaEntrada.cxmldanfe.
//COPY-LOB FROM FILE c-arq-xml TO lc-xml.
//ASSIGN infNotaEntrada.cxmldanfe64 = lc-xml.

RUN pi-gera-danfe.
ASSIGN c-arq-pdf = SESSION:TEMP-DIRECTORY + "FT0518" + nota-fiscal.nr-nota-fis + ".PDF".

FILE-INFO:FILE-NAME = c-arq-pdf.
IF FILE-INFO:FULL-PATHNAME = ? THEN DO.
   MESSAGE 'NÆo foi possivel Gerar o DANFE,' SKIP
           'Nota nÆo poder  ser Enviada'
       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
   RETURN 'ADM-ERROR'.
END.
COPY-LOB FROM FILE c-arq-pdf TO infNotaEntrada.cpdfdanfe64.


// Itens 
FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.
    FIND item WHERE
         item.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.

    CREATE infItens.
    ASSIGN infItens.itid = i-id
           infItens.nqtd = it-nota-fisc.qt-faturada[1]
           infItens.nvalorunitario = ROUND(it-nota-fisc.vl-tot-item / it-nota-fisc.qt-faturada[1],7) 
           //infItens.cproduto = it-nota-fisc.it-codigo
           infItens.carmazem = '01'
           //infItens.clote = it-nota-fisc.cod-refer
           infItens.clotevalidade = '20991231'
           infItens.cdescricao = item.desc-item
           infItens.cncm = item.class-fiscal
           infItens.nfatorconvercao = 1
           infItens.lcontrolaserie = NO.

    IF  lCodigoProdUnificado THEN DO:
        ASSIGN infItens.cproduto = it-nota-fisc.it-codigo   + "-" +  upper(it-nota-fisc.cod-refer)
               infItens.clote    = ''.        
    END.
    ELSE DO:
         ASSIGN infItens.cproduto = it-nota-fisc.it-codigo   
               infItens.clote     = it-nota-fisc.cod-refer.     
    END.
    /*MESSAGE infItens.nvalorunitario
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/


    ASSIGN infItens.largtecido = ENTRY(2,ENTRY(5,item.narrativa,CHR(10)),":") NO-ERROR.

    FIND item-ext WHERE
         item-ext.it-codigo = item.it-codigo NO-LOCK NO-ERROR.

    IF AVAIL item-ext THEN 
       ASSIGN infItens.gramatura = DECIMAL(TRIM(STRING(item-ext.gramatura,">>9"))).

    ASSIGN infItens.regralavagem = item.cod-image.

    ASSIGN infItens.paisOrigem = ENTRY(2,ENTRY(4,ITEM.narrativa,CHR(10)),":") NO-ERROR.
    ASSIGN infItens.composicao = ENTRY(2,ENTRY(6,ITEM.narrativa,CHR(10)),":") NO-ERROR.

    IF item.un = 'KG' THEN
       ASSIGN infItens.rendimento = ENTRY(2,ENTRY(7,ITEM.narrativa,CHR(10)),":") NO-ERROR.
END.

DEF DATASET dsNFe SERIALIZE-HIDDEN FOR infNotaEntrada, infItens DATA-RELATION relx FOR infNotaEntrada, infItens RELATION-FIELDS (nfid,itid) NESTED.

RUN pi-chama-api.
IF RETURN-VALUE = 'ADM-ERROR' THEN
   RETURN 'ADM-ERROR'.

RETURN 'ADM-OK'.

//-------------- Procedures
PROCEDURE pi-chama-api.

    DEF VAR h-dataset AS HANDLE.
    DEF VAR iSitRet AS INTEGER.
    DEFINE VARIABLE cErro AS CHARACTER   NO-UNDO.

    ASSIGN h-dataset = DATASET dsNFe:HANDLE.

    //char ao inv‚s da BO quando quiser testar apenas o json gerado
    //RUN esapi/gerarJson2Conferencia.p(h-dataset,'dataset','c:\temp\json_envio_nf_import.json').



    /* 
    COMENTADO DEVIDO PROBLEMA NA boApisLisa NO AMBIENTE DE PRODU€ÇO.
    RUN esbo/boAPIsLisa.p PERSIST SET h-handle.

    RUN iniciar IN h-handle.
    RUN retirarNoRoot IN h-handle(YES).
    //RUN setOpcaoTestarAPI IN h-handle('php').
    RUN enviarDadosNFImportacao IN h-handle (INPUT h-dataset,
                                             INPUT 'dataset',
                                             INPUT infNotaEntrada.cnota).

    RUN getStatusRetorno IN h-handle (OUTPUT iSitRet). 
    RUN getTTRetorno IN h-handle (OUTPUT TABLE ttJson).
    RUN getErro IN h-handle(OUTPUT cErro).
    RUN finalizar IN h-handle.
    */

    RUN lisa/enviarNFRemessa.p(INPUT h-dataset,
                               INPUT infNotaEntrada.cNota,
                               OUTPUT cErro).
    IF cErro <> '' THEN DO:
       MESSAGE 'ERRO ao Enviar NF para Armazem Geral' SKIP 
               'Comunique … TI os erros a seguir...' SKIP(2)
               cErro
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       RETURN 'ADM-ERROR'.
    END.
   
END PROCEDURE.

PROCEDURE pi-gera-danfe.
    FIND CURRENT nota-fiscal SHARE-LOCK NO-ERROR.
    ASSIGN nota-fiscal.ind-sit-nota = 1.
    FIND CURRENT nota-fiscal NO-LOCK NO-ERROR.
        
    RUN esapi/imprime-nfs.p (INPUT nota-fiscal.nr-nota-fis,
                             INPUT nota-fiscal.nr-nota-fis,
                             INPUT nota-fiscal.cod-estabel,
                             INPUT nota-fiscal.serie).      
END.

