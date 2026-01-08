
//LOG-MANAGER:LOGGING-LEVEL = 5.
//LOG-MANAGER:LOGFILE-NAME = 'C:\temp\nota' + STRING(TIME) + '.txt'.
{esapi/infNotaEntrada.i}

    
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
    FIELD cRend                 AS CHAR.
	

DEFINE VARIABLE  hBo   AS HANDLE.
DEFINE VARIABLE cErros AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-id   AS INTEGER     NO-UNDO.


DEF INPUT PARAMETER p-row-di135 AS ROWID.

RUN criarInfNotaEntrada(p-row-di135,
                        NO,
                        OUTPUT i-id,
                        OUTPUT cErros).
IF cErros <> '' THEN DO:
   MESSAGE cErros
       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
   RETURN 'ADM-ERROR'.

END.

// Itens 
FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.
    FIND item WHERE
         item.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.

    CREATE infItens.
    ASSIGN infItens.itid = i-id
           infItens.nqtd = it-nota-fisc.qt-faturada[1]
           infItens.nvalorunitario = ROUND(it-nota-fisc.vl-tot-item / it-nota-fisc.qt-faturada[1],7) 
           infItens.cproduto = it-nota-fisc.it-codigo
           infItens.carmazem = cArmazem
           infItens.clote = it-nota-fisc.cod-refer
           infItens.clotevalidade = '99991231'
           infItens.cdescricao = item.desc-item
           infItens.cncm = item.class-fiscal
           infItens.nfatorconvercao = 1
           infItens.lcontrolaserie = NO.

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
       ASSIGN infItens.cRend = ENTRY(2,ENTRY(7,ITEM.narrativa,CHR(10)),":") NO-ERROR.
END.

DEF DATASET dsNFe SERIALIZE-HIDDEN FOR infNotaEntrada, infItens DATA-RELATION relx FOR infNotaEntrada, infItens RELATION-FIELDS (nfid,itid) NESTED.

RUN pi-chama-api.


//-------------- Procedures
PROCEDURE pi-chama-api.

    DEF VAR h-dataset AS HANDLE.
    DEF VAR iSitRet AS INTEGER.
    DEFINE VARIABLE cErro AS CHARACTER   NO-UNDO.

    ASSIGN h-dataset = DATASET dsNFe:HANDLE.

    //char ao inv‚s da BO quando quiser testar apenas o json gerado
    //RUN esapi/gerarJson2Conferencia.p(h-dataset,'dataset','c:\temp\json_envio_nf_import.json').


    
    RUN esbo/boClienteAPI.p PERSISTENT SET hBo .


    RUN iniciar IN hBo.
    //RUN retirarNoRoot IN hBo(YES).
    RUN setDominio      IN hBO('sobradocomercio114725.protheus.cloudtotvs.com.br').
    RUN setPorta        IN hBO(4050).
    RUN setPathPrinc    IN hBO('/rest/NOTAENTRADA').
    RUN setMetodo       IN hBO('POST').
    RUN setDirJson      IN hBo('\\pv1\pv2\integracao_lisa').
    RUN setUrlJson      IN hBo('http://imaonline.imatextil.com.br/iol/integracao_lisa/').
    RUN sincrHandle     IN hBo(h-dataset,1,'dataset').
    RUN setHttps        IN hBo(NO).
    RUN setTipoConteudo IN hBo('application/json').
    RUN sincrChaveCab   IN hBo('tenantid','03,08').
    RUN sincrChaveCab   IN hBo('authorization','Basic YXBpd21zOmFwaXdtcyMyNDVA').
    RUN exec            IN hBo(1234,'teste1234').
    RUN finalizar IN hBo.
    
   
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

