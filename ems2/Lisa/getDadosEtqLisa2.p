{esapi/analisarJsonObject2.i}
{esp/util.i}
{esp/params.i}
{lisa/codProdUnif.i}
{lisa/etqlisa.i}
DEFINE TEMP-TABLE ttExcluir NO-UNDO
    FIELD idRegistro AS INT.
    
DEFINE BUFFER bfEtq FOR ttEtq.    
DEFINE INPUT  PARAMETER pItCodigo       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pCodRefer       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pLogAtuEnd      AS LOGICAL     NO-UNDO.
DEFINE INPUT  PARAMETER pDtAtuEndIni    AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER pDtAtuEndFim    AS DATE        NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttEtq .

DEFINE VARIABLE cErros          AS CHARACTER   NO-UNDO. 
DEFINE VARIABLE iCont           AS INTEGER     NO-UNDO.
DEFINE VARIABLE iEtq            AS INTEGER     NO-UNDO.

DEFINE TEMP-TABLE ttReg NO-UNDO
    FIELD id    AS INT.
    

RUN lisa/consultarEtiquetas.p(pItCodigo,pCodRefer,pLogAtuEnd,pDtAtuEndIni,pDtAtuEndFim,OUTPUT TABLE ttJson,OUTPUT cErros,NO).

//cria agrupamentos partindo do principio que o campo quantidade
// ‚ o primeiro do registro do json
RUN setAgrupTtJson('items','quantidade').
OUTPUT TO c:\temp\ttjson11.txt.
FOR EACH ttJson:
    EXPORT DELIMITER "|" ttJson.

END.
OUTPUT CLOSE .

FOR EACH ttJson
    WHERE ttJson.tag_pai    = 'items'
    AND   ttJson.tag        = ''
    AND   ttJson.valor      = 'json'
    :
    CREATE ttReg.
    ASSIGN ttReg.id = ttJson.id.
END.

FIND LAST ttEtq USE-INDEX unico NO-ERROR.
IF AVAIL ttEtq THEN DO:
   ASSIGN iEtq = ttEtq.id .     
END.
FOR EACH ttReg:
    ASSIGN iEtq = iEtq + 1.
    CREATE ttEtq.
    ASSIGN ttEtq.id      = iEtq
           ttEtq.origem  = 'LISA'
           ttEtq.qtPeca  = 1 .
    FOR EACH ttJson
        WHERE ttJson.agrupjson = ttReg.id
        USE-INDEX indagrup2.
        
        
        FIND ttEtq WHERE ttEtq.id = iEtq  NO-ERROR.
        /*MESSAGE "id corrente:"  ttEtq.id
               VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/


        CASE ttJson.tag:
            WHEN 'produto' THEN
                ASSIGN ttEtq.itCodigo    = ttJson.valor.
            WHEN 'descricao' THEN
                ASSIGN ttEtq.descricao   = ttJson.valor.
            WHEN 'lote' THEN
                ASSIGN ttEtq.codRefer    = ttJson.valor.
            WHEN 'codigoRolo' THEN
                ASSIGN ttEtq.numRolo     = int(ttJson.valor).
            WHEN 'cntr' THEN
                ASSIGN ttEtq.nrContainer = int(ttJson.valor).
            WHEN 'pedido' THEN
                ASSIGN ttEtq.pedido        = ttJson.valor.
            WHEN 'pedidoCliente' THEN
               ASSIGN ttEtq.pedidoCliente  = ttJson.valor.
            WHEN 'prepedido'     THEN
               ASSIGN ttEtq.prePedido       = ttJson.valor.
             WHEN 'id'     THEN  DO:
               IF CAN-FIND( FIRST bfEtq WHERE bfEtq.idEtq = ttJson.valor) THEN DO:
                  CREATE ttExcluir.
                  ASSIGN ttExcluir.idRegistro = ttEtq.id.
               END.
               ASSIGN ttEtq.idEtq           = ttJson.valor.           
             END.
               
            WHEN  'quantidade' THEN
               ASSIGN ttEtq.quantidade      = DECIMAL(REPLACE(ttJson.valor,'.',',')). 
            WHEN 'endereco' THEN DO:
              ASSIGN ttEtq.localiz          = ttJson.valor.
             
            END.
            
            WHEN 'origem' THEN DO:
                   IF ttjson.valor = 'jsonarray'  THEN
                      ASSIGN ttEtq.agrupOrigem = ttJson.id + 1 .
            END.
              
        END CASE.
    END. 

END.

IF lCodigoProdUnificado THEN DO:
   FOR EACH ttEtq:
       IF NUM-ENTRIES(ttEtq.itCodigo,"-") > 1 THEN DO:
           ASSIGN ttEtq.codRefer = ENTRY(2,ttEtq.itCodigo,"-")
                  ttEtq.itCodigo = ENTRY(1,ttEtq.itCodigo,"-")
                  .                
       END.
       
   END.
END.
FOR EACH ttEtq:
    FOR EACH ttJson
        WHERE ttJson.agrupJson = ttEtq.agrupOrigem
        AND   ttJson.tag_pai = 'origem':
        CASE ttJson.tag:
            WHEN 'id' THEN
            DO:
               ASSIGN ttEtq.numEtqLisaOri =  ttjson.valor.                
            END.
            WHEN 'quantidade' THEN
            DO:
             // ASSIGN ttReservas.qtAtuEtqCortada =  dec(ttjson.valor).  
             ASSIGN ttEtq.qtEtqLisaOri =  dec(replace(ttjson.valor,'.',',')) .  
              /*MESSAGE 'qt.json:' ttjson.valor SKIP
                     'qt.conv:' ttReservas.qtAtuEtqCortada SKIP
                  VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
            END.
            WHEN 'codigoRolo' THEN DO:
                ASSIGN ttEtq.numRoloLisaOri = INT(ttJson.valor).                
            END.            
        END CASE.        
    END.
    ASSIGN iCont = iCont + 1 .
    
END.



//exclui os registros de etiqueta vindo da tag origem
FOR EACH ttExcluir:
    FIND ttEtq
    WHERE ttEtq.id = ttExcluir.idRegistro NO-ERROR.
    IF AVAIL ttEtq THEN
    DO:
        DELETE ttEtq.        
    END.
END.


//calcula situa‡Æo 
FOR EACH ttEtq:
    IF ttEtq.pedido <> '' THEN
       ASSIGN ttEtq.codSituacao = "Faturada".
    ELSE DO:
        IF ttEtq.prePedido <> '' THEN
           ASSIGN ttEtq.CodSituacao    = "Alocada".
        ELSE
           ASSIGN ttEtq.codSituacao    = "Estoque".
    END.
END.



IF cErros <> '' THEN
   MESSAGE cErros
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

/*
FOR EACH ttJson.
    ASSIGN iCont =iCont + 1.
END.                        */





 /*       estrutura json linsa em 03/08/2023
           "quantidade": 60,
            "produto": "525116",
            "produtoLisa": "IMA525116",
            "descricao": "VISCOSE SATIN LISA TINTO IMP. 1,45 m  VISCOSE 100%",
            "codigoRolo": "1",
            "variante": "",
            "lote": "D54",
            "id": "00140963",
            "pedido": "",
            "pedidoCliente": "",
            "status": "P",
            "prePedido": "",
            "cntr": "263722"
  
  
  
  */
  
 
