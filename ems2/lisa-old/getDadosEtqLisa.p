{esapi/analisarJsonObject2.i}

{lisa/etqlisa.i}

DEFINE INPUT  PARAMETER pItCodigo  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pCodRefer  AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttEtq .

DEFINE VARIABLE cErros AS CHARACTER   NO-UNDO.

RUN lisa/consultarEtiquetas.p(pItCodigo,pCodRefer,OUTPUT TABLE ttJson,OUTPUT cErros,NO).

//cria agrupamentos partindo do principio que o campo quantidade
// ‚ o primeiro do registro do json
RUN setAgrupTtJson('items','quantidade').
OUTPUT TO c:\temp\ttjson11.txt.
FOR EACH ttJson:
    EXPORT DELIMITER "|" ttJson.

END.
OUTPUT CLOSE .
     

FOR EACH ttJson
    WHERE ttJson.agrup > 0
    USE-INDEX indagrup  BREAK BY ttJson.agrup.
    /*MESSAGE ttjson.agrup SKIP
            ttjson.tag SKIP

        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
    IF FIRST-OF(ttJson.agrup) THEN DO:
       CREATE ttEtq.
       ASSIGN ttEtq.id      = ttJson.agrup
              ttEtq.origem  = 'LISA'
              ttEtq.qtPeca  = 1 .
      /* MESSAGE "agrup criado:"  ttJson.agrup
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
    END.
    FIND ttEtq WHERE ttEtq.id = ttJson.Agrup  NO-ERROR.
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
         WHEN 'id'     THEN
           ASSIGN ttEtq.idEtq           = ttJson.valor.
        WHEN  'quantidade' THEN
           ASSIGN ttEtq.quantidade      = DECIMAL(REPLACE(ttJson.valor,'.',','))
           . 
        WHEN 'endereco' THEN DO:
          ASSIGN ttEtq.localiz          = ttJson.valor.
         
        END.
          
    END CASE.
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
