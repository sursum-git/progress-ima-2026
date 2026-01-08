DEFINE BUFFER cotacao FOR mgcad.cotacao.
DEFINE BUFFER moeda FOR mgcad.moeda.
DEF INPUT PARAMETER p-numero-ordem LIKE ordem-compra.numero-ordem.
DEF INPUT PARAMETER p-mo-codigo LIKE moeda.mo-codigo.
DEF INPUT PARAMETER p-dt-cotacao LIKE cotacao-item.data-cotacao.

DEFINE TEMP-TABLE tt-cotacao-item NO-UNDO LIKE cotacao-item
       FIELD r-Rowid AS ROWID.

DEFINE TEMP-TABLE RowErrors NO-UNDO
       FIELD ErrorSequence    AS INTEGER
       FIELD ErrorNumber      AS INTEGER
       FIELD ErrorDescription AS CHARACTER
       FIELD ErrorParameters  AS CHARACTER
       FIELD ErrorType        AS CHARACTER
       FIELD ErrorHelp        AS CHARACTER
       FIELD ErrorSubType     AS CHARACTER.
    
DEF VAR h-boin082     AS HANDLE NO-UNDO.

IF NOT VALID-HANDLE(h-boin082) or
   h-boin082:TYPE      <> "PROCEDURE":U OR
   h-boin082:FILE-NAME <> "inbo/boin082.p":U THEN
   RUN inbo/boin082.p PERSISTENT SET h-boin082.

FIND ordem-compra  WHERE
     ordem-compra.numero-ordem = p-numero-ordem NO-ERROR.

FIND pedido-compr OF ordem-compra NO-LOCK NO-ERROR.

FIND cotacao WHERE
     cotacao.mo-codigo = p-mo-codigo AND 
     cotacao.ano-periodo = STRING(YEAR(p-dt-cotacao),"9999") + 
                           STRING(MONTH(p-dt-cotacao),"99")
     NO-LOCK NO-ERROR.

IF NOT AVAIL cotacao THEN DO.
   MESSAGE 'NÆo encontrado Cota‡Æo para a Moeda 3 na data ' STRING(p-dt-cotacao ,"99/99/9999") 
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
   RETURN 'ADM-ERROR'.
END.

FIND ITEM OF ordem-compra NO-LOCK NO-ERROR.

CREATE cotacao-item.
ASSIGN cotacao-item.it-codigo     = ordem-compra.it-codigo
       cotacao-item.cod-emitente  = ordem-compra.cod-emitente
       cotacao-item.numero-ordem  = ordem-compra.numero-ordem
       cotacao-item.data-cotacao  = p-dt-cotacao
       cotacao-item.un            = ITEM.un      
       cotacao-item.preco-unit    = ordem-compra.preco-fornec
       cotacao-item.pre-unit-for  = ordem-compra.preco-fornec
       cotacao-item.preco-fornec  = ordem-compra.preco-fornec
       cotacao-item.mo-codigo     = cotacao.mo-codigo  
       cotacao-item.aliquota-icm  = 12
       cotacao-item.cod-cond-pag  = 1
       cotacao-item.cod-comprado  = ordem-compra.cod-comprado
       cotacao-item.cot-aprovada  = YES
       cotacao-item.data-atualiz  = TODAY
       cotacao-item.hora-atualiz  = STRING(TIME,"HH:MM:SS")
       cotacao-item.cod-transp    = ordem-compra.cod-transp
       cotacao-item.cdn-pais-orig = 10
       SUBSTR(cotacao-item.char-1,1,1)   = "0" 
       SUBSTR(cotacao-item.char-1,21,3)  = "FOB"
       SUBSTR(cotacao-item.char-1,41,2)  = "0"
       SUBSTR(cotacao-item.char-1,61,2)  = "26"
       SUBSTR(cotacao-item.char-1,81,10) = ITEM.class-fiscal
       SUBSTR(cotacao-item.char-2,41,6)  = STRING(ordem-compra.cod-emitente,">>>>>9")
       SUBSTR(cotacao-item.char-2,61,1)  = "0"
       SUBSTR(cotacao-item.char-2,81,8)  = STRING(p-dt-cotacao,"99/99/99").

IF VALID-HANDLE(h-boin082) THEN
   DELETE PROCEDURE h-boin082.

