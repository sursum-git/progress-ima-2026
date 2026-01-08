DEF INPUT PARAMETER p-numero-ordem LIKE ordem-compra.numero-ordem.
DEF INPUT PARAMETER p-mo-codigo LIKE moeda.mo-codigo.
DEF INPUT PARAMETER p-dt-cotacao LIKE cotacao-item.data-cotacao.

DEFINE TEMP-TABLE tt-cotacao-imp NO-UNDO LIKE cotacao-item
       FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE RowErrors NO-UNDO
       FIELD ErrorSequence    AS INTEGER
       FIELD ErrorNumber      AS INTEGER
       FIELD ErrorDescription AS CHARACTER
       FIELD ErrorParameters  AS CHARACTER
       FIELD ErrorType        AS CHARACTER
       FIELD ErrorHelp        AS CHARACTER
       FIELD ErrorSubType     AS CHARACTER.
                 
FIND ordem-compra  WHERE
     ordem-compra.numero-ordem = p-numero-ordem NO-ERROR.

FIND pedido-compr OF ordem-compra NO-LOCK NO-ERROR.

FOR EACH tt-cotacao-imp:
    DELETE tt-cotacao-imp.
END.    

EMPTY TEMP-TABLE RowErrors.

CREATE tt-cotacao-imp.
ASSIGN tt-cotacao-imp.numero-ordem   = p-numero-ordem
       tt-cotacao-imp.cod-emitente   = ordem-compra.cod-emitente
       tt-cotacao-imp.it-codigo      = ordem-compra.it-codigo
       tt-cotacao-imp.seq-cotac      = 0
       tt-cotacao-imp.mapa-cotacao   = 0
       tt-cotacao-imp.cod-incoterm   = 'FOB'
       tt-cotacao-imp.cod-pto-contr  = 10
       tt-cotacao-imp.cod-fabricante = ordem-compra.cod-emitente
       tt-cotacao-imp.cdn-pais-orig  = emitente.pais
       tt-cotacao-imp.regime-import  = 0
       tt-cotacao-imp.class-fiscal   = ITEM.class-fisc
       tt-cotacao-imp.aliq-ii        = 26
       tt-cotacao-imp.aliq-ipi       = 0
       tt-cotacao-imp.cod-itiner     = 0
       tt-cotacao-imp.i-informa      = 0
       tt-cotacao-imp.da-entrega-embarque = ?.
    
RUN setUsuario in hDBOCotacao-itemi (INPUT c-seg-usuario).
    
RUN validateIncotermPedido in hDBOCotacao-itemi (INPUT gi-num-pedido-i,
                                                 INPUT tt-cotacao-imp.numero-ordem,
                                                 INPUT tt-cotacao-imp.cod-incoterm,
                                                 OUTPUT TABLE tt-bo-erro APPEND).
ASSIGN c-return-value = RETURN-VALUE.                                                             

IF c-return-value <> "NOK":U  THEN 
   RUN validateCreateImp IN hDBOCotacao-itemi (INPUT TABLE tt-cotacao-item,
                                               INPUT TABLE tt-cotacao-imp,
                                               OUTPUT TABLE tt-bo-erro APPEND).                                                   

IF VALID-HANDLE(hDBOCotacao-itemi) THEN
   DELETE PROCEDURE hDBOCotacao-itemi.

