{lisa\varpropsComuns.i}
DEF TEMP-TABLE infNotaVenda NO-UNDO SERIALIZE-NAME ""
    FIELD nfid            AS INTEGER SERIALIZE-HIDDEN
    FIELD codigoCliente   AS CHAR
    FIELD pedidoCliente   AS CHAR
    FIELD prePedido       AS CHAR
    FIELD aprovado        AS LOGICAL FORMAT "true/false"
    FIELD trocaNota       AS LOGICAL FORMAT "true/false"
    FIELD observacoes     AS CHAR
    FIELD nota            AS CHAR
    FIELD serie           AS CHAR
    FIELD xml             AS CLOB
    FIELD danfeBase64     AS BLOB.   
    
DEF TEMP-TABLE infNotaRemessa NO-UNDO
    FIELD ccodFilial            AS CHAR
    FIELD cpedido               AS CHAR 
    FIELD ccgc                  AS CHAR
    FIELD ctipo                 AS CHAR 
    FIELD cdocumentoPDFbase64   AS BLOB
    FIELD curldocumentoPDF      AS CHAR.



PROCEDURE criarInfNotaVenda.
    DEFINE INPUT  PARAMETER pRowid      AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER pAcao       AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pObs        AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pTrocaNF    AS LOGICAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER i-id        AS INTEGER     NO-UNDO.
    DEFINE BUFFER bfNf2 FOR nota-fiscal.
    
    DEFINE VARIABLE c-arq-xml AS CHAR.
    DEFINE VARIABLE c-arq-pdf AS CHAR.

    IF pAcao = 'aprovar' THEN DO:
      FIND nota-fiscal WHERE
         ROWID(nota-fiscal) = p-row-di135 NO-LOCK NO-ERROR.
      FIND bfNf2
          WHERE ROWID(bfnf2) = pRowid NO-LOCK NO-ERROR.
      
      FIND ped-venda WHERE
           ped-venda.nr-pedcli = nota-fiscal.nr-pedcli AND
           ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
           NO-LOCK NO-ERROR.

    END.
    ELSE DO: //reprovar
         FIND ped-venda NO-LOCK 
              WHERE ROWID(ped-venda) = pRowid NO-ERROR.
    END.
         
        
    FIND ped-venda-ext WHERE
         ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND
         ped-venda-ext.nr-pedido = ped-venda.nr-pedido
         SHARE-LOCK NO-ERROR.
    
    
    
    ASSIGN i-id = RANDOM(1,99999).

    CREATE infNotaVenda.
    ASSIGN infNotaVenda.nfid            = i-id
           infNotaVenda.codigoCliente   = cChave
           infNotaVenda.pedidoCliente   = ped-venda.nr-pedcli
           infNotaVenda.prePedido       = ped-venda-ext.nr-pedext
           infNotaVenda.aprovado        = IF pAcao = 'aprovar' THEN YES ELSE NO
           infNotaVenda.trocaNota       = IF pTrocaNF THEN YES ELSE NO
           infNotaVenda.observacoes     = pObs
           infNotaVenda.nota            = IF AVAIL bfNf2 THEN bfNf2.nr-nota-fis ELSE ''
           infNotaVenda.serie           = IF AVAIL bfNf2 THEN bfNf2.serie ELSE ''.

    IF pAcao = 'aprovar' THEN DO:
       
      
      FIND estabelec OF bfNf2 NO-LOCK NO-ERROR.
      FIND param-nf-estab OF bfNf2 NO-LOCK NO-ERROR.
           
      ASSIGN c-arq-xml = param-nf-estab.cod-caminho-xml + "\" + bfNf2.cod-estabel + FILL("0", (3 - LENGTH(bfNf2.serie))) + bfNf2.serie + bfNf2.nr-nota-fis + ".xml".
  
      FILE-INFO:FILE-NAME = c-arq-xml.
      IF FILE-INFO:FILE-NAME <> ? THEN DO.
          COPY-LOB FROM FILE c-arq-xml TO infNotaVenda.xml.
  
         //COPY-LOB FROM FILE c-arq-xml TO lc-xml.
         //ASSIGN infNotaVenda.xml = lc-xml.
      END.
  
      RUN pi-gera-danfe (INPUT ROWID(bfNf2) ).
  
      ASSIGN c-arq-pdf = SESSION:TEMP-DIRECTORY + "FT0518" + bfNf2.nr-nota-fis + ".PDF".
      FILE-INFO:FILE-NAME = c-arq-pdf.
      IF FILE-INFO:FILE-NAME <> ? THEN DO.
         COPY-LOB FROM FILE c-arq-pdf TO infNotaVenda.danfeBase64.
      END.
     
    END.
    



END PROCEDURE.

PROCEDURE pi-gera-danfe.
    DEF INPUT PARAMETER p-row-nota AS ROWID.
    DEF BUFFER b2-nota-fiscal FOR nota-fiscal.

    FIND b2-nota-fiscal WHERE
         ROWID(b2-nota-fiscal) = p-row-nota SHARE-LOCK NO-ERROR.
    ASSIGN b2-nota-fiscal.ind-sit-nota = 1.
    FIND CURRENT b2-nota-fiscal NO-LOCK NO-ERROR.
        
    RUN esapi/imprime-nfs.p (INPUT b2-nota-fiscal.nr-nota-fis,
                             INPUT b2-nota-fiscal.nr-nota-fis,
                             INPUT b2-nota-fiscal.cod-estabel,
                             INPUT b2-nota-fiscal.serie).      
END.
