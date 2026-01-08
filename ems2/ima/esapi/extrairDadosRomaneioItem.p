{esapi/extrairDadosRomaneioItem.i}

DEFINE INPUT  PARAMETER pEstab AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pSerie AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pNota  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pItem  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pRef   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pTipo  AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttContainer.

DEFINE VARIABLE dProp  AS DECIMAL     NO-UNDO.



  
FIND nota-fiscal NO-LOCK
    WHERE nota-fiscal.cod-estabel  = pEstab
    AND   nota-fiscal.serie        = pSerie
    AND   nota-fiscal.nr-nota-fis  = pNota
    NO-ERROR.
IF NOT AVAIL nota-fiscal THEN DO:
   RETURN 'nok'.
END.

FOR EACH it-nota-fisc OF nota-fiscal  NO-LOCK
        WHERE it-nota-fisc.it-codigo    = pItem
        AND   it-nota-fisc.cod-refer    = pRef .

    ASSIGN dProp = 1.
    IF pTipo <> 'fat' THEN
       RUN esapi/getPropNfVendaDevol.p(nota-fiscal.cod-estabel,
                                       nota-fiscal.serie,
                                       nota-fiscal.nr-nota-fis,
                                       nota-fiscal.cod-emitente,
                                       nota-fiscal.nat-operacao,
                                       it-nota-fisc.nr-seq-fat,
                                      INPUT-OUTPUT dProp).
    FIND ped-item  NO-LOCK
        WHERE ped-item.nome-abrev   = it-nota-fisc.nome-ab-cli
        AND   ped-item.nr-pedcli    = it-nota-fisc.nr-pedcli
        AND   ped-item.nr-sequencia = it-nota-fisc.nr-seq-ped  NO-ERROR.
    IF AVAIL ped-item THEN DO:
        FOR EACH ped-item-rom WHERE
                 ped-item-rom.nome-abrev   = ped-item.nome-abrev AND
                 ped-item-rom.nr-pedcli    = ped-item.nr-pedcli  AND
                 ped-item-rom.nr-sequencia = ped-item.nr-sequencia NO-LOCK.
            FIND ob-etiqueta WHERE
                    ob-etiqueta.cod-estabel  = ped-item-rom.cod-estabel AND
                    ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                    NO-LOCK NO-ERROR.
            IF AVAIL ob-etiqueta THEN DO:
               FIND ttContainer
                   WHERE ttContainer.nrContainer = ob-etiqueta.nr-container
                   NO-ERROR.
               IF NOT AVAIL ttContainer THEN DO:
                  CREATE ttContainer.
                  ASSIGN ttContainer.nrContainer = ob-etiqueta.nr-container.
               END.
               ASSIGN ttContainer.quantidade = ttContainer.quantidade + ob-etiqueta.quantidade.
            END.
        END.
        FOR EACH ttContainer:
            ASSIGN ttcontainer.perc = ttContainer.quantidade / it-nota-fisc.qt-faturada[1] * dProp.
        END.
    END.
END.

/*







       FIND ped-venda-ext WHERE
            ped-venda-ext.cod-estabel = nota-fiscal.cod-estabel AND
            ped-venda-ext.nr-pedido = INT(nota-fiscal.nr-pedcli)
            NO-LOCK NO-ERROR.
       IF AVAIL ped-venda-ext THEN
          ASSIGN i-tot-vol = ped-venda-ext.qt-fardos.

       FOR EACH ped-item-res WHERE
                ped-item-res.cod-estabel = nota-fiscal.cod-estabel AND
                ped-item-res.serie       = nota-fiscal.serie AND
                ped-item-res.nr-nota-fis = INT(nota-fiscal.nr-nota-fis) AND
                ped-item-res.faturado    = YES NO-LOCK.
   
           FOR EACH ped-item-rom WHERE
                    ped-item-rom.nome-abrev   = ped-item-res.nome-abrev AND
                    ped-item-rom.nr-pedcli    = ped-item-res.nr-pedcli  AND
                    ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia NO-LOCK.
   
               FIND ob-etiqueta WHERE
                    ob-etiqueta.cod-estabel  = ped-item-rom.cod-estabel AND
                    ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                    NO-LOCK NO-ERROR.
                 
               FIND ITEM WHERE
                    ITEM.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.
                 
               ASSIGN i-tot-etq = i-tot-etq + 1.
               ASSIGN de-tot-qtd  = de-tot-qtd + ob-etiqueta.quantidade.
           END.
       END.
       IF de-tot-qtd = 0 THEN NEXT.
   
       ASSIGN de-tot-peso = nota-fiscal.peso-bru-tot.

       FIND transporte WHERE
            transporte.nome-abrev = nota-fiscal.nome-transp NO-LOCK NO-ERROR.
   
       FIND emitente WHERE
            emitente.nome-abrev = nota-fiscal.nome-ab-cli NO-LOCK NO-ERROR.
   
      /* ASSIGN c-nr-pedcli   = nota-fiscal.nr-pedcli
              c-nome-abrev  = STRING(emitente.cod-emit) + "-" + nota-fiscal.nome-ab-cli
              c-transp      = STRING(transporte.cod-transp) + "-" + nota-fiscal.nome-transp
              c-nr-nota-fis = nota-fiscal.nr-nota-fis.*/
   
       FIND ped-venda-ext WHERE
            ped-venda-ext.cod-estabel = nota-fiscal.cod-estabel AND
            ped-venda-ext.nr-pedido = INTEGER(nota-fiscal.nr-pedcli)
            NO-LOCK NO-ERROR.
       ASSIGN i-tot-vol = 0.
       IF AVAIL ped-venda-ext THEN
          ASSIGN i-tot-vol = ped-venda-ext.qt-fardos.

       FOR EACH ped-item-res WHERE
                ped-item-res.cod-estabel = nota-fiscal.cod-estabel AND
                ped-item-res.serie       = nota-fiscal.serie AND
                ped-item-res.nr-nota-fis = INT(nota-fiscal.nr-nota-fis) AND
                ped-item-res.faturado    = YES NO-LOCK
                BY ped-item-res.nr-sequencia.
   
           ASSIGN i-qt-etq = 0.
           FOR EACH ped-item-rom WHERE
                    ped-item-rom.nome-abrev   = ped-item-res.nome-abrev AND
                    ped-item-rom.nr-pedcli    = ped-item-res.nr-pedcli  AND
                    ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia NO-LOCK.
               ASSIGN i-qt-etq = i-qt-etq + 1.
           END.

           FIND item WHERE
                item.it-codigo = ped-item-res.it-codigo NO-LOCK NO-ERROR.
           FIND ped-item-ext OF ped-item-res NO-LOCK NO-ERROR.
           
           
   
           FOR EACH ped-item-rom WHERE
                    ped-item-rom.nome-abrev   = ped-item-res.nome-abrev AND
                    ped-item-rom.nr-pedcli    = ped-item-res.nr-pedcli  AND
                    ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia
                    BREAK BY ped-item-rom.nr-volume.
   
               FIND ob-etiqueta WHERE
                    ob-etiqueta.cod-estabel  = ped-item-rom.cod-estabel AND
                    ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                    NO-LOCK NO-ERROR.

               

               ASSIGN i-imp-etq = i-imp-etq + 1.

               CREATE tt-itens-romaneio.
               ASSIGN 
               tt-itens-romaneio.codEstabel = pEstab
               tt-itens-romaneio.serie      = pSerie
               tt-itens-romaneio.notaFiscal = pNota
               tt-itens-romaneio.seq        = ped-item-res.nr-sequencia
               tt-itens-romaneio.numEtq     = ped-item-rom.num-etiqueta
               tt-itens-romaneio.qtidade    = ob-etiqueta.quantidade.
               //tt-itens-romaneio.peso       =

               RUN sincrTTItemEtq(
                    INPUT ob-etiqueta.it-codigo,
                    INPUT ob-etiqueta.cod-refer,
                    OUTPUT tt-itens-romaneio.idItem

                   ).
               
                


           END.                                 
       END.
  

       CREATE tt-Romaneio.
       ASSIGN 
       tt-Romaneio.codEstabel = pEstab
       tt-Romaneio.serie      = pSerie
       tt-Romaneio.notaFiscal = pNota
       tt-Romaneio.qtEtq      = i-qt-etq
       tt-Romaneio.totVolume  = i-tot-vol
       tt-Romaneio.totPeso    = de-tot-peso
       tt-Romaneio.totEtq     = i-tot-etq
       tt-Romaneio.impEtq     = i-imp-etq
       tt-Romaneio.totQtd     = de-tot-qtd
       tt-Romaneio.cPedido    = nota-fiscal.nr-pedcli
       tt-Romaneio.cNomeAbrev = STRING(emitente.cod-emit) + "-" + nota-fiscal.nome-ab-cli
       tt-Romaneio.cTransp    = STRING(transporte.cod-transp) + "-" + nota-fiscal.nome-transp .
   END.


PROCEDURE sincrTTItemEtq:

    DEFINE INPUT  PARAMETER pItem       AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER PRefer      AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER idItem      AS INTEGER     NO-UNDO.
    
    FIND LAST bf NO-ERROR.
    FIND FIRST tt-dados-itens-romaneio
        WHERE tt-dados-itens-romaneio.itCodigo = pItem
        AND   tt-dados-itens-romaneio.codRefer = pRefer
        NO-ERROR.
    IF NOT AVAIL tt-dados-itens-romaneio THEN DO:
       FIND ITEM NO-LOCK
        WHERE ITEM.it-codigo = pItem NO-ERROR.

       CREATE tt-dados-itens-romaneio.
       ASSIGN tt-dados-itens-romaneio.id       = IF AVAIL bf THEN bf.id + 1 ELSE 1
              tt-dados-itens-romaneio.itCodigo = pItem    
              tt-dados-itens-romaneio.codRefer = pRefer
              tt-dados-itens-romaneio.descItem = IF AVAIL ITEM  THEN ITEM.desc-item ELSE ''

              .
    END.
    
    ASSIGN idItem = tt-dados-itens-romaneio.id .

    



END PROCEDURE.
  */
