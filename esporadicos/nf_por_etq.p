DEFINE TEMP-TABLE ttNfItemContainer
    FIELD codEstab      AS CHAR
    FIELD serie         AS CHAR
    FIELD nrNotaFis     AS CHAR
    FIELD itCodigo      AS CHAR
    FIELD codRefer      AS CHAR
    FIELD nrContainer   AS INT
    FIELD quantidade    AS DECIMAL
    .

DEFINE TEMP-TABLE ttNfItemContainerEtq NO-UNDO LIKE ttNfItemContainer
    FIELD numEtq    AS INT.


{esp/util.i}
    
FOR EACH nota-fiscal NO-LOCK
    WHERE nota-fiscal.dt-emis-nota >=  07.01.2024
    AND NOTA-fiscal.dt-cancela = ? AND
    CAN-FIND (FIRST ped-venda
        WHERE ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
        AND  ped-venda.nr-pedcli   = nota-fiscal.nr-pedcli  )
        .
    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.
       FIND ped-item
           WHERE ped-item.nome-abrev   = it-nota-fisc.nome-ab-cli
           AND   ped-item.nr-pedcli    = it-nota-fisc.nr-pedcli
           AND   ped-item.it-codigo    = it-nota-fisc.it-codigo
           AND   ped-item.cod-refer    = it-nota-fisc.cod-refer
           AND   ped-item.nr-sequencia = it-nota-fisc.nr-seq-ped
           //AND int(ped-item.cod-entrega)  = it-nota-fisc.nr-entrega
           NO-LOCK NO-ERROR.
       IF AVAIL ped-item THEN DO:
          FOR EACH ped-item-rom
              where ped-item.nome-abrev   = ped-item-rom.nome-abrev   
              AND   ped-item.nr-pedcli    = ped-item-rom.nr-pedcli    
              AND   ped-item.nr-sequencia = ped-item-rom.nr-sequencia 
              NO-LOCK.

              FOR EACH ob-etiqueta
                  WHERE ob-etiqueta.cod-estabel  =  ped-item-rom.cod-estabel
                  AND   ob-etiqueta.num-etiqueta =  ped-item-rom.num-etiqueta NO-LOCK.

                  FIND ttNfItemContainer
                  WHERE ttNfItemContainer.codEStab     = nota-fiscal.cod-estabel
                  AND   ttNfItemContainer.serie        = nota-fiscal.serie
                  AND   ttNfItemContainer.nrNotaFis    = nota-fiscal.nr-nota-fis
                  AND   ttNfItemContainer.itCodigo     = it-nota-fisc.it-codigo
                  AND   ttNfItemContainer.codRefer     = it-nota-fisc.cod-refer
                  AND   ttNfItemContainer.nrContainer  = ob-etiqueta.nr-container NO-LOCK NO-ERROR.
                  IF NOT AVAIL ttNfItemContainer THEN DO:
                     CREATE ttNfItemContainer.
                     ASSIGN 
                     ttNfItemContainer.codEStab     = nota-fiscal.cod-estabel   
                     ttNfItemContainer.serie        = nota-fiscal.serie         
                     ttNfItemContainer.nrNotaFis    = nota-fiscal.nr-nota-fis   
                     ttNfItemContainer.itCodigo     = it-nota-fisc.it-codigo    
                     ttNfItemContainer.codRefer     = it-nota-fisc.cod-refer    
                     ttNfItemContainer.nrContainer  = ob-etiqueta.nr-container.
                  END.
                  ASSIGN ttNfItemContainer.quantidade = ttNfItemContainer.quantidade + ob-etiqueta.quantidade .



                  FIND ttNfItemContainerEtq
                  WHERE ttNfItemContainerEtq.codEStab     = nota-fiscal.cod-estabel
                  AND   ttNfItemContainerEtq.serie        = nota-fiscal.serie
                  AND   ttNfItemContainerEtq.nrNotaFis    = nota-fiscal.nr-nota-fis
                  AND   ttNfItemContainerEtq.itCodigo     = it-nota-fisc.it-codigo
                  AND   ttNfItemContainerEtq.codRefer     = it-nota-fisc.cod-refer
                  AND   ttNfItemContainerEtq.nrContainer  = ob-etiqueta.nr-container
                  AND   ttNfItemContainerEtq.numEtq       = ob-etiqueta.num-etiqueta NO-LOCK NO-ERROR.
                  IF NOT AVAIL ttNfItemContainerEtq THEN DO:
                     CREATE ttNfItemContainerEtq.
                     ASSIGN ttNfItemContainerEtq.codEStab     = nota-fiscal.cod-estabel 
                            ttNfItemContainerEtq.serie        = nota-fiscal.serie       
                            ttNfItemContainerEtq.nrNotaFis    = nota-fiscal.nr-nota-fis 
                            ttNfItemContainerEtq.itCodigo     = it-nota-fisc.it-codigo  
                            ttNfItemContainerEtq.codRefer     = it-nota-fisc.cod-refer  
                            ttNfItemContainerEtq.nrContainer  = ob-etiqueta.nr-container
                            ttNfItemContainerEtq.numEtq       = ob-etiqueta.num-etiqueta
                            ttNfItemContainer.quantidade      = ob-etiqueta.quantidade.                                                         
                  END.
              END.
          END.
       END.
    END.
END.

{esp/exportarTabelaCsv3.i ttNfItemContainer " " " " "ttNfItemContainer"}

{esp/exportarTabelaCsv3.i ttNfItemContainerEtq " " " " "ttNfItemContainerEtq"}
