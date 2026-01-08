{esbo/boMovtoEstoqEtq.i}

FOR FIRST nota-fiscal FIELDS(nr-pedcli nome-abrev cod-estabel serie nr-nota-fis) NO-LOCK
        WHERE nota-fiscal.cod-estabel = cEstab
        AND   nota-fiscal.serie       = cSerie
        AND   nota-fiscal.nr-nota-fis = cNrNotaFis,
        EACH it-nota-fisc  
        WHERE it-nota-fisc.cod-estabel  = nota-fiscal.cod-estabel
        AND   it-nota-fisc.serie        = nota-fiscal.serie
        AND   it-nota-fisc.nr-nota-fis  = nota-fiscal.nr-nota-fis
        AND   it-nota-fisc.nr-seq-fat = nrSeq NO-LOCK, 
        EACH ped-item-res FIELDS(cod-estabel serie nr-nota-fis nr-sequencia) 
        WHERE ped-item-res.cod-estabel  = nota-fiscal.cod-estabel 
        AND   ped-item-res.serie        = nota-fiscal.serie 
        AND   ped-item-res.nr-nota-fis  = INT(nota-fiscal.nr-nota-fis) 
       // AND   ped-item-res.nr-seq-fat   = it-nota-fisc.nr-seq-fat 
        AND   ped-item-res.it-codigo    = it-nota-fisc.it-codigo
        AND   ped-item-res.cod-refer    = it-nota-fisc.cod-refer
        AND   ped-item-res.faturado    = YES NO-LOCK,
        EACH ped-item-rom FIELDS(cod-estabel num-etiqueta quantidade )
            WHERE ped-item-rom.cod-estabel = nota-fiscal.cod-estabel
            AND   ped-item-rom.nome-abrev  = nota-fiscal.nome-ab-cli
            AND   ped-item-rom.nr-pedcli   = nota-fiscal.nr-pedcli
            AND   ped-item-rom.nr-sequencia= ped-item-res.nr-sequencia
            NO-LOCK,                 
        EACH ob-etiqueta FIELDS(num-etiqueta cod-estabel nr-container quantidade) NO-LOCK
           WHERE ob-etiqueta.cod-estabel   = nota-fiscal.cod-estabel
           AND   ob-etiqueta.num-etiqueta  = ped-item-rom.num-etiqueta  :   
            CREATE ttEtq.
             ASSIGN ttEtq.cod-estabel  = nota-fiscal.cod-estabel
                    ttEtq.num-etiqueta = ob-etiqueta.num-etiqueta
                    ttEtq.nr-container = ob-etiqueta.nr-container
                    ttEtq.quantidade   = ob-etiqueta.quantidade .  
           
           
END.   

 {esp/exportarTabelaCsv3.i ttEtq " " " " "ttEtq"  } 
