OUTPUT TO c:\temp\nf_repres_505.txt.
FOR EACH nota-fiscal  NO-LOCK
    WHERE nota-fiscal.cod-estabel = '5'
    AND   nota-fiscal.serie = '3'
    AND   dt-emis-nota  >=09.22.2023:
    IF can-find(FIRST fat-repre 
        WHERE fat-repre.cod-estabel = nota-fiscal.cod-estabel
        AND   fat-repre.serie       = nota-fiscal.serie
        AND   fat-repre.nr-fatura   = nota-fiscal.nr-fatura) THEN
        NEXT.
    
    FOR EACH fat-duplic NO-LOCK
        WHERE fat-duplic.cod-estabel = nota-fiscal.cod-estabel
        AND   fat-duplic.serie       = nota-fiscal.serie
        AND   fat-duplic.nr-fatura   = nota-fiscal.nr-fatura .
        ASSIGN fat-duplic.flag-atualiz  = NO.

    END.

    FIND FIRST ped-venda 
        WHERE ped-venda.nr-pedido =  int(nota-fiscal.nr-pedcli)
        NO-LOCK NO-ERROR.
    IF AVAIL ped-venda THEN DO:                                
       DISP nota-fiscal.nr-nota-fis.
       FOR EACH ped-repre OF ped-venda:
           CREATE fat-repre.
           ASSIGN fat-repre.cod-estabel = ped-venda.cod-estabel
                  fat-repre.serie       = nota-fiscal.serie
                  fat-repre.nr-fatura   = nota-fiscal.nr-fatura
                  fat-repre.perc-comis  = ped-repre.perc-comis
                  fat-repre.comis-emis  = 0
                  fat-repre.idi-tip-comis-agent = 1
                  fat-repre.idi-tip-comis-agent = 1
                  fat-repre.idi-liber-pagto-comis-agent = 1
                  fat-repre.nome-ab-rep  = ped-repre.nome-ab-rep .

       END.
    END.

END.
