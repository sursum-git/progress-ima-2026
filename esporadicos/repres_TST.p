DEFINE VARIABLE iCont AS INT   NO-UNDO.
    
DEFINE VARIABLE cRepres AS CHARACTER   NO-UNDO.
ASSIGN cRepres = 'repTST'.
FOR EACH repres
    WHERE repres.cod-rep = 14360.
    FOR EACH ped-venda OF repres.
        FOR EACH ped-repre OF ped-venda:
            ASSIGN ped-repre.nome-ab-rep = cRepres.

        END.
        ASSIGN ped-venda.no-ab-reppri = cRepres.

    END.
    FOR EACH nota-fiscal OF repres:
        FOR EACH fat-repre 
            WHERE fat-repre.cod-estabel = nota-fiscal.cod-estabel
            AND   fat-repre.serie   = nota-fiscal.serie
            AND  fat-repre.nr-fatura = nota-fiscal.nr-fatura.
            ASSIGN fat-repre.nome-ab-rep = cRepres.
           
        END.
        ASSIGN nota-fiscal.no-ab-reppri = cRepres.

    END.
    ASSIGN iCont = 0.
    FOR EACH emitente 
        WHERE emitente.cod-rep = repres.cod-rep.
        ASSIGN iCont = iCont + 1.
        ASSIGN emitente.nome-emit = "cliente teste" + STRING(iCont).


    END.
    ASSIGN repres.nome = 'repres teste'
           repres.nome-abrev = cRepres.
END.
