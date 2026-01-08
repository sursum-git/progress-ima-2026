DEF TEMP-TABLE tt-work
    FIELD dia AS CHAR
    FIELD mes AS CHAR
    FIELD qtd-prod  AS DEC.

{utp/utapi011.i} /* Gera‡Æo de Graficos */

DEF VAR i-ct AS INT.
DEF VAR i-point AS INT INITIAL 1.
DEF VAR h-utapi011 AS HANDLE NO-UNDO.
DEF VAR i-numsets AS INT INITIAL 0.
    
    EMPTY TEMP-TABLE tt-work.
    DO i-ct = 1 TO 10.
       CREATE tt-work.
       ASSIGN tt-work.dia = string(i-ct) 
              tt-work.mes = '08'
              tt-work.qtd-prod = RANDOM(100,1100).
    END.
    
    EMPTY TEMP-TABLE tt-atributos.
    EMPTY TEMP-TABLE tt-sets.
    EMPTY TEMP-TABLE tt-points-2.
    EMPTY TEMP-TABLE tt-dados.
    EMPTY TEMP-TABLE tt-erros.

    /* Configura‡Æo Geral do Grafico */
    /*                               */
    CREATE tt-atributos.
    ASSIGN tt-atributos.cod-versao-integracao = 3
           tt-atributos.graphtype             = 3
           tt-atributos.graphtitle            = "PRODU€ÇO DIµRIA DE BAGS        Per¡odo: " +  
                                                STRING(TODAY,"99/99/9999") + " a " + STRING(08.31.2008,"99/99/9999") 
           tt-atributos.lefttitle             = 'Quantidade (t)'
           tt-atributos.lefttitlestyle        = 1
           tt-atributos.bottomtitle           = 'D I A S'
           tt-atributos.numgraph              = 1.


    /* Configura‡Æo das Variantes do Grafico (Linhas ou  Barras */
    /*                                                          */
    ASSIGN i-numsets = 1.
    CREATE tt-sets.
    ASSIGN tt-sets.NumSet   = i-numsets 
           tt-sets.NumGraph = 1
           tt-sets.ColorSet = 1
           tt-sets.legendText = "Qtde Produzida (t)".

    ASSIGN i-numsets = i-numsets + 1.

    FOR EACH tt-work WHERE NO-LOCK.
        CREATE tt-points-2.
        ASSIGN tt-points-2.NumPoint  = i-point
               tt-points-2.NumGraph  = 1
               tt-points-2.labeltext = tt-work.dia.

        ASSIGN i-numsets = 1.
        CREATE tt-dados.
        ASSIGN tt-dados.NumPoint   = i-point
               tt-dados.NumSet     = i-numsets
               tt-dados.NumGraph   = 1
               tt-dados.graphdata  = tt-work.qtd-prod.
        ASSIGN i-numsets = i-numsets + 1.

        ASSIGN i-point = i-point + 1.
    END.

    RUN utp/utapi011.p PERSISTENT SET h-utapi011.
    
    RUN pi-execute IN h-utapi011 (INPUT  TABLE tt-atributos,
                                  INPUT  TABLE tt-points-2,
                                  INPUT  TABLE tt-sets,
                                  INPUT  TABLE tt-dados,
                                  INPUT  TABLE tt-ylabels,
                                  OUTPUT TABLE tt-erros).
    
    IF RETURN-VALUE = "NOK" THEN DO.
       FOR EACH tt-erros: 
           DISP cod-erro desc-erro FORMAT "x(100)" WITH 1 COL WIDTH 500. 
       END.
    END.
    
    DELETE PROCEDURE h-utapi011.
