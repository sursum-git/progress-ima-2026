/**********************************************************************************************
programa: esbo/boSaldo.p
objetivo: retornar um tabela temporaria com codigo e descri‡Æo de itens
que tenham saldo no pi ou pe par atender a necessidade do BOT.
IMPORTANTE:nÆo sÆo tratadas aqui permissäes especificas de representantes no caso do PI
e no caso do PE nÆo ‚ avaliado aloca‡Æo, mas apenas se existe saldo em estoque.
Desenv: Tadeu
data: 08/2025
**********************************************************************************************/
{esp/util.i}

DEFINE VARIABLE nrContainerIni      AS INTEGER NO-UNDO.
DEFINE VARIABLE nrContainerFim      AS INTEGER NO-UNDO.

DEFINE VARIABLE cListaPermContainer AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hBoLocaisEstoq      AS HANDLE      NO-UNDO.

DEFINE TEMP-TABLE ttLocais NO-UNDO LIKE locais_estoq_portal .

{esbo/boSaldo2.i}
/*DEFINE TEMP-TABLE ttPermisContainer
    FIELD nrContainer AS INT. */

PROCEDURE iniciar:

    
    IF NOT VALID-HANDLE(hBoLocaisEstoq) THEN DO:
       RUN esbo/boWeb100.p PERSIST SET hBoLocaisEstoq.
       RUN iniciar IN hBoLocaisEstoq.
    END.



END PROCEDURE.

PROCEDURE finalizar:      

     IF VALID-HANDLE(hBoLocaisEstoq) THEN DO:
      RUN finalizar IN hBoLocaisEstoq.
    END.
    
    DELETE OBJECT THIS-PROCEDURE.

END PROCEDURE.

PROCEDURE limparTtS:

    
    EMPTY TEMP-TABLE ttEstabDepos.
    EMPTY TEMP-TABLE ttItens.
    //EMPTY TEMP-TABLE ttPermisContainer.

END PROCEDURE.

PROCEDURE inserirEstabDepos:

    DEFINE INPUT  PARAMETER pEstab AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pDepos AS CHARACTER   NO-UNDO.
    CREATE ttEstabDepos.
    ASSIGN ttEstabDepos.codEstab = pEstab
           ttEstabDepos.codDepos = pDepos .
END PROCEDURE.

PROCEDURE setParamsLocaisEstoque:

    RUN setValsIni  IN hBoLocaisEstoq.
    RUN exec        IN hBoLocaisEstoq.
    RUN getTtResult IN hBoLocaisEstoq(OUTPUT TABLE ttLocais).
    EMPTY TEMP-TABLE ttEstabDepos.
    FOR EACH ttLocais:
        RUN inserirEstabDepos(ttLocais.cod_estab,ttLocais.cod_depos).
    END.

END PROCEDURE. 

PROCEDURE setTTItensFaturaveis:

    FOR EACH ITEM fields(it-codigo desc-item ind-item-fat ge-codigo )NO-LOCK
    WHERE ind-item-fat = YES
    AND ITEM.ge-codigo >= 50 AND ge-codigo <= 69:
        CREATE ttItens.
        ASSIGN ttItens.itCodigo     = ITEM.it-codigo
               ttItens.descItem    = ITEM.desc-item.
    END.


END PROCEDURE.

PROCEDURE getSaldo:
    DEFINE VARIABLE dSaldo        AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dSaldoSemPerc AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dPedida       AS DECIMAL     NO-UNDO.    
    
    FOR EACH pp-container FIELDS(nr-container situacao) NO-LOCK
    WHERE pp-container.situacao = 1 :
        FOR EACH pp-it-container FIELDS(it-codigo qt-pedida perc-dsp-vend qt-vendida nr-container) OF pp-container            
            WHERE pp-it-container.qt-pedida * pp-it-container.perc-dsp-vend / 100 - pp-it-container.qt-vendida > 0
            BREAK BY pp-it-container.it-codigo.
            IF FIRST-OF(pp-it-container.it-codigo) THEN
            DO:
                FIND ttItens
                WHERE ttItens.itCodigo = pp-it-container.it-codigo
                NO-ERROR.
                IF AVAIL ttItens THEN DO:
                   ASSIGN ttItens.logPI = YES .                                   
                END.                                           
            END.            
        END.
    END.   
       
    FOR EACH ttEstabDepos,
        EACH saldo-estoq FIELDS(qtidade-atu it-codigo cod-estab cod-depos ) NO-LOCK
        WHERE saldo-estoq.cod-estab  = ttEstabDepos.codEstab
        AND   saldo-estoq.cod-depos  = ttEstabDepos.codDepos       
        BREAK BY saldo-estoq.it-Codigo.
        IF FIRST-OF(saldo-estoq.it-codigo) THEN
        DO: 
            FIND ttItens
            WHERE ttItens.itCodigo = saldo-estoq.it-codigo NO-ERROR.  
            IF AVAIL ttItens THEN
            DO:
                ASSIGN ttItens.logPE = YES.        
            END.
            
        END. 
    END.        
    
    //limpa os sem saldo
    FOR EACH ttItens
    WHERE ttItens.logPi = NO AND ttItens.logPe = NO:
        DELETE ttItens.
    END.
    
    
    
    
END PROCEDURE. 


PROCEDURE getTTItens:
    DEFINE OUTPUT PARAMETER TABLE FOR ttItens.

END PROCEDURE.


