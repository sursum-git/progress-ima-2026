/*
programa:esbo/boPermisContainer
objetivo: Manter as informaá‰es da tabela de permiss∆o de container.
03/2022
*/
{esp/util.i}
DEFINE VARIABLE nrContainer AS INTEGER     NO-UNDO.
DEFINE VARIABLE codRepres   AS INTEGER     NO-UNDO.
/*DEFINE VARIABLE cListaContainers AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cListaRepres     AS CHARACTER   NO-UNDO.*/


PROCEDURE setContainer:

    DEFINE INPUT  PARAMETER pContainer AS INTEGER     NO-UNDO.
    ASSIGN nrContainer = pContainer.

END PROCEDURE.

PROCEDURE setCodRep:

    DEFINE INPUT  PARAMETER pRepres AS INTEGER     NO-UNDO.
    ASSIGN codRepres = pRepres.


END PROCEDURE.


PROCEDURE getContainersRepres:
    
    DEFINE OUTPUT PARAMETER cLista AS CHARACTER   NO-UNDO.

    FOR EACH pp_container_permissao NO-LOCK
        WHERE pp_container_permissao.cod_repres = codRepres ,
        EACH pp-container 
        WHERE pp-container.nr-container = pp_container_permissao.nr_container
        AND   pp-container.situacao = 1.

        RUN incrValor(INPUT-OUTPUT cLista, 
                      INPUT string(pp-container.nr-container),
                      ',').
    END.

END PROCEDURE.

PROCEDURE getRepresContainer:

    DEFINE OUTPUT PARAMETER cLista AS CHARACTER   NO-UNDO.

    FOR EACH pp_container_permissao NO-LOCK
        WHERE pp_container_permissao.nr_container = nrContainer ,
        EACH pp-container 
        WHERE pp-container.nr-container = pp_container_permissao.nr_container
        AND   pp-container.situacao = 1.

        RUN incrValor(INPUT-OUTPUT cLista, 
                      INPUT string(pp_container_permissao.cod_repres),
                      ',').
    END.


END PROCEDURE.




