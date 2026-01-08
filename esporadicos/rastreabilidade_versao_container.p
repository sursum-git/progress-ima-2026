DEFINE STREAM sContainer.
DEFINE STREAM sVersoes.
DEFINE STREAM scustos.
DEFINE STREAM sDescontos.
DEFINE STREAM sDespesas.
DEFINE STREAM sNfs.
DEFINE STREAM sPgtos.

OUTPUT STREAM sContainer TO c:\temp\container.txt.
OUTPUT STREAM sVersoes   TO c:\temp\versaoCusto.txt.
OUTPUT STREAM sCustos    TO c:\temp\custosContainer.txt.
OUTPUT STREAM sDescontos TO c:\temp\descontosContainer.txt.
OUTPUT STREAM sDespesas  TO c:\temp\despesasContainer.txt.
OUTPUT STREAM sNfs       TO c:\temp\Nfscontainer.txt.
OUTPUT STREAM sPgtos     TO c:\temp\PgtosContainer.txt.
FIND FIRST pp-container
    WHERE pp-container.nr-container = 152816 NO-LOCK NO-ERROR.
EXPORT STREAM sContainer DELIMITER "|" pp-container.


FOR EACH versoes_item_custo_container NO-LOCK
WHERE container_id = pp-container.nr-container:
    EXPORT STREAM sVersoes DELIMITER  "|" versoes_item_custo_container. 
    FOR EACH ITEM_container_custos OF versoes_item_custo_container NO-LOCK:
        EXPORT STREAM sCustos DELIMITER "|" ITEM_container_custos .  
    END.
    FOR EACH ITEM_container_custo_descontos OF versoes_item_custo_container NO-LOCK:
        EXPORT STREAM sDescontos DELIMITER "|"  ITEM_container_custo_descontos.
    END.
    FOR EACH ITEM_container_custo_despesas OF versoes_item_custo_container NO-LOCK:
        EXPORT STREAM sDespesas DELIMITER "|"  ITEM_container_custo_despesas.
    END.
    FOR EACH ITEM_container_custo_nfs OF versoes_item_custo_container NO-LOCK:
        EXPORT STREAM sNfs  DELIMITER "|"  ITEM_container_custo_nfs.
    END.
    FOR EACH ITEM_container_custo_pgtos OF versoes_item_custo_container NO-LOCK:
        EXPORT STREAM sPgtos DELIMITER "|" ITEM_container_custo_pgtos.
    END.
END.
OUTPUT STREAM sContainer CLOSE.
OUTPUT STREAM sVersoes   CLOSE.
OUTPUT STREAM sCustos    CLOSE.
OUTPUT STREAM sDescontos CLOSE.
OUTPUT STREAM sDespesas  CLOSE.
OUTPUT STREAM sNfs       CLOSE.
OUTPUT STREAM sPgtos     CLOSE.





