/**************************************************************************
Programa: esapi/setDtRecbtoContainersFechados.p 
Autor: Tadeu Silva Parreiras
Objetivo: filtra os container fechados que est∆o sem data de recebimento 
informada. Verifica se existe nota fiscal mae informada e coloca a data da 
nota fiscal como data de recebimento.
Data: 05/2024
Modificacoes:
*****************************************************************************/
        
            
FOR EACH pp-container
    WHERE pp-container.dt-recebimento = ?
    AND situacao = 3.
    FIND FIRST nfs_container 
        WHERE nfs_container.container_id = pp-container.nr-container
        NO-LOCK NO-ERROR.
    IF AVAIL nfs_container THEN DO:
       FIND nota-fiscal NO-LOCK
           WHERE nota-fiscal.cod-estabel = nfs_container.estab
           AND   nota-fiscal.serie       = nfs_container.serie
           AND   nota-fiscal.nr-nota-fis = nfs_container.documento
           AND   nota-fiscal.dt-cancela  = ?
           NO-ERROR.
       IF AVAIL nota-fiscal THEN DO:
          DISP nota-fiscal.dt-emis-nota.
          ASSIGN pp-container.dt-recebimento = nota-fiscal.dt-emis-nota.
       END.                                                             
    END.                                                                
END.
