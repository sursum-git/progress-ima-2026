OUTPUT TO c:\temp\docum-est.txt NO-CONVERT .
PUT "estab|dt.emissao|nf|cod.emitente|nome Emitetne|serie|Valor Total|Container|Nat.Opera‡Æo" SKIP.
FOR EACH docum-est
    WHERE docum-est.nat-operacao >= '31201'
    AND  docum-est.nat-operacao <=  '31205z' NO-LOCK:
    FIND emitente OF docum-est NO-LOCK NO-ERROR.
    FIND nfs_container 
        WHERE  nfs_container.cod_emitente   = emitente.cod-emitente
        AND    nfs_container.estab          = docum-est.cod-estabel
        AND    nfs_container.serie          = docum-est.serie-docto
        AND    nfs_container.documento      = docum-est.nro-docto
        AND    nfs_container.nat_operacao   = docum-est.nat-operacao
        NO-LOCK NO-ERROR .

    EXPORT DELIMITER "|" docum-est.cod-estabel
         docum-est.dt-emissao
         docum-est.nro-docto
         docum-est.cod-emitente
         emitente.nome-emit
         docum-est.serie-docto
         docum-est.tot-valor 
         IF AVAIL nfs_container THEN string(nfs_container.container_id) ELSE "NÆo Lan‡ada no M¢dulo de importa‡Æo"
         docum-est.nat-operacao  .

END.

OUTPUT CLOSE.
