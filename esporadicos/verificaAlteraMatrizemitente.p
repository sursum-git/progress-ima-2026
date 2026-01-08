DEFINE BUFFER bf-emitente FOR emitente.
DEFINE VARIABLE l AS LOGICAL     NO-UNDO INIT NO FORMAT "Sim/N∆o".
UPDATE l LABEL "atualiza".
OUTPUT TO c:\temp\emitenteMatriz.txt.
FOR EACH emitente EXCLUSIVE-LOCK:

    IF emitente.nome-abrev <> emitente.nome-matriz THEN DO:
       FIND FIRST bf-emitente
           WHERE substr(bf-emitente.cgc,1,12)    = SUBSTR(emitente.cgc,1,8) + '0001' 
           NO-LOCK NO-ERROR.
       IF AVAIL bf-emitente THEN DO:
          IF bf-emitente.nome-abrev <> emitente.nome-matriz THEN DO:
             EXPORT DELIMITER "|" "matriz incorreta" 
             emitente.cod-emitente emitente.nome-abrev emitente.nome-matriz
             bf-emitente.nome-abrev .
             IF l THEN
                ASSIGN emitente.nome-matriz = bf-emitente.nome-abrev.
          END.
          ELSE DO:
               EXPORT DELIMITER "|" "matriz correta" 
             emitente.cod-emitente emitente.nome-abrev emitente.nome-matriz
             bf-emitente.nome-abrev .

          END.
       END.
       ELSE DO:
           EXPORT DELIMITER "|" "matriz n∆o encontrada" 
             emitente.cod-emitente emitente.nome-abrev emitente.nome-matriz.
             ASSIGN emitente.nome-matriz = emitente.nome-abrev.
       END.
    END. 
END.
