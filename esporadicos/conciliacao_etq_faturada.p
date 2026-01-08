DEFINE TEMP-TABLE tt
    FIELD nrNotaFis AS CHAR
    FIELD numEtiq   AS INT
    FIELD situacao  AS INT
    FIELD qtEtq     AS DECIMAL.
    
FOR EACH movto-estoq NO-LOCK
    WHERE movto-estoq.cod-estabel = '505'
    AND  movto-estoq.dt-trans     >= 08.30.2023
    AND  movto-estoq.dt-trans     <= 09.11.2023 
    AND  movto-estoq.cod-depos     = 'ITA'
    AND  movto-estoq.it-codigo     = '945008'
    AND  movto-estoq.cod-refer     = 'B01' 
    AND  movto-estoq.esp-docto     =  22 .
    FIND nota-fiscal NO-LOCK
        WHERE nota-fiscal.cod-estabel   = movto-estoq.cod-estabel
        AND   nota-fiscal.serie         = movto-estoq.serie
        AND   nota-fiscal.nr-nota-fis   = movto-estoq.nro-docto
        AND   nota-fiscal.dt-cancel    = ?
        NO-ERROR .
    IF AVAIL nota-fiscal THEN DO:
       FIND ped-venda NO-LOCK
           WHERE ped-venda.nome-abrev   = nota-fiscal.nome-ab-cli
           AND   ped-venda.nr-pedcli    = nota-fiscal.nr-pedcli
           NO-ERROR.
       IF AVAIL ped-venda THEN DO:
          FOR EACH ped-item-rom OF ped-venda NO-LOCK:
              FIND ob-etiqueta NO-LOCK
                  WHERE ob-etiqueta.cod-estabel   = ped-venda.cod-estabel
                  AND   ob-etiqueta.num-etiqueta  = ped-item-rom.num-etiqueta
                  NO-ERROR.
              IF AVAIL ob-etiqueta THEN DO:
                 CREATE tt.
                 ASSIGN tt.nrNotaFis = movto-estoq.nro-docto
                        tt.numEtiq   = ob-etiqueta.num-etiqueta
                        tt.situacao  = ob-etiqueta.situacao
                        tt.qtEtq     = ob-etiqueta.quantidade.
              END.
              ELSE DO:
                  MESSAGE 'etq nao encontrada'
                      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

              END.
          END.

       END.
       ELSE DO:
           /*MESSAGE 'nao achou pedido' SKIP
               movto-estoq.cod-estabel SKIP
               movto-estoq.serie SKIP
               movto-estoq.nro-docto SKIP
               movto-estoq.nat-operacao
               VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
       END.
    END.   

END.
OUTPUT TO c:\temp\etq.txt.
FOR EACH tt:
    EXPORT DELIMITER "|" tt.

END.

OUTPUT CLOSE.
