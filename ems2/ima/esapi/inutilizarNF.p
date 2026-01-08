DEFINE INPUT  PARAMETER pCodEstabel AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pSerie      AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pNf         AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pReabrirPed AS LOGICAL     NO-UNDO.

DO ON ERROR UNDO:
   FIND nota-fiscal 
       WHERE nota-fiscal.cod-estabel   = pCodEstabel
       AND nota-fiscal.serie           = pSerie
       AND nota-fiscal.nr-nota-fis     = pNF
       NO-LOCK NO-ERROR.
   IF AVAIL nota-fiscal THEN DO:
      ASSIGN nota-fiscal.idi-sit-nf-eletro = 7.
      IF pReabrirPed THEN DO:
         FIND ped-venda
           WHERE ped-venda.nome-abrev   = nota-fiscal.nome-ab-cli
           AND   ped-venda.nr-pedcli    = nota-fiscal.nr-pedcli
           NO-LOCK NO-ERROR.
         IF AVAIL ped-venda THEN DO:
            ASSIGN ped-venda.cod-sit-ped = 1
                 ped-venda.dsp-pre-fat = YES.
            FOR EACH ped-item OF ped-venda
              WHERE ped-item.cod-sit-item <> 6 :
              ASSIGN ped-item.cod-sit-item = 1
                     ped-item.qt-atendida = 0.
              FOR EACH ped-ent OF ped-item:   
                  ASSIGN ped-ent.cod-sit-ent  = 1
                         ped-ent.qt-atendida  = 0.
              END.
            END.
         END.
      END.
   END.
END.


