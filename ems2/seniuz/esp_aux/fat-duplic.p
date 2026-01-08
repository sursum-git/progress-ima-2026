DEF VAR c-nr-fatura LIKE fat-duplic.nr-fatura.
REPEAT:
   UPDATE c-nr-fatura.
   IF NOT CAN-FIND(FIRST fat-duplic WHERE fat-duplic.cod-estabel = "2"         
                                      AND fat-duplic.serie       = "1"         
                                      AND fat-duplic.nr-fatura   = c-nr-fatura) THEN DO:
      MESSAGE "Fatura n∆o encontrada!"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      UNDO,RETRY.
   END.
   FOR EACH fat-duplic WHERE fat-duplic.cod-estabel = "2"
                         AND fat-duplic.serie       = "1"
                         AND fat-duplic.nr-fatura   = c-nr-fatura.
       DISPLAY fat-duplic.nome-ab-cli
               fat-duplic.nr-fatura
               fat-duplic.parcela FORMAT "x(3)"
               fat-duplic.dt-vencimen.
       UPDATE fat-duplic.dt-vencimen.
   END.
END.
