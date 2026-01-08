OUTPUT TO c:/temp/rep-4-154.txt.
FOR EACH emitente WHERE emitente.cod-rep = 4 EXCLUSIVE-LOCK.
    DISP emitente.cod-emitente
         emitente.nome-abrev
         emitente.cod-rep
         "Mudado para: 154" LABEL "Mudado".
    /*ASSIGN emitente.cod-rep = 154.*/
END.
OUTPUT CLOSE.
