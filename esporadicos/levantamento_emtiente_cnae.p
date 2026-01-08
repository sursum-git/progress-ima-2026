DEFINE BUFFER bf FOR emitente_cnae.
DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.
DEFINE VARIABLE iSit AS INTEGER     NO-UNDO.
OUTPUT TO c:\temp\analise.txt.
FOR EACH emitente NO-LOCK:
    
    FIND FIRST emitente_cnae
         WHERE emitente_cnae.cod_emitente = emitente.cod-emitente NO-LOCK NO-ERROR.
    IF NOT AVAIL emitente_cnae THEN NEXT.
    PUT emitente.cod-emitente "|".
    FIND FIRST emitente_cnae 
      WHERE emitente_cnae.cod_emitente = emitente.cod-emitente
      AND emitente_cnae.cod_tipo_cnae = 1 NO-LOCK NO-ERROR.
     IF NOT AVAIL emitente_cnae THEN DO:
        PUT 'n∆o tem cnae primario|'.
        ASSIGN iSit = 0.
     END.
     ELSE DO:
        PUT 'tem cnae primario|'.
        ASSIGN iSit = 1.
     END.
     FIND FIRST emitente_cnae
         WHERE emitente_cnae.cod_tipo_cnae = 0
         AND emitente_cnae.cod_emitente = emitente.cod-emitente EXCLUSIVE-LOCK NO-ERROR.
     IF AVAIL emitente_cnae THEN DO:
         PUT "tem cnae 0|".
         IF iSit = 1 THEN DO:
            ASSIGN emitente_cnae.cod_tipo_cnae = 2
                   isit = 9.
         END.
            
     END.
     ELSE DO:
         PUT 'n∆o tem cnae 0|'.
     END.
     
     ASSIGN iCont = 0.
     FOR EACH emitente_cnae
         WHERE emitente_cnae.cod_emitente = emitente.cod-emitente NO-LOCK.
        ASSIGN iCont = iCont + 1.
     END.

     PUT iCont "|" iSit.
     PUT SKIP.
END.
/*FOR EACH emitente_cnae EXCLUSIVE-LOCK
        WHERE  emitente_cnae.cod_tipo_cnae = 0.
     /*FIND FIRST bf 
         WHERE bf.cod_emitente = emitente_cnae.cod_emitente
         AND emitente_cnae.cod_tipo_cnae = 1 NO-LOCK NO-ERROR.
     IF AVAIL bf THEN
        ASSIGN emitente_cnae.cod_tipo_cnae = 2.  
     ELSE
        DISP emitente_cnae.cod_emitente . */


     
END.*/
