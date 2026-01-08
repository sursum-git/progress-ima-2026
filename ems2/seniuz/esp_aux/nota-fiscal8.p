DEF VAR l-copiar AS LOG FORMAT "Sim/NÆo".
FIND nota-fiscal WHERE nota-fiscal.cod-estabel = '2'
                   AND nota-fiscal.serie       = '3'
                   AND nota-fiscal.nr-nota-fis = '0008668'.
DISPLAY nota-fiscal.endereco       
        nota-fiscal.cidade        
        nota-fiscal.bairro        
        nota-fiscal.cep           
        WITH SIDE-LABELS 1 COLUMN.
FIND emitente WHERE emitente.cod-emitente = nota-fiscal.cod-emitente NO-LOCK.
DISPLAY emitente.endereco
        emitente.cidade
        emitente.bairro
        emitente.cep
        WITH SIDE-LABELS 1 COLUMN.
REPEAT:
   UPDATE l-copiar.
   IF l-copiar THEN DO:
      ASSIGN nota-fiscal.endereco = emitente.endereco 
             nota-fiscal.cidade   = emitente.cidade   
             nota-fiscal.bairro   = emitente.bairro   
             nota-fiscal.cep      = emitente.cep.     
   END.
   LEAVE.
END.
