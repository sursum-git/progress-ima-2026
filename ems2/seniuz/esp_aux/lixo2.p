FOR EACH emitente WHERE emitente.identific <> 2.
    IF INDEX(emitente.endereco,",") <> 0 AND SUBSTR(emitente.endereco,INDEX(emitente.endereco,",") + 1,1) <> " " THEN
       UPDATE emitente.endereco.
END.
