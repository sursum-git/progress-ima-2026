FOR EACH estrutura WHERE estrutura.es-codigo    >= "ctfou03110"
                     AND estrutura.es-codigo    <= "ctfou03110"
                     AND estrutura.data-inicio  >= 01/01/0001
                     AND estrutura.data-inicio  <= 12/31/9999
                     AND estrutura.data-termino >= 01/01/0001
                     AND estrutura.data-termino <= 12/31/9999
                   NO-LOCK:
    DISP estrutura.es-codigo
         estrutura.it-codigo
         estrutura.sequencia.
END.
