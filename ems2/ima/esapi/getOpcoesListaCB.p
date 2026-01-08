DEFINE INPUT  PARAMETER iLista AS INT.
DEFINE OUTPUT PARAMETER cLista AS CHARACTER   NO-UNDO FORMAT 'x(500)'.

DEFINE VARIABLE incr AS CHARACTER   NO-UNDO FORMAT 'x(50)'.
FOR EACH opcoes_lista
    WHERE opcoes_lista.lista_id = iLista NO-LOCK.
   
    ASSIGN  incr= opcoes_lista.desc_opcao + "," + opcoes_lista.cod_opcao.
            cLista = IF cLista = '' THEN incr ELSE cLista + ',' + incr .
END.
