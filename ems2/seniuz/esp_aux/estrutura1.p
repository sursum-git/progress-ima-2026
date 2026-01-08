DEF VAR c-item LIKE saldo-estoq.it-codigo.

FOR EACH ref-estrut WHERE
         ref-estrut.it-codigo = '501823' AND
         ref-estrut.cod-ref-it = '2365782' NO-LOCK.


    FIND estrutura WHERE
         estrutura.it-codigo = ref-estrut.it-codigo AND
         estrutura.es-codigo = ref-estrut.es-codigo.

    IF estrutura.fantasma THEN 
       RUN pi-ver-estrutura (estrutura.es-codigo). 
    ELSE DO.
        FIND item WHERE
             item.it-codigo = estrutura.es-codigo NO-LOCK NO-ERROR.
        IF item.ge-codigo = 50 THEN
           ASSIGN c-item = estrutura.es-codigo.
    END.
END.

PROCEDURE pi-ver-estrutura.
    DEF INPUT PARAMETER p-it-codigo AS CHAR.
    FOR EACH estrutura WHERE
             estrutura.it-codigo = p-it-codigo NO-LOCK.

        IF estrutura.fantasma THEN
           RUN pi-ver-estrutura (estrutura.es-codigo).
        ELSE DO.
            FIND item WHERE
                 item.it-codigo = estrutura.es-codigo NO-LOCK NO-ERROR.
            IF item.ge-codigo = 50 THEN
               ASSIGN c-item = estrutura.es-codigo.
        END.
    END.
END PROCEDURE.


DISP c-item.
