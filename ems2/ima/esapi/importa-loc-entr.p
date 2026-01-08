DEFINE TEMP-TABLE tt-loc-entr LIKE loc-entr.
DEFINE TEMP-TABLE tt-aux LIKE loc-entr.

DEF BUFFER empresa FOR mgadm.empresa.

DEFINE VAR c-arq-log           AS CHAR.

DEF INPUT PARAMETER TABLE FOR tt-loc-entr.

ASSIGN c-arq-log = "\\ima-srv-file\publico\integra-loc-entrega" + STRING(TODAY,"99-99-9999") + ".txt".

OUTPUT TO VALUE(c-arq-log) APPEND.
    PUT UNFORMATTED "Iniciando Integra‡Æo " AT 7
        NOW
        SKIP.
OUTPUT CLOSE.

FIND FIRST tt-loc-entr NO-LOCK NO-ERROR.
FOR EACH loc-entr WHERE
         loc-entr.nome-abrev = tt-loc-entr.nome-abrev EXCLUSIVE-LOCK.

    CREATE tt-aux.
    BUFFER-COPY loc-entr TO tt-aux.

    DELETE loc-entr.
END.
FOR EACH tt-aux NO-LOCK.
    CREATE loc-entr.
    BUFFER-COPY tt-aux TO loc-entr. 
END.

OUTPUT TO VALUE(c-arq-log) APPEND.
   PUT UNFORMATTED "Local de Entrega Integrado " AT 7
       NOW
       SKIP.
OUTPUT CLOSE.
    
/* fim de programa */




