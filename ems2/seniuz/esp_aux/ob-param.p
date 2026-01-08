/* Programa: ob-param.p
** Mostra/atualiza campo usuar-estab da tabala ob-apram
*/

DEF VAR c-usuar-estab LIKE ob-param.usuar-estab.

DEF TEMP-TABLE tt-usu-estab
    FIELD usuario AS CHAR
    FIELD estab   AS CHAR
    INDEX ch-usu-estab estab usuario.

FIND FIRST ob-param NO-LOCK NO-ERROR.

DEF VAR i-ct AS INT.
DEF VAR c-usuario AS CHAR.
DEF VAR c-cod-estab AS CHAR.

DO i-ct = 1 TO NUM-ENTRIES(ob-param.usuar-estab,",").
   ASSIGN c-usuario = ENTRY(1,ENTRY(i-ct,ob-param.usuar-estab,","),":")
          c-cod-estab = ENTRY(2,ENTRY(i-ct,ob-param.usuar-estab,","),":").

   CREATE tt-usu-estab.
   ASSIGN tt-usu-estab.usuario = c-usuario
          tt-usu-estab.estab   = c-cod-estab.

END.

/* Mostra */
FOR EACH tt-usu-estab BY tt-usu-estab.usuario
                      BY tt-usu-estab.estab:
    DISP tt-usu-estab.usuario FORMAT "x(12)"
         tt-usu-estab.estab.
    ASSIGN c-usuar-estab = IF c-usuar-estab = "" THEN tt-usu-estab.usuario + ':' + tt-usu-estab.estab
                                                 ELSE c-usuar-estab + ',' + 
                                                      tt-usu-estab.usuario + ':' + tt-usu-estab.estab.
END.

/* Exporta para TXT, para conferˆncia */
/*
OUTPUT TO c:/temp/lixo.txt.
PUT UNFORMAT ob-param.usuar-estab SKIP
     LENGTH(ob-param.usuar-estab) SKIP
     c-usuar-estab                SKIP
     LENGTH(c-usuar-estab).
OUTPUT CLOSE.
*/

/* Atualiza */
/*
ASSIGN ob-param.usuar-estab = c-usuar-estab.
*/
