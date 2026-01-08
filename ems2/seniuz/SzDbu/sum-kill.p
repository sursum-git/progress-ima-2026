DEF VAR c-comando AS CHAR FORMAT "x(30)".
DEF VAR c-dlc     AS CHAR FORMAT "x(20)".
DEF VAR c-arquivo AS CHAR FORMAT "x(30)".

INPUT THROUGH echo $DLC NO-ECHO.  /* o echo tem que ser minusculo pois nesse */
   SET c-dlc.                     /* caso ‚ um comando linux e nÆo progress */
INPUT CLOSE. 

ASSIGN c-arquivo = SESSION:TEMP-DIRECTORY + "sum-kill.tmp".

FOR EACH DICTDB._user WHERE
         DICTDB._user._disable = YES AND
         DICTDB._user._password = ENCODE('out').

    OUTPUT TO VALUE(c-arquivo) APPEND.
       PUT STRING(TODAY,"99/99/9999") + " " + STRING(TIME,"HH:MM:SS")
           "Desconectando: " 
           DICTDB._user._user-name 
           DICTDB._user._userid
           SKIP.
    OUTPUT CLOSE.

    ASSIGN c-comando = c-dlc + '/bin/proshut ' + PDBNAME('DICTDB') + '.db -C disconnect ' + STRING(DICTDB._user._userid) + ' >>' + c-arquivo.

    OS-COMMAND SILENT VALUE(c-comando). 

    DELETE DICTDB._user.
END.




