DEF VAR c-comando AS CHAR FORMAT "x(30)".
DEF VAR c-dlc AS CHAR FORMAT "x(20)".
DEF VAR c-arquivo AS CHAR FORMAT "x(30)".

/*
INPUT THROUGH ECHO $DLC NO-ECHO.
   SET c-dlc. 
INPUT CLOSE. 
*/
ASSIGN c-dlc = "/usr/dlc10".

FOR EACH DICTDB._user WHERE
         DICTDB._user._disable = YES AND
         DICTDB._user._password = ENCODE('out').

    ASSIGN c-arquivo = SESSION:TEMP-DIRECTORY + "sum-kill.tmp".
    ASSIGN c-comando = c-dlc + '/bin/proshut ' + PDBNAME('DICTDB') + '.db -C disconnect ' + STRING(DICTDB._user._userid) + ' >>' + c-arquivo.

    OS-COMMAND SILENT VALUE(c-comando). 

END.







/*
# Executa o programa de Atualizacao da Tabela...
${DLC}/bin/_progres -pf cdcom.pf  -U ${USER} -P ${PASS} \
                    -pf movcom.pf -U ${USER} -P ${PASS} \
                    -pf cdind.pf  -U ${USER} -P ${PASS} \
                    -pf movind.pf -U ${USER} -P ${PASS} \
                    -pf cdadm.pf  -U ${USER} -P ${PASS} \
                    -pf movadm.pf -U ${USER} -P ${PASS} \
                    -pf mgmp.pf \
                    -pf plind.pf \
                    -b -p psp/ps-ut03.p \
                    -D 200 -d dmy -E -h 10 -o lp -v6q -v6colon
*/

