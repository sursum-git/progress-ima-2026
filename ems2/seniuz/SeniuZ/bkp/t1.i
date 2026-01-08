DEF VAR c-comando AS CHAR.
DEF VAR c-dlc AS CHAR.


MESSAGE {1}
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

/*
FOR EACH {1}._user WHERE
         {1}._user._disabled = YES AND
         {1}._user._password = ENCODE("out").

    INPUT THROUGH ECHO $DLC NO-ECHO.
       SET c-dlc. 
    INPUT CLOSE. 

    ASSIGN c-comando = c-dlc + "/bin/proshut " + "{1}" + " -C disconnect " + {1}._user._userid.

    MESSAGE c-comando
        VIEW-AS ALERT-BOX INFO BUTTONS OK.


    /*OS-COMMAND SILENT VALUE(c-comando).*/
END.
*/
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


 via shell
 ${DIR_PROGRESS}/bin/proshut ${BANCO} -C disconnect ${_USER_NUM} >/dev/null
 2>&1
*/

