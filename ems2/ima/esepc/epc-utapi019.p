{include/i-epc200.i1} /* defini‡Æo da temp-table tt-epc */

DEF INPUT        PARAM p-ind-event AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE       FOR tt-epc.

DEF VAR cComandoEmail              AS CHAR NO-UNDO.

IF p-ind-event = "eMailBlat" THEN DO:
   FIND FIRST param-global NO-LOCK NO-ERROR.

   FIND FIRST tt-epc
        WHERE tt-epc.cod-event     = "eMailBlat":U
        AND   tt-epc.cod-parameter = "CommandEmail":U
        EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL tt-epc THEN 
       ASSIGN cComandoEmail = tt-epc.val-parameter.
    
    /*Habilita Log do BLAT*/
    ASSIGN cComandoEmail = cComandoEmail + " -log c:\temp\blat.log -debug":U.

    /*Define o parƒmetro Hostname*/
    /*ASSIGN cComandoEmail = cComandoEmail + " -hostname " + chr(34) + "localhost" + chr(34):U.*/

    /*Define a autentificacao*/
   /* ASSIGN cComandoEmail = cComandoEmail 
                         + " -u "  + CHR(34) + SUBSTR(param-global.char-2,200,50) + CHR(34) 
                         + " -pw " + CHR(34) + SUBSTR(param-global.char-2,251,15) + CHR(34).*/

    IF AVAIL tt-epc THEN 
       ASSIGN tt-epc.val-parameter = cComandoEmail.

    OUTPUT TO c:\temp\comando-email.txt APPEND.
       EXPORT cComandoEmail. 
    OUTPUT CLOSE.

    IF AVAIL tt-epc THEN ASSIGN  tt-epc.val-parameter = cComandoEmail.
END.
