DEFINE VARIABLE cDirCertificados    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNomeCertificado    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hBoGetArq           AS HANDLE      NO-UNDO.
DEFINE VARIABLE cErro               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE listaCertificados   AS CHARACTER   NO-UNDO.

FUNCTION getDirCertProgress RETURN CHARACTER():

    RETURN OS-GETENV('dlc') + '/certs/' .

END FUNCTION.

PROCEDURE setDirCertificados:

    DEFINE INPUT  PARAMETER pDir AS CHARACTER   NO-UNDO.
    ASSIGN cDirCertificados = pDir.


END PROCEDURE.

PROCEDURE setNomeCertificado:
    DEFINE INPUT  PARAMETER pNomeCertificado AS CHARACTER   NO-UNDO.
    ASSIGN cNomeCertificado     = pNomeCertificado .
           

END PROCEDURE.


PROCEDURE setListaCertificados:

    DEFINE INPUT  PARAMETER pLista AS CHARACTER   NO-UNDO.
    ASSIGN listaCertificados = pLista.

END PROCEDURE.

PROCEDURE exec:

    DEFINE VARIABLE cArqCert AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iCont    AS INTEGER     NO-UNDO.
    ASSIGN cErro = ''.
    
    IF listaCertificados <> '' THEN DO:


       REPEAT iCont = 1 TO NUM-ENTRIES(listaCertificados,','):
           ASSIGN cArqCert = cDirCertificados + '/' + ENTRY(iCont,listaCertificados,',') .
           MESSAGE cArqCert
               VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
           IF SEARCH(cArqCert) <> ? THEN
               RUN _cmdCert(cArqCert).
           ELSE 
               ASSIGN cErro = "Arquivo:" + cArqCert + " n∆o encontrado".
       END.
    END.
    ELSE DO:
        IF SEARCH( getDirCertProgress() + cNomeCertificado ) = ?  THEN DO:
           ASSIGN cArqCert = SEARCH(cDirCertificados + '/' + cNomeCertificado).
           IF cArqCert = ? THEN
              ASSIGN cErro = cerro + chr(13) + 'Certificado:' + cNomeCertificado + ' n∆o foi encontrado no diret¢rio:' + cDirCertificados .
           ELSE DO:
             RUN _cmdCert(cArqCert).   
           END.
        END.
    END.
END PROCEDURE.

PROCEDURE _cmdCert:

    DEFINE INPUT  PARAMETER pArquivo AS CHARACTER   NO-UNDO.
    OS-COMMAND SILENT VALUE(OS-GETENV('dlc') + '/bin/certutil -import ' + pArquivo ).

END PROCEDURE.

PROCEDURE getErro.

    DEFINE OUTPUT PARAMETER pErro AS CHARACTER   NO-UNDO.
    ASSIGN pErro = cErro.

END PROCEDURE.




