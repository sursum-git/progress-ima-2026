/****************************************************************************
programa:esapi/getArqsDir.p
objetivo:extrair os arquivos a partir de um diretorio especifico
podendo extrair todos os niveis ou apenas o primeiro nivel
autor:tadeu silva
data:11/2023
***************************************************************************/
{esapi/getArqsDir.i}
DEFINE INPUT  PARAMETER pDir            AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pNivelInferior  AS LOGICAL     NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttArquivo .



DEFINE VARIABLE nomeArquivo         AS CHARACTER NO-UNDO. /*file name*/
DEFINE VARIABLE nomeCompletoArquivo AS CHARACTER NO-UNDO. /*file name with path*/
DEFINE VARIABLE tipoArquivo         AS CHARACTER NO-UNDO. /*file type*/
INPUT FROM OS-DIR(pDir).

REPEAT:
    IMPORT nomeArquivo nomeCompletoArquivo tipoArquivo.
    IF nomeArquivo = "." OR nomeArquivo = ".." THEN NEXT.
    IF INDEX(tipoArquivo, "D":U) = 1 AND pNivelInferior THEN 
       RUN esapi/getArqsDir.p(nomeCompletoArquivo,pNivelInferior,INPUT-OUTPUT TABLE ttArquivo).
    IF INDEX(tipoArquivo, "F":U) > 0 THEN DO:
        CREATE ttArquivo.
        ASSIGN ttArquivo.diretorio = pDir
               ttArquivo.arquivo   = nomeArquivo
               ttArquivo.tipo      = tipoArquivo
               .                                
    /*  MESSAGE nomeArquivo SKIP 
              nomeCompletoArquivo SKIP
             tipoArquivo
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
    END.
//DISP nomeCompletoArquivo  FORMAT 'x(80)' WITH WIDTH 550.
END.
INPUT CLOSE.
