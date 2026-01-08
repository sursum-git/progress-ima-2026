/*
programa: esapi/sincrArqDanfeNF.p
objetivo: salvar o PDF da DANFE da nota fiscal para arquivamento 
   autor: Tadeu Silva
    data: 09/2025
*/

DEFINE INPUT  PARAMETER pRowid      AS ROWID       NO-UNDO.
DEFINE INPUT  PARAMETER pArquivo    AS CHARACTER   NO-UNDO.

DEFINE VARIABLE caminhoDanfe AS CHARACTER   NO-UNDO.
DEFINE VARIABLE caminhoXML   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDirDanfe    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hBoParam     AS HANDLE      NO-UNDO.


ASSIGN pArquivo = SESSION:TEMP-DIRECTORY + pArquivo.

RUN esbo/boConsParam.p PERSIST SET hBoParam.

FOR FIRST nota-fiscal fields(cod-estabel serie nr-nota-fis dt-emis-nota)NO-LOCK
    WHERE ROWID(nota-fiscal) = pRowid:
    
    

    RUN getDirDanfe IN hBoParam(nota-fiscal.cod-estabel,OUTPUT cDirDanfe).
    IF VALID-HANDLE(hBoParam) THEN DO:
        DELETE PROCEDURE hBoParam.    
    END.        
    RUN esapi/getArqsNf.p(ROWID(nota-fiscal), OUTPUT caminhoDanfe, OUTPUT caminhoXML).                                                        
    
    IF SEARCH(caminhoDanfe) = ? THEN
    DO:
        /*MESSAGE 'vou criar o arquivo:' SKIP
            caminhoDanfe                 
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
        RUN esapi/sincrDirs.p(caminhoDanfe,"\",YES,4).
        OS-COPY value(pArquivo)  value(caminhoDanfe).          
    END.       
    
    /*MESSAGE 'arquivo:' pArquivo SKIP
             'nf:'      nota-fiscal.nr-nota-fis SKIP
            'dir danfe:' cDirDanfe SKIP
            'caminho:' caminhoDanfe         
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
END.
