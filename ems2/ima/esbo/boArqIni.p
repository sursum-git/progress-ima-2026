/*
programa:boArqIni
objetivo:extrair para uma tabela temporaria as informa‡äes
de determinado arquivo .ini
*/

{esbo/boArqIni.i}
              
DEFINE VARIABLE cArquivoIni AS CHARACTER   NO-UNDO.

PROCEDURE setArquivoIni:

    DEFINE INPUT  PARAMETER pArquivoIni AS CHARACTER   NO-UNDO.
    ASSIGN cArquivoIni = pArquivoIni.

END PROCEDURE.

PROCEDURE getDados:
    
    INPUT FROM VALUE(cArquivoIni).
    REPEAT:
        CREATE ttIni.
        IMPORT delimiter "="  ttIni.
    END.
END PROCEDURE.

PROCEDURE getTTIni:                          
    DEFINE OUTPUT PARAMETER TABLE FOR ttIni .

END PROCEDURE.
