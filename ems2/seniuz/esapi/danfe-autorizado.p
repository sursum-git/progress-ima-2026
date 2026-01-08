DEF INPUT  PARAM pCodEstabel AS CHARACTER.
DEF INPUT  PARAM pSerie      AS CHARACTER.
DEF INPUT  PARAM pNrNotaFis  AS CHARACTER.
DEF OUTPUT PARAM pRetorno    AS LOGICAL.

DEFINE TEMP-TABLE tt-import NO-UNDO
    FIELD cnpj        AS CHAR
    FIELD serie       AS CHAR
    FIELD nr-nota-fis AS CHAR
    FIELD ds-chave    AS CHAR
    FIELD codigo      AS CHAR
    FIELD indefinido1 AS CHAR FORMAT "x(100)"
    FIELD indefinido2 AS CHAR FORMAT "x(100)"
    FIELD indefinido3 AS CHAR FORMAT "x(100)"
    FIELD iLinha      AS INT
    FIELD FullPath    AS CHAR
    FIELD FILENAME    AS CHAR.    

DEFINE TEMP-TABLE TT_File NO-UNDO 
    FIELD FILENAME AS CHARACTER
    FIELD FullPath AS CHARACTER
    FIELD FILE     AS CHARACTER.

FIND gati-nfe-param WHERE 
     gati-nfe-param.cod-estabel = pCodEstabel NO-LOCK.

DEFINE TEMP-TABLE tt-arquivos
       FIELD arq-xml AS CHAR.

DEF VAR c-file      AS CHAR.
DEF VAR c-comando   AS CHAR.
DEF VAR c-arq-txt   AS CHAR.
DEF VAR c-pasta-txt AS CHAR FORMAT "x(50)".

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

FIND gati-nfe-param WHERE 
     gati-nfe-param.cod-estabel = pCodEstabel NO-LOCK.
    
FIND FIRST sit-nf-eletro WHERE 
           sit-nf-eletro.cod-estabel   = pCodEstabel AND 
           sit-nf-eletro.cod-serie     = pSerie      AND 
           sit-nf-eletro.cod-nota-fisc = pNrNotaFis 
           NO-LOCK NO-ERROR.

IF AVAIL sit-nf-eletro AND
   sit-nf-eletro.idi-sit-nf-eletro = 3 THEN DO.
   ASSIGN pRetorno = YES.
   RETURN.    /* Retorna daqui caso encontre a situacao da NFe */
END.

FIND FIRST gati-nfe WHERE
           gati-nfe.cod-estabel = pCodEstabel AND  
           gati-nfe.serie       = pSerie      AND  
           gati-nfe.nr-nota-fis = pNrNotaFis       
           NO-LOCK NO-ERROR.

IF AVAIL gati-nfe AND
   gati-nfe.acao = 3 THEN DO.
   ASSIGN pRetorno = YES.
   RETURN.    /* Retorna daqui caso encontre a situacao da NFe das tabelas da Gati */
END.

/* Procura nos arquivos txt */

ASSIGN c-file = gati-nfe-param.end-imp-nfe + "\*.txt"
       c-arq-txt = SESSION:TEMP-DIRECTORY + c-seg-usuario + ".txt"
       c-comando = 'DIR /b ' + c-file + '>' + c-arq-txt.

OS-COMMAND SILENT VALUE(c-comando).
    
EMPTY TEMP-TABLE tt-arquivos.

INPUT FROM VALUE(c-arq-txt).
REPEAT.
  CREATE tt-arquivos.
  IMPORT tt-arquivos.
END.
INPUT CLOSE.

FOR EACH tt-arquivos WHERE
         tt-arquivos.arq-xml MATCHES "*.txt" NO-LOCK.

    CREATE TT_File. 
    ASSIGN TT_File.FILENAME = tt-arquivos.arq-xml
           TT_File.FullPath = gati-nfe-param.end-imp-nfe
           TT_File.FILE = 'F'. 
END.

FOR EACH TT_File WHERE 
         TT_File.FILE = 'F' AND
         SUBSTRING(TT_File.FILENAME,LENGTH(TT_File.FILENAME) - 2,3) = 'TXT' NO-LOCK.
    
    INPUT FROM VALUE(TT_File.FullPath + "\" + tt_file.FILENAME) NO-CONVERT.
    REPEAT:
        CREATE tt-import.
        IMPORT DELIMITER ";" tt-import.

        ASSIGN iLinha             = iLinha + 1
               tt-import.iLinha   = iLinha
               tt-import.FullPath = TT_File.FullPath
               tt-import.FILENAME = TT_File.FILENAME.
    END.
    INPUT CLOSE.
END.
ASSIGN pNrNotaFis = STRING(INT(pNrNotaFis)).  /* Retira os Zeros … Esquerda */

FOR EACH tt-import WHERE
         tt-import.serie = pSerie AND 
         tt-import.nr-nota-fis = pNrNotaFis NO-LOCK,
    FIRST msg-ret-nf-eletro WHERE
          msg-ret-nf-eletro.cod-msg = tt-import.codigo NO-LOCK.

    IF msg-ret-nf-eletro.cod-grp-msg = '20' THEN  /* 20 = Nota Fiscal Autorizada para uso */ 
      ASSIGN pRetorno = YES.  
END.


/* novo enviado pela gati 
INPUT FROM OS-DIR(gati-nfe-param.end-imp-nfe) NO-ECHO. 
REPEAT:
   CREATE TT_File. 
   IMPORT TT_File.FILENAME 
          TT_File.FullPath
          TT_File.FILE. 
END. 

FOR EACH TT_File WHERE 
         TT_File.FILE = 'F' AND 
         SUBSTRING(TT_File.FILENAME,LENGTH(TT_File.FILENAME) - 2,3) = 'TXT'.

    INPUT FROM VALUE(TT_File.FullPath) NO-CONVERT.
    REPEAT:
        CREATE tt-import.
        IMPORT DELIMITER ";" tt-import.
        ASSIGN iLinha             = iLinha + 1
               tt-import.iLinha   = iLinha
               tt-import.FullPath = TT_File.FullPath
               tt-import.FILENAME = TT_File.FILENAME.
    END.
    INPUT CLOSE.
END.

FOR EACH tt-import.      
    IF pNrNotaFis <> '' AND pSerie <> '' AND 
       tt-import.serie = pSerie    AND
       tt-import.nr-nota-fis = pNrNotaFis THEN NEXT.
    ELSE
       DELETE tt-import.
END.

FOR FIRST estabelec WHERE 
          estabelec.cod-estabel = gati-nfe-param.cod-estabel NO-LOCK,
    EACH tt-import,
    FIRST msg-ret-nf-eletro WHERE 
          msg-ret-nf-eletro.cod-msg = tt-import.codigo NO-LOCK.

    assign tt-import.nr-nota-fis = string(int(tt-import.nr-nota-fis),"9999999").

    CASE msg-ret-nf-eletro.cod-grp-msg:
        WHEN '20' THEN DO: /* 20 - Nota Fiscal Autorizada para uso */      
           ASSIGN pRetorno = YES.  
        END.
    END.
END.
*/

