DEFINE TEMP-TABLE ttArquivo NO-UNDO
    FIELD diretorio AS CHAR FORMAT 'x(100)'
    FIELD arquivo   AS CHAR FORMAT 'x(100)'
    FIELD tipo      AS CHAR FORMAT 'x(20)'
    FIELD resultado AS CHAR FORMAT 'x(100)' 
    FIELD logErro   AS LOGICAL
    INDEX primario AS PRIMARY diretorio arquivo 
    INDEX erro logErro
    INDEX indtipo tipo diretorio arquivo
   .

