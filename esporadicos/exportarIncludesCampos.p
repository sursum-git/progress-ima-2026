DEFINE TEMP-TABLE ttMD
       FIELD labelCampo         AS CHAR FORMAT 'x(100)'
       FIELD nomeCampo          AS CHAR FORMAT 'x(100)'
       FIELD tipo               AS CHAR FORMAT 'x(50)'
       FIELD formato            AS CHAR FORMAT 'x(50)'
       FIELD valor              AS CHAR FORMAT 'x(4000)'
       FIELD Visao              AS CHAR FORMAT 'x(4000)'
       FIELD tabela             AS CHAR FORMAT 'x(50)'
       FIELD banco              AS CHAR FORMAT 'x(12)'.
      
DEFINE VARIABLE hQuery          AS HANDLE  NO-UNDO.
DEFINE VARIABLE hQueryMD        AS HANDLE  NO-UNDO.
DEFINE VARIABLE hBf             AS HANDLE  NO-UNDO EXTENT 10.
DEFINE VARIABLE iNumVar         AS INTEGER NO-UNDO INITIAL 10. 
DEFINE VARIABLE i               AS INTEGER     NO-UNDO.
DEFINE VARIABLE iDB             AS INTEGER     NO-UNDO.
DEFINE VARIABLE i2              AS INTEGER     NO-UNDO.
DEFINE VARIABLE cListaCampos    AS CHARACTER   NO-UNDO FORMAT 'x(500)'.
DEFINE VARIABLE cBanco          AS CHARACTER   NO-UNDO FORMAT 'x(50)'.
DEFINE VARIABLE pTabela         AS CHARACTER   NO-UNDO FORMAT 'x(50)'.
DEFINE VARIABLE bhFile          AS HANDLE      NO-UNDO.
DEFINE VARIABLE bhField         AS HANDLE      NO-UNDO.
DEFINE VARIABLE bhTabelas       AS HANDLE      NO-UNDO EXTENT 18.
DEFINE VARIABLE cCondicao       AS CHARACTER   NO-UNDO FORMAT 'x(60)'.
DEFINE VARIABLE cValor01        AS CHARACTER   NO-UNDO FORMAT 'x(50)'.
DEFINE VARIABLE cValor02        AS CHARACTER   NO-UNDO FORMAT 'x(50)'.
DEFINE VARIABLE  cInclude AS CHARACTER   NO-UNDO FORMAT 'x(50)'.
/*UPDATE pTabela cCondicao WITH 1 COL .*/

REPEAT iDB = 1 TO NUM-DBS:
    ASSIGN cBanco = LDBNAME(iDB).
    /*DISP cBanco.*/
    CREATE QUERY hQueryMD.
    CREATE BUFFER bhFile  FOR TABLE cBanco + '._file'.
    CREATE BUFFER bhField FOR TABLE cBanco + '._field'.
    /*hQueryMD:SET-BUFFERS(bhFile:HANDLE).*/
    hQueryMD:ADD-BUFFER(bhFile).
    hQueryMD:ADD-BUFFER(bhField).
    hQueryMD:QUERY-PREPARE('for each ' + cBanco + '._file no-lock, each _field of _file no-lock ').
    /*hQueryMD:QUERY-PREPARE('for each _file no-lock  , each _field of _file no-lock ').*/
    hQueryMD:QUERY-OPEN. 
    REPEAT:
      hQueryMD:GET-NEXT().
      IF hQueryMD:QUERY-OFF-END THEN LEAVE.
      CREATE ttMD.
      ASSIGN ttMD.labelCampo = bhField:BUFFER-FIELD('_label'):BUFFER-VALUE()
             ttMD.nomeCampo  = bhField:BUFFER-FIELD('_field-name'):BUFFER-VALUE()
             ttMD.tipo       = bhField:BUFFER-FIELD('_data-type'):BUFFER-VALUE() 
             ttMD.formato    = bhField:BUFFER-FIELD('_format'):BUFFER-VALUE()
             ttMD.visao      = bhField:BUFFER-FIELD('_view-as'):BUFFER-VALUE()
             ttMD.tabela     = bhFile:BUFFER-FIELD('_file-name'):BUFFER-VALUE()
             ttMD.banco      = cBanco.
      
    END.
    hQueryMD:QUERY-CLOSE().
    bhFile:BUFFER-RELEASE().
    bhField:BUFFER-RELEASE().
    DELETE OBJECT hQueryMD.
    DELETE OBJECT bhFile.
    DELETE OBJECT bhField.
END.
/* OUTPUT TO c:\temp\teste.txt.                                                        */
/* FOR EACH ttMD:                                                                      */
/*     EXPORT DELIMITER "|"  ttMD.banco ttMd.tabela ttMd.nomeCampo ttMD.visao.         */
/*        /*ASSIGN ttMD.valor = bhTabelas[1]:BUFFER-FIELD(nomeCampo):BUFFER-VALUE().*/ */
/* END.                                                                                */
/* OUTPUT CLOSE.                                                                       */
/*DELETE OBJECT hQuery.*/
/*DELETE OBJECT bhTabelas[1].*/

OUTPUT TO value(session:TEMP-DIRECTORY + "includes.p").
PUT 'output to value(session:TEMP-DIRECTORY + "includes.txt").' SKIP.
FOR EACH ttMD:
    
    RUN pibuscarOpcoes(ttMd.visao, OUTPUT cInclude).
    IF cInclude <> 'n/a' THEN DO:
       ASSIGN cInclude = cInclude + ".i 3 ~}".
       IF INDEX(ttMD.banco,'esp') = 0  THEN
          PUT "put '" ttMD.banco "|" ttMD.tabela "|" ttmd.nomeCampo  "| ' ~{" cInclude  "  skip." SKIP .
    END.
    
                                                 
END.
PUT 'output close.' SKIP.
OUTPUT CLOSE.

RUN  value(session:TEMP-DIRECTORY + "includes.p").

PROCEDURE piBuscarOpcoes:
    DEFINE INPUT  PARAMETER pViewAS  AS CHARACTER   NO-UNDO FORMAT 'x(150)'.
    DEFINE OUTPUT PARAMETER cInclude AS CHARACTER   NO-UNDO FORMAT 'x(50)'.
    DEFINE VARIABLE cArqInclude      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE i2 AS INTEGER     NO-UNDO.
    DEFINE VARIABLE i AS INTEGER     NO-UNDO.
   REPEAT i = 1  TO LENGTH(pViewAs):
     IF LOOKUP(SUBSTR(pviewas,i,1),'~{,~}') > 0 THEN
        ASSIGN i2 = i2 + 1.
  END.
  IF i2 = 2 THEN 
   ASSIGN cInclude = entry(2,pViewAs,'~{')
          cInclude  = ENTRY(1, cInclude,'.i').
  ELSE
     ASSIGN cInclude = 'n/a'.
  
  IF cInclude <> 'n/a' THEN DO:
     ASSIGN cArqInclude = cInclude + ".i".
/*      MESSAGE cArqInclude                    */
/*          VIEW-AS ALERT-BOX INFO BUTTONS OK. */
     IF SEARCH(cArqInclude) = ? THEN DO:
        ASSIGN cInclude = "n/a".
     END.
  END.
END PROCEDURE.
