DEFINE VARIABLE cLinha     AS CHARACTER   NO-UNDO FORMAT 'x(200)'.
DEFINE VARIABLE cRegistro  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cConta     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cContaPai  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCC        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ctipo      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNivel     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDescConta AS CHARACTER   NO-UNDO FORMAT 'x(100)'.
DEFINE VARIABLE cNatureza  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iNum       AS INTEGER     NO-UNDO.
DEFINE TEMP-TABLE ttEstrut
    FIELD conta AS CHAR
    FIELD contapai AS CHAR.

DEFINE TEMP-TABLE tt50
    FIELD linha AS CHAR FORMAT 'x(200)'
    FIELD conta AS CHAR
    FIELD numero AS INT.

DEFINE TEMP-TABLE tt51
    FIELD linha AS CHAR FORMAT 'x(200)'
    FIELD conta AS CHAR
    FIELD num050 AS INT
    FIELD cc     AS CHAR.

DEFINE STREAM s1.
OUTPUT STREAM s1 TO c:\temp\novo_arq_02.txt.

INPUT FROM c:\temp\estrut_ctbl.txt.
REPEAT:
    CREATE ttEstrut.
    IMPORT DELIMITER "|" ttEstrut.
END.
INPUT CLOSE.

INPUT FROM c:\temp\j.txt.
REPEAT:
    IMPORT UNFORM cLinha.
    
    IF cLinha = ''  THEN NEXT. 
    ASSIGN cRegistro = ENTRY(2,cLinha,"|").
    IF cRegistro = 'j050' THEN DO:
       ASSIGN cConta    = substr(ENTRY(7,cLinha,"|"),1,7)
              ctipo     = ENTRY(5,cLinha,"|")
              cNivel    = ENTRY(6,cLinha,"|")
              cContaPai = substr(ENTRY(8,cLinha,"|"),1,7)
             cDescConta = ENTRY(9,cLinha,"|")
             cNatureza  = ENTRY(4,cLinha,"|")
             iNum       = iNum + 1.
       FIND FIRST tt50
           WHERE tt50.conta = cConta NO-ERROR.
       IF NOT AVAIL tt50 THEN DO:
          CREATE tt50.
          ASSIGN tt50.linha = cLinha
                 tt50.conta = cConta
                 tt50.numero = iNum.
       END.
       
    END.
    IF cRegistro = 'j051' THEN DO:
       ASSIGN cCC = ENTRY(3,cLinha,"|").
       FIND FIRST tt51
           WHERE tt51.conta = cConta
           AND   tt51.cc    = cCC NO-ERROR.
       IF NOT AVAIL tt51 THEN DO:
          CREATE tt51.
          ASSIGN tt51.conta = cConta 
                 tt51.cc    = cCC
                 tt51.linha = cLinha
                 tt51.num050 = iNum.
       END. 
    END.


    

END.
INPUT CLOSE.
OUTPUT STREAM s1 CLOSE.

OUTPUT TO c:\temp\registros_nao_duplic.txt.
FOR EACH tt50:
    EXPORT tt50.linha.
    FOR EACH tt51
        WHERE tt51.conta = tt50.conta.
        EXPORT tt51.linha.
    END.
END.

OUTPUT CLOSE.


