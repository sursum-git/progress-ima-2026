DEFINE VARIABLE cCusto AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCont  AS INTEGER     NO-UNDO.
DEFINE VARIABLE numLinhaPaiCorrente AS INTEGER     NO-UNDO.
DEFINE TEMP-TABLE ttDados
    FIELD num_linha AS INT
    FIELD linha AS CHAR FORMAT 'x(100)' EXTENT 9
    FIELD num_linha_pai AS INT.
DEFINE BUFFER bf-ttDados FOR ttDados.
INPUT FROM c:\temp\j_resultados_teste.txt.
REPEAT:
    ASSIGN iCont = iCont + 1.
    CREATE ttDados.
    IMPORT DELIMITER "|"  ttDados.
    ASSIGN ttDados.num_linha = iCont.
END.

INPUT CLOSE.

FOR EACH ttDados.

    IF ttDados.linha[1] = 'j050' AND ttDados.linha[4] = 'A' THEN DO:
        ASSIGN numLinhaPaiCorrente = ttDados.num_linha.
        ASSIGN cCusto = SUBSTR(ttDados.linha[6],7,5).
        FIND FIRST bf-ttDados
            WHERE bf-ttDados.num_linha = ttDados.num_linha - 1
            NO-ERROR.
        IF AVAIL bf-ttDados THEN
           ASSIGN bf-ttDados.linha[4] = 'A'.
        
    END.
    IF ttDados.linha[1] = 'j051' THEN DO:
       ASSIGN ttDados.num_linha_pai = numLinhaPaiCorrente - 1.
       FIND FIRST bf-ttDados
            WHERE bf-ttDados.num_linha = ttDados.num_linha - 1
            AND bf-ttDados.linha[1] = 'j050' NO-ERROR.
       IF AVAIL bf-ttDados THEN DO:
           /*MESSAGE ttdados.linha[3]
               VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
          ASSIGN iCont = iCont + 1 .
          CREATE bf-ttDados.
          ASSIGN bf-ttDados.num_linha = iCont
                 bf-ttDados.linha[1] = 'J051'
                 bf-ttDados.linha[2] = cCusto
                 bf-ttDados.linha[3] = ttDados.linha[3]
                 bf-ttdados.num_linha_pai = numLinhaPaiCorrente - 1.
       END.
    END.   
END.

/*OUTPUT TO c:\temp\conf.txt.
FOR EACH ttDados:

     
END.*/
OUTPUT CLOSE.

OUTPUT TO c:\temp\novo_arquivo.txt.
    FOR EACH ttDados
        WHERE ttDados.num_linha_pai = 0.
        /*MESSAGE ttDados.linha[1] length(ttDados.linha[6])
            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
        IF ttDados.linha[1] = 'j050' AND LENGTH(ttDados.linha[6]) = 7  THEN
           EXPORT DELIMITER "|" ttDados.

        /*ELSE*/
        /*MESSAGE ttDados.linha[6]
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
            */
        FOR EACH bf-ttDados
            WHERE bf-ttDados.num_linha_pai = ttDados.num_linha
            AND bf-ttDAdos.num_linha_pai <> 0.
            EXPORT DELIMITER "|" bf-ttDados.
        END.
    END.

OUTPUT CLOSE.
