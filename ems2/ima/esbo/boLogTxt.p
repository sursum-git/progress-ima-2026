
/*
programa:boLogtxt
Objetivo: Bo para gera‡Æo de log em txt
*/

DEFINE TEMP-TABLE ttNiveis no-undo
    FIELD nivel      AS CHAR FORMAT 'x(50)'
    FIELD ordem      AS INT INIT 999
    FIELD tamanho    AS INT
    FIELD descricao  AS CHAR FORMAT 'x(50)'
    FIELD vlCorrente AS CHAR FORMAT 'x(3000)'.

DEFINE TEMP-TABLE ttDados no-undo
    FIELD nivel         LIKE ttNiveis.nivel
    FIELD valor         AS CHAR FORMAT 'x(1000)'
    FIELD linha         AS INT
    FIELD logImpresso   AS LOGICAL INIT NO
    INDEX ind-impreso logImpresso
    INDEX ind-nivel-linha nivel linha.


DEFINE TEMP-TABLE ttLinhas no-undo
    FIELD linha AS INT.

DEFINE VARIABLE iSeqDados        AS INTEGER  INIT 0    NO-UNDO.
DEFINE VARIABLE cPrefixoLOG      AS CHARACTER   NO-UNDO FORMAT 'x(50)'.
DEFINE VARIABLE cExtensaoLOG     AS CHARACTER   NO-UNDO .
DEFINE VARIABLE cDirLog          AS CHARACTER   NO-UNDO FORMAT 'x(200)'.
DEFINE VARIABLE cUsuarioCorrente AS CHARACTER  FORMAT 'x(20)' NO-UNDO.
DEFINE VARIABLE cArqCompleto     AS CHARACTER   NO-UNDO.


DEFINE VARIABLE lArqPorDia       AS LOGICAL     NO-UNDO INIT NO.
DEFINE VARIABLE lArqPorUsuario   AS LOGICAL     NO-UNDO INIT NO.

PROCEDURE setArqPorDia:

    DEFINE INPUT  PARAMETER pPorDia AS LOGICAL    NO-UNDO.
    ASSIGN lArqPorDia = pPorDia.

END PROCEDURE.

PROCEDURE setArquivoPorUsuario:

    DEFINE INPUT  PARAMETER pArqPorUsuario AS LOGICAL     NO-UNDO.
    ASSIGN lArqPorUsuario = pArqPorUsuario.

END PROCEDURE.


PROCEDURE setDados:

    DEFINE INPUT  PARAMETER pNivel LIKE ttNiveis.nivel  NO-UNDO.
    DEFINE INPUT  PARAMETER pValor LIKE ttDados.valor NO-UNDO.
    
    FIND FIRST ttDados
        WHERE ttDados.nivel = pNivel
        AND   ttDados.linha =  iSeqDados
        USE-INDEX ind-nivel-linha NO-LOCK NO-ERROR.
    IF AVAIL ttDados THEN
       ASSIGN ttDados.valor = pvalor.
    ELSE DO:
       CREATE ttDados.
       ASSIGN ttDados.nivel = pNivel
              ttDados.valor = replace(pValor,chr(10),"|")
              ttDados.linha = iSeqDados.
    END.

    

    FIND FIRST ttLinhas
        WHERE ttLinhas.linha = ttDados.linha NO-ERROR.
    IF NOT AVAIL ttLinhas THEN DO:
       CREATE ttLinhas.
       ASSIGN ttLinhas.linha = ttDados.linha.
    END.
    RUN setVlNivelCorrente(pNivel,pValor).


END PROCEDURE.

PROCEDURE setVlNivelCorrente:

DEFINE INPUT  PARAMETER cNivel LIKE ttNiveis.nivel   NO-UNDO.
DEFINE INPUT  PARAMETER cVlCorrente    LIKE ttNiveis.vlCorrente  NO-UNDO.

FIND FIRST ttNiveis
    WHERE ttNiveis.nivel = cNivel
    NO-ERROR.
IF AVAIL ttNiveis THEN 
   ASSIGN ttNiveis.vlCorrente = cVlCorrente.


END PROCEDURE.

PROCEDURE setNivel:

    DEFINE INPUT  PARAMETER pNivel   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pOrdem   AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pTamanho AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pDescricao LIKE ttNiveis.descricao   NO-UNDO.
    FIND FIRST ttNiveis
        WHERE ttNiveis.nivel = pNivel NO-ERROR.
    IF NOT AVAIL ttNiveis THEN DO:
       CREATE ttNiveis.
       ASSIGN ttNiveis.nivel = pNivel.
    END.
    ASSIGN ttNiveis.ordem    = pOrdem
           ttNiveis.tamanho  = pTamanho
           ttNiveis.descricao = pDescricao.

END PROCEDURE.

PROCEDURE zerarSeqDados:

    ASSIGN iSeqDados = 0.


END PROCEDURE.

PROCEDURE incrSeqDados:
    ASSIGN iSeqDados = iSeqDados + 1.
    IF iSeqDados > 1 THEN DO:
      FOR EACH ttNiveis:                                         
          FIND FIRST ttDados                                     
              WHERE ttDados.nivel = ttNiveis.nivel               
              AND   ttDados.linha = iSeqDados
              USE-INDEX ind-nivel-linha NO-ERROR.          
          IF NOT AVAIL ttDados THEN DO:                          
              /*MESSAGE 'nao achei registro' SKIP                  
                  'nivel' ttNiveis.nivel SKIP                    
                  'linha' iSeqDados SKIP                         
                  'vl.corrente' ttNiveis.vlCorrente SKIP         
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.  */           
              RUN setDados(ttNiveis.nivel,ttNiveis.vlCorrente).  
          END.                                                   
          ELSE DO:                                               
              /*MESSAGE 'achei registro' SKIP                      
                  'nivel' ttNiveis.nivel SKIP                    
                  'linha' iSeqDados SKIP                         
                  'vl.registro' ttDados.valor SKIP               
                  VIEW-AS ALERT-BOX INFO BUTTONS OK. */            
                                                                 
          END.                                                   
      END.                                                       
    END.
    


END PROCEDURE.


PROCEDURE setPrefixoArquivoLog:

    DEFINE INPUT  PARAMETER pPrefix LIKE cPrefixoLOG NO-UNDO .
    ASSIGN cPrefixoLOG = pPrefix .

END PROCEDURE.


PROCEDURE setExtArquivoLog:

    DEFINE INPUT  PARAMETER pExt LIKE cExtensaoLOG NO-UNDO .
    ASSIGN cExtensaoLOG = pExt .

END PROCEDURE.


PROCEDURE setDiretorioLog:

    DEFINE INPUT  PARAMETER pDir LIKE cDirLog NO-UNDO .
    ASSIGN cDirLog = pDir.

END PROCEDURE.


PROCEDURE gerarLogs:

    
    DEFINE VARIABLE cDiaCorrente     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE qtProximaColuna  AS INTEGER  INIT 1   NO-UNDO.
    DEFINE VARIABLE lNovoArquivo     AS LOGICAL     NO-UNDO.
    //RUN getTTDados.
    
    ASSIGN cArqCompleto = cDirLog + "/" + cPrefixoLOG .

    IF  lArqPorUsuario THEN DO:
        ASSIGN cArqCompleto = cArqCompleto + "_"  + cUsuarioCorrente.
    END.

    IF  lArqPorDia THEN DO:
        ASSIGN cDiaCorrente = STRING(TODAY,"99/99/9999")
               cDiaCorrente = SUBSTR(cDiaCorrente,7,4) + '_' + 
                              SUBSTR(cDiaCorrente,4,2) + '_' +
                              SUBSTR(cDiaCorrente,1,2).
               cArqCompleto = cArqCompleto + "_" + cDiaCorrente.
    END.

    ASSIGN cArqCompleto = cArqCompleto + "." +  cExtensaoLog.
    ASSIGN lNovoArquivo =  SEARCH(cArqCompleto) = ? .

    OUTPUT TO VALUE(cArqCompleto) APPEND NO-CONVERT.
    /*MESSAGE 'entrei no arquivo'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    ASSIGN qtProximaColuna = 1.
    IF lNovoArquivo THEN DO:
       FOR EACH ttNiveis:
           PUT UNFORM ttNiveis.descricao AT  qtProximaColuna .
          
           ASSIGN qtProximaColuna = qtProximaColuna + ttNiveis.tamanho.
       END.
       PUT SKIP.
    END.
       
    FOR EACH ttLinhas:
        /*MESSAGE 'entrei linha'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
        ASSIGN qtProximaColuna = 1.
        FOR EACH ttNiveis BY ttNiveis.ordem:
            /*MESSAGE 'entrei nivel'
                VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
            FOR EACH ttDados
                WHERE ttDados.linha = ttLinhas.linha
                AND   ttDados.nivel = ttNiveis.nivel 
                AND   ttDados.logImpresso = NO.
                /*MESSAGE 'entrei dados'
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
                ASSIGN ttDados.logImpresso = YES.
                PUT UNFORM ttDados.valor AT qtProximaColuna.
                ASSIGN qtProximaColuna = qtProximaColuna + ttNiveis.tamanho  .
            END.
        END.
        PUT SKIP.
    END.
    //PUT SKIP.
    OUTPUT CLOSE.
END PROCEDURE.

PROCEDURE getNomeArquivo:
    DEFINE OUTPUT PARAMETER cArq AS CHARACTER   NO-UNDO.
    ASSIGN cArq = cArqCompleto.
END PROCEDURE.

PROCEDURE getTTDados:
    OUTPUT TO value(SESSION:TEMP-DIRECTORY + STRING(TIME) + '.csv').
    PUT "nivel; valor; linha; impresso?" SKIP.
    FOR EACH ttDados:
        EXPORT DELIMITER ";" ttDados.
    END.

    OUTPUT CLOSE.
END PROCEDURE.










