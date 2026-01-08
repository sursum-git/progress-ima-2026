/********************************************************************
programa: esapi/sincrDirs.p
objetivo: Verificar se existem as pastas de um Caminho especifico
          e cria-las caso necessario
   autor: Tadeu Silva
    data: 09/2025
********************************************************************/
DEFINE INPUT  PARAMETER pCaminho         AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pDivisorDir      AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pTemNomeArquivo  AS LOGICAL     NO-UNDO.
DEFINE INPUT  PARAMETER pNivelInicial    AS INTEGER     NO-UNDO.
{esp/util.i}

DEFINE VARIABLE iQt                   AS INTEGER     NO-UNDO.
DEFINE VARIABLE qtNiveis              AS INTEGER     NO-UNDO.
DEFINE VARIABLE qtNivelDesconsiderar  AS INTEGER     NO-UNDO.
DEFINE VARIABLE dirCorrente           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lCaminhoRede          AS LOGICAL     NO-UNDO.
DEFINE VARIABLE divisorCorrente       AS CHARACTER   NO-UNDO.

ASSIGN qtNivelDesconsiderar     = IF pTemNomeArquivo THEN 1 ELSE 0
       .
       
IF SUBSTR(pCaminho,1,2) = "\\" THEN DO:
  ASSIGN  pCaminho     = REPLACE(pCaminho,"\\","")  
          lCaminhoRede = YES.          
END.
ASSIGN qtNiveis                 = NUM-ENTRIES(pCaminho,pDivisorDir) .



REPEAT iQt = 1 TO qtNiveis - qtNivelDesconsiderar:

      IF iQt = 1 THEN DO:
          IF   lCaminhoRede THEN
          DO:
            ASSIGN dirCorrente      = "\\" + dirCorrente
                   divisorCorrente  = ''.              
          END.
          ELSE DO:
            ASSIGN divisorCorrente = pDivisorDir.
          END.                  
      END.
      ELSE DO:
         ASSIGN divisorCorrente = pDivisorDir.
      END.    
      /*MESSAGE 'dircorrente:' dirCorrente SKIP
              'pCaminho:' ENTRY(iQt,pCaminho,divisorCorrente) SKIP
              'qt:' iQt SKIP
              'divisor:' divisorCorrente
         VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
      RUN incrValor(INPUT-OUTPUT dirCorrente,
                    ENTRY(iQt,pCaminho,pDivisorDir),
                    divisorCorrente).
      /*MESSAGE 'diretorio corrente:' dirCorrente
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.                        */
      IF iQT >= pNivelInicial THEN DO:
         FILE-INFO:FILE-NAME = dirCorrente .
         IF FILE-INFO:FULL-PATHNAME = ? THEN
         DO:           
           //OS-COMMAND  VALUE('mkdir ' + dirCorrente). 
           OS-CREATE-DIR value(dirCorrente).
         END.                                      
      END.                                   
END.
       
       




