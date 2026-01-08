DEFINE TEMP-TABLE ttEms2Jan
    FIELD conta AS CHAR
    FIELD cc    AS CHAR
    FIELD descricao AS CHAR FORMAT 'x(30)'
    FIELD vlSaldoInicial AS DECIMAL FORMAT "->>>,>>>>,>>9.99"
    FIELD vlDebito       AS DECIMAL FORMAT "->>>,>>>>,>>9.99"
    FIELD vlCredito      AS DECIMAL FORMAT "->>>,>>>>,>>9.99"
    FIELD vlSaldoFinal   AS DECIMAL FORMAT "->>>,>>>>,>>9.99"
    FIELD LOG_encontrado AS LOGICAL.

DEFINE TEMP-TABLE ttEms2Fev
    FIELD conta AS CHAR
    FIELD cc    AS CHAR
    FIELD descricao AS CHAR FORMAT 'x(30)'
    FIELD vlSaldoInicial AS DECIMAL FORMAT "->>>,>>>>,>>9.99"
    FIELD vlDebito       AS DECIMAL FORMAT "->>>,>>>>,>>9.99"
    FIELD vlCredito      AS DECIMAL FORMAT "->>>,>>>>,>>9.99"
    FIELD vlSaldoFinal   AS DECIMAL FORMAT "->>>,>>>>,>>9.99"
    FIELD LOG_encontrado AS LOGICAL.

DEFINE TEMP-TABLE ttTotvs12
    FIELD registro              AS CHAR
    FIELD conta                 AS CHAR
    FIELD cc                    AS CHAR
    FIELD vlSaldoInicial        AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD vlSaldoInicialAbs     AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD sinalIni              AS CHAR
    FIELD vlDebito              AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD vlCredito             AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD vlSaldoFinal          AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD vlSaldoFinalAbs       AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD sinalFinal            AS CHAR
    FIELD mes                   AS INT
    FIELD vlSaldoInicialEms2    AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD vlSaldoInicialAbsEms2 AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD sinalIniEms2          AS CHAR
    FIELD vlDebitoEms2          AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD vlCreditoEms2         AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD vlSaldoFinalEms2      AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD vlSaldoFinalAbsEms2   AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD vlsaldoInicialDif     AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD vlDebitoDif           AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD vlCreditoDif          AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD vlSaldoFinalDif       AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD sinalFinalEms2        AS CHAR
    FIELD LOG_criado            AS LOGICAL.

 DEFINE BUFFER bf-ttTotvs12 FOR ttTotvs12.

INPUT FROM c:\temp\ecfExportado_ajustado_a00_k155.txt.
REPEAT:
   CREATE ttTotvs12.
   IMPORT DELIMITER "|" ttTotvs12.
   ASSIGN mes = 0.
   IF sinalIni = 'db' THEN
      ASSIGN ttTotvs12.vlSaldoInicialAbs = ttTotvs12.vlSaldoInicial.
   ELSE
     ASSIGN ttTotvs12.vlSaldoInicialAbs = ttTotvs12.vlSaldoInicial * -1.

  IF sinalFinal = 'db' THEN
      ASSIGN ttTotvs12.vlSaldoFinalAbs = ttTotvs12.vlSaldoFinal.
   ELSE
     ASSIGN ttTotvs12.vlSaldoFinalAbs = ttTotvs12.vlSaldoFinal * -1.  
END.
INPUT CLOSE.

INPUT FROM c:\temp\ecfExportado_ajustado_a01_k155.txt.
REPEAT:
   CREATE ttTotvs12.
   IMPORT DELIMITER "|" ttTotvs12.
   ASSIGN mes = 1.
   IF sinalIni = 'db' THEN
      ASSIGN ttTotvs12.vlSaldoInicialAbs = ttTotvs12.vlSaldoInicial.
   ELSE
     ASSIGN ttTotvs12.vlSaldoInicialAbs = ttTotvs12.vlSaldoInicial * -1.

  IF sinalFinal = 'db' THEN
      ASSIGN ttTotvs12.vlSaldoFinalAbs = ttTotvs12.vlSaldoFinal.
   ELSE
     ASSIGN ttTotvs12.vlSaldoFinalAbs = ttTotvs12.vlSaldoFinal * -1.
END.
INPUT CLOSE.


INPUT FROM c:\temp\ecfExportado_ajustado_a02_k155.txt.
REPEAT:
   CREATE ttTotvs12.
   IMPORT DELIMITER "|" ttTotvs12.
   ASSIGN mes = 2.
   IF sinalIni = 'db' THEN
      ASSIGN ttTotvs12.vlSaldoInicialAbs = ttTotvs12.vlSaldoInicial.
   ELSE
     ASSIGN ttTotvs12.vlSaldoInicialAbs = ttTotvs12.vlSaldoInicial * -1.

  IF sinalFinal = 'db' THEN
      ASSIGN ttTotvs12.vlSaldoFinalAbs = ttTotvs12.vlSaldoFinal.
   ELSE
     ASSIGN ttTotvs12.vlSaldoFinalAbs = ttTotvs12.vlSaldoFinal * -1.
END.
INPUT CLOSE.


INPUT FROM c:\temp\balancete_jan_ems2_lev.csv.
REPEAT:
    CREATE ttEms2Jan.
    IMPORT DELIMITER ";" ttEms2Jan. 
END.   
INPUT CLOSE.

INPUT FROM c:\temp\balancete_fev_ems2_lev.csv.
REPEAT:
    CREATE ttEms2Fev.
    IMPORT DELIMITER ";" ttEms2Fev. 
END.
INPUT CLOSE.
/*FOR EACH ttTotvs12:
    DISP ttTotvs12 WITH 1 COL WIDTH 550.
END. */


FOR EACH ttEms2Jan WHERE ttEms2Jan.conta = '':
    DELETE ttEms2Jan  .
END.
FOR EACH ttEms2Fev WHERE ttEms2Fev.conta = '':
    DELETE ttEms2Fev  .
END.


FOR EACH ttTotvs12
    WHERE ttTotvs12.mes = 1 :
    FIND FIRST ttEms2Jan
        WHERE ttEms2Jan.conta = ttTotvs12.conta
        AND  ttEms2Jan.cc     = ttTotvs12.cc
        NO-ERROR.
    IF AVAIL ttems2jan THEN DO:

       ASSIGN ttEms2jan.LOG_encontrado         = YES
              ttTotvs12.vlSaldoInicialEms2     = IF ttEms2jan.vlSaldoInicial >= 0 THEN ttEms2jan.vlSaldoInicial ELSE ttEms2jan.vlSaldoInicial * -1
              ttTotvs12.vlSaldoInicialAbsEms2  = ttEms2jan.vlSaldoInicial
              ttTotvs12.sinalIniEms2           = IF ttEms2jan.vlSaldoInicial >= 0 THEN "" ELSE "CR"
              ttTotvs12.vlDebitoEms2           = ttEms2jan.vlDebito
              ttTotvs12.vlCreditoEms2          = ttEms2jan.vlCredito
              ttTotvs12.vlSaldoFinalEms2       = IF ttems2jan.vlSaldoFinal >= 0 THEN ttEms2jan.vlSaldoFinal ELSE ttEms2jan.vlSaldoFinal * -1
              ttTotvs12.vlSaldoFinalAbsEms2    = ttEms2jan.vlSaldoFinal
              ttTotvs12.sinalFinalEms2         = IF ttems2jan.vlSaldoFinal >= 0 THEN "" ELSE "CR".
    END.
END.




FOR EACH ttems2jan
    WHERE ttems2jan.LOG_encontrado = NO.
    CREATE ttTotvs12.
    ASSIGN ttTotvs12.conta               = ttems2jan.conta
           ttTotvs12.cc                  = ttems2jan.cc
           ttTotvs12.LOG_criado          = YES
           ttTotvs12.vlSaldoInicial      = IF ttems2jan.vlSaldoInicial >= 0 THEN ttems2jan.vlSaldoInicial ELSE ttems2jan.vlSaldoInicial * -1
           ttTotvs12.sinalIni            = IF ttems2jan.vlSaldoInicial >= 0 THEN '' ELSE 'CR'
           ttTotvs12.vlSaldoInicialAbs   = ttems2jan.vlSaldoInicial
           ttTotvs12.vlDebito            = ttems2jan.vlDebito
           ttTotvs12.vlCredito           = ttems2jan.vlCredito
           ttTotvs12.vlSaldoFinal        = IF ttems2jan.vlSaldoFinal >= 0 THEN ttems2jan.vlSaldoFinal ELSE  ttems2jan.vlSaldoFinal  * -1 
           ttTotvs12.sinalFinal          = IF ttems2jan.vlSaldoFinal >= 0 THEN '' ELSE 'CR'
           ttTotvs12.vlSaldoFinalAbs     = ttems2jan.vlSaldoFinal 
           ttTotvs12.mes                 = 1
           ttTotvs12.registro            = IF int(SUBSTR(ttems2jan.conta,1,1)) < 3 THEN 'K155' ELSE 'K355'
           ttTotvs12.vlSaldoInicialEms2  = ttTotvs12.vlSaldoInicial
           ttTotvs12.sinalIniEms2        = ttTotvs12.sinalIni
           ttTotvs12.vlDebitoEms2        = ttTotvs12.vlDebito
           ttTotvs12.vlCreditoEms2       = ttTotvs12.vlCredito
           ttTotvs12.vlSaldoFinalEms2    = ttTotvs12.vlSaldoFinal
           ttTotvs12.sinalFinalEms2      = ttTotvs12.sinalFinal.
END.


FOR EACH ttTotvs12
    WHERE ttTotvs12.mes = 2 :
    FIND FIRST ttEms2Fev
        WHERE ttEms2Fev.conta = ttTotvs12.conta
        AND  ttEms2Fev.cc     = ttTotvs12.cc
        NO-ERROR.
    IF AVAIL ttems2Fev THEN DO:

       ASSIGN ttEms2Fev.LOG_encontrado         = YES
              ttTotvs12.vlSaldoInicialEms2     = IF ttEms2Fev.vlSaldoInicial >= 0 THEN ttEms2fev.vlSaldoInicial ELSE ttEms2fev.vlSaldoInicial * -1
              ttTotvs12.vlSaldoInicialAbsEms2  = ttEms2fev.vlSaldoInicial
              ttTotvs12.sinalIniEms2           = IF ttEms2fev.vlSaldoInicial >= 0 THEN "" ELSE "CR"
              ttTotvs12.vlDebitoEms2           = ttEms2fev.vlDebito
              ttTotvs12.vlCreditoEms2          = ttEms2fev.vlCredito
              ttTotvs12.vlSaldoFinalEms2       = IF ttems2fev.vlSaldoFinal >= 0 THEN ttEms2fev.vlSaldoFinal ELSE ttEms2fev.vlSaldoFinal * -1
              ttTotvs12.vlSaldoFinalAbsEms2    = ttEms2fev.vlSaldoFinal
              ttTotvs12.sinalFinalEms2         = IF ttems2fev.vlSaldoFinal >= 0 THEN "" ELSE "CR".
    END.
END.




FOR EACH ttems2fev
    WHERE ttems2fev.LOG_encontrado = NO.
    CREATE ttTotvs12.
    ASSIGN ttTotvs12.conta               = ttems2fev.conta
           ttTotvs12.cc                  = ttems2fev.cc
           ttTotvs12.LOG_criado          = YES
           ttTotvs12.vlSaldoInicial      = IF ttems2fev.vlSaldoInicial >= 0 THEN ttems2fev.vlSaldoInicial ELSE ttems2fev.vlSaldoInicial * -1
           ttTotvs12.sinalIni            = IF ttems2fev.vlSaldoInicial >= 0 THEN '' ELSE 'CR'
           ttTotvs12.vlSaldoInicialAbs   = ttems2fev.vlSaldoInicial
           ttTotvs12.vlDebito            = ttems2fev.vlDebito
           ttTotvs12.vlCredito           = ttems2fev.vlCredito
           ttTotvs12.vlSaldoFinal        = IF ttems2fev.vlSaldoFinal >= 0 THEN ttems2fev.vlSaldoFinal ELSE  ttems2fev.vlSaldoFinal  * -1 
           ttTotvs12.sinalFinal          = IF ttems2fev.vlSaldoFinal >= 0 THEN '' ELSE 'CR'
           ttTotvs12.vlSaldoFinalAbs     = ttems2fev.vlSaldoFinal 
           ttTotvs12.mes                 = 1
           ttTotvs12.registro            = IF int(SUBSTR(ttems2fev.conta,1,1)) < 3 THEN 'K155' ELSE 'K355'
           ttTotvs12.vlSaldoInicialEms2  = ttTotvs12.vlSaldoInicial
           ttTotvs12.sinalIniEms2        = ttTotvs12.sinalIni
           ttTotvs12.vlDebitoEms2        = ttTotvs12.vlDebito
           ttTotvs12.vlCreditoEms2       = ttTotvs12.vlCredito
           ttTotvs12.vlSaldoFinalEms2    = ttTotvs12.vlSaldoFinal
           ttTotvs12.sinalFinalEms2      = ttTotvs12.sinalFinal.
END.


FOR EACH ttTotvs12:

    ASSIGN  ttTotvs12.vlsaldoInicialDif = ttTotvs12.vlSaldoInicialAbs - ttTotvs12.vlSaldoInicialAbsEms2     
            ttTotvs12.vlDebitoDif       = ttTotvs12.vldebito  - ttTotvs12.vlDebitoEms2  
            ttTotvs12.vlCreditoDif      = ttTotvs12.vlCredito - ttTotvs12.vlCreditoEms2  
            ttTotvs12.vlSaldoFinalDif   = ttTotvs12.vlSaldoFinalAbs - ttTotvs12.vlSaldoFinalAbsEms2.
END.

OUTPUT TO c:\temp\tttotvs12.txt.
  FOR EACH ttTotvs12:
      EXPORT DELIMITER "|" ttTotvs12.
  END.
OUTPUT CLOSE.


/*
FOR EACH ttEms2Jan:
    DISP ttEms2Jan WITH WIDTH 550  .
END.
FOR EACH ttEms2Fev:
    DISP ttEms2fev WITH WIDTH 550  .
END. 
*/








