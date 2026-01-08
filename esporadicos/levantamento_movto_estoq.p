

DEFINE TEMP-TABLE tt
    FIELD conta AS CHAR FORMAT 'x(12)'
    FIELD cc    AS CHAR 
    FIELD contasaldo AS CHAR FORMAT 'x(12)'
    FIELD ccSaldo AS CHAR
    FIELD itcodigo AS CHAR FORMAT 'x(20)'
    FIELD valor AS DECIMAL
    FIELD tipo  AS INT
    FIELD mes   AS INT
    FIELD ano   AS INT
    FIELD dia   AS INT
    FIELD qt    AS DECIMAL.

DEFINE TEMP-TABLE ttResumoItem
    FIELD itCodigo AS CHAR FORMAT 'x(20)'
    FIELD valor    AS DECIMAL
    FIELD qt       AS DECIMAL
    FIELD qtMesAnt AS DECIMAL
    FIELD qtMesAtu AS DECIMAL
    FIELD vlMesAnt AS DECIMAL
    FIELD vlMesAtu AS DECIMAL
    FIELD difQt    AS DECIMAL
    FIELD difVl    AS DECIMAL.

DEFINE TEMP-TABLE ttInvent
    FIELD classif   AS CHAR FORMAT 'X(30)'
    FIELD itCodigo  AS CHAR FORMAT 'x(20)'
    FIELD descricao AS CHAR FORMAT 'x(50)'
    FIELD unid      AS CHAR
    FIELD quant     AS DECIMAL
    FIELD vlUnit    AS DECIMAL
    FIELD vlTotal   AS DECIMAL
    FIELD mes-ano   AS CHAR
    FIELD LOG_distr AS LOGICAL INIT NO.

DEFINE VARIABLE cMes AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAno AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMesAnoAnt AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMesAnoAtu AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dtIni AS DATE        NO-UNDO.
DEFINE VARIABLE dtFim AS DATE        NO-UNDO.


UPDATE cMes cAno.

ASSIGN cMesAnoAtu = cMes + "-" + cAno.
ASSIGN dtini = DATE('01/' + cMes + '/' + cAno).
IF cMes <> '12' THEN DO:
   ASSIGN dtFim = DATE('01/' + STRING(INT(cMes) + 1,'99') + '/' + cAno) - 1.
          
END.
ELSE
  ASSIGN dtFim = DATE('01/01/' + STRING(INT(cAno) + 1 ,'9999') ) - 1.


IF cMes <> '01' THEN
   ASSIGN cMesAnoAnt = STRING(INT(cMes) - 1,'99') + "-" + cAno.
ELSE
  ASSIGN cMesAnoAnt = STRING(INT(cMes) + 12 - 1,'99') + "-" +  STRING(INT(cAno) - 1,'9999').


 


FOR EACH movto-estoq
    WHERE movto-estoq.dt-trans >= 01.01.2017
    AND   movto-estoq.dt-trans <= 01.31.2017 
    NO-LOCK.
    FIND FIRST ITEM OF movto-estoq
        WHERE ITEM.ge-codigo = 60
        NO-LOCK NO-ERROR.
    IF NOT AVAIL ITEM THEN NEXT.
    //DISP movto-estoq.valor-mat-m[1] WITH 1 COL WIDTH 550.
    FIND FIRST tt
        WHERE tt.conta      = movto-estoq.ct-codigo
        AND   tt.contasaldo = movto-estoq.ct-saldo
        AND   tt.cc         = movto-estoq.sc-codigo
        AND   tt.ccsaldo    = movto-estoq.sc-saldo
        AND   tt.itcodigo   = movto-estoq.it-codigo
        AND   tt.tipo       = movto-estoq.tipo-trans
        AND   tt.ano        = YEAR(movto-estoq.dt-trans)
        AND   tt.mes        = MONTH(movto-estoq.dt-trans)  
        AND   tt.dia        = DAY(movto-estoq.dt-trans)
        NO-LOCK NO-ERROR.
    IF NOT AVAIL tt THEN DO:
       CREATE tt.
       ASSIGN tt.conta = movto-estoq.ct-codigo
              tt.tipo  = movto-estoq.tipo-trans
              tt.ano   = YEAR(movto-estoq.dt-trans)
              tt.mes   = MONTH(movto-estoq.dt-trans)
              tt.dia   = DAY(movto-estoq.dt-trans)
              tt.conta      = movto-estoq.ct-codigo
              tt.contasaldo = movto-estoq.ct-saldo
              tt.cc         = movto-estoq.sc-codigo
              tt.ccsaldo    = movto-estoq.sc-saldo
              tt.itcodigo   = movto-estoq.it-codigo .
    END.
    ASSIGN tt.valor = tt.valor + movto-estoq.valor-mat-m[1]
           tt.qt    = tt.qt    + movto-estoq.quantidade.
    
END.

FOR EACH tt:
    FIND FIRST ttResumoItem
        WHERE tt.itCodigo = ttResumoitem.itCodigo
        NO-LOCK NO-ERROR.
    IF NOT AVAIL ttResumoItem THEN DO:
       CREATE ttResumoItem.
       ASSIGN ttResumoItem.itCodigo = tt.itCodigo.
    END.
    ASSIGN ttResumoItem.qt    = ttResumoItem.qt + IF tt.tipo = 1 THEN tt.qt ELSE tt.qt * -1
           ttResumoItem.valor = ttResumoItem.valor + IF tt.tipo = 1 THEN tt.valor ELSE tt.valor * -1 .
END.

INPUT FROM VALUE('c:\temp\inv_ate_' + cMesAnoAtu + '.csv').

REPEAT:
    
    CREATE ttInvent.
    IMPORT DELIMITER ";" ttInvent.
END.

INPUT CLOSE.

OUTPUT TO c:\temp\ttinvent.
FOR EACH ttinvent:
    EXPORT DELIMITER "|" ttInvent.
END.
OUTPUT CLOSE.


FOR EACH ttResumoItem:
    

    FOR EACH ttInvent
        WHERE ttInvent.itCodigo = ttResumoItem.itCodigo.
        ASSIGN ttInvent.LOG_distr = YES.
     
        IF ttInvent.mes-ano = cMesAnoAtu THEN
           ASSIGN ttResumoItem.qtMesAtu =  ttInvent.quant
                  ttResumoItem.vlMesAtu =  ttInvent.vlTotal.
        IF ttInvent.mes-ano = cMesAnoAnt THEN
           ASSIGN ttResumoItem.qtMesAnt = ttInvent.quant
                  ttResumoItem.vlMesAnt = ttInvent.vlTotal.
    
        ASSIGN ttResumoItem.difQt = ttResumoItem.qtMesAtu - ttResumoItem.qtMesAnt - ttResumoItem.qt
               ttResumoItem.difVl = ttResumoItem.vlMesAtu - ttResumoItem.vlMesAnt - ttResumoItem.valor.
    END.

END.

OUTPUT TO c:\temp\ttinventSemDistr.txt.

FOR EACH ttInvent
    WHERE ttInvent.LOG_distr  = NO.
    
    FIND FIRST ttResumoItem
        WHERE ttResumoItem.itCodigo = ttInvent.itCodigo
        NO-LOCK NO-ERROR.
    IF NOT AVAIL ttResumoItem THEN DO:
       
        CREATE ttResumoItem.
        ASSIGN 
        ttResumoItem.itCodigo  = ttInvent.itCodigo
        ttResumoItem.valor     = 0 
        ttResumoItem.qt        = 0 .
    
    END.

    ASSIGN 
        ttResumoItem.qtMesAnt   = IF ttInvent.mes-ano = cMesAnoAnt THEN ttInvent.quant ELSE ttResumoItem.qtMesAnt
        ttResumoItem.qtMesAtu   = IF ttInvent.mes-ano = cMesAnoAtu THEN ttInvent.quant ELSE ttResumoItem.qtMesAtu
        ttResumoItem.vlMesAnt   = IF ttInvent.mes-ano = cMesAnoAnt THEN ttInvent.vlTotal ELSE ttResumoItem.vlMesAnt 
        ttResumoItem.vlMesAtu   = IF ttInvent.mes-ano = cMesAnoAtu THEN ttInvent.vlTotal ELSE ttResumoItem.vlMesAtu 
        ttResumoItem.difQt      = ttResumoItem.qtMesAtu - ttResumoItem.qtMesAnt - ttResumoItem.qt
        ttResumoItem.difVl      = ttResumoItem.vlMesAtu - ttResumoItem.vlMesAnt - ttResumoItem.valor. 
    
 /*   IF ttInvent.itCodigo = '130058' THEN               */
 /*   MESSAGE "mes reg. invent:" ttInvent.mes-ano SKIP   */
 /*           "mes ano ant. param:" cMesAnoAnt SKIP      */
 /*           "mes ano atu param:" cMesAnoAtu SKIP       */
 /*           "qt.mes ant:" ttResumoItem.qtMesAnt SKIP   */
 /*           "qt.mes atu:" ttResumoItem.qtMesAtu SKIP   */
 /*           "vl.mes ant:" ttResumoItem.vlMesAnt SKIP   */
 /*           "vl.mes atu:" ttResumoItem.vlMesAtu SKIP   */
 /*                                                      */
 /*    VIEW-AS ALERT-BOX INFO BUTTONS OK.                */

    EXPORT DELIMITER "|" ttInvent.
END.
OUTPUT CLOSE.


OUTPUT TO c:\temp\ttResumoItem.txt.
 FOR EACH ttResumoItem BY ttResumoItem.itcodigo:
     EXPORT DELIMITER "|" ttResumoItem.
 END.
 OUTPUT CLOSE.

OUTPUT TO c:\temp\ttAnalitico.txt.
FOR EACH tt:
    EXPORT DELIMITER ";" tt.
END.




OUTPUT CLOSE.
