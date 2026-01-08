/**************************************************************************
PADRAO DE BO DE CONSULTA
Programa: boCalcTipFrete.p
Autor: 
Objetivo: 
Data: 
Modificacoes:
*****************************************************************************/


&SCOPED-DEFINE ttParam ttParam
&SCOPED-DEFINE boMsg   HBoMsg

DEFINE TEMP-TABLE {&ttparam}
    FIELD a AS CHAR.
    
    .

DEFINE VARIABLE {&boMsg}   AS HANDLE      NO-UNDO.
DEFINE VARIABLE iTransp    AS INTEGER     NO-UNDO.

{esp/util.i}
{esp/setProp.i  {&ttparam} }

PROCEDURE iniciar:
RUN esbo/boMsg.p PERSIST SET {&boMsg}.
CREATE {&ttparam}.
    
END PROCEDURE.

PROCEDURE finalizar:
    IF VALID-HANDLE({&boMsg}) THEN
       DELETE PROCEDURE {&boMsg}.
    DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

PROCEDURE exec:
    FIND FIRST {&ttparam} NO-ERROR.


END PROCEDURE.


PROCEDURE exportarTTParam:

OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY +  'PARAMETROS_' + PROGRAM-NAME(1) + '.txt').
    FOR EACH {&ttParam}.
        DISP {&ttParam} WITH 1 COL WIDTH 550.
    END.
OUTPUT CLOSE.


END PROCEDURE.
