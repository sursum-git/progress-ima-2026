/**************************************************************************
PADRAO DE BO DE CONSULTA
Programa: esbo/boWeb100.p
Autor: Tadeu Silva
Objetivo:  Consultas a tabela locais_estoq_portal
Data: 04/2024
Modificacoes:
*****************************************************************************/


&SCOPED-DEFINE ttParam  ttParam
&SCOPED-DEFINE boMsg    HBoMsg
&SCOPED-DEFINE ttResult ttResult
&SCOPED-DEFINE tabela   locais_estoq_portal
DEFINE TEMP-TABLE {&ttparam}
    FIELD codEstab AS CHAR EXTENT 2
    .

DEFINE VARIABLE {&boMsg}   AS HANDLE      NO-UNDO.
DEFINE TEMP-TABLE {&ttResult} LIKE {&tabela} .


{esp/util.i}
{esp/setProp.i  {&ttparam} }

DEFINE VARIABLE h-acomp AS HANDLE      NO-UNDO.

PROCEDURE iniciar:
RUN esbo/boMsg.p PERSIST SET {&boMsg}.
CREATE {&ttparam}.
    
END PROCEDURE.

PROCEDURE finalizar:
    IF VALID-HANDLE({&boMsg}) THEN
       DELETE PROCEDURE {&boMsg}.
    DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

PROCEDURE setValsIni:

    FIND FIRST {&ttParam} NO-ERROR.
    IF AVAIL {&ttParam} THEN DO:
       ASSIGN {&ttParam}.codEstab[1] = ''
              {&ttParam}.codEstab[2] = 'zzzz'.
    END.

            

END PROCEDURE.

PROCEDURE setTTParam:

    DEFINE INPUT PARAMETER TABLE FOR {&ttParam}.

END PROCEDURE.

PROCEDURE setAcomp:

    DEFINE INPUT  PARAMETER logHabilita AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pHAComp     AS HANDLE      NO-UNDO.

    RUN setHabilita IN h-acomp(logHabilita).
    IF valid-handle(phAcomp) THEN DO:
       RUN setHandle IN h-acomp(phAComp).
    END.
    ELSE DO:
       RUN setTitulo IN h-acomp('Locais de Estoque').
    END.

    

END PROCEDURE.


PROCEDURE setBoMsg:

    DEFINE INPUT  PARAMETER pHBoMsg AS HANDLE      NO-UNDO.
    ASSIGN {&boMsg} = pHBoMsg.

END PROCEDURE.



PROCEDURE exec:

    RUN limparTTMsg IN {&boMsg}.
    FIND FIRST {&ttparam} NO-ERROR.
    IF NOT AVAIL {&ttParam} THEN DO:
       RUN setMsg IN {&bomsg}(1,'NÆo foram passados parametros').
       RETURN 'nok'.
    END.

    FOR EACH {&tabela}
        WHERE {&tabela}.cod_estab       >= {&ttParam}.codEstab[1] 
        AND   {&tabela}.cod_estab       <= {&ttParam}.codEstab[2]
        AND   {&tabela}.dt_hora_inicio  <= NOW
        AND   {&tabela}.dt_hora_fim     >= NOW .
        //to do filters

        CREATE {&ttResult}.
        BUFFER-COPY {&tabela} TO {&ttResult}.
    END.
        


END PROCEDURE.


PROCEDURE exportarTTParam:

OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY +  'PARAMETROS_' + PROGRAM-NAME(1) + '.txt').
    FOR EACH {&ttParam}.
        DISP {&ttParam} WITH 1 COL WIDTH 550.
    END.
OUTPUT CLOSE.


END PROCEDURE.

PROCEDURE getErro:

    DEFINE OUTPUT PARAMETER pErro AS CHARACTER   NO-UNDO.
    RUN getErro IN {&boMsg}.

END PROCEDURE.

PROCEDURE getTTResult:

    DEFINE OUTPUT PARAMETER TABLE FOR ttResult.

END PROCEDURE.
