/**************************************************************************
Programa:esbo/boes049Cons.p
Autor:Tadeu Silva 
Objetivo: Consultas da tabela ob-etiqueta 
Data: 12/2023
Modificacoes:
*****************************************************************************/


&SCOPED-DEFINE ttParam ttParam
&SCOPED-DEFINE boMsg   HBoMsg
&SCOPED-DEFINE ttResult ttResult
&SCOPED-DEFINE tabela   ob-etiqueta


DEFINE TEMP-TABLE {&ttparam}
    FIELD cod-estabel    LIKE {&tabela}.cod-estabel        EXTENT 2
    FIELD num-etiqueta   LIKE {&tabela}.num-etiqueta       EXTENT 2
    FIELD it-codigo      LIKE {&tabela}.it-codigo          EXTENT 2
    FIELD cod-Refer      LIKE {&tabela}.cod-refer          EXTENT 2
    FIELD nr-Container   LIKE {&tabela}.nr-container       EXTENT 2
    FIELD num-Rolo-imp   LIKE {&tabela}.num-rolo-imp       EXTENT 2
    FIELD situacao       LIKE {&tabela}.situacao           EXTENT 2
    .

DEFINE VARIABLE {&boMsg}   AS HANDLE      NO-UNDO.
DEFINE TEMP-TABLE {&ttResult} LIKE {&tabela}.

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

PROCEDURE setValsIni:

    FIND FIRST {&ttParam} NO-ERROR.
    //setar os valores iniciais


END PROCEDURE.

PROCEDURE setTTParam:

    DEFINE INPUT PARAMETER TABLE FOR {&ttParam}.

END PROCEDURE.


PROCEDURE exec:
    RUN limparTTMsg IN {&boMsg}.
    FIND FIRST {&ttparam} NO-ERROR.
    IF NOT AVAIL {&ttParam} THEN DO:
       RUN setMsg IN {&bomsg}(1,'NÆo foram passados parametros').
       RETURN 'nok'.
    END.

    FOR EACH {&tabela} NO-LOCK
        WHERE {&tabela}.cod-estabel     >= {&ttParam}.cod-estabel[1]    
        AND   {&tabela}.cod-estabel     <= {&ttParam}.cod-estabel[2]     
        AND   {&tabela}.num-etiqueta    >= {&ttParam}.num-etiqueta[1]   
        AND   {&tabela}.num-etiqueta    <= {&ttParam}.num-etiqueta[2]   
        AND   {&tabela}.it-codigo       >= {&ttParam}.it-codigo[1]      
        AND   {&tabela}.it-codigo       <= {&ttParam}.it-codigo[2]      
        AND   {&tabela}.cod-Refer       >= {&ttParam}.cod-refer[1]      
        AND   {&tabela}.cod-Refer       <= {&ttParam}.cod-refer[2]      
        AND   {&tabela}.nr-Container    >= {&ttParam}.nr-container[1] 
        AND   {&tabela}.nr-Container    <= {&ttParam}.nr-container[2] 
        AND   {&tabela}.num-Rolo-imp    >= {&ttParam}.num-rolo-imp[1]
        AND   {&tabela}.num-Rolo-imp    <= {&ttParam}.num-rolo-imp[2]
        AND   {&tabela}.situacao        >= {&ttParam}.situacao[1]
        AND   {&tabela}.situacao        <= {&ttParam}.situacao[2] .

        BUFFER-COPY {&tabela} TO ttResult.
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
