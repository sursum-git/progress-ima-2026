/**************************************************************************
PADRAO DE BO DE CONSULTA
Programa: 
Autor: 
Objetivo: 
Data: 
Modificacoes:
*****************************************************************************/


&SCOPED-DEFINE ttParam  ttParam
&SCOPED-DEFINE boMsg    HBoMsg
&SCOPED-DEFINE ttResult ttResult
&SCOPED-DEFINE tabela   fats_repres_clientes
DEFINE TEMP-TABLE {&ttResult} NO-UNDO LIKE {&tabela}.
DEFINE VARIABLE {&boMsg}   AS HANDLE      NO-UNDO.
{esbo/boFatLocal.i {&ttparam} ttLocal}
{esp/util.i}
{esp/setProp.i  {&ttparam} }

DEFINE VARIABLE h-acomp AS HANDLE      NO-UNDO.

PROCEDURE iniciar:
RUN esbo/boMsg.p PERSIST SET {&boMsg}.
RUN esbo/boAcomp.p PERSIST SET h-acomp.
CREATE {&ttparam}.
    
END PROCEDURE.

PROCEDURE finalizar:
    IF VALID-HANDLE({&boMsg}) THEN
       DELETE PROCEDURE {&boMsg}.
    IF VALID-HANDLE(h-acomp) THEN
       RUN finalizar IN h-acomp.
    DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

PROCEDURE setValsIni:

    FIND FIRST {&ttParam} NO-ERROR.
    ASSIGN {&ttParam}.data[1] = 01.01.2001
           {&ttParam}.data[2] = 01.01.2060
           {&ttParam}.estab[1] = ''
           {&ttParam}.estab[2] = 'zzzz'
           {&ttParam}.uf[1] = ''
           {&ttParam}.uf[2] = 'zzzz'
           .        
           
    
            

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
       RUN setTitulo IN h-acomp('Faturamento').
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
   
    
    
    FOR EACH {&tabela} NO-LOCK
        WHERE {&tabela}.dt_ini_mes  >= {&ttParam}.data[1]
        AND   {&tabela}.dt_fim_mes  <= {&ttParam}.data[2]
        AND   {&tabela}.cod_estab  >= {&ttParam}.estab[1]
        AND   {&tabela}.cod_estab  <= {&ttParam}.estab[2],
        EACH emitente NO-LOCK
        WHERE emitente.cod-emitente = {&tabela}.cod_emitente
          AND emitente.estado >= {&ttParam}.uf[1]
          AND emitente.estado <= {&ttParam}.uf[2]
       
        .
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

PROCEDURE gerarTtLocal:

FOR EACH {&ttResult}:

    FOR FIRST emitente 
        WHERE emitente.cod-emitente = {&ttResult}.cod_emitente
          :
        
    END.
    FIND ttLocal WHERE ttLocal.cidade = emitente.cidade 
                   AND ttLocal.uf     = emitente.estado
                   AND ttLocal.mes    = {&ttResult}.mes
                   AND ttLocal.ano    = {&ttResult}.ano
                   AND ttLocal.estab  = {&ttResult}.cod_estab NO-ERROR.
   IF NOT AVAIL ttLocal THEN  DO:
      CREATE ttLocal.
      ASSIGN ttLocal.cidade = emitente.cidade 
             ttLocal.uf     = emitente.estado
             ttLocal.mes    = {&ttResult}.mes
             ttLocal.ano    = {&ttResult}.ano
             ttLocal.estab  = {&ttResult}.cod_estab.
       
   END.
   
   ASSIGN ttLocal.vlFat         = ttLocal.vlFat         + {&ttResult}.vl_fat
          ttLocal.vlDesc        = ttLocal.vlDesc        + {&ttResult}.vl_desconto
          ttLocal.vlDevol       = ttLocal.vlDevol       + {&ttResult}.vl_devolucoes
          ttLocal.vlDevolDesc   = ttLocal.vlDevolDesc   + {&ttResult}.vl_desconto_dev.
END.


END PROCEDURE.

PROCEDURE getTtLocal:

DEFINE OUTPUT PARAMETER TABLE FOR ttLocal.

END PROCEDURE.
