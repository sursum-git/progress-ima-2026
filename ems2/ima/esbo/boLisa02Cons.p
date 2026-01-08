/**************************************************************************
PADRAO DE BO DE CONSULTA
Programa: esbo/bolisa02cons
Autor: Tadeu Silva Parreiras
Objetivo: Consultas a tabela romaneios_retorno_lisa
Data:04/2024 
Modificacoes:
*****************************************************************************/


&SCOPED-DEFINE ttParam  ttParam
&SCOPED-DEFINE boMsg    HBoMsg
&SCOPED-DEFINE ttResult ttResult
&SCOPED-DEFINE tabela   romaneios_retorno_lisa
DEFINE TEMP-TABLE {&ttparam}
    FIELD retornoLisaID AS  INT EXTENT 2
    FIELD itCodigo      AS CHAR EXTENT 2
    FIELD codRefer      AS CHAR EXTENT 2
    .
{esbo/boLisa02Cons.i}
DEFINE VARIABLE {&boMsg}   AS HANDLE      NO-UNDO.
DEFINE TEMP-TABLE {&ttResult} LIKE {&tabela}.



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
    ASSIGN {&ttParam}.retornoLisaId [1] = 0
           {&ttParam}.retornoLisaId [2] = 9999999
           {&ttParam}.itCodigo[1]       = ''
           {&ttParam}.itCodigo[2]       = 'zzzzzzzzzzzzzz'
           {&ttParam}.codRefer[1]       = ''
           {&ttParam}.codRefer[2]       = 'zzzzz'
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
       RUN setTitulo IN h-acomp('Extra‡Æo Dados Ped.LISA').
    END.

    

END PROCEDURE.


PROCEDURE setBoMsg:

    DEFINE INPUT  PARAMETER pHBoMsg AS HANDLE      NO-UNDO.
    ASSIGN {&boMsg} = pHBoMsg.

END PROCEDURE.

PROCEDURE exec:
    RUN limparTTMsg IN {&boMsg}.
    EMPTY TEMP-TABLE {&ttResult}.
    FIND FIRST {&ttparam} NO-ERROR.
    IF NOT AVAIL {&ttParam} THEN DO:
       RUN setMsg IN {&bomsg}(1,'NÆo foram passados parametros').
       RETURN 'nok'.
    END.

/*    MESSAGE 

      'retorno ini:' {&ttParam}.retornoLisaId[1]    
      'retorno fim:' {&ttParam}.retornoLisaId[2]    
      'produto ini:' {&ttParam}.itCodigo[1]         
      'produto fim:' {&ttParam}.itCodigo[2]         
      'ref.ini:' {&ttParam}.codRefer[1]         
      'ref.fim:' {&ttParam}.codRefer[2]         

        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        */
    FOR EACH {&tabela} NO-LOCK
        WHERE {&tabela}.retorno_lisa_id     >= {&ttParam}.retornoLisaId[1]
        AND   {&tabela}.retorno_lisa_id     <= {&ttParam}.retornoLisaId[2]
        AND   {&tabela}.it_codigo           >= {&ttParam}.itCodigo[1]
        AND   {&tabela}.it_codigo           <= {&ttParam}.itCodigo[2]
        AND   {&tabela}.cod_refer           >= {&ttParam}.codRefer[1]
        AND   {&tabela}.cod_refer           <= {&ttParam}.codRefer[2]
        .
        CREATE {&ttResult}.
        BUFFER-COPY {&tabela} TO {&ttResult}.
    END.

END PROCEDURE.

PROCEDURE gerarTtQtItemRef:
    EMPTY TEMP-TABLE ttQtItemRef.
    DEFINE VARIABLE iQt AS INTEGER     NO-UNDO.
    FOR EACH {&ttResult}:
        FIND ttQtItemRef
            WHERE ttQtItemRef.retornoLisaId = {&ttResult}.retorno_lisa_id
            AND   ttQtItemRef.itCodigo      = {&ttResult}.it_codigo
            AND   ttQtItemRef.codRefer      = {&ttResult}.cod_refer NO-ERROR.
        IF NOT AVAIL ttQtItemRef THEN DO:
           ASSIGN iQt = iQt + 1 .
           CREATE ttQtItemRef.
           ASSIGN ttQtItemRef.id            = iQt
                  ttQtItemRef.retornoLisaId = {&ttResult}.retorno_lisa_id        
                  ttQtItemRef.itCodigo      = {&ttResult}.it_codigo             
                  ttQtItemRef.codRefer      = {&ttResult}.cod_refer             
                  .
        END.
        ASSIGN ttQtItemRef.qt = ttQtItemRef.qt + {&ttResult}.quantidade .
    END.



END PROCEDURE.

PROCEDURE getTtQtItemRef:

    DEFINE OUTPUT PARAMETER TABLE FOR ttQtItemRef.
    RUN gerarTtQtItemRef.


END PROCEDURE.


PROCEDURE getSomaQtRetorno:
    //s¢ tem sentido passar este metodo quando o filtro for por retorno, item e ref especificos
    
    DEFINE OUTPUT PARAMETER dSoma AS DECIMAL     NO-UNDO.
    RUN gerarTTQtItemRef.
    FOR EACH ttQtItemRef:
        ASSIGN dSoma = dSoma  + ttQtItemRef.qt .
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

    DEFINE OUTPUT PARAMETER TABLE FOR {&ttResult}.

END PROCEDURE.
