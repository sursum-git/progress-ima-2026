/**************************************************************************
PADRAO DE BO DE CONSULTA
Programa:esbo/boMemorandos.p
Autor: Tadeu Silva Parreiras
Objetivo: Buscar os memorandos conforme o tipo de memorando considerando os diversos n¡veis existentes( global, empresa, estab e programa) para serem utilizandos em programas como impressÆo da NF e do pedido compra por exemplo.
Data:  07/2023
Modificacoes:
*****************************************************************************/


&SCOPED-DEFINE ttParam ttParam
&SCOPED-DEFINE boMsg   HBoMsg

DEFINE TEMP-TABLE {&ttparam} NO-UNDO
    FIELD codPrograma       AS CHAR INIT '*'
    FIELD codEmpresa        AS CHAR INIT '*'
    FIELD codEstab          AS CHAR INIT '*'
    FIELD tipoMemorandoId   AS INT .
DEFINE VARIABLE cMemorando AS CHARACTER  FORMAT 'x(32000)' NO-UNDO.

DEFINE VARIABLE {&boMsg}   AS HANDLE      NO-UNDO.

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
    DEFINE VARIABLE iNivel      AS INTEGER     NO-UNDO INIT 4.
    DEFINE VARIABLE cEmpresa    AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cEstab      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cPrograma   AS CHARACTER   NO-UNDO.
    FIND FIRST {&ttparam} NO-ERROR.

       
    //a partir do tipo de memorando busca do n¡vel mais especifico ao mais generico
    DO WHILE iNivel > 0:
       CASE iNivel:

           WHEN 1  THEN
             ASSIGN cEmpresa    = '*'
                    cEstab      = '*'
                    cPrograma   = '*'.
           WHEN 2 THEN
             ASSIGN cEmpresa    = {&ttParam}.codEmpresa
                    cEstab      = '*'
                    cPrograma   = '*'.
           WHEN 3 THEN
             ASSIGN cEmpresa    = '*'
                    cEstab      = {&ttParam}.codEstab
                    cPrograma   = '*'.
           WHEN 4 THEN
             ASSIGN cEmpresa    = '*'
                    cEstab      = '*'
                    cPrograma   = {&ttParam}.codPrograma .

       END CASE.

       FIND FIRST memorandos NO-LOCK
        WHERE memorandos.tipo_memorando_id  = {&ttParam}.tipoMemorandoId
        AND   memorandos.num_nivel          =  iNivel
        AND   memorandos.dt_hr_ini          <= NOW
        AND   memorandos.dt_hr_fim          >= NOW
        AND   memorandos.cod_empresa        = cEmpresa
        AND   memorandos.cod_estab          = cEstab
        AND   memorandos.cod_programa       = cPrograma NO-ERROR .
        IF AVAIL memorandos THEN DO:
           ASSIGN cMemorando = memorandos.Texto.
           LEAVE.
        END.     
        ASSIGN iNivel = iNivel - 1.

    END.
    





END PROCEDURE.


PROCEDURE getTextoMemorando:

    DEFINE OUTPUT PARAMETER pMemorando AS CHARACTER   NO-UNDO.
    ASSIGN pMemorando = cMemorando.

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

