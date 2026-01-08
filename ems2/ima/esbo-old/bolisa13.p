/**************************************************************************
PADRAO DE BO DE CRUD
Programa:esbo/boLisa13.p
Autor: tadeu Silva Parreiras 
Objetivo: Manter a tabela etiqueta_LISA
Data:12/2023
Modificacoes:
*****************************************************************************/


&SCOPED-DEFINE tabela   etiqueta_lisa
&SCOPED-DEFINE ttReg   ttReg 
&SCOPED-DEFINE boMsg   HBoMsg

DEFINE TEMP-TABLE {&ttReg} LIKE {&tabela}.

DEFINE VARIABLE {&boMsg}   AS HANDLE      NO-UNDO.
DEFINE VARIABLE lErro      AS LOGICAL     NO-UNDO.

{esp/util.i}
{esp/setProp.i  {&ttReg} }
{utp/ut-glob.i}
PROCEDURE iniciar:
RUN esbo/boMsg.p PERSIST SET {&boMsg}.
END PROCEDURE.

PROCEDURE finalizar:
    IF VALID-HANDLE({&boMsg}) THEN
       DELETE PROCEDURE {&boMsg}.
    DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.



PROCEDURE setTTReg:

    DEFINE INPUT PARAMETER TABLE FOR {&ttReg}.

END PROCEDURE.


PROCEDURE exec:
    FIND FIRST {&ttReg} NO-ERROR.

END PROCEDURE.


PROCEDURE incluir:

    
    RUN limparTTMsg IN {&boMsg} .
    RUN validacoes('incluir').
    RUN validacoes('comum').
    RUN getErro IN {&boMsg}(OUTPUT lErro) .
    IF lErro THEN RETURN 'nok'.

    CREATE {&tabela}.
    ASSIGN {&tabela}.etiqueta_lisa_id       = next-value(seq_etiqueta_lisa).
          
    BUFFER-COPY {&ttReg} EXCEPT {&ttReg}.etiqueta_lisa_id TO {&tabela} .
    ASSIGN  {&tabela}.dt_hr_criacao          = NOW
            {&tabela}.cod_usuario_criacao    = c-seg-usuario
           .
    



END PROCEDURE.


PROCEDURE alterar:
    RUN limparTTMsg IN {&boMsg} .
    RUN validacoes('alterar').
    RUN validacoes('comum').
    RUN getErro(OUTPUT lErro) .
    IF lErro THEN RETURN 'nok'.
    FIND CURRENT {&tabela} EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL {&tabela} THEN DO:
       BUFFER-COPY {&ttReg} TO {&tabela}.
       RELEASE {&tabela} NO-ERROR.
    END.


END PROCEDURE.


PROCEDURE excluir:
    RUN limparTTMsg IN {&boMsg} .
    RUN validacoes('excluir').
    RUN validacoes('comum').
    RUN getErro IN {&boMsg}(OUTPUT lErro) .
    IF lErro THEN RETURN 'nok'.

    FIND CURRENT {&tabela} NO-LOCK NO-ERROR.
    IF AVAIL {&tabela} THEN
       DELETE {&tabela}.


END PROCEDURE.


PROCEDURE validacoes:

   DEFINE INPUT  PARAMETER pAcao AS CHARACTER   NO-UNDO. //incluir,alterar,excluir,comum
   
   FIND FIRST {&ttReg} NO-ERROR.
   IF NOT AVAIL {&ttReg} THEN DO:
      RUN setMsg IN {&bomsg}(1,'Dados n∆o inseridos na tabela Tempor†ria','erro').
      RETURN 'nok'.
   END.

   FIND {&tabela} NO-LOCK
        WHERE {&tabela}.etiqueta_lisa_id = {&ttReg}.etiqueta_lisa_id
        NO-ERROR.
   CASE pAcao:
       WHEN 'incluir' THEN DO:
           
            IF AVAIL {&tabela} THEN DO:
               RUN setMsg IN {&boMsg}(10,'Registro j† existe com o Id:' + STRING({&ttReg}.etiqueta_lisa_id),'erro').
               RETURN 'nok'.
            END.

            IF CAN-FIND( {&tabela} 
            WHERE {&tabela}.id_etq_lisa = {&ttReg}.id_etq_lisa) THEN DO:
               RUN setMsg IN {&boMsg}(11,'Registro j† existe com o numero da etiqueta Lisa:' + STRING({&ttReg}.id_etq_lisa),'erro' ).
               RETURN 'nok'.
            END.

            IF CAN-FIND( {&tabela} 
            WHERE {&tabela}.cod_estabel = {&ttReg}.cod_estabel AND {&tabela}.num_etiqueta = {&ttReg}.num_etiqueta) THEN DO:
               RUN setMsg IN {&boMsg}(11,'Registro j† existe para o estabelecimento:' + {&ttReg}.cod_estabel + ' e numero da etiqueta IMA:' + STRING({&ttReg}.num_etiqueta),'erro' ).
               RETURN 'nok'.
            END.
       END.
       WHEN 'alterar' THEN DO:
            IF NOT AVAIL {&tabela} THEN DO:
               RUN setMsg IN {&boMsg}(20,'Registro n∆o encontrado para alteraá∆o','erro').
               RETURN 'nok'.
            END.

       END.
       WHEN 'excluir' THEN DO:
            IF NOT AVAIL {&tabela} THEN DO:
               RUN setMsg IN {&boMsg}(30,'Registro n∆o encontrado para exclus∆o','erro').
               RETURN 'nok'.
            END.
       END.
       WHEN 'comum' THEN DO:

       END.


   END CASE.


END PROCEDURE.

PROCEDURE getErro:

    DEFINE OUTPUT PARAMETER pErro AS CHARACTER   NO-UNDO.
    RUN getErro IN {&boMsg}.

END PROCEDURE.


PROCEDURE verifExist:


    FIND {&tabela} NO-LOCK
        WHERE {&tabela}.etiqueta_lisa_id = {&ttReg}.etiqueta_lisa_id
        NO-ERROR.
   IF AVAIL {&tabela} THEN
      RETURN 'ok'.
   ELSE
      RETURN 'nok'.


END PROCEDURE .

PROCEDURE  sincronizar:

    RUN verifExist.
    IF RETURN-VALUE = 'NOK' THEN
       RUN incluir.
    ELSE
       RUN alterar.

END PROCEDURE.
