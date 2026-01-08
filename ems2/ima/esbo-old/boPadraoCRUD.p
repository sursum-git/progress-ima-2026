/**************************************************************************
PADRAO DE BO DE CRUD
Programa:
Autor: 
Objetivo: 
Data: 
Modificacoes:
*****************************************************************************/

&SCOPED-DEFINE tabela    <tabela>   
&SCOPED-DEFINE cpId      <campo id>
&SCOPED-DEFINE cpChavePai <campo chave estrangeira pai>
&SCOPED-DEFINE exceptCps {&cpId} {&cpChavePai}
&SCOPED-DEFINE CpsUnico   <campos chave unica>
&SCOPED-DEFINE varChavePai <campo chave estrangeira pai>

&SCOPED-DEFINE seqId     <sequencia campo id>
&SCOPED-DEFINE ttReg   ttReg 
&SCOPED-DEFINE boMsg   HBoMsg

DEFINE TEMP-TABLE {&ttReg} LIKE {&tabela}.

DEFINE VARIABLE {&boMsg}        AS HANDLE      NO-UNDO.
DEFINE VARIABLE lErro           AS LOGICAL     NO-UNDO.
DEFINE VARIABLE rowidCorrente   AS ROWID       NO-UNDO.

{esp/util.i}
{esp/setProp.i  {&ttReg} }
{esbo/boPadraoCRUD.i}

PROCEDURE iniciar:
RUN esbo/boMsg.p PERSIST SET {&boMsg}.
CREATE {&ttReg}.
END PROCEDURE.

PROCEDURE finalizar:
    IF VALID-HANDLE({&boMsg}) THEN
       DELETE PROCEDURE {&boMsg}.
    DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.



PROCEDURE setTTReg:

    DEFINE INPUT PARAMETER TABLE FOR {&ttReg}.

END PROCEDURE.

PROCEDURE getRowidRegCorrente:

    DEFINE INPUT  PARAMETER pRowid AS ROWID       NO-UNDO.
    ASSIGN pRowid = rowidCorrente .

END PROCEDURE.



PROCEDURE validacoes:

   DEFINE INPUT  PARAMETER pAcao AS CHARACTER   NO-UNDO. //incluir,alterar,excluir,comum

   FIND FIRST {&ttReg} NO-ERROR.

   RUN limparTTMsg IN {&boMsg} .

   CASE pAcao:
       WHEN 'incluir' THEN DO:
           RUN verifExist(OUTPUT lAChou).
           IF lAchou THEN DO:
              RUN setMsg IN {&boMsg}(2,'J† existe um registro com valores iguais para os campos:' + {&CpsUnico}).
              RETURN 'nok'.
           END.

       END.
       WHEN 'alterar' THEN DO:

       END.
       WHEN 'excluir' THEN DO:

       END.
       WHEN 'comum' THEN DO:
         IF NOT CAN-FIND(FIRST {&ttReg}) THEN DO:
            RUN setMsg IN {&bomsg}(1,'Tabela Tempor†ria n∆o preenchida').
            RETURN 'nok'.
         END.
       END.


   END CASE.


END PROCEDURE.

PROCEDURE getErro:

    DEFINE OUTPUT PARAMETER pErro AS CHARACTER   NO-UNDO.
    RUN getErro IN {&boMsg}.

END PROCEDURE.



