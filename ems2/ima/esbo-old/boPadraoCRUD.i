
DEFINE TEMP-TABLE {&ttReg} LIKE {&tabela}.
DEFINE VARIABLE {&boMsg}        AS HANDLE       NO-UNDO.
DEFINE VARIABLE lErro           AS LOGICAL      NO-UNDO.
DEFINE VARIABLE rowidCorrente   AS ROWID        NO-UNDO.
&IF "{&varChavePai}" <> '' &THEN
   DEFINE VARIABLE {&varChavePai}  AS INT64       NO-UNDO.
&ENDIF
DEFINE VARIABLE idRegCorrente   AS INT64        NO-UNDO.


PROCEDURE iniciar:
    RUN esbo/boMsg.p PERSIST SET {&boMsg}.
    CREATE {&ttReg}.
END PROCEDURE.

PROCEDURE finalizar:
    IF VALID-HANDLE({&boMsg}) THEN
       DELETE PROCEDURE {&boMsg}.
    DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

PROCEDURE setIdPai:

    DEFINE INPUT  PARAMETER  pID AS INT64     NO-UNDO.

    &IF "{&varChavePai}" <> '' &THEN
       ASSIGN {&varChavePai} = pID .
    &ENDIF

END PROCEDURE.

PROCEDURE getRowidRegCorrente:

    DEFINE INPUT  PARAMETER pRowid AS ROWID       NO-UNDO.
    ASSIGN pRowid = rowidCorrente .

END PROCEDURE.

PROCEDURE getIdRegCorrente:
    DEFINE OUTPUT PARAMETER pId AS INT64     NO-UNDO.
    ASSIGN pId = idRegCorrente .

END PROCEDURE.



PROCEDURE setTTReg:

    DEFINE INPUT PARAMETER TABLE FOR {&ttReg}.
    FIND FIRST {&ttReg} NO-ERROR.
    IF AVAIL {&ttReg} THEN DO:
       &IF "{&cpChavePai}" <> '' AND  "{&varChavePai}" <> '' &THEN
           ASSIGN {&ttReg}.{&cpChavePai} = {&varChavePai} .
       &ENDIF 
    END.
    

    IF lookup('setTTRegEspec',THIS-PROCEDURE:INTERNAL-ENTRIES) > 0 THEN
       RUN setTTRegEspec.
  

END PROCEDURE.

PROCEDURE incluir:
    RUN validacoes('comum').
    RUN validacoes('incluir').
    RUN getErro IN {&boMsg}(OUTPUT lErro) .
    IF lErro THEN RETURN 'nok'.

    FIND FIRST {&ttReg} NO-ERROR.
    CREATE {&tabela}.
    BUFFER-COPY {&ttReg} EXCEPT {&cpId} TO {&tabela}.
    ASSIGN {&tabela}.{&cpId} = next-value({&seqId}).
    
    ASSIGN rowidCorrente = ROWID({&tabela})
           idRegCorrente = {&tabela}.{&cpid}.

END PROCEDURE.


PROCEDURE alterar:
    RUN validacoes('comum').
    RUN validacoes('alterar').
    RUN getErro(OUTPUT lErro) .
    IF lErro THEN RETURN 'nok'.
    FIND FIRST {&ttReg} NO-ERROR.
    FIND {&tabela} exclusive-LOCK WHERE
        {&tabela}.{&cpId} = {&ttReg}.{&cpId} NO-ERROR.
    IF AVAIL {&tabela} THEN DO:
       ASSIGN rowidCorrente = ROWID({&tabela})
              idRegCorrente = {&tabela}.{&cpid}.
       BUFFER-COPY {&ttReg}  TO {&tabela}. //{&exceptCps}
       RELEASE {&tabela}.
    END.
    ELSE DO:
        RUN setMsg IN {&bomsg}(3,'Registro N∆o encontrado para alteraá∆o','erro').
    END.
    
END PROCEDURE.


PROCEDURE excluir:
    
    RUN validacoes('excluir').
    RUN validacoes('comum').
    RUN getErro IN {&boMsg}(OUTPUT lErro) .
    IF lErro THEN RETURN 'nok'.
    FIND FIRST {&ttReg} NO-ERROR.
    FIND {&tabela} EXCLUSIVE-LOCK WHERE
        {&tabela}.{&cpId} = {&ttReg}.{&cpId} NO-ERROR.
    IF AVAIL {&tabela} THEN DO:
       ASSIGN rowidCorrente = ROWID({&tabela})
              idRegCorrente = {&tabela}.{&cpid}.
       DELETE {&tabela}.
       RELEASE {&tabela}.
    END.
    ELSE DO:
       RUN setMsg IN {&bomsg}(4,'Registro N∆o encontrado para exclus∆o','erro').
    END.

END PROCEDURE.

PROCEDURE verifExist:
    DEFINE OUTPUT PARAMETER lAChou AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE iCont       AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cp          AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cValorCp    AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE listaCps    AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE condicao    AS CHARACTER   NO-UNDO.
    
    ASSIGN listaCps = '{&CpsUnico}' .
    IF listaCps = '' THEN DO: 
       //ASSIGN lAchou = NO.
       RETURN 'ok'.
    END.
    //se n∆o achar ttreg sai 
    IF NOT CAN-FIND(FIRST {&ttReg}) THEN RETURN 'ok'.
    //{esp/exportarTabelacsv3.i {&ttReg} " " " " "  "ttRegPadraoCRUD" }
    REPEAT iCont = 1 TO NUM-ENTRIES(listaCps).
       ASSIGN cp = trim(ENTRY(iCont,listaCps)).     
       RUN getVlCpTT(TEMP-TABLE {&ttreg}:HANDLE,cp,0,OUTPUT cValorCp).
       RUN incrValor(INPUT-OUTPUT condicao, cp + " = " + quoter(trim(cValorCp)), " AND " ).  
       
    END.
    RUN esapi/verifExist.p('{&tabela}',
                           '{&cpId}',
                           condicao,
                           '', //inner
                           OUTPUT lAchou
                           ).

END PROCEDURE.

PROCEDURE getErro:

    DEFINE OUTPUT PARAMETER pErro AS CHARACTER   NO-UNDO.
    RUN getErros IN {&boMsg}(OUTPUT pErro).

END PROCEDURE.
