DEFINE INPUT  PARAMETER pItCodigo       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pQtAnterior     AS DECIMAL     NO-UNDO.
DEFINE INPUT  PARAMETER pQtAtual        AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER pErro           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hBoEsp800               AS HANDLE      NO-UNDO.
{method/dbotterr.i}
{esbo/boesp800.i ttPendsBook}
{esp/util.i}

RUN esbo/boesp800.p PERSIST SET hBoESP800.
RUN setConstraintPend IN hBoEsp800(pItCodigo).
RUN openQueryStatic IN hBoEsp800 (INPUT "pend":U).
RUN getLast IN hBoEsp800.
IF RETURN-VALUE = 'nok' THEN DO: //n∆o existe pendencia atual de geraá∆o de book
    RUN openQueryStatic IN hBoEsp800 (INPUT "main":U).
    RUN emptyRowErrors  IN hBoEsp800.
    CREATE ttPendsBook.
    ASSIGN ttPendsBook.pend_book_id   = NEXT-VALUE(seq_pends_book)
        ttPendsBook.dt_hr_registro = NOW
        ttPendsBook.it_codigo      = pItCodigo
        ttPendsBook.num_situacao   = 1 //pendente
        ttPendsBook.qt_anterior    = pQtAnterior
        ttPendsBook.qt_atual       = pQtAtual
      .

    RUN setRecord       IN hBoEsp800(TABLE ttPendsBook).
    RUN createRecord    IN hBoEsp800.
    RUN getRowErrors    IN hBoEsp800(OUTPUT TABLE rowErrors).
    IF can-find(FIRST rowErrors) THEN DO:
        FOR EACH rowErrors:
            RUN incrValor(INPUT-OUTPUT pErro,string(rowErrors.ErrorSequence) + "-" + string(rowErrors.ErrorNumber) + "-" +  rowErrors.ErrorDescription,CHR(13) ).
        END.
    END.
END.

IF VALID-HANDLE(hBOesp800) THEN
   RUN destroy IN hBOesp800.


FUNCTION getDescrSituacao RETURNS CHAR(iSituacao AS INT):

    CASE iSituacao:
        WHEN 1 THEN
            RETURN 'Pendente'.
        WHEN 2 THEN
            RETURN 'Gerado'.
        WHEN 3 THEN
            RETURN 'Com Erro'.
        WHEN 4 THEN
            RETURN 'Liberado Estilo'.
        WHEN 5 THEN
            RETURN 'Gerado ap¢s Liberado'.

    END CASE.

END FUNCTION.

