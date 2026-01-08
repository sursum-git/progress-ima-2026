/**********************************************************************************************
programa: esapi/boSincrPendBook.p
objetivo: registrar historico de origem e pendencia de gera‡Æo de book para possibilitar a 
gera‡Æo do book por demanda.
Desenv: Tadeu
data: 04/2024
**********************************************************************************************/
{esp/util.i}
{method/dbotterr.i}
{esbo/boEsp800.i ttPendBook}
{esbo/boEs600.i  ttHistPendBook}

DEFINE INPUT  PARAMETER pItem       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pRef        AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pQtAnterior AS DECIMAL     NO-UNDO.
DEFINE INPUT  PARAMETER pQtAtual    AS DECIMAL     NO-UNDO.
DEFINE INPUT  PARAMETER pTabela     AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pAcao       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pChave      AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER cErros      AS CHARACTER   NO-UNDO.


DEFINE VARIABLE hBoEsp800   AS HANDLE      NO-UNDO. // pendsBook
DEFINE VARIABLE hBoEs600    AS HANDLE      NO-UNDO. // histPendBook
DEFINE VARIABLE idPendBook  AS INTEGER     NO-UNDO.



RUN esbo/boEsp800.p  PERSIST SET hBoEsp800.
RUN esbo/boEs600.p   PERSIST SET hBoEs600.
RUN getIdPendBook(pItem, OUTPUT idPendBook).
//necess rio criar a pendencia do book
IF idPendBook = 0 THEN DO:
   RUN criarPendBook(INPUT  pItem,
                     OUTPUT idPendBook
                     ).
END.
ELSE DO:
   RUN atuDtHrPendBook.
END.
//se criou a pendencia com sucesso, cria o historico da pendencia
IF idPendBook <> 0 THEN DO:
   RUN criarHistPendBook(
            INPUT idPendBook,
            INPUT  pItem,
            INPUT  pRef,
            INPUT  pQtAnterior,
            INPUT  pQtATual,
            INPUT  pTabela,
            INPUT  pAcao,
            INPUT  pChave).


END.



RUN destroy IN hBoEsp800.
RUN destroy IN hBoEs600.



PROCEDURE getIdPendBook.

    DEFINE INPUT  PARAMETER  pItem    AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT  PARAMETER pId      AS INT64 NO-UNDO.

    RUN setConstraintPend   IN hBoEsp800(pItem).
    RUN openQueryStatic     IN hBoEsp800('Pend').
    RUN getFirst IN hBoEsp800.
    IF RETURN-VALUE = 'OK' THEN DO:
       RUN getKey IN hBoEsp800(OUTPUT pId).
    END.   
    



END PROCEDURE.


PROCEDURE atuDtHrPendBook.

    RUN getRecord IN hBoEsp800(OUTPUT TABLE ttPendBook).
    FIND FIRST ttPendBook NO-ERROR.
    IF AVAIL ttPendBook THEN DO:
       ASSIGN ttPendBook.dt_hr_ult_atu = NOW .
       RUN setRecord    IN hBoEsp800(INPUT TABLE ttPendBook).
       RUN updateRecord IN hBoEsp800.
       RUN getRowErrors IN hBoEsp800(OUTPUT TABLE rowErrors).
       RUN getMsgRowErrors.p(INPUT TABLE rowErrors, OUTPUT cErros).
    END.




END PROCEDURE.





PROCEDURE criarHistPendBook:

    DEFINE INPUT  PARAMETER pIdPendBook     AS INT64       NO-UNDO.
    DEFINE INPUT  PARAMETER pItem           AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pRef            AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pQtAnterior     AS DECIMAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pQtAtual        AS DECIMAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pTabela         AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pAcao           AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pChave          AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE idHistPendBook AS INTEGER     NO-UNDO.

    RUN openQueryStatic IN hBoEs600('main').
    RUN emptyRowErrors  IN hBoEs600.
    CREATE ttHistPendBook.
    ASSIGN ttHistPendBook.it_codigo     = pItem
           ttHistPendBook.cod_refer     = pRef
           ttHistPendBook.qt_anterior   = pQtAnterior
           ttHistPendBook.qt_atual      = pQtAtual
           ttHistPendBook.cod_tabela    = pTabela
           ttHistPendBook.cod_acao      = pAcao
           ttHistPendBook.pend_book_id  = pIdPendBook
           ttHistPendBook.chave         = pChave
           .
    RUN setRecord IN hBoes600(INPUT TABLE ttHistPendBook).
    RUN createRecord IN hBoEs600.
    IF RETURN-VALUE = 'ok' THEN DO:
       RUN getKey IN hBoEs600(OUTPUT idHistPendBook).
    END.
    ELSE DO:
       RUN getRowErrors IN hBoEs600(OUTPUT TABLE rowErrors).
       RUN getMsgRowErrors.p(INPUT TABLE rowErrors, OUTPUT cErros).
    END.
END PROCEDURE.

PROCEDURE criarPendBook:

    DEFINE INPUT  PARAMETER pItem       AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER idPendBook  AS INT64       NO-UNDO.

    RUN openQueryStatic IN hBoEsp800('Main').
    RUN emptyRowErrors  IN hBoEsp800.
    CREATE ttPendBook.
    ASSIGN ttPendBook.it_codigo     = pItem .
    RUN setRecord IN hBoEsp800(INPUT TABLE ttPendBook).
    RUN createRecord IN hBoEsp800.
    IF RETURN-VALUE = 'ok' THEN DO:
       RUN getKey IN hBoEsp800(OUTPUT idPendBook).
    END.
    ELSE DO:
       RUN getRowErrors IN hBoEsp800(OUTPUT TABLE rowErrors).
       RUN getMsgRowErrors.p(INPUT TABLE rowErrors, OUTPUT cErros).
    END.

END PROCEDURE.
