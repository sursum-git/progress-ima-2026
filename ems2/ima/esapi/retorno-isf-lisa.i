DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
DEF VAR c-nr-pedcli AS CHAR.
DEF VAR c-pre-pedido AS CHAR.
DEF VAR c-chave AS CHAR.
DEF VAR c-msg AS CHAR.
DEF VAR c-it-codigo AS CHAR.
DEF VAR c-arq-retorno AS CHAR.
DEF VAR l-erro AS LOG.
DEFINE VARIABLE lAchouEtq AS LOGICAL     NO-UNDO.

DEF TEMP-TABLE ttReservas NO-UNDO
    FIELD cod-estabel       AS CHAR
    FIELD pedido-lisa       AS CHAR
    FIELD nr-pedcli         AS CHAR
    FIELD it-codigo         AS CHAR
    FIELD cod-refer         AS CHAR
    FIELD nr-container      AS CHAR
    FIELD num-rolo-imp      AS CHAR
    FIELD num-etiqueta      LIKE ob-etiqueta.num-etiqueta
    FIELD idEtqLisa         AS CHAR
    FIELD quantidade        AS DECIMAL
    FIELD agrupOrigem       AS INT
    FIELD numEtqOrigemCorte AS CHAR
    FIELD qtAtuEtqCortada   AS DECIMAL
    FIELD numRoloOrigem     AS INT.
