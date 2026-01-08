DEFINE TEMP-TABLE RowErrors NO-UNDO
    FIELD ErrorSequence    AS INTEGER
    FIELD ErrorNumber      AS INTEGER 
	FIELD ErrorDescription AS CHARACTER 
    FIELD ErrorParameters  AS CHAR
	FIELD ErrorType        AS CHARACTER
	FIELD ErrorHelp        AS CHARACTER
	FIELD ErrorSubType     AS CHARACTER
INDEX seq ErrorSequence.
    

PROCEDURE inserirErroManual:
    DEFINE INPUT  PARAMETER pNumeroErro AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pDescErro   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pParametros AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pTipoErro   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pAjuda     AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pSubtipo   AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE iUltSeq AS INTEGER     NO-UNDO.

    DEFINE BUFFER bf FOR Rowerrors.
    FIND LAST bf NO-ERROR.
    IF AVAIL bf THEN
       ASSIGN iUltSeq = bf.errorSequence.
    ELSE
       ASSIGN iUltSeq = 10.
    

    create RowErrors.
    ASSIGN rowErrors.errorSequence      = iUltSeq + 10
           rowErrors.errorNumber        = pNumeroErro
           rowErrors.errorDescription   = pDescErro
           rowErrors.errorParameters    = pParametros
           rowErrors.errorType          = pTipoErro
           rowErrors.errorHelp          = pAjuda
           rowErrors.errorSubType       = pSubTipo .

END.
