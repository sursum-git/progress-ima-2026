DEFINE INPUT  PARAMETER codParam LIKE im-param.cod-param NO-UNDO.
DEFINE OUTPUT PARAMETER valParam LIKE im-param.val-param  NO-UNDO.

FIND FIRST im-param
    WHERE im-param.cod-param = codParam
    NO-LOCK NO-ERROR.
ASSIGN valParam = IF AVAIL im-param THEN im-param.val-param ELSE ''.

