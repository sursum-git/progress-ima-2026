DEF SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

FUNCTION getGlobalVar RETURNS CHAR (variavel AS CHAR):
    IF variavel = 'c-seg-usuario' THEN
        RETURN c-seg-usuario.
END FUNCTION.
    
