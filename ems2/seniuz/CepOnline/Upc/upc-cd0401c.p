DEFINE NEW GLOBAL SHARED VAR h-estado       AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-cidade       AS HANDLE.

DEF BUFFER cidade FOR ems2ima.cidade.

FIND cidade WHERE
     cidade.estado = h-estado:SCREEN-VALUE AND
     cidade.cidade = h-cidade:SCREEN-VALUE NO-LOCK NO-ERROR.
IF NOT AVAIL cidade AND h-estado:SCREEN-VALUE <> "EX" THEN DO.
   MESSAGE 'Cidade ' h-cidade:SCREEN-VALUE ' n∆o Cadastrada para a UF Informada....' SKIP(1)
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
   APPLY 'ENTRY' TO h-cidade.
   RETURN NO-APPLY.
END.

ASSIGN h-cidade:SCREEN-VALUE = UPPER(h-cidade:SCREEN-VALUE).

