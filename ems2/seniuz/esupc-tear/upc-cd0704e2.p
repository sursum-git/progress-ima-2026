DEFINE NEW GLOBAL SHARED VAR h-endereco-cob     AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-cep-cob          AS HANDLE.

IF h-endereco-cob:SCREEN-VALUE = "" THEN DO.
   APPLY "entry" TO h-cep-cob.
   RETURN NO-APPLY.
END.
