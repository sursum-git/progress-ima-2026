DEFINE VARIABLE hBoNat001 AS HANDLE      NO-UNDO.
RUN esbo/bonat001.p PERSISTENT SET hBoNat001.
DEFINE VAR NatOperacao            AS CHARACTER   NO-UNDO.
DEFINE VAR CodParamNatOperacao    AS INTEGER     NO-UNDO.
DEFINE VARIABLE cErros AS CHARACTER   NO-UNDO FORMAT 'x(400)'.
RUN buscarNatOperacao IN hBoNat001
    (
        2  ,       
        NO ,       
       1  ,             
        5  ,              
        OUTPUT NatOperacao,
        OUTPUT CodParamNatOperacao 
    ).
RUN retornarErros IN hBoNat001(OUTPUT cErros).
IF cErros <> '' THEN
MESSAGE cErros
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
ELSE
  MESSAGE natOperacao SKIP
          codParamNatOperacao
      VIEW-AS ALERT-BOX INFO BUTTONS OK.



