DEF VAR h-ceapi001k AS HANDLE NO-UNDO.

{cdp/cd0666.i}
{cep/ceapi001k.i}

RUN cep/ceapi001k.p PERSISTENT SET h-ceapi001k.

RUN pi-execute IN h-ceapi001k (INPUT-OUTPUT TABLE tt-movto,
                               INPUT-OUTPUT TABLE tt-erro,
               	               INPUT YES). /* Deleta erros? */

DELETE PROCEDURE h-ceapi001k.

ASSIGN h-ceapi001k = ?.


