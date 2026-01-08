DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

/* Includes para gerar Movto Estoq */
{cdp/cd0666.i}
{cep/ceapi001k.i}

DEF TEMP-TABLE tt-aux LIKE movto-estoq.

DEF VAR h-ceapi001k AS HANDLE NO-UNDO.

FIND FIRST param-estoq NO-LOCK NO-ERROR.

FOR EACH tt-movto.
    DELETE tt-movto.
END.

INPUT FROM p:\toninho\movto-act10.txt.
REPEAT.
   CREATE tt-aux.
   IMPORT tt-aux.
END.
INPUT CLOSE.

FOR EACH tt-aux.
    IF tt-aux.it-codigo = '' THEN NEXT.
    IF tt-aux.dt-trans <> 10.01.2015 THEN NEXT.
                               
    CREATE tt-movto.
    ASSIGN tt-movto.cod-versao-integracao   = 1                       /* acrescentado*/
           tt-movto.cod-prog-orig           = "CE0220"                /* acrescentado*/
           tt-movto.usuario                 = c-seg-usuario           /* acrescentado*/
           tt-movto.cod-estabel             = tt-aux.cod-estabel
           tt-movto.ct-codigo               = param-estoq.ct-acerto
           tt-movto.sc-codigo               = param-estoq.sc-acerto
           tt-movto.esp-docto               = 6
           tt-movto.tipo-trans              = IF tt-aux.tipo-trans = 1 THEN 2 ELSE 1
           tt-movto.cod-depos               = tt-aux.cod-depos
           tt-movto.dt-trans                = tt-aux.dt-trans - 1
           tt-movto.it-codigo               = tt-aux.it-codigo
           tt-movto.cod-refer               = tt-aux.cod-refer
           tt-movto.lote                    = tt-aux.lote
           tt-movto.quantidade              = tt-aux.quantidade
           tt-movto.valor-mat-m[1]          = tt-aux.valor-mat-m[1]
           tt-movto.un                      = tt-aux.un
           tt-movto.dt-vali-lote            = 12.31.9999
           tt-movto.descricao-db            = "Movimento de Acerto para n∆o Gerar ACT".
END.

RUN cep/ceapi001k.p PERSISTENT SET h-ceapi001k.
RUN pi-execute IN h-ceapi001k (INPUT-OUTPUT TABLE tt-movto,
                               INPUT-OUTPUT TABLE tt-erro,
                               INPUT YES). /* Deleta erros? */

DELETE PROCEDURE h-ceapi001k.
ASSIGN h-ceapi001k = ?.

FOR EACH tt-erro NO-LOCK.
   MESSAGE tt-erro.mensagem 
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.


