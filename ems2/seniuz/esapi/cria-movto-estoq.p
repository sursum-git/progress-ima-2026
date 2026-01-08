DEF INPUT  PARAMETER p-cod-estabel AS CHAR.
DEF INPUT  PARAMETER p-it-codigo   AS CHAR.
DEF INPUT  PARAMETER p-cod-refer   AS CHAR.
DEF INPUT  PARAMETER p-lote        AS CHAR.
DEF INPUT  PARAMETER p-qtde        AS DEC.
DEF INPUT  PARAMETER p-esp-docto   AS INT.
DEF INPUT  PARAMETER p-tipo-trans  AS INT.
DEF INPUT  PARAMETER p-justif      AS CHAR.
DEF OUTPUT PARAMETER p-erro        AS CHAR.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

/* Includes para gerar Movto Estoq */
{cdp/cd0666.i}
{cep/ceapi001k.i}

DEF VAR h-ceapi001k AS HANDLE NO-UNDO.
DEF VAR c-ct-codigo AS CHAR.
DEF VAR c-sc-codigo AS CHAR.

FIND FIRST param-estoq NO-LOCK NO-ERROR.

FIND ITEM WHERE
     ITEM.it-codigo = p-it-codigo NO-LOCK NO-ERROR.

FOR EACH tt-movto.
    DELETE tt-movto.
END.

CASE p-esp-docto.
    WHEN 30 THEN ASSIGN c-ct-codigo = '41400003'
                        c-sc-codigo = '30110'.
    WHEN 33 THEN ASSIGN c-ct-codigo = param-estoq.ct-tr-transf
                        c-sc-codigo = param-estoq.sc-tr-transf.
    OTHERWISE ASSIGN c-ct-codigo = param-estoq.ct-acerto
                     c-sc-codigo = param-estoq.sc-acerto.
END CASE.

CREATE tt-movto.
ASSIGN tt-movto.cod-versao-integracao   = 1                       /* acrescentado*/
       tt-movto.cod-prog-orig           = "ESSP0145"              /* acrescentado*/
       tt-movto.usuario                 = c-seg-usuario           /* acrescentado*/
       tt-movto.cod-estabel             = p-cod-estabel
       tt-movto.ct-codigo               = c-ct-codigo
       tt-movto.sc-codigo               = c-sc-codigo
       tt-movto.esp-docto               = p-esp-docto
       tt-movto.tipo-trans              = p-tipo-trans
       tt-movto.cod-depos               = item.deposito-pad
       tt-movto.dt-trans                = TODAY
       tt-movto.it-codigo               = p-it-codigo
       tt-movto.cod-refer               = p-cod-refer
       tt-movto.lote                    = p-lote
       tt-movto.quantidade              = p-qtde
       tt-movto.un                      = item.un
       tt-movto.dt-vali-lote            = 12.31.9999
       tt-movto.descricao-db            = p-justif.

RUN cep/ceapi001k.p PERSISTENT SET h-ceapi001k.
RUN pi-execute IN h-ceapi001k (INPUT-OUTPUT TABLE tt-movto,
                               INPUT-OUTPUT TABLE tt-erro,
                               INPUT YES). /* Deleta erros? */

DELETE PROCEDURE h-ceapi001k.
ASSIGN h-ceapi001k = ?.

FIND FIRST tt-erro NO-LOCK NO-ERROR.
IF NOT AVAIL tt-erro THEN
   RETURN 'OK'.
ELSE DO.
   ASSIGN p-erro = tt-erro.mensagem.
   RETURN 'ADM-ERROR'.
END.

