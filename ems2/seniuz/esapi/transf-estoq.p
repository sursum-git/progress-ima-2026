DEF INPUT  PARAMETER p-cod-estabel      AS CHAR.
DEF INPUT  PARAMETER p-it-codigo        AS CHAR.
DEF INPUT  PARAMETER p-cod-depos-orig   AS CHAR.
DEF INPUT  PARAMETER p-cod-depos-dest   AS CHAR.
DEF INPUT  PARAMETER p-qtde             AS DEC.
DEF INPUT  PARAMETER p-descricao-db     AS CHAR.
DEF OUTPUT PARAMETER p-erro             AS CHAR.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

/* Includes para gerar Movto Estoq */
{cdp/cd0666.i}
{cep/ceapi001.i}

FIND FIRST param-estoq NO-LOCK NO-ERROR.

FIND ITEM WHERE
     ITEM.it-codigo = p-it-codigo NO-LOCK NO-ERROR.

DO TRANSACTION:
   RUN pi-cria-movto (INPUT 2,
                      INPUT p-cod-depos-orig).   /* Saida */
   IF RETURN-VALUE = 'ADM-ERROR' THEN
      UNDO, RETURN "ADM-ERROR".
    
   RUN pi-cria-movto (INPUT 1,
                      INPUT p-cod-depos-dest).   /* Entrada */
   IF RETURN-VALUE = 'ADM-ERROR' THEN 
      UNDO, RETURN "ADM-ERROR".
END.


PROCEDURE pi-cria-movto.
    DEF INPUT PARAMETER p-tipo-trans AS INTEGER.
    DEF INPUT PARAMETER p-cod-depos AS CHAR.

    FOR EACH tt-movto.
        DELETE tt-movto.
    END.

    CREATE tt-movto.
    ASSIGN tt-movto.cod-versao-integracao   = 1                       /* acrescentado*/
           tt-movto.cod-prog-orig           = "ESSP0102"              /* acrescentado*/
           tt-movto.usuario                 = c-seg-usuario           /* acrescentado*/
           tt-movto.cod-estabel             = p-cod-estabel
           tt-movto.ct-codigo               = param-estoq.ct-tr-transf
           tt-movto.sc-codigo               = param-estoq.sc-tr-transf
           tt-movto.conta-contabil          = param-estoq.conta-transf 
           tt-movto.esp-docto               = 33 
           tt-movto.tipo-trans              = p-tipo-trans
           tt-movto.cod-depos               = p-cod-depos
           tt-movto.dt-trans                = TODAY
           tt-movto.it-codigo               = p-it-codigo
           tt-movto.quantidade              = p-qtde
           tt-movto.un                      = item.un
           tt-movto.dt-vali-lote            = 12.31.9999
           tt-movto.descricao-db            = p-descricao-db.
    
    RUN cep/ceapi001.p (INPUT-OUTPUT TABLE tt-movto,
                        INPUT-OUTPUT TABLE tt-erro,
                        INPUT YES). /* Deleta erros? */
    
    FIND FIRST tt-erro NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-erro THEN
       RETURN 'OK'.
    ELSE DO.
       MESSAGE tt-erro.mensagem
           VIEW-AS ALERT-BOX INFO BUTTONS OK.

       ASSIGN p-erro = tt-erro.mensagem.
       RETURN 'ADM-ERROR'.
    END.
END PROCEDURE.

