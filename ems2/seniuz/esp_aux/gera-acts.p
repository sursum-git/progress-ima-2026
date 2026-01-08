DEF TEMP-TABLE tt-aux LIKE movto-estoq.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
DEF VAR c-erro AS CHAR.

/* Includes para gerar Movto Estoq */
{cdp/cd0666.i}
{cep/ceapi001k.i}

DEF VAR h-ceapi001k AS HANDLE NO-UNDO.

FIND FIRST param-estoq NO-LOCK NO-ERROR.

RUN pi-exporta.

PROCEDURE pi-exporta.
    OUTPUT TO c:\temp\movto-act.txt.
    FOR EACH movto-estoq WHERE
             movto-estoq.dt-trans = 07.01.2015 AND
             movto-estoq.esp-docto = 2 NO-LOCK.
        EXPORT movto-estoq.
    END.
    OUTPUT CLOSE.
END PROCEDURE.


PROCEDURE pi-importa.
    DEF VAR i-tipo-trans AS INT.

    INPUT FROM c:\temp\movto-act.txt.
    REPEAT.
        CREATE tt-aux.
        IMPORT tt-aux.
    END.
    INPUT CLOSE.

    FOR EACH tt-aux.
        ASSIGN i-tipo-trans = IF tt-aux.tipo-trans = 1 THEN 2 ELSE 1.

        RUN esapi/cria-movto-estoq.p (INPUT tt-aux.cod-estabel,
                                      INPUT tt-aux.it-codigo,
                                      INPUT tt-aux.cod-refer,
                                      INPUT tt-aux.lote, 
                                      INPUT tt-aux.quantidade,
                                      INPUT tt-aux.valor-mat-m[1],
                                      INPUT 6,   /* DIV */
                                      INPUT i-tipo-trans,   /* Sa¡da */
                                      INPUT "Acerto das ACTs Retroativas",
                                      OUTPUT c-erro). 
    
        IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
           MESSAGE "Erro ao Gerar ACTs" SKIP
                   c-erro
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
    END.
END PROCEDURE.


PROCEDURE pi-cria-movto.
    DEF INPUT  PARAMETER p-cod-estabel AS CHAR.
    DEF INPUT  PARAMETER p-it-codigo   AS CHAR.
    DEF INPUT  PARAMETER p-cod-refer   AS CHAR.
    DEF INPUT  PARAMETER p-lote        AS CHAR.
    DEF INPUT  PARAMETER p-qtde        AS DEC.
    DEF INPUT  PARAMETER p-valor       AS DEC.
    DEF INPUT  PARAMETER p-esp-docto   AS INT.
    DEF INPUT  PARAMETER p-tipo-trans  AS INT.
    DEF INPUT  PARAMETER p-justif      AS CHAR.
    DEF OUTPUT PARAMETER p-erro        AS CHAR.
    
    FIND ITEM WHERE
         ITEM.it-codigo = p-it-codigo NO-LOCK NO-ERROR.
    
    FOR EACH tt-movto.
        DELETE tt-movto.
    END.
    
    CREATE tt-movto.
    ASSIGN tt-movto.cod-versao-integracao   = 1                       /* acrescentado*/
           tt-movto.cod-prog-orig           = "ESSP0145"              /* acrescentado*/
           tt-movto.usuario                 = c-seg-usuario           /* acrescentado*/
           tt-movto.cod-estabel             = p-cod-estabel
           tt-movto.ct-codigo               = IF p-esp-docto = 33 
                                                 THEN param-estoq.ct-tr-transf
                                                 ELSE param-estoq.ct-acerto
           tt-movto.sc-codigo               = IF p-esp-docto = 33 
                                                 THEN param-estoq.sc-tr-transf
                                                 ELSE param-estoq.sc-acerto
           tt-movto.esp-docto               = p-esp-docto
           tt-movto.tipo-trans              = p-tipo-trans
           tt-movto.cod-depos               = item.deposito-pad
           tt-movto.dt-trans                = 06.30.2015
           tt-movto.it-codigo               = p-it-codigo
           tt-movto.cod-refer               = p-cod-refer
           tt-movto.lote                    = p-lote
           tt-movto.quantidade              = p-qtde
           tt-movto.valor-mat-m[1]          = p-valor
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

END PROCEDURE.

