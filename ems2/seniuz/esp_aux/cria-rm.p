/* Includes para gerar Movto Estoq */
{cdp/cd0666.i}
{cep/ceapi001.i}

DEF INPUT PARAMETER c-it-codigo LIKE movto-estoq.it-codigo.
DEF INPUT PARAMETER c-cod-refer LIKE movto-estoq.cod-refer.   
DEF INPUT PARAMETER de-quantidade LIKE movto-estoq.quantidade. 
DEF INPUT PARAMETER da-dt-trans LIKE movto-estoq.dt-trans.

RUN pi-baixa-estoq (INPUT c-it-codigo,
                   INPUT c-cod-refer,
                   INPUT c-cod-refer,
                   INPUT de-quantidade,
                   INPUT da-dt-trans).

PROCEDURE pi-baixa-estoq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-it-codigo  AS CHAR.
    DEF INPUT PARAMETER p-cod-refer  AS CHAR.
    DEF INPUT PARAMETER p-lote       AS CHAR.
    DEF INPUT PARAMETER p-qtde       AS DEC.
    DEF INPUT PARAMETER p-dt-trans   AS DATE.

    FIND FIRST param-estoq NO-LOCK NO-ERROR.

    FIND ITEM WHERE
         ITEM.it-codigo = p-it-codigo NO-LOCK NO-ERROR.

    FOR EACH tt-movto.
        DELETE tt-movto.
    END.

    CREATE tt-movto.
    ASSIGN tt-movto.cod-versao-integracao = 1                 /* acrescentado*/
           tt-movto.cod-prog-orig = "ESSP0205"                /* acrescentado*/
           tt-movto.usuario = 'super'                         /* acrescentado*/
           tt-movto.cod-estabel  = '1'                        /* param-estoq.estabel-pad */
           tt-movto.ct-codigo = '410403'
           tt-movto.sc-codigo = '10207'
           tt-movto.conta-contabil = '41040310207'
           tt-movto.esp-docto = 30
           tt-movto.tipo-trans = 2
           tt-movto.cod-depos = item.deposito-pad
           tt-movto.dt-trans = p-dt-trans
           tt-movto.it-codigo = p-it-codigo
           tt-movto.cod-refer = p-cod-refer
           tt-movto.lote = p-lote
           tt-movto.quantidade = p-qtde
           tt-movto.un = item.un
           tt-movto.dt-vali-lote = 12.31.9999.

    RUN cep/ceapi001.p (INPUT-OUTPUT TABLE tt-movto,
                        INPUT-OUTPUT TABLE tt-erro,
                        INPUT YES). /* Deleta erros? */
     
    
    FIND FIRST tt-erro NO-LOCK NO-ERROR.
    IF AVAIL tt-erro THEN DO.
       FOR EACH tt-erro.
           MESSAGE "Erro ao Baixar o Item" SKIP
                   tt-erro.mensagem
                   VIEW-AS ALERT-BOX ERROR.
       END.
       RETURN 'ADM-ERROR'.
    END.
    
    RETURN 'OK'.
END PROCEDURE.



