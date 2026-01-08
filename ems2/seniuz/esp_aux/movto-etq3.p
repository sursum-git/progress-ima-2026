/* Includes para gerar Movto Estoq */
{cdp/cd0666.i}
{cep/ceapi001.i}

DEF TEMP-TABLE tt-itens
    FIELD it-codigo AS CHAR
    FIELD cod-refer AS CHAR
    FIELD qtidade AS DEC
    FIELD qt-mov AS DEC.

DEF VAR de-tot AS DEC.

FOR EACH movto-etq WHERE
         movto-etq.dt-trans = 02.29.2012 AND 
         movto-etq.esp-docto = 'CON' AND 
         index(movto-etq.char-1,"usuario: super") > 0  
         NO-LOCK.  

    FIND ob-etiqueta WHERE
         ob-etiqueta.cod-estab = '1' AND
         ob-etiqueta.num-etiqueta = movto-etq.num-etiqueta NO-LOCK NO-ERROR.

    FIND tt-itens WHERE
         tt-itens.it-codigo = ob-etiqueta.it-codigo AND
         tt-itens.cod-refer = ob-etiqueta.cod-refer NO-ERROR.
    IF NOT AVAIL tt-itens THEN DO.
       CREATE tt-itens.
       ASSIGN tt-itens.it-codigo = ob-etiqueta.it-codigo 
              tt-itens.cod-refer = ob-etiqueta.cod-refer .
    END.
    ASSIGN tt-itens.qtidade = tt-itens.qtidade + ob-etiqueta.quantidade.
END.

FOR EACH tt-itens 
         BY tt-itens.it-codigo
         BY tt-itens.cod-refer.

    DISP tt-itens.it-codigo
         tt-itens.cod-refer.
    PAUSE 0.

    FIND saldo-estoq WHERE
         saldo-estoq.cod-estabel = '1' AND
         saldo-estoq.it-codigo = tt-itens.it-codigo AND
         saldo-estoq.cod-refer = tt-itens.cod-refer AND
         saldo-estoq.lote = 'CA' + tt-itens.cod-refer
         NO-ERROR.
    IF NOT AVAIL saldo-estoq THEN NEXT.
    IF saldo-estoq.qtidade-atu <= 0 THEN NEXT.

    RUN pi-baixa-estoq (INPUT saldo-estoq.it-codigo,
                        INPUT saldo-estoq.cod-refer,
                        INPUT saldo-estoq.lote,
                        INPUT tt-itens.qtidade).
END.



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
           tt-movto.dt-trans = 04.01.2012
           tt-movto.it-codigo = p-it-codigo
           tt-movto.cod-refer = p-cod-refer
           tt-movto.lote = p-lote
           tt-movto.quantidade = p-qtde
           tt-movto.un = item.un
           tt-movto.dt-vali-lote = 12.31.9999.

    RUN cep/ceapi001.p (INPUT-OUTPUT TABLE tt-movto,
                        INPUT-OUTPUT TABLE tt-erro,
                        INPUT YES). /* Deleta erros? */
     
    /*
    FIND FIRST tt-erro NO-LOCK NO-ERROR.
    IF AVAIL tt-erro THEN DO.
       FOR EACH tt-erro.
           MESSAGE "Erro ao Baixar o Item" SKIP
                   tt-erro.mensagem
                   VIEW-AS ALERT-BOX ERROR.
       END.
       RETURN 'ADM-ERROR'.
    END.
    */
    RETURN 'OK'.
END PROCEDURE.

