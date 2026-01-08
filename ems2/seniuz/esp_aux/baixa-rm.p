/* Includes para gerar Movto Estoq */
{cdp/cd0666.i}
{cep/ceapi001.i}

DEF VAR c-lote AS CHAR.

FOR EACH movto-etq WHERE
         movto-etq.dt-trans >= 04.01.2012 AND
         movto-etq.dt-trans <= 05.15.2012 AND
         movto-etq.esp-docto = 'CON' NO-LOCK.

    FIND ob-etiqueta WHERE
         ob-etiqueta.cod-estabel = '1' AND
         ob-etiqueta.num-etiqueta = movto-etq.num-etiqueta NO-LOCK NO-ERROR.
    IF NOT AVAIL ob-etiqueta THEN NEXT.

    IF ob-etiqueta.quantidade = 0 THEN NEXT.

    IF NOT ob-etiqueta.nr-lote BEGINS 'CA' THEN NEXT.

    /*
    IF ob-etiqueta.it-codigo <> '520026' THEN NEXT.
    IF ob-etiqueta.cod-refer <> '035' THEN NEXT.
    */
    
    ASSIGN c-lote = IF ob-etiqueta.nr-lote BEGINS 'CA'
                    THEN ob-etiqueta.nr-lote + ob-etiqueta.cod-refer
                    ELSE ob-etiqueta.cod-refer.
    
    FIND FIRST movto-estoq WHERE
               movto-estoq.dt-trans = movto-etq.dt-trans AND
               movto-estoq.it-codigo = ob-etiqueta.it-codigo AND
               movto-estoq.cod-refer = ob-etiqueta.cod-refer AND
               movto-estoq.lote = c-lote AND
               movto-estoq.esp-docto = 30 AND
               movto-estoq.quantidade = ob-etiqueta.quantidade
               NO-LOCK NO-ERROR.

    IF NOT AVAIL movto-estoq THEN DO.
       ASSIGN c-lote = ob-etiqueta.cod-refer.

       FIND FIRST movto-estoq WHERE
                  movto-estoq.dt-trans = movto-etq.dt-trans AND
                  movto-estoq.it-codigo = ob-etiqueta.it-codigo AND
                  movto-estoq.cod-refer = ob-etiqueta.cod-refer AND
                  movto-estoq.lote = c-lote AND
                  movto-estoq.esp-docto = 30 AND
                  movto-estoq.quantidade = ob-etiqueta.quantidade
                  NO-LOCK NO-ERROR.

       IF NOT AVAIL movto-estoq THEN DO.
          FIND saldo-estoq WHERE
               saldo-estoq.cod-estabel = '1' AND
               saldo-estoq.it-codigo = ob-etiqueta.it-codigo AND
               saldo-estoq.cod-refer = ob-etiqueta.cod-refer AND
               saldo-estoq.lote = 'CA' + ob-etiqueta.cod-refer AND
               saldo-estoq.qtidade-atu >= ob-etiqueta.quantidade
               NO-LOCK NO-ERROR.

          IF AVAIL saldo-estoq THEN
             RUN pi-baixa-estoq (INPUT ob-etiqueta.it-codigo,
                                 INPUT ob-etiqueta.cod-refer,
                                 INPUT 'CA' + ob-etiqueta.cod-refer,
                                 INPUT ob-etiqueta.quantidade,
                                 INPUT movto-etq.dt-trans).
          ELSE
             RUN pi-baixa-estoq (INPUT ob-etiqueta.it-codigo,
                                 INPUT ob-etiqueta.cod-refer,
                                 INPUT ob-etiqueta.cod-refer,
                                 INPUT ob-etiqueta.quantidade,
                                 INPUT movto-etq.dt-trans).
       END.
    END.
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

