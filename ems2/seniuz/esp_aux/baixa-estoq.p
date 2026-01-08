/* Includes para gerar Movto Estoq */
{cdp/cd0666.i}
{cep/ceapi001.i}

DEF TEMP-TABLE tt-itens
    FIELD it-codigo AS CHAR
    FIELD cod-refer AS CHAR
    FIELD qtidade-etq AS DEC
    FIELD qtidade-atu AS DEC.


FOR EACH saldo-estoq WHERE
         saldo-estoq.qtidade-atu > 0 NO-LOCK,
    FIRST ITEM OF saldo-estoq WHERE
          ITEM.ge-codigo >= 50 AND
          ITEM.ge-codigo <= 60 NO-LOCK.
         
    IF ITEM.un <> 'm' THEN NEXT.

    DISP saldo-estoq.it-codigo. PAUSE 0. 

    FIND tt-itens WHERE
         tt-itens.it-codigo = saldo-estoq.it-codigo AND
         tt-itens.cod-refer = saldo-estoq.cod-refer NO-ERROR.
    IF NOT AVAIL tt-itens THEN DO.
       CREATE tt-itens.
       ASSIGN tt-itens.it-codigo = saldo-estoq.it-codigo 
              tt-itens.cod-refer = saldo-estoq.cod-refer.
    END.
    ASSIGN tt-itens.qtidade-atu = tt-itens.qtidade-atu + saldo-estoq.qtidade-atu.
END.



FOR EACH tt-itens.
    FOR EACH ob-etiqueta WHERE
             ob-etiqueta.it-codigo = tt-itens.it-codigo AND
             ob-etiqueta.cod-refer = tt-itens.cod-refer AND
             (ob-etiqueta.situacao = 3 OR ob-etiqueta.situacao = 4) NO-LOCK.
    
        DISP ob-etiqueta.num-etiqueta. PAUSE 0. 
        ASSIGN tt-itens.qtidade-etq = tt-itens.qtidade-etq + ob-etiqueta.quantidade.
    END.
END.

FOR EACH tt-itens WHERE
         tt-itens.qtidade-atu > tt-itens.qtidade-etq NO-LOCK.

    IF tt-itens.qtidade-atu - tt-itens.qtidade-etq <= 5 THEN DO.
       FIND saldo-estoq WHERE
            saldo-estoq.cod-estabel = '1' AND
            saldo-estoq.it-codigo = tt-itens.it-codigo AND
            saldo-estoq.cod-refer = tt-itens.cod-refer AND
            saldo-estoq.lote = 'CA' + tt-itens.cod-refer
            NO-LOCK NO-ERROR.

       IF AVAIL saldo-estoq AND
          saldo-estoq.qtidade-atu >=  tt-itens.qtidade-atu - tt-itens.qtidade-etq THEN 
          RUN pi-baixa-estoq (INPUT saldo-estoq.it-codigo,
                              INPUT saldo-estoq.cod-refer,
                              INPUT saldo-estoq.lote,
                              INPUT tt-itens.qtidade-atu - tt-itens.qtidade-etq).
       ELSE
           RUN pi-baixa-estoq (INPUT tt-itens.it-codigo,
                               INPUT tt-itens.cod-refer,
                               INPUT tt-itens.cod-refer,
                               INPUT tt-itens.qtidade-atu - tt-itens.qtidade-etq).
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


