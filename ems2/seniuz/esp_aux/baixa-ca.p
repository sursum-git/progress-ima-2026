/* Includes para gerar Movto Estoq */
{cdp/cd0666.i}
{cep/ceapi001.i}

DEF TEMP-TABLE tt-itens
    FIELD it-codigo LIKE saldo-estoq.it-codigo
    FIELD cod-refer LIKE saldo-estoq.cod-refer
    FIELD lote      LIKE saldo-estoq.lote
    FIELD qtidade-etq LIKE saldo-estoq.qtidade-atu
    FIELD qtidade-atu LIKE saldo-estoq.qtidade-atu.

DEF TEMP-TABLE tt-etq-itens
    FIELD it-codigo LIKE saldo-estoq.it-codigo
    FIELD cod-refer LIKE saldo-estoq.cod-refer
    FIELD lote      LIKE saldo-estoq.lote
    FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
DEF VAR de-qtd LIKE saldo-estoq.qtidade-atu.
DEF VAR i-ct AS INT.
DEF VAR i-cont AS INT.

FOR EACH ob-etiqueta WHERE
         ob-etiqueta.cod-estabel = '1' AND
         ob-etiqueta.localiz = '050001' AND
         ob-etiqueta.nr-lote = 'ca' AND
         ob-etiqueta.situacao = 3 AND
         ob-etiqueta.quantidade <= 11
         NO-LOCK BY ob-etiqueta.num-etiqueta.

    FIND tt-itens WHERE
         tt-itens.it-codigo = ob-etiqueta.it-codigo AND
         tt-itens.cod-refer = ob-etiqueta.cod-refer AND
         tt-itens.lote = ob-etiqueta.nr-lote + ob-etiqueta.cod-refer
         NO-ERROR.

    IF NOT AVAIL tt-itens THEN DO.
       CREATE tt-itens.
       ASSIGN tt-itens.it-codigo = ob-etiqueta.it-codigo
              tt-itens.cod-refer = ob-etiqueta.cod-refer
              tt-itens.lote = ob-etiqueta.nr-lote + ob-etiqueta.cod-refer.
    END.
    ASSIGN tt-itens.qtidade-etq = tt-itens.qtidade-etq + ob-etiqueta.quantidade.


    CREATE tt-etq-itens.
    ASSIGN tt-etq-itens.it-codigo = ob-etiqueta.it-codigo
           tt-etq-itens.cod-refer = ob-etiqueta.cod-refer
           tt-etq-itens.lote = ob-etiqueta.nr-lote + ob-etiqueta.cod-refer
           tt-etq-itens.num-etiqueta = ob-etiqueta.num-etiqueta.
END.


ASSIGN i-ct = 0.
FOR EACH tt-itens.

    ASSIGN i-ct = i-ct + 1.

    /*
    DISP tt-itens.it-codigo
         tt-itens.cod-refer
         tt-itens.lote
         tt-itens.qtidade-etq
         i-ct
         WITH WIDTH 550. */

END.


FOR EACH tt-itens BY tt-itens.it-codigo. 

    ASSIGN i-cont = i-cont + 1.

    DISP tt-itens.it-codigo
         tt-itens.cod-refer
         tt-itens.lote
         i-cont " / " i-ct.
    PAUSE 0.

    FIND ITEM WHERE
         ITEM.it-codigo = tt-itens.it-codigo NO-LOCK NO-ERROR.

    FOR EACH tt-etq-itens WHERE
             tt-etq-itens.it-codigo = tt-itens.it-codigo AND
             tt-etq-itens.cod-refer = tt-itens.cod-refer AND
             tt-etq-itens.lote = tt-itens.lote.
        FIND ob-etiqueta WHERE
             ob-etiqueta.cod-estab = '1' AND
             ob-etiqueta.num-etiqueta = tt-etq-itens.num-etiqueta.
        ASSIGN ob-etiqueta.situacao = 7.
    END.

    FIND saldo-estoq WHERE
         saldo-estoq.cod-estabel = '1' AND
         saldo-estoq.it-codigo = tt-itens.it-codigo AND
         saldo-estoq.cod-refer = tt-itens.cod-refer AND
         saldo-estoq.lote = tt-itens.lote
         NO-ERROR.
    IF NOT AVAIL saldo-estoq THEN NEXT.

    ASSIGN saldo-estoq.qt-alocada = saldo-estoq.qt-alocada - tt-itens.qtidade-etq.
    IF saldo-estoq.qt-alocada < 0 THEN
       ASSIGN saldo-estoq.qt-alocada = 0.

    RUN pi-acerta-estoq (INPUT tt-itens.it-codigo,
                         INPUT tt-itens.cod-refer,
                         INPUT tt-itens.lote,
                         INPUT tt-itens.qtidade-etq,
                         INPUT 6,
                         INPUT 2).
END.



PROCEDURE pi-acerta-estoq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-it-codigo  AS CHAR.
    DEF INPUT PARAMETER p-cod-refer  AS CHAR.
    DEF INPUT PARAMETER p-lote       AS CHAR.
    DEF INPUT PARAMETER p-qtde       AS DEC.
    DEF INPUT PARAMETER p-esp-docto  AS INT.
    DEF INPUT PARAMETER p-tipo-trans AS INT.

    FIND FIRST param-estoq NO-LOCK NO-ERROR.


    FIND ITEM WHERE
         ITEM.it-codigo = p-it-codigo NO-LOCK NO-ERROR.

    FOR EACH tt-movto.
        DELETE tt-movto.
    END.

    CREATE tt-movto.
    ASSIGN tt-movto.cod-versao-integracao = 1                 /* acrescentado*/
           tt-movto.cod-prog-orig = "ESSP0145"                /* acrescentado*/
           tt-movto.usuario = c-seg-usuario                   /* acrescentado*/
           tt-movto.cod-estabel  = '1'                        /* param-estoq.estabel-pad */
           tt-movto.ct-codigo = IF p-esp-docto = 33 
                                THEN param-estoq.ct-tr-transf
                                ELSE param-estoq.ct-acerto
           tt-movto.sc-codigo = IF p-esp-docto = 33 
                                THEN param-estoq.sc-tr-transf
                                ELSE param-estoq.sc-acerto
           tt-movto.conta-contabil = IF p-esp-docto = 33
                                     THEN param-estoq.conta-transf
                                     ELSE param-estoq.conta-acerto
           tt-movto.esp-docto = p-esp-docto
           tt-movto.tipo-trans = p-tipo-trans
           tt-movto.cod-depos = item.deposito-pad
           tt-movto.dt-trans = TODAY
           tt-movto.it-codigo = p-it-codigo
           tt-movto.cod-refer = p-cod-refer
           tt-movto.lote = p-lote
           tt-movto.quantidade = p-qtde
           tt-movto.descricao-db = 'Baixa CA'
           tt-movto.un = item.un
           tt-movto.dt-vali-lote = 12.31.9999.
           

    RUN cep/ceapi001.p (INPUT-OUTPUT TABLE tt-movto,
                        INPUT-OUTPUT TABLE tt-erro,
                        INPUT YES). /* Deleta erros? */
    /*
    FIND FIRST tt-erro NO-LOCK NO-ERROR.
    IF AVAIL tt-erro THEN DO.
       FOR EACH tt-erro.
           MESSAGE "Erro ao reportar o item " SKIP
                   tt-erro.mensagem
                   VIEW-AS ALERT-BOX ERROR.
       END.
    END.
    */
END PROCEDURE.



