/* Includes para gerar Movto Estoq */
{cdp/cd0666.i}
{cep/ceapi001.i}
{utp/ut-glob.i}


DEF TEMP-TABLE tt-aux
    FIELD it-codigo LIKE movto-estoq.it-codigo
    FIELD lote      LIKE movto-estoq.lote
    FIELD qt-entrada LIKE movto-estoq.quantidade COLUMN-LABEL "ent"
    FIELD qt-saida LIKE movto-estoq.quantidade COLUMN-LABEL "Sai"
    INDEX indice1 it-codigo lote. 

       /*
FOR EACH movto-estoq WHERE
         movto-estoq.dt-trans >= 04.01.2007 AND
         movto-estoq.dt-trans <= 04.30.2007 AND 
         movto-estoq.esp-docto = 33 AND 
         movto-estoq.cod-depos = 'EXP' NO-LOCK.

    FIND ITEM WHERE
         ITEM.it-codigo = movto-estoq.it-codigo NO-LOCK.

    IF ITEM.ge-codigo < 51 OR 
       ITEM.ge-codigo > 58  THEN NEXT.

    FIND tt-aux WHERE
         tt-aux.it-codigo = movto-estoq.it-codigo AND
         tt-aux.lote      = movto-estoq.cod-refer 
        NO-ERROR.

    IF NOT AVAIL tt-aux THEN DO.
       CREATE tt-aux.
       ASSIGN tt-aux.it-codigo = movto-estoq.it-codigo
              tt-aux.lote      = movto-estoq.cod-refer.
    END.

    IF movto-estoq.tipo-trans = 1 THEN
       ASSIGN tt-aux.qt-entrada = tt-aux.qt-entrada + (movto-estoq.quantidade * ITEM.peso-liquido).
    ELSE
       ASSIGN tt-aux.qt-saida = tt-aux.qt-saida + (movto-estoq.quantidade * ITEM.peso-liquido).
END.

FOR EACH tt-aux WHERE
        (tt-aux.qt-saida - tt-aux.qt-entrada) > 0 NO-LOCK.

    DISP tt-aux.it-codigo
         tt-aux.lote
         (tt-aux.qt-saida - tt-aux.qt-entrada) (TOTAL)
         WITH WIDTH 550.

END.
*/


RUN pi-acerta-estoq (INPUT '599909',
                     INPUT '',
                     INPUT '',
                     INPUT 303.69,
                     INPUT 6,
                     INPUT 1).



PROCEDURE pi-acerta-estoq.

    DEF INPUT PARAMETER p-it-codigo AS CHAR.
    DEF INPUT PARAMETER p-cod-refer AS CHAR.
    DEF INPUT PARAMETER p-lote AS CHAR.
    DEF INPUT PARAMETER p-qtde AS DEC.
    DEF INPUT PARAMETER p-esp-docto AS INT.
    DEF INPUT PARAMETER p-tipo-trans AS INT.

    FIND FIRST param-estoq NO-LOCK NO-ERROR.

    FIND ITEM WHERE
         ITEM.it-codigo = p-it-codigo NO-LOCK NO-ERROR.

    FOR EACH tt-movto.
        DELETE tt-movto.
    END.

    CREATE tt-movto.
    ASSIGN tt-movto.cod-versao-integracao   = 1                       /* acrescentado*/
           tt-movto.cod-prog-orig           = "ESSP0145"              /* acrescentado*/
           tt-movto.usuario                 = c-seg-usuario           /* acrescentado*/
           tt-movto.cod-estabel             = param-estoq.estabel-pad
           tt-movto.ct-codigo               = param-estoq.ct-tr-transf
           tt-movto.sc-codigo               = param-estoq.sc-tr-transf
           tt-movto.conta-contabil          = param-estoq.conta-transf
           tt-movto.esp-docto               = p-esp-docto
           tt-movto.tipo-trans              = p-tipo-trans
           tt-movto.cod-depos               = item.deposito-pad
           tt-movto.dt-trans                = 04.30.2007
           tt-movto.it-codigo               = p-it-codigo
           tt-movto.cod-refer               = p-cod-refer
           tt-movto.lote                    = p-lote
           tt-movto.quantidade              = p-qtde
           tt-movto.un                      = item.un
           tt-movto.dt-vali-lote            = 12.31.9999.

    RUN cep/ceapi001.p (INPUT-OUTPUT TABLE tt-movto,
                        INPUT-OUTPUT TABLE tt-erro,
                        INPUT YES). /* Deleta erros? */
    
    FIND FIRST tt-erro NO-LOCK NO-ERROR.
    IF AVAIL tt-erro THEN DO.
       FOR EACH tt-erro.
           MESSAGE "Erro ao reportar o item " SKIP
                   tt-erro.mensagem
                   VIEW-AS ALERT-BOX ERROR.
       END.
    END.

END PROCEDURE.
