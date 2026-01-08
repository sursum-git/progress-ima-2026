 /* Includes para gerar Movto Estoq */
{cdp/cd0666.i}
{cep/ceapi001.i}

DEF TEMP-TABLE tt-aux
    FIELD it-codigo LIKE ITEM.it-codigo
    FIELD cod-refer LIKE ob-etiqueta.cod-refer
    FIELD descricao LIKE ITEM.desc-item
    FIELD qt-ems    AS DEC
    FIELD qt-etq    AS DEC
    FIELD c-dif       AS CHAR
    FIELD tp-mov    AS CHAR
    FIELD lote      LIKE ob-etiqueta.nr-lote
    FIELD de-dif    AS DEC FORMAT "->>>,>>9.99".

INPUT FROM c:\temp\difest2809.csv.
REPEAT.
   CREATE tt-aux.
   IMPORT DELIMITER ";" tt-aux.
END.
INPUT CLOSE.

FOR EACH tt-aux.
    IF tt-aux.it-codigo = "" THEN NEXT.

    ASSIGN tt-aux.de-dif = ABS(tt-aux.qt-ems - tt-aux.qt-etq)
           tt-aux.lote = ENTRY(2,TRIM(tt-aux.cod-refer)," ")
           tt-aux.cod-refer = ENTRY(1,TRIM(tt-aux.cod-refer)," ").

    IF tp-mov = 'SAI' THEN
       RUN pi-acerta-estoq (INPUT tt-aux.it-codigo,
                            INPUT tt-aux.cod-refer,
                            INPUT tt-aux.lote + tt-aux.cod-refer,
                            INPUT tt-aux.de-dif,
                            INPUT 6, 
                            INPUT 2).
    ELSE 
        RUN pi-acerta-estoq (INPUT tt-aux.it-codigo,
                             INPUT tt-aux.cod-refer,
                             INPUT tt-aux.lote + tt-aux.cod-refer,
                             INPUT tt-aux.de-dif,
                             INPUT 6,
                             INPUT 1).
END.


PROCEDURE pi-acerta-estoq :
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
    ASSIGN tt-movto.cod-versao-integracao   = 1                       /* acrescentado*/
           tt-movto.cod-prog-orig           = "ACT-EST"               /* acrescentado*/
           tt-movto.usuario                 = 'super'                 /* acrescentado*/
           tt-movto.cod-estabel             = param-estoq.estabel-pad
           tt-movto.ct-codigo               = IF p-esp-docto = 33 
                                                 THEN param-estoq.ct-tr-transf
                                                 ELSE param-estoq.ct-acerto
           tt-movto.sc-codigo               = IF p-esp-docto = 33 
                                                 THEN param-estoq.sc-tr-transf
                                                 ELSE param-estoq.sc-acerto
           tt-movto.conta-contabil          = IF p-esp-docto = 33
                                                 THEN param-estoq.conta-transf
                                                 ELSE param-estoq.conta-acerto
           tt-movto.esp-docto               = p-esp-docto
           tt-movto.tipo-trans              = p-tipo-trans
           tt-movto.cod-depos               = item.deposito-pad
           tt-movto.dt-trans                = TODAY
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

