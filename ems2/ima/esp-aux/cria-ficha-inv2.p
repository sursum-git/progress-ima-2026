DEF TEMP-TABLE tt-aux
    FIELD it-codigo    AS CHAR
    FIELD cod-refer    AS CHAR
    FIELD filler1      AS CHAR
    FIELD descricao    AS CHAR
    FIELD qtidade-atu  AS DEC
    FIELD qt-entrada   AS DEC
    FIELD qt-saida     AS DEC
    FIELD qt-final     AS DEC
    FIELD observ       AS CHAR.

DEF BUFFER b-movto-estoq FOR movto-estoq.

DEF VAR i-nr-ficha         AS INTEGER.
DEF VAR de-qtidade-atu     LIKE saldo-estoq.qtidade-atu.
DEF VAR da-data-inv        AS DATE FORMAT "99/99/9999".

ASSIGN da-data-inv = 06.01.2018.

FIND LAST inventario WHERE
          inventario.dt-saldo = da-data-inv NO-LOCK USE-INDEX nr-ficha NO-ERROR.

ASSIGN i-nr-ficha = IF AVAIL inventario 
                    THEN inventario.nr-ficha 
                    ELSE 0. 
    
INPUT FROM "p:\INVENTµRIO MED 01 JUNHO 2018.csv".
REPEAT.
    CREATE tt-aux.
    IMPORT DELIMITER ";" tt-aux.
END.
INPUT CLOSE.

FOR EACH tt-aux.
    IF tt-aux.it-codigo = '' THEN NEXT.

    IF LENGTH(tt-aux.cod-refer) < 3 THEN
       ASSIGN tt-aux.cod-refer = STRING(INT(tt-aux.cod-refer),"999").

    FIND inventario WHERE
         inventario.dt-saldo = 06.01.2018 AND
         inventario.it-codigo = tt-aux.it-codigo AND
         inventario.cod-refer = tt-aux.cod-refer
         NO-LOCK NO-ERROR.

    IF AVAIL inventario THEN NEXT.

    FIND item WHERE
         item.it-codigo = tt-aux.it-codigo NO-LOCK NO-ERROR.

    DISP tt-aux WITH WIDTH 550.
    PAUSE 0.

    FOR EACH saldo-estoq WHERE 
             saldo-estoq.it-codigo = ITEM.it-codigo and
             saldo-estoq.cod-refer = tt-aux.cod-refer AND
             saldo-estoq.lote = tt-aux.cod-refer NO-LOCK.

        ASSIGN de-qtidade-atu = 0.
        RUN esapi/calc-saldo-data.p (INPUT saldo-estoq.cod-estabel,
                                     INPUT saldo-estoq.cod-depos,
                                     INPUT saldo-estoq.it-codigo,
                                     INPUT saldo-estoq.cod-refer,
                                     INPUT saldo-estoq.lote,
                                     INPUT da-data-inv, 
                                     OUTPUT de-qtidade-atu).

        ASSIGN i-nr-ficha = i-nr-ficha + 1.

        FIND LAST b-movto-estoq WHERE
                  b-movto-estoq.cod-estabel = saldo-estoq.cod-estabel AND
                  b-movto-estoq.it-codigo = saldo-estoq.it-codigo AND
                  b-movto-estoq.cod-refer = saldo-estoq.cod-refer AND
                  b-movto-estoq.lote = saldo-estoq.lote AND
                  b-movto-estoq.tipo-trans = 1
                  NO-LOCK NO-ERROR.

        FIND LAST movto-estoq WHERE
                  movto-estoq.cod-estabel = saldo-estoq.cod-estabel AND
                  movto-estoq.it-codigo = saldo-estoq.it-codigo AND
                  movto-estoq.cod-refer = saldo-estoq.cod-refer AND
                  movto-estoq.lote = saldo-estoq.lote AND
                  movto-estoq.tipo-trans = 2
                  NO-LOCK NO-ERROR.

        CREATE inventario.
        ASSIGN inventario.dt-saldo       = da-data-inv
               inventario.nr-ficha       = i-nr-ficha
               inventario.cod-estabel    = saldo-estoq.cod-estabel
               inventario.cod-depos      = saldo-estoq.cod-depos
               inventario.it-codigo      = saldo-estoq.it-codigo
               inventario.cod-refer      = saldo-estoq.cod-refer
               inventario.lote           = saldo-estoq.lote
               inventario.dt-ult-entra   = IF AVAIL b-movto-estoq
                                           THEN b-movto-estoq.dt-trans
                                           ELSE da-data-inv
               inventario.dt-ult-saida   = IF AVAIL movto-estoq
                                           THEN movto-estoq.dt-trans
                                           ELSE da-data-inv
               inventario.dec-2          = tt-aux.qt-final
               inventario.qtidade-atu    = de-qtidade-atu 
               inventario.val-apurado[1] = de-qtidade-atu + tt-aux.qt-entrada - tt-aux.qt-saida
               inventario.situacao       = 4.

    END.
    
END.

