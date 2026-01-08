DEF TEMP-TABLE tt-aux
    FIELD dt-trans    LIKE movto-estoq.dt-trans
    FIELD nr-ord-prod LIKE movto-estoq.nr-ord-prod
    FIELD nr-reporte  LIKE movto-estoq.nr-reporte
    FIELD it-codigo   LIKE movto-estoq.it-codigo
    FIELD cod-refer   LIKE movto-estoq.cod-refer
    FIELD lote        LIKE movto-estoq.lote
    FIELD qtd-rep     LIKE movto-estoq.quantidade
    FIELD qtd-etq     LIKE movto-estoq.quantidade.

DEF VAR de-qt-etq AS DEC.
DEF VAR de-dif AS DEC.

RUN pi-calc (1).  /* ACA - Produzido  */
RUN pi-calc (8).  /* EAC - Estorno do Produzido */

OUTPUT TO c:\temp\erro-rep01.txt.

FOR EACH tt-aux.
    IF tt-aux.qtd-etq <> tt-aux.qtd-rep THEN DO.
       ASSIGN de-dif = tt-aux.qtd-rep - tt-aux.qtd-etq.

       IF de-dif >= 20 THEN
          DISP tt-aux.dt-trans
               tt-aux.nr-ord-prod
               tt-aux.it-codigo
               tt-aux.cod-refer
               tt-aux.lote
               tt-aux.qtd-rep
               tt-aux.qtd-etq
               de-dif
               WITH WIDTH 350.
    END.
END.  
OUTPUT CLOSE.


PROCEDURE pi-calc.
    DEF INPUT PARAMETER p-esp AS INT.

    FOR EACH movto-estoq WHERE
             movto-estoq.esp-docto = p-esp AND
             movto-estoq.dt-trans >= 01.01.2008 AND
             movto-estoq.dt-trans <= 01.31.2008 NO-LOCK USE-INDEX esp-data.
    
        IF movto-estoq.lote BEGINS 'CA' THEN NEXT.
    
        FIND ord-prod WHERE
             ord-prod.nr-ord-prod = movto-estoq.nr-ord-prod NO-LOCK NO-ERROR.
        IF ord-prod.cd-planej <> 'automatico' THEN NEXT.
    
        ASSIGN de-qt-etq = 0.
        FOR EACH ob-etiqueta WHERE
                 ob-etiqueta.nr-reporte = movto-estoq.nr-reporte NO-LOCK.
            ASSIGN de-qt-etq = de-qt-etq + ob-etiqueta.quantidade.
        END.
    
        FIND FIRST tt-aux WHERE
                   tt-aux.dt-trans  = movto-estoq.dt-trans AND
                   tt-aux.it-codigo = movto-estoq.it-codigo AND 
                   tt-aux.lote      = movto-estoq.lote
                   NO-ERROR.      
        IF NOT AVAIL tt-aux THEN DO.
           CREATE tt-aux.
           ASSIGN tt-aux.dt-trans     = movto-estoq.dt-trans   
                  tt-aux.nr-ord-prod  = movto-estoq.nr-ord-prod
                  tt-aux.nr-reporte   = movto-estoq.nr-reporte 
                  tt-aux.it-codigo    = movto-estoq.it-codigo  
                  tt-aux.cod-refer    = movto-estoq.cod-refer
                  tt-aux.lote         = movto-estoq.lote.
        END.
    
        ASSIGN tt-aux.qtd-etq = tt-aux.qtd-etq + de-qt-etq.
        IF movto-estoq.esp-docto = 1 THEN
           ASSIGN tt-aux.qtd-rep = tt-aux.qtd-rep + movto-estoq.quantidade.
        ELSE
           ASSIGN tt-aux.qtd-rep = tt-aux.qtd-rep - movto-estoq.quantidade.
    END.
END PROCEDURE.

