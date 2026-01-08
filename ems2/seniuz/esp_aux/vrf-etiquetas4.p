
DEF TEMP-TABLE tt-retalho
    FIELD cod-estabel  LIKE ob-etiqueta.cod-estabel
    FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta
    FIELD nr-ord-prod  LIKE ord-prod.nr-ord-prod
    FIELD qtd-ordem    LIKE ord-prod.qt-ordem
    FIELD qtd-retalho  LIKE ord-prod.qt-produzida
    FIELD reportado    AS LOG INIT NO
    INDEX ch-ordem IS PRIMARY nr-ord-prod.

DEF BUFFER b-etiqueta  FOR ob-etiqueta.
DEF BUFFER b-ordem-benefic FOR ordem-benefic.

DEF VAR de-tot-aca     AS   DEC.
DEF VAR i-cont AS INT.
DEF VAR i-ct AS INT.

FOR EACH ob-etiqueta WHERE
         ob-etiqueta.it-codigo = '501829' 
         BY ob-etiqueta.dt-emissao
         BY ob-etiqueta.hr-emissao.

    IF ob-etiqueta.nr-lote <> 'sc' THEN NEXT.

    ASSIGN de-tot-aca = 0
           i-cont = 0.

    DO i-ct = NUM-ENTRIES(ob-etiqueta.ob-origem,";") TO 1 BY -1.

       IF i-cont > 5 THEN NEXT.  /* reporta as ultimas 5 ob's */

       FIND FIRST b-ordem-benefic WHERE
                  b-ordem-benefic.cod-estab = ob-etiqueta.cod-estab AND
                  b-ordem-benefic.nr-ob = INTEGER(ENTRY(i-ct,ob-etiqueta.ob-origem,";"))
                  NO-LOCK NO-ERROR.

       IF NOT AVAIL b-ordem-benefic THEN NEXT.

       FOR EACH b-etiqueta WHERE
                b-etiqueta.cod-estab = b-ordem-benefic.cod-estab AND
                b-etiqueta.nr-ob = b-ordem-benefic.nr-ob AND
                b-etiqueta.nr-reporte > 0 NO-LOCK.
           FIND FIRST movto-estoq WHERE 
                      movto-estoq.nr-reporte = b-etiqueta.nr-reporte AND
                      movto-estoq.esp-docto = 1  /* aca */
                      USE-INDEX nr-reporte NO-LOCK NO-ERROR.

           IF NOT AVAIL movto-estoq THEN NEXT.

           FIND ord-prod WHERE
                ord-prod.nr-ord-prod = movto-estoq.nr-ord-prod
                NO-LOCK NO-ERROR.
           IF ord-prod.estado > 7 THEN NEXT.

           FIND tt-retalho WHERE
                tt-retalho.num-etiqueta = ob-etiqueta.num-etiqueta AND
                tt-retalho.nr-ord-prod = movto-estoq.nr-ord-prod NO-LOCK NO-ERROR.
           IF NOT AVAIL tt-retalho THEN DO.
              CREATE tt-retalho.
              ASSIGN tt-retalho.cod-estabel = ob-etiqueta.cod-estabel
                     tt-retalho.num-etiqueta = ob-etiqueta.num-etiqueta
                     tt-retalho.nr-ord-prod = movto-estoq.nr-ord-prod
                     tt-retalho.qtd-ordem = movto-estoq.quantidade.
           END.
       END.
       ASSIGN i-cont = i-cont + 1.
    END.

    FOR EACH tt-retalho WHERE
             tt-retalho.num-etiqueta = ob-etiqueta.num-etiqueta.
    
        FIND b-etiqueta WHERE
             b-etiqueta.cod-estabel = tt-retalho.cod-estabel AND
             b-etiqueta.num-etiqueta = tt-retalho.num-etiqueta
             NO-LOCK NO-ERROR.

       DISP tt-retalho.num-etiqueta
            tt-retalho.nr-ord-prod
            tt-retalho.qtd-ordem (TOTAL)
            b-etiqueta.quantidade.
    
    END.

END.
