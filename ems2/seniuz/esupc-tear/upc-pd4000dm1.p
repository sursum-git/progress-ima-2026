DEF INPUT PARAM p-wgh-frame      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-nome-abrev AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-nr-pedcli AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR l-copia-reservas AS LOG.

DEF VAR h-objeto      AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-browse      AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-query       AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-buffer      AS WIDGET-HANDLE NO-UNDO.
DEF VAR hb-nome-abrev AS WIDGET-HANDLE NO-UNDO.
DEF VAR hb-nr-pedcli  AS WIDGET-HANDLE NO-UNDO.

DEF BUFFER b-ped-item-res FOR ped-item-res.
DEF BUFFER b-ped-item-ext FOR ped-item-ext.
DEFINE TEMP-TABLE tt-copia-pedido 
       FIELD nr-pedcli LIKE ped-venda.nr-pedcli
       FIELD nome-abrev LIKE ped-venda.nome-abrev.

DEFINE TEMP-TABLE RowErrors NO-UNDO
       FIELD ErrorSequence    AS INTEGER
       FIELD ErrorNumber      AS INTEGER
       FIELD ErrorDescription AS CHARACTER
       FIELD ErrorParameters  AS CHARACTER
       FIELD ErrorType        AS CHARACTER
       FIELD ErrorHelp        AS CHARACTER
       FIELD ErrorSubType     AS CHARACTER.

DEF VAR h-bodi159com AS HANDLE.

IF NOT VALID-HANDLE(h-bodi159com) OR
   h-bodi159com:TYPE      <> "PROCEDURE":U OR
   h-bodi159com:FILE-NAME <> "dibo/bodi159com.p":U THEN
   RUN dibo/bodi159com.p PERSISTENT SET h-bodi159com.

ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
ASSIGN h-objeto = h-objeto:FIRST-CHILD.
DO WHILE VALID-HANDLE(h-objeto):
   IF h-objeto:TYPE <> "field-group" THEN DO:
      IF h-objeto:NAME = 'br-digita' THEN DO.
         ASSIGN h-browse = h-objeto:HANDLE
                h-query  = h-browse:QUERY.
      END.
      ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.
   ELSE 
      ASSIGN h-objeto = h-objeto:FIRST-CHILD.
END.
ASSIGN h-buffer = h-query:GET-BUFFER-HANDLE(1)
       hb-nome-abrev = h-buffer:BUFFER-FIELD(1)
       hb-nr-pedcli = h-buffer:BUFFER-FIELD(2).

h-query:GET-FIRST.
REPEAT WHILE NOT h-query:QUERY-OFF-END.
    CREATE tt-copia-pedido.
    ASSIGN tt-copia-pedido.nome-abrev = hb-nome-abrev:BUFFER-VALUE
           tt-copia-pedido.nr-pedcli = hb-nr-pedcli:BUFFER-VALUE.

    h-query:GET-NEXT.
END.

APPLY 'CHOOSE' TO SELF. 

FOR EACH tt-copia-pedido.
    FIND ped-venda WHERE
         ped-venda.nome-abrev = tt-copia-pedido.nome-abrev AND
         ped-venda.nr-pedcli = tt-copia-pedido.nr-pedcli NO-ERROR.

    IF AVAIL ped-venda THEN
       ASSIGN ped-venda.observacoes = ped-venda.observacoes + CHR(10) + CHR(10) +
                                     "Este Pedido foi COPIADO do Pedido " + h-nr-pedcli:SCREEN-VALUE + 
                                     " do Cliente " + h-nome-abrev:SCREEN-VALUE +
                                     IF l-copia-reservas 
                                     THEN " COM as Reservas."
                                     ELSE " SEM as Reservas.".


    /* Copia Acondicionamentos */
    FOR EACH b-ped-item-ext WHERE
             b-ped-item-ext.nome-abrev = h-nome-abrev:SCREEN-VALUE AND
             b-ped-item-ext.nr-pedcli = h-nr-pedcli:SCREEN-VALUE NO-LOCK.

        FIND ped-item WHERE
             ped-item.nome-abrev   = tt-copia-pedido.nome-abrev AND
             ped-item.nr-pedcli    = tt-copia-pedido.nr-pedcli AND
             ped-item.nr-sequencia = b-ped-item-ext.nr-sequencia AND
             ped-item.it-codigo    = b-ped-item-ext.it-codigo AND
             ped-item.cod-refer    = b-ped-item-ext.cod-refer
             NO-LOCK NO-ERROR.
    
        IF AVAIL ped-item THEN
           BUFFER-COPY b-ped-item-ext TO ped-item-ext
                       ASSIGN ped-item-ext.nome-abrev = tt-copia-pedido.nome-abrev
                              ped-item-ext.nr-pedcli  = tt-copia-pedido.nr-pedcli.
    END.


    /* Copia Reservas */
    IF l-copia-reservas THEN DO.
       FOR EACH b-ped-item-res WHERE
                b-ped-item-res.nome-abrev = h-nome-abrev:SCREEN-VALUE AND
                b-ped-item-res.nr-pedcli = h-nr-pedcli:SCREEN-VALUE AND
                b-ped-item-res.faturado  = NO EXCLUSIVE-LOCK.

           FIND ped-item WHERE
                ped-item.nome-abrev   = tt-copia-pedido.nome-abrev AND
                ped-item.nr-pedcli    = tt-copia-pedido.nr-pedcli AND
                ped-item.nr-sequencia = b-ped-item-res.nr-sequencia AND
                ped-item.it-codigo    = b-ped-item-res.it-codigo AND
                ped-item.cod-refer    = b-ped-item-res.cod-refer
                NO-LOCK NO-ERROR.

           IF AVAIL ped-item THEN DO.
              BUFFER-COPY b-ped-item-res TO ped-item-res
                          ASSIGN ped-item-res.nome-abrev = tt-copia-pedido.nome-abrev
                                 ped-item-res.nr-pedcli  = tt-copia-pedido.nr-pedcli.

              /* Altera Romaneio */
              FOR EACH ped-item-rom WHERE
                       ped-item-rom.nome-abrev = b-ped-item-res.nome-abrev AND
                       ped-item-rom.nr-pedcli = b-ped-item-res.nr-pedcli AND
                       ped-item-rom.nr-sequencia = b-ped-item-res.nr-sequencia
                       EXCLUSIVE-LOCK.

                  ASSIGN ped-item-rom.nome-abrev = tt-copia-pedido.nome-abrev
                         ped-item-rom.nr-pedcli = tt-copia-pedido.nr-pedcli.
              END.

              /* Elimina Reserva Origem */
              DELETE b-ped-item-res.
           END.
       END.
    END.

    


    FIND emitente WHERE
         emitente.nome-abrev = ped-venda.nome-abrev NO-LOCK NO-ERROR.

    ASSIGN ped-venda.nat-operacao = emitente.nat-operacao.
    FOR EACH ped-item OF ped-venda.
        ASSIGN ped-item.nat-operacao = emitente.nat-operacao
               ped-item.dt-entrega = ped-venda.dt-entrega.
    END.

    RUN completeOrder IN h-bodi159com (INPUT ROWID(ped-venda),
                                       OUTPUT TABLE Rowerrors).
    FOR EACH Rowerrors:
        MESSAGE rowerrors.errornumber
                rowerrors.errordescription VIEW-AS ALERT-BOX.
    END.
END.

IF VALID-HANDLE(h-bodi159com) THEN
   DELETE PROCEDURE h-bodi159com.
