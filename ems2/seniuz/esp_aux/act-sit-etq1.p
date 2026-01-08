DEF VAR i-num-etiqueta LIKE ob-etiqueta.num-etiqueta.
DEF VAR l-confirma     AS LOG FORMAT "Sim/N∆o" LABEL "Confirma?".
DEF VAR c-situacao     AS CHAR LABEL "Situaá∆o".
DEF VAR i-situacao     LIKE ob-etiqueta.situacao.

REPEAT ON ENDKEY UNDO,LEAVE:
    UPDATE i-num-etiqueta WITH 15 DOWN.

    FIND ob-etiqueta WHERE ob-etiqueta.num-etiqueta = i-num-etiqueta
                     NO-ERROR.
    IF NOT AVAIL ob-etiqueta THEN DO:
       MESSAGE "Etiqueta n∆o encontrada!"
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       UNDO,RETRY.
    END.

    {esinc/i-dsrb.i ob-etiqueta.situacao ob-etiqueta.situacao c-situacao}
    DISPLAY c-situacao.

    FIND ped-item-rom WHERE ped-item-rom.nr-ob      = ob-etiqueta.nr-ob
                        AND ped-item-rom.nr-seq-etq = ob-etiqueta.nr-sequencia
                      NO-ERROR.
    IF AVAIL ped-item-rom THEN
       DISPLAY ped-item-rom.nr-pedcli FORMAT "x(6)" LABEL "Pedido"
               ped-item-rom.nome-abrev.

    IF ob-etiqueta.situacao = 5 AND NOT AVAIL ped-item-rom THEN DO:
       MESSAGE "Etiqueta FATURADA est† SEM Romaneio." SKIP
               "Cuidado com a Alteraá∆o ser feita!"
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.

    ASSIGN l-confirma = NO.
    UPDATE i-situacao
           l-confirma.

    IF l-confirma THEN DO:
       ASSIGN ob-etiqueta.situacao = i-situacao.
       IF i-situacao <> 5 AND AVAIL ped-item-rom THEN
          DELETE ped-item-rom.
    END.

END.
