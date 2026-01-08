DEFINE PARAMETER BUFFER p-table FOR nota-fiscal.
DEFINE PARAMETER BUFFER p-table-old FOR nota-fiscal.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF VAR c-arq-saida AS CHAR.
DEF VAR c-var-saida AS CHAR.
DEF VAR c-cnpj-tra  LIKE transporte.cgc.
DEF VAR c-nome-tra  LIKE transporte.nome.

/* Criou Nota Fiscal */
IF NEW p-table THEN DO.
   FOR EACH it-nota-fisc OF p-table NO-LOCK.
       FOR EACH ped-item-res WHERE 
                ped-item-res.nome-abrev    = p-table.nome-ab-cli AND 
                ped-item-res.nr-pedcli     = it-nota-fisc.nr-pedcli AND
                ped-item-res.it-codigo     = it-nota-fisc.it-codigo AND 
                ped-item-res.nr-sequencia  = it-nota-fisc.nr-seq-ped.

           ASSIGN ped-item-res.faturado    = YES 
                  ped-item-res.cod-estabel = p-table.cod-estabel
                  ped-item-res.serie       = p-table.serie
                  ped-item-res.nr-nota-fis = INT(p-table.nr-nota-fis).

           FOR EACH ped-item-rom WHERE
                    ped-item-rom.nome-abrev = ped-item-res.nome-abrev AND
                    ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli AND
                    ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia
                    EXCLUSIVE-LOCK.
               FIND ob-etiqueta WHERE
                    ob-etiqueta.cod-estabel = ped-item-rom.cod-estabel AND
                    ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                    SHARE-LOCK NO-ERROR.
               IF AVAIL ob-etiqueta THEN
                  ASSIGN ob-etiqueta.situacao = 5.
           END.
       END.
   END.

   FOR EACH ob-etiqueta WHERE
            ob-etiqueta.it-codigo = 'VOLUME' AND
            ob-etiqueta.cod-refer = '' AND
            ob-etiqueta.ob-origem = p-table.nr-pedcli SHARE-LOCK
            USE-INDEX indice6.

       IF ob-etiqueta.cod-estabel <> p-table.cod-estabel THEN NEXT.

       ASSIGN ob-etiqueta.cod-refer = p-table.nr-nota-fis.
   END.
END.

/* Informouu data de saida da NF */
IF p-table.dt-saida <> ? AND p-table-old.dt-saida = ? THEN DO:
   FIND transporte WHERE 
        transporte.nome-abrev = p-table.nome-transp NO-LOCK NO-ERROR.
   FIND emitente WHERE 
        emitente.cod-emitente = p-table.cod-emitente NO-LOCK NO-ERROR.
   IF AVAIL transporte THEN
      ASSIGN c-cnpj-tra = transporte.cgc
             c-nome-tra = transporte.nome.
   ELSE
      ASSIGN c-cnpj-tra = p-table.cgc
             c-nome-tra = emitente.nome-emit.

   FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.
       FIND ITEM WHERE ITEM.it-codigo = it-nota-fisc.it-codigo
                 NO-LOCK NO-ERROR.
       IF AVAIL ITEM AND 
          ITEM.fm-codigo = "62" THEN DO: /* Res¡duos */
          ASSIGN c-arq-saida = SESSION:TEMP-DIRECTORY + 
                               STRING(YEAR(p-table.dt-emis-nota),"9999") +
                               STRING(MONTH(p-table.dt-emis-nota),"99") +
                               STRING(DAY(p-table.dt-emis-nota),"99") + "_E.TXT".

          ASSIGN c-var-saida = TRIM(it-nota-fisc.nr-nota-fis) + "|" +
                               TRIM(it-nota-fisc.it-codigo) + "|" +
                               TRIM(ITEM.un) + "|" +
                               TRIM(STRING(it-nota-fisc.qt-faturada[1] * 100,"999999999999")) + "|" +
                               TRIM(c-cnpj-tra) + "|" +
                               TRIM(c-nome-tra) + "|" +
                               TRIM(p-table.cgc) + "|" +
                               TRIM(emitente.nome-emit) + "|" +
                               STRING(DAY(p-table.dt-emis-nota),"99") + STRING(MONTH(p-table.dt-emis-nota),"99") +
                                                                        STRING(YEAR(p-table.dt-emis-nota),"9999") + "|" +
                               STRING(TIME,"HH:MM").

          OUTPUT TO VALUE(c-arq-saida) APPEND.
              PUT c-var-saida FORMAT "x(132)"
                  SKIP.
          OUTPUT CLOSE.
       END.
   END.
END.

/* Nota est  sendo Cancelada */
IF p-table.dt-cancela <> p-table-old.dt-cancela AND
   p-table.dt-cancela <> ? THEN DO.
   FOR EACH ped-item-res WHERE
            ped-item-res.cod-estabel = p-table.cod-estabel AND
            ped-item-res.serie       = p-table.serie AND
            ped-item-res.nr-nota-fis = INT(p-table.nr-nota-fis) AND
            ped-item-res.faturado    = YES.
       ASSIGN ped-item-res.serie       = ""
              ped-item-res.nr-nota-fis = 0
              ped-item-res.faturado    = NO.

       FOR EACH ped-item-rom WHERE
                ped-item-rom.nome-abrev = ped-item-res.nome-abrev AND
                ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli AND
                ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia
                BREAK BY ped-item-rom.nr-volume.

           FIND ob-etiqueta WHERE
                ob-etiqueta.cod-estabel = ped-item-rom.cod-estabel AND
                ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                SHARE-LOCK NO-ERROR.
           IF AVAIL ob-etiqueta THEN
              ASSIGN ob-etiqueta.situacao = 4.
       END.
   END.

   FOR EACH ob-etiqueta WHERE
            ob-etiqueta.it-codigo = 'VOLUME' AND
            ob-etiqueta.cod-refer = p-table.nr-nota-fis AND
            ob-etiqueta.ob-origem = p-table.nr-pedcli SHARE-LOCK
            USE-INDEX indice6.

       IF ob-etiqueta.cod-estabel <> p-table.cod-estabel THEN NEXT.

       ASSIGN ob-etiqueta.cod-refer = ''.
   END.
END.

/* Tratativa GATI */
IF p-table.ind-tip-nota     = 8 
OR p-table-old.ind-tip-nota = 8 THEN DO:
    
    ASSIGN p-table.hr-confirma = string(TIME,"HH:MM").

END.
