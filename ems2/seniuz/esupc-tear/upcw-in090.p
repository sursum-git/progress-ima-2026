DEFINE PARAMETER BUFFER p-table FOR docum-est.
DEFINE PARAMETER BUFFER p-table-old FOR docum-est.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

IF p-table.esp-docto = 20 AND
   p-table-old.ce-atual = NO AND p-table.ce-atual = YES THEN DO.
   FOR EACH item-doc-est OF docum-est NO-LOCK,
       FIRST item WHERE
             item.it-codigo = item-doc-est.it-codigo AND
             item.ge-codigo >= 50 AND
             item.ge-codigo <= 58 NO-LOCK.

       FIND nota-fiscal WHERE
            nota-fiscal.cod-estabel = p-table.cod-estabel AND
            nota-fiscal.serie = item-doc-est.serie-comp AND
            nota-fiscal.nr-nota-fis = item-doc-est.nro-comp
            NO-LOCK NO-ERROR.

       FIND it-nota-fisc OF nota-fiscal WHERE
            it-nota-fisc.nr-seq-fat = item-doc-est.seq-comp AND
            it-nota-fisc.it-codigo = item-doc-est.it-codigo
            NO-LOCK NO-ERROR.

       FOR EACH ped-item-res WHERE 
                ped-item-res.nome-abrev   = nota-fiscal.nome-ab-cli AND 
                ped-item-res.nr-pedcli    = it-nota-fisc.nr-pedcli AND
                ped-item-res.it-codigo    = it-nota-fisc.it-codigo AND 
                ped-item-res.nr-sequencia = it-nota-fisc.nr-seq-ped
                NO-LOCK.
           FOR EACH ped-item-rom WHERE
                    ped-item-rom.nome-abrev = ped-item-res.nome-abrev AND
                    ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli AND
                    ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia AND
                    ped-item-rom.marca = "DEV" EXCLUSIVE-LOCK.
               FIND ob-etiqueta WHERE
                    ob-etiqueta.cod-estabel = ped-item-rom.cod-estabel AND
                    ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                    EXCLUSIVE-LOCK NO-ERROR.

               IF AVAIL ob-etiqueta THEN
                  ASSIGN ob-etiqueta.situacao = 3.

               CREATE movto-etq.
               ASSIGN movto-etq.dt-trans = TODAY
                      movto-etq.esp-docto = 'DEV'
                      movto-etq.nro-docto = STRING(ob-etiqueta.nr-ob)
                      movto-etq.num-etiqueta = ob-etiqueta.num-etiqueta 
                      movto-etq.quantidade = ob-etiqueta.quantidade
                      movto-etq.tipo-trans = YES
                      movto-etq.char-1 = "Usuario: " + c-seg-usuario + FILL(" ",10) +
                                         "Hora: " + STRING(TIME,"HH:MM:SS") + FILL(" ",10) +
                                         "Situacao: " + STRING(ob-etiqueta.situacao) + FILL(" ",10) +
                                         "Programa: upcw-in090.p".

               DELETE ped-item-rom.
           END.
       END.
   END.
END.

