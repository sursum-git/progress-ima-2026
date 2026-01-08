FIND nota-fiscal WHERE
     nota-fiscal.cod-estabel = "2" AND
     nota-fiscal.serie       = "1" AND 
     nota-fiscal.cod-emitente = 7592 AND 
     nota-fiscal.nr-nota-fis  = '0138562' NO-LOCK NO-ERROR.
IF AVAIL nota-fiscal THEN DO:
   DISP nota-fiscal.nr-pedcli
        nota-fiscal.nr-nota-fis
        nota-fiscal.nome-ab-cli.
   FOR EACH dev-item-rom WHERE
            dev-item-rom.nome-abrev = nota-fiscal.nome-ab-cli AND
            dev-item-rom.nr-pedcli  = nota-fiscal.nr-pedcli NO-LOCK.
       FIND ob-etiqueta WHERE
            ob-etiqueta.num-etiqueta = dev-item-rom.num-etiqueta NO-ERROR.
       IF AVAIL ob-etiqueta THEN
          ASSIGN ob-etiqueta.localizacao = "599599".
          DISP dev-item-rom.nome-abrev
               dev-item-rom.nr-pedcli 
               dev-item-rom.num-etiqueta
               ob-etiqueta.situacao VIEW-AS FILL-IN
               ob-etiqueta.localizacao 
               ob-etiqueta.quantidade (TOTAL)
               WITH WIDTH 550.
   END.

END.

