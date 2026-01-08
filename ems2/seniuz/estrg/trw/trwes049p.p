DEFINE PARAMETER BUFFER p-ob-etiqueta FOR ob-etiqueta.
DEFINE PARAMETER BUFFER p-ob-etiqueta-old FOR ob-etiqueta.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
DEF VAR c-especie AS CHAR.
DEF VAR l-tipo-trans AS LOG INIT YES.
DEF VAR c-programas AS CHAR.
DEF VAR c-narrativa AS CHAR.

IF PROGRAM-NAME(1) <> ? THEN
   ASSIGN c-programas = PROGRAM-NAME(1) + FILL(" ",3).
IF PROGRAM-NAME(2) <> ? THEN
   ASSIGN c-programas = c-programas + PROGRAM-NAME(2) + FILL(" ",3).
IF PROGRAM-NAME(3) <> ? THEN
   ASSIGN c-programas = c-programas + PROGRAM-NAME(3) + FILL(" ",3).
IF PROGRAM-NAME(4) <> ? THEN
   ASSIGN c-programas = c-programas + PROGRAM-NAME(4) + FILL(" ",3).
IF PROGRAM-NAME(5) <> ? THEN
   ASSIGN c-programas = c-programas + PROGRAM-NAME(5) + FILL(" ",3).
IF PROGRAM-NAME(6) <> ? THEN
   ASSIGN c-programas = c-programas + PROGRAM-NAME(6) + FILL(" ",3).
IF PROGRAM-NAME(7) <> ? THEN
   ASSIGN c-programas = c-programas + PROGRAM-NAME(7) + FILL(" ",3).
IF PROGRAM-NAME(8) <> ? THEN
   ASSIGN c-programas = c-programas + PROGRAM-NAME(8) + FILL(" ",3).
IF PROGRAM-NAME(9) <> ? THEN
   ASSIGN c-programas = c-programas + PROGRAM-NAME(9) + FILL(" ",3).
IF PROGRAM-NAME(10) <> ? THEN
   ASSIGN c-programas = c-programas + PROGRAM-NAME(10) + FILL(" ",3).
         
IF NEW p-ob-etiqueta THEN DO.
   ASSIGN c-narrativa = "Usuario: " + c-seg-usuario + FILL(" ",10) +
                        "Hora: " + STRING(TIME,"HH:MM:SS") + FILL(" ",10) +
                        "Loc Ant: " + p-ob-etiqueta-old.localizacao + FILL(" ",5) +
                        "Nova Loc: " + p-ob-etiqueta.localizacao + FILL(" ",5) +
                        "Programa: " + IF c-programas <> ? 
                                       THEN c-programas
                                       ELSE "Indefinido".
   RUN pi-cria-log (INPUT 'CRI', INPUT c-narrativa).

END.
ELSE DO.
   IF p-ob-etiqueta.localizacao <> p-ob-etiqueta-old.localizacao THEN DO.
      ASSIGN c-narrativa = "Usuario: " + c-seg-usuario + FILL(" ",10) +
                           "Hora: " + STRING(TIME,"HH:MM:SS") + FILL(" ",10) +
                           "Loc Ant: " + p-ob-etiqueta-old.localizacao + FILL(" ",5) +
                           "Nova Loc: " + p-ob-etiqueta.localizacao + FILL(" ",5) +
                           "Programa: " + IF c-programas <> ? 
                                          THEN c-programas
                                          ELSE "Indefinido".

      RUN pi-cria-log (INPUT 'LOC', INPUT c-narrativa).
   END.

   IF p-ob-etiqueta.cod-depos <> p-ob-etiqueta-old.cod-depos THEN DO.
       ASSIGN c-narrativa = "Usuario: " + c-seg-usuario + FILL(" ",10) +
                            "Hora: " + STRING(TIME,"HH:MM:SS") + FILL(" ",10) +
                            "Loc Ant: " + p-ob-etiqueta-old.cod-depos + FILL(" ",5) +
                            "Nova Loc: " + p-ob-etiqueta.cod-depos + FILL(" ",5) +
                            "Programa: " + IF c-programas <> ? 
                                           THEN c-programas
                                           ELSE "Indefinido".

       RUN pi-cria-log (INPUT 'DEF', INPUT c-narrativa).
   END.

   IF p-ob-etiqueta.quantidade <> p-ob-etiqueta-old.quantidade THEN DO.
       ASSIGN c-narrativa = "Usuario: " + c-seg-usuario + FILL(" ",10) +
                            "Hora: " + STRING(TIME,"HH:MM:SS") + FILL(" ",10) +
                            "Qtde Ant: " + STRING(p-ob-etiqueta-old.quantidade) + FILL(" ",5) +
                            "Nova Qtde: " + STRING(p-ob-etiqueta.quantidade) + FILL(" ",5) +
                            "Programa: " + IF c-programas <> ? 
                                           THEN c-programas
                                           ELSE "Indefinido".

       RUN pi-cria-log (INPUT 'ALT', INPUT c-narrativa).
   END.

   IF p-ob-etiqueta.situacao <> p-ob-etiqueta-old.situacao THEN DO.
      FIND bc-etiqueta WHERE
           bc-etiqueta.progressivo = p-ob-etiqueta.progressivo SHARE-LOCK NO-ERROR.
    
      IF AVAIL bc-etiqueta THEN DO.
         CASE p-ob-etiqueta.situacao:
              WHEN 3 THEN DO.
                  IF bc-etiqueta.cod-estado <> 2 THEN
                     ASSIGN bc-etiqueta.cod-estado = 2.
              END.
              WHEN 4 THEN DO.
                  IF bc-etiqueta.cod-estado <> 3 THEN
                     ASSIGN bc-etiqueta.cod-estado = 3.
              END.
              WHEN 5 THEN DO.
                  IF bc-etiqueta.cod-estado <> 4 THEN
                     ASSIGN bc-etiqueta.cod-estado = 4.
              END.
              WHEN 9 THEN DO.
                  IF bc-etiqueta.cod-estado <> 99 THEN
                     ASSIGN bc-etiqueta.cod-estado = 99.
              END.
         END CASE.
      END.
    
      CASE p-ob-etiqueta.situacao.
           WHEN 0 THEN ASSIGN c-especie = 'CRI'.
           WHEN 1 THEN ASSIGN c-especie = 'IMP'.
           WHEN 2 THEN ASSIGN c-especie = 'REV'.
           WHEN 3 THEN ASSIGN c-especie = 'EST'.
           WHEN 4 THEN ASSIGN c-especie = 'RES'.
           WHEN 5 THEN ASSIGN c-especie = 'FAT'.
           WHEN 6 THEN ASSIGN c-especie = 'BEN'.
           WHEN 7 THEN ASSIGN c-especie = 'CON'.
           WHEN 8 THEN ASSIGN c-especie = 'BLQ'.
           WHEN 9 THEN ASSIGN c-especie = 'ACT'.
      END CASE.
    
      ASSIGN c-narrativa = "Usuario: " + c-seg-usuario + FILL(" ",10) +
                           "Hora: " + STRING(TIME,"HH:MM:SS") + FILL(" ",10) +
                           "Sit Ant: " + STRING(p-ob-etiqueta-old.situacao) + FILL(" ",5) +
                           "Sit Nova: " + STRING(p-ob-etiqueta.situacao) + FILL(" ",5) +
                           "Programa: " + IF c-programas <> ? 
                                          THEN c-programas
                                          ELSE "Indefinido".

      RUN pi-cria-log (INPUT c-especie, INPUT c-narrativa).
   END.
END.
    


PROCEDURE pi-cria-log.
    DEF INPUT PARAMETER p-esp-docto AS CHAR.
    DEF INPUT PARAMETER p-narrativa AS CHAR.

    CREATE movto-etq.
    ASSIGN movto-etq.cod-estabel = p-ob-etiqueta.cod-estabel
           movto-etq.dt-trans = TODAY
           movto-etq.esp-docto = p-esp-docto
           movto-etq.nro-docto = STRING(p-ob-etiqueta.nr-ob)
           movto-etq.num-etiqueta = p-ob-etiqueta.num-etiqueta 
           movto-etq.quantidade = p-ob-etiqueta.quantidade
           movto-etq.tipo-trans = l-tipo-trans
           movto-etq.char-1 = p-narrativa.
END PROCEDURE.
