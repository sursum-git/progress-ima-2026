/* Programa: imp-desenhos.p
** Objetivo: Importar desenhos da planilha da grava‡Æo. 
*/

DEF TEMP-TABLE tt-desenhos
    FIELD codigo      AS CHAR
    FIELD situacao    AS INT
    FIELD estilo      AS CHAR 
    FIELD ordem-grav  AS CHAR
    FIELD box         AS CHAR
    FIELD caderno     AS CHAR
    FIELD largura     AS DEC
    FIELD filme       AS CHAR
    FIELD montagem    AS CHAR
    FIELD obs-cliente AS CHAR
    FIELD fundo       AS CHAR
    FIELD observ      AS CHAR.
    
input from "c:/temp/desenhos.csv".
SET ^.

repeat:
   create tt-desenhos.
   import delimiter ";" tt-desenhos.
end.
input close.

FOR EACH desenhos.
    DELETE desenhos.
END.

FOR EACH tt-desenhos:
    DISP tt-desenhos WITH SIDE-LABELS 1 COLUMN.
    CREATE desenhos.
    ASSIGN desenhos.codigo      = tt-desenhos.codigo
           desenhos.situacao    = tt-desenhos.situacao
           desenhos.estilo      = tt-desenhos.estilo
           desenhos.ordem-grav  = tt-desenhos.ordem-grav
           desenhos.BOX         = tt-desenhos.BOX
           desenhos.caderno     = tt-desenhos.caderno
           desenhos.largura     = tt-desenhos.largura
           desenhos.filme       = tt-desenhos.filme
           desenhos.montagem    = tt-desenhos.montagem
           desenhos.obs-cliente = tt-desenhos.obs-cliente
           desenhos.fundo       = tt-desenhos.fundo
           desenhos.observ      = tt-desenhos.observ.
END.

