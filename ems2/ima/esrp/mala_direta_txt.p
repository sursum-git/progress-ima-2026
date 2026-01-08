DEF SHARED TEMP-TABLE tt-disp-colunas
    FIELD ordem  AS INT
    FIELD coluna AS CHAR.

DEF SHARED TEMP-TABLE tt-conteudo
    FIELD cod-emitente   AS INT FORMAT ">>>>>9"
    FIELD nome-ab-cli    AS CHAR FORMAT "x(25)"          
    FIELD nome-emit      AS CHAR FORMAT "x(45)"          
    FIELD endereco       AS CHAR FORMAT "x(45)"          
    FIELD cidade         AS CHAR FORMAT "x(45)"          
    FIELD bairro         AS CHAR FORMAT "x(30)"          
    FIELD estado         AS CHAR FORMAT "X(2)" 
    FIELD CEP            AS CHAR FORMAT "x(20)"
    FIELD endereco-cob   AS CHAR FORMAT "x(45)"          
    FIELD cidade-cob     AS CHAR FORMAT "x(45)"          
    FIELD bairro-cob     AS CHAR FORMAT "x(30)"          
    FIELD estado-cob     AS CHAR FORMAT "X(2)"           
    FIELD CEP-cob        AS CHAR FORMAT "x(20)"
    FIELD email          AS CHAR FORMAT "x(60)" 
    FIELD ind-cre-cli    AS INT FORMAT "9"
    FIELD cod-ramo-ativ  AS INT FORMAT ">>>>>9"
    FIELD desc-ramo-ativ AS CHAR FORMAT "x(45)"
    FIELD telefone       AS CHAR FORMAT "x(40)"
    FIELD dt-ult-compra  AS DATE
    FIELD tot-venda      AS DEC
    FIELD cod-rep-ven    AS INT
    FIELD nom-rep-ven    AS CHAR FORMAT "x(45)"
    FIELD cod-rep-cad    AS INT
    FIELD nom-rep-cad    AS CHAR FORMAT "x(45)"
    FIELD cgc            AS CHAR FORMAT "x(45)".

DEFINE NEW GLOBAL SHARED VARIABLE caminho       AS CHARACTER  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE nome-arquivo  AS CHARACTER  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-tg-partir AS LOG  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE salvar-como   AS CHARACTER  NO-UNDO.

DEF VAR c-colunas AS CHAR.
DEF VAR i-ct AS INT.

FOR EACH tt-disp-colunas NO-LOCK BREAK BY tt-disp-colunas.ordem.
    ASSIGN c-colunas = IF c-colunas = "" 
                       THEN TRIM(tt-disp-colunas.coluna)
                       ELSE c-colunas + "#" + TRIM(tt-disp-colunas.coluna).
END.

IF var-tg-partir THEN DO:
   FOR EACH tt-conteudo BREAK BY tt-conteudo.nom-rep-ven. 
       IF FIRST-OF(tt-conteudo.nom-rep-ven) THEN DO: 
          ASSIGN salvar-como = caminho + nome-arquivo + "-" + tt-conteudo.nom-rep-ven + ".txt".
          OUTPUT TO VALUE(salvar-como).

          PUT trim(tt-conteudo.nom-rep-ven)
              SKIP(1).

          PUT c-colunas SKIP.
       END.

       DO i-ct = 1 TO NUM-ENTRIES(c-colunas,"#").
          CASE ENTRY(i-ct,c-colunas,"#").
              WHEN  "cod-emitente"     THEN PUT UNFORMATTED tt-conteudo.cod-emit.
              WHEN  "nome-ab-cli"      THEN PUT UNFORMATTED tt-conteudo.nome-ab-cli.
              WHEN  "nome-emit"        THEN PUT UNFORMATTED tt-conteudo.nome-emit.              
              WHEN  "endereco"         THEN PUT UNFORMATTED tt-conteudo.endereco.
              WHEN  "bairro"           THEN PUT UNFORMATTED tt-conteudo.bairro.
              WHEN  "cidade"           THEN PUT UNFORMATTED tt-conteudo.cidade.
              WHEN  "estado"           THEN PUT UNFORMATTED tt-conteudo.estado.
              WHEN  "CEP"              THEN PUT UNFORMATTED tt-conteudo.cep.
              WHEN  "endereco-cob"     THEN PUT UNFORMATTED tt-conteudo.endereco-cob.
              WHEN  "bairro-cob"       THEN PUT UNFORMATTED tt-conteudo.bairro-cob.
              WHEN  "cidade-cob"       THEN PUT UNFORMATTED tt-conteudo.cidade-cob.
              WHEN  "estado-cob"       THEN PUT UNFORMATTED tt-conteudo.estado-cob.
              WHEN  "CEP-cob"          THEN PUT UNFORMATTED tt-conteudo.cep-cob.
              WHEN  "email"            THEN PUT UNFORMATTED tt-conteudo.email.
              WHEN  "cod-ramo-ativ"    THEN PUT UNFORMATTED tt-conteudo.cod-ramo-ativ.
              WHEN  "desc-ramo-ativ"   THEN PUT UNFORMATTED tt-conteudo.desc-ramo-ativ.
              WHEN  "telefone"         THEN PUT UNFORMATTED tt-conteudo.telefone.
              WHEN  "dt-ult-compra"    THEN PUT UNFORMATTED tt-conteudo.dt-ult-compra.
              WHEN  "tot-venda"        THEN PUT UNFORMATTED tt-conteudo.tot-venda.
              WHEN  "cod-rep-ven"      THEN PUT UNFORMATTED tt-conteudo.cod-rep-ven.
              WHEN  "nom-rep-ven"      THEN PUT UNFORMATTED tt-conteudo.nom-rep-ven.
              WHEN  "cod-rep-cad"      THEN PUT UNFORMATTED tt-conteudo.cod-rep-cad.
              WHEN  "nom-rep-cad"      THEN PUT UNFORMATTED tt-conteudo.nom-rep-cad.
              WHEN  "cgc"              THEN PUT UNFORMATTED tt-conteudo.cgc.
          END CASE.
          PUT "#".
       END.
       PUT SKIP.
       
       If LAST-OF(tt-conteudo.nom-rep-ven) THEN DO:
          OUTPUT CLOSE.
       END.
   END.
END.
ELSE DO:
   ASSIGN salvar-como = caminho + nome-arquivo + ".txt".
   OUTPUT TO VALUE(salvar-como).
   PUT c-colunas SKIP.
   
   FOR EACH tt-conteudo NO-LOCK. 
       DO i-ct = 1 TO NUM-ENTRIES(c-colunas,"#").
          CASE ENTRY(i-ct,c-colunas,"#").
               WHEN  "cod-emitente"     THEN PUT UNFORMATTED tt-conteudo.cod-emit.
               WHEN  "nome-ab-cli"      THEN PUT UNFORMATTED tt-conteudo.nome-ab-cli.
               WHEN  "nome-emit"        THEN PUT UNFORMATTED tt-conteudo.nome-emit.              
               WHEN  "endereco"         THEN PUT UNFORMATTED tt-conteudo.endereco.
               WHEN  "bairro"           THEN PUT UNFORMATTED tt-conteudo.bairro.
               WHEN  "cidade"           THEN PUT UNFORMATTED tt-conteudo.cidade.
               WHEN  "estado"           THEN PUT UNFORMATTED tt-conteudo.estado.
               WHEN  "CEP"              THEN PUT UNFORMATTED tt-conteudo.cep.
               WHEN  "endereco-cob"     THEN PUT UNFORMATTED tt-conteudo.endereco-cob.
               WHEN  "bairro-cob"       THEN PUT UNFORMATTED tt-conteudo.bairro-cob.
               WHEN  "cidade-cob"       THEN PUT UNFORMATTED tt-conteudo.cidade-cob.
               WHEN  "estado-cob"       THEN PUT UNFORMATTED tt-conteudo.estado-cob.
               WHEN  "CEP-cob"          THEN PUT UNFORMATTED tt-conteudo.cep-cob.
               WHEN  "email"            THEN PUT UNFORMATTED tt-conteudo.email.
               WHEN  "cod-ramo-ativ"    THEN PUT UNFORMATTED tt-conteudo.cod-ramo-ativ.
               WHEN  "desc-ramo-ativ"   THEN PUT UNFORMATTED tt-conteudo.desc-ramo-ativ.
               WHEN  "telefone"         THEN PUT UNFORMATTED tt-conteudo.telefone.
               WHEN  "dt-ult-compra"    THEN PUT UNFORMATTED tt-conteudo.dt-ult-compra.
               WHEN  "tot-venda"        THEN PUT UNFORMATTED tt-conteudo.tot-venda.
               WHEN  "cod-rep-ven"      THEN PUT UNFORMATTED tt-conteudo.cod-rep-ven.
               WHEN  "nom-rep-ven"      THEN PUT UNFORMATTED tt-conteudo.nom-rep-ven.
               WHEN  "cod-rep-cad"      THEN PUT UNFORMATTED tt-conteudo.cod-rep-cad.
               WHEN  "nom-rep-cad"      THEN PUT UNFORMATTED tt-conteudo.nom-rep-cad.
               WHEN  "cgc"              THEN PUT UNFORMATTED tt-conteudo.cgc.
          END CASE.
          PUT "#".
       END.
       PUT SKIP.
   END.
   OUTPUT CLOSE.
END.


