DEFINE TEMP-TABLE ttArquivo
      FIELD secao      AS CHAR
      FIELD divisao    AS CHAR
      FIELD grupo      AS CHAR
      FIELD classe     AS CHAR
      FIELD subclasse  AS CHAR FORMAT 'X(20)'
      FIELD descricao  AS CHAR FORMAT 'X(50)'.
DEFINE TEMP-TABLE ttSecao
    FIELD codigo    AS CHAR
    FIELD descricao AS CHAR FORMAT 'x(100)'.
DEFINE TEMP-TABLE ttDivisao
    FIELD codigo    AS CHAR
    FIELD descricao AS CHAR FORMAT 'x(100)'.
DEFINE TEMP-TABLE ttGrupo
    FIELD codigo    AS CHAR
    FIELD descricao AS CHAR FORMAT 'x(100)'.
DEFINE TEMP-TABLE ttClasse
    FIELD codigo    AS CHAR
    FIELD descricao AS CHAR FORMAT 'x(100)'.
DEFINE TEMP-TABLE ttSubclasse
    FIELD codigo    AS CHAR FORMAT 'x(10)'
    FIELD descricao AS CHAR FORMAT 'x(100)'.
DEFINE VARIABLE cCnae AS CHARACTER   NO-UNDO.

INPUT FROM c:\cnae22.csv.

REPEAT:
    CREATE ttArquivo.
    IMPORT DELIMITER ";" ttArquivo.
END.
/*OUTPUT TO c:\temp\ttarquivo.txt.
FOR EACH ttArquivo.
    DISP ttArquivo.
END.*/

FOR EACH ttArquivo.
/*    DISP tt.*/
    IF  ttArquivo.secao <> '' AND ttArquivo.divisao = ''
        AND ttArquivo.grupo = '' AND ttArquivo.classe = ''
        AND ttArquivo.subclasse = '' THEN DO:
        CREATE ttSecao.
        ASSIGN ttSecao.codigo      = ttArquivo.secao
               ttSecao.descricao   = ttArquivo.descricao.
    END.

    IF  ttArquivo.secao <> '' AND ttArquivo.divisao <> ''
        AND ttArquivo.grupo = '' AND ttArquivo.classe = ''
        AND ttArquivo.subclasse = '' THEN DO:
        CREATE ttDivisao.
        ASSIGN ttDivisao.codigo      = ttArquivo.divisao
               ttDivisao.descricao   = ttArquivo.descricao.
    END.

    IF  ttArquivo.secao <> '' AND ttArquivo.divisao <> ''
        AND ttArquivo.grupo <> '' AND ttArquivo.classe = ''
        AND ttArquivo.subclasse = '' THEN DO:
        CREATE ttGrupo.
        ASSIGN ttGrupo.codigo      = ttArquivo.grupo
               ttGrupo.descricao   = ttArquivo.descricao.
    END.

    IF  ttArquivo.secao <> '' AND ttArquivo.divisao <> ''
        AND ttArquivo.grupo <> '' AND ttArquivo.classe <> ''
        AND ttArquivo.subclasse = '' THEN DO:
        CREATE ttClasse.
        ASSIGN ttCLasse.codigo      = ttArquivo.classe
               ttClasse.descricao   = ttArquivo.descricao.
    END.
    
    IF  ttArquivo.secao <> '' AND ttArquivo.divisao <> ''
        AND ttArquivo.grupo <> '' AND ttArquivo.classe <> ''
        AND ttArquivo.subclasse <> '' THEN DO:
        CREATE ttSubclasse.
        ASSIGN ttSubclasse.codigo      = ttArquivo.subclasse
               ttSubclasse.descricao   = ttArquivo.descricao.
    END.

END.

/* OUTPUT TO c:\temp\cnaes.txt.         */
/* FOR EACH ttSubclasse:                */
/*                                      */
/*     DISP ttSubclasse WITH WIDTH 600. */
/* END.                                 */
/*                                      */

FOR EACH ttSecao:
    CREATE secoes.
    ASSIGN secoes.cod_secao  = ttSecao.codigo
           secoes.descricao = ttSecao.descricao.
    /*DISP ttSecao WITH WIDTH 550.*/
END.
FOR EACH ttDivisao:
    CREATE divisoes.
    ASSIGN divisoes.cod_divisao  = ttDivisao.codigo
           divisoes.descricao = ttDivisao.descricao.
    DISP ttDivisao WITH WIDTH 600.
END.
FOR EACH ttGrupo:
    CREATE grupos.
    ASSIGN grupos.cod_grupo  = ttGrupo.codigo
           grupos.descricao = ttGrupo.descricao.
    /*DISP ttGrupo WITH WIDTH 600.*/
END.
FOR EACH ttCLasse:
    CREATE classes.
    ASSIGN classes.cod_classe  = ttClasse.codigo
           classes.descricao = ttClasse.descricao.
    /*DISP ttClasse WITH WIDTH 600.*/
END.

FOR EACH ttSubclasse:
    CREATE cnaes.
/*     ASSIGN cCnae  = ttSubclasse.codigo                 */
/*            cCnae  = REPLACE(ttSubclasse.codigo,"-","") */
/*            cCnae  = REPLACE(cCnae,"/","")              */
    ASSIGN 
           cnaes.cod_cnae  = ttsubclasse.codigo
           cnaes.descricao = ttSubclasse.descricao.
    /*DISP ttSubclasse WITH WIDTH 600.*/
END.

