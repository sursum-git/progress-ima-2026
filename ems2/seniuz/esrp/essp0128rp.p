/* Programa: ESSP0128.P
** Objetivo: imprimir Etiquetas codigo de Barras
**           As funá‰es deste programa foram escritas utilizando linguagem
**           de impressora PCL-5 e est∆o na include sz-pcl.i.
** Autor...: SENIUZ - Antonio G. Souza (Set 2006)
*/

/* include de controle de vers∆o */
{include/i-prgvrs.i ESSP0128RP 2.04.00.000}

DEFINE TEMP-TABLE tt-param NO-UNDO
    FIELD destino          AS INTEGER
    FIELD arquivo          AS CHAR FORMAT "x(35)"
    FIELD usuario          AS CHAR FORMAT "x(12)"
    FIELD data-exec        AS DATE
    FIELD hora-exec        AS INTEGER
    FIELD nr-nota-fis      LIKE mp-entr-mat.nro-docto
    FIELD cod-emit         LIKE mp-entr-mat.cod-emit
    FIELD padrao-ini       LIKE mp-fardo.padrao
    FIELD padrao-fin       LIKE mp-fardo.padrao
    FIELD fardo-ini        LIKE mp-fardo.nr-fardo
    FIELD fardo-fin        LIKE mp-fardo.nr-fardo
    FIELD tp-etiqueta      AS   INT.

DEFINE TEMP-TABLE tt-raw-digita
       FIELD raw-digita AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

/* Definiá∆o de Temp-Tables */
DEFINE TEMP-TABLE tt-fardo LIKE mp-fardo.

/* definiá∆o de vari†veis  */
DEF VAR h-acomp AS HANDLE NO-UNDO.
DEF VAR i-ct-lin AS INT.
DEF VAR i-ct-col AS INT.
DEF VAR c-classif AS CHAR.

/* include padr∆o para vari†veis de relat¢rio  */
{include/i-rpvar.i}

/* include padr∆o para output de relat¢rios */
{include/i-rpout.i}

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Imprimindo_Etiquetas *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

/* include das funá‰es em PCL */
{esinc/sz-pcl.i}

IF tt-param.nr-nota-fis <> 0 AND 
   tt-param.cod-emit <> 0 THEN DO.
   FOR EACH mp-entr-mat WHERE
            mp-entr-mat.cod-emit = tt-param.cod-emit AND
            mp-entr-mat.nro-docto = tt-param.nr-nota-fis NO-LOCK,
       EACH mp-fardo OF mp-entr-mat WHERE
            mp-fardo.nr-fardo >= tt-param.fardo-ini AND
            mp-fardo.nr-fardo <= tt-param.fardo-fin AND
            mp-fardo.padrao   >= tt-param.padrao-ini AND
            mp-fardo.padrao   <= tt-param.padrao-fin NO-LOCK.

       CREATE tt-fardo.
       BUFFER-COPY mp-fardo TO tt-fardo.
   END.
END.
ELSE DO.
   FOR EACH mp-fardo WHERE
            mp-fardo.nr-fardo >= tt-param.fardo-ini AND
            mp-fardo.nr-fardo <= tt-param.fardo-fin AND
            mp-fardo.padrao   >= tt-param.padrao-ini AND
            mp-fardo.padrao   <= tt-param.padrao-fin NO-LOCK.

       CREATE tt-fardo.
       BUFFER-COPY mp-fardo TO tt-fardo.
   END.
END.

/* Altera a Tipo de Impress∆o para A4 e Retira Margens */
PUT UNFORMATTED 
    "~033&l26A"
    "~033&l1E".

IF tt-param.tp-etiqueta = 1 THEN
   RUN pi-etiqueta-final.
ELSE
   RUN pi-etiqueta-prov.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i &STREAM="stream str-rp"}
RUN pi-finalizar IN h-acomp.
RETURN "OK":U.


/* Procedure para processar etiqueta final (apos analises) */
PROCEDURE pi-etiqueta-final.
    DEF VAR i-lin AS INT EXTENT 6 INIT [0,870,1770,2640].

    ASSIGN i-ct-lin = 0.
    FOR EACH tt-fardo NO-LOCK.

        ASSIGN i-ct-lin = i-ct-lin + 1.
        IF i-ct-lin > 4 THEN DO.
           PAGE.
           ASSIGN i-ct-lin = 1.
        END.

        FIND mp-entr-mat OF tt-fardo NO-LOCK NO-ERROR.
        FIND emitente WHERE
             emitente.cod-emit = mp-entr-mat.cod-emit 
             NO-LOCK NO-ERROR.

        FIND mp-tipo where
             mp-tipo.codigo = tt-fardo.cd-tipo NO-LOCK NO-ERROR.

        FIND mp-coloracao WHERE
             mp-coloracao.codigo = tt-fardo.cd-coloracao NO-LOCK NO-ERROR.

        ASSIGN c-classif = tt-fardo.letra + STRING(tt-fardo.cd-coloracao) + 
                           STRING(tt-fardo.cd-tipo).

        RUN pi-imp-etq-final (INPUT i-lin[i-ct-lin], 0).
    END.
END PROCEDURE.


/* Procedure para processar etiquetas provis¢rias */
PROCEDURE pi-etiqueta-prov.
    DEF VAR i-lin AS INT EXTENT 6 INIT [0,600,1200,1800,2400,2970.17].
    DEF VAR i-col AS INT EXTENT 3 INIT [0,820,1640].

    ASSIGN i-ct-lin = 1
           i-ct-col = 0.
    FOR EACH tt-fardo NO-LOCK.
        ASSIGN i-ct-col = i-ct-col + 1.

        IF i-ct-col > 3 THEN DO.
           ASSIGN i-ct-col = 1
                  i-ct-lin = i-ct-lin + 1.

           IF i-ct-lin > 3 THEN DO.
               ASSIGN i-ct-lin = 1.
               PAGE.
           END.
        END.

        FIND mp-entr-mat OF tt-fardo NO-LOCK NO-ERROR.

        FIND emitente WHERE
             emitente.cod-emit = mp-entr-mat.cod-emit NO-LOCK NO-ERROR.

        RUN pi-imp-etq-prov (INPUT i-lin[i-ct-lin * 2 - 1], INPUT i-col[i-ct-col]). 
        RUN pi-imp-etq-prov (INPUT i-lin[i-ct-lin * 2], INPUT i-col[i-ct-col]).

        FIND mp-fardo WHERE
             mp-fardo.nr-fardo = tt-fardo.nr-fardo NO-ERROR.
        ASSIGN mp-fardo.situacao = IF mp-fardo.situacao = 1 
                                   THEN 2 ELSE mp-fardo.situacao.
    END.
END PROCEDURE.


/* Procedure para Imprimir Etiquetas Definitvas (apos analises) */
PROCEDURE pi-imp-etq-final.
    DEF INPUT PARAMETER i-lin AS INT.
    DEF INPUT PARAMETER i-col AS INT.

    PUT UNFORMATTED 
        fn-retangulo(INPUT i-col, INPUT i-lin + 40, INPUT i-col + 1800, INPUT i-lin + 690, INPUT 6)
        fn-retangulo(INPUT i-col + 1900, INPUT i-lin + 40, INPUT i-col + 2330, INPUT i-lin + 690, INPUT 6). 

    PUT UNFORMATTED 
        fn-texto(INPUT i-col + 30, INPUT i-lin + 70, INPUT "CODIFICAÄ«O:",INPUT 16602, INPUT 5, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 770, INPUT i-lin + 70, INPUT "PADR«O:",INPUT 16602, INPUT 5, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 790, INPUT i-lin + 160, INPUT STRING(tt-fardo.padrao),INPUT 16602, INPUT 25, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-linha(INPUT i-col + 750, INPUT i-lin + 40, INPUT 650, INPUT 4, INPUT "V")
        fn-linha(INPUT i-col + 750, input i-lin + 190, INPUT 1050, INPUT 4, INPUT "H")
        fn-linha(INPUT i-col + 1900, input i-lin + 190, INPUT 430, INPUT 4, INPUT "H")

        fn-texto(INPUT i-col + 50, INPUT i-lin + 420, INPUT c-classif, INPUT 16602, INPUT 90, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 790, INPUT i-lin + 420, INPUT STRING(tt-fardo.nr-fardo,"99999999"), INPUT 16602, INPUT 50, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      
        fn-texto(INPUT i-col + 1930, INPUT i-lin + 110, INPUT c-classif, INPUT 16602, INPUT 18, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 2075, INPUT i-lin + 110, INPUT STRING(tt-fardo.nr-fardo,"99999999"),INPUT 16602, INPUT 12, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-linha(input i-col, INPUT i-lin + 460, INPUT 750, INPUT 4, INPUT "H") 
        fn-texto(INPUT i-col + 1910, INPUT i-lin + 225, INPUT "PADR«O:",INPUT 16602, INPUT 5, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1930, INPUT i-lin + 275, INPUT tt-fardo.padrao,INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1910, INPUT i-lin + 325, INPUT "PROCED“NCIA:",INPUT 16602, INPUT 5, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1930, INPUT i-lin + 375, INPUT mp-entr-mat.procedencia,INPUT 16602, INPUT 10, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1910, INPUT i-lin + 425, INPUT "FORNECEDOR:",INPUT 16602, INPUT 5, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1930, INPUT i-lin + 475, INPUT STRING(mp-entr-mat.cod-emit) + " - " + emitente.nome-abrev,INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")      
        fn-texto(INPUT i-col + 1910, INPUT i-lin + 525, INPUT "NOTA FISCAL:",INPUT 16602, INPUT 5, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1930, INPUT i-lin + 575, INPUT STRING(mp-entr-mat.nro-docto),INPUT 16602, INPUT 10, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1910, INPUT i-lin + 625, INPUT "DATA RECEBIMENTO:",INPUT 16602, INPUT 5, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1930, INPUT i-lin + 675, INPUT STRING(mp-entr-mat.dt-recebimento,"99/99/9999"),INPUT 16602, INPUT 10, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      
        fn-texto(INPUT i-col + 30, INPUT i-lin + 490, INPUT "TONALIDADE:",INPUT 16602, INPUT 5, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 70, INPUT i-lin + 565, INPUT mp-coloracao.tonalidade,INPUT 16602, INPUT 20, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 30, INPUT i-lin + 600, INPUT "TIPO:",INPUT 16602, INPUT 5, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 70, INPUT i-lin + 670, INPUT mp-tipo.tipo,INPUT 16602, INPUT 20, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
  
    PUT UNFORMATTED
        fn-code25 (INPUT i-col + 950, INPUT i-lin + 500,
                   INPUT STRING(tt-fardo.nr-fardo,"99999999"),
                   INPUT "H",
                   INPUT 3.0,
                   INPUT 8.0).

    PUT UNFORMATTED
        fn-code25 (INPUT i-col + 2010, INPUT i-lin + 125,
                   INPUT STRING(tt-fardo.nr-fardo,"99999999"),
                   INPUT "H",
                   INPUT 1.0,
                   INPUT 3.0).
     
END PROCEDURE.


/* Procedure para Imprimir Etiquetas Provis¢rias */
PROCEDURE pi-imp-etq-prov.
    DEF INPUT PARAMETER i-lin AS INT.
    DEF INPUT PARAMETER i-col AS INT.

    PUT UNFORMATTED 
        fn-retangulo(INPUT i-col, INPUT i-lin, INPUT i-col + 690, INPUT i-lin + 450, INPUT 4)
        fn-linha(INPUT i-col + 340, INPUT i-lin, INPUT 135, INPUT 4, INPUT "V")
        fn-linha(INPUT i-col + 510, INPUT i-lin, INPUT 135, INPUT 4, INPUT "V")
        fn-linha(INPUT i-col, INPUT i-lin + 135, INPUT 690, INPUT 4, INPUT "H").

    PUT UNFORMATTED
        fn-code25 (INPUT i-col + 50, input i-lin + 70,
                   INPUT STRING(tt-fardo.nr-fardo,"99999999"),
                   INPUT "H",
                   INPUT 1.0,
                   INPUT 3.0).

    PUT UNFORMATTED 
        fn-texto(INPUT i-col + 30, INPUT i-lin + 60, INPUT STRING(tt-fardo.nr-fardo,"99999999"),INPUT 16602, INPUT 15, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 400, INPUT i-lin + 30, INPUT "Tipo",INPUT 16602, INPUT 5, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 545, INPUT i-lin + 30, INPUT "Coloraá∆o",INPUT 16602, INPUT 5, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 10, INPUT i-lin + 160, INPUT "PADR«O",INPUT 16602, INPUT 5, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 30, INPUT i-lin + 200, INPUT tt-fardo.padrao,INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 10, INPUT i-lin + 240, INPUT "PROCED“NCIA",INPUT 16602, INPUT 5, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 30, INPUT i-lin + 280, INPUT mp-entr-mat.procedencia,INPUT 16602, INPUT 10, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 10, INPUT i-lin + 320, INPUT "FORNECEDOR",INPUT 16602, INPUT 5, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 30, INPUT i-lin + 360, INPUT STRING(mp-entr-mat.cod-emit) + " - " + emitente.nome-abrev,INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")      
        fn-texto(INPUT i-col + 10, INPUT i-lin + 400, INPUT "NOTA FISCAL",INPUT 16602, INPUT 5, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 30, INPUT i-lin + 440, INPUT STRING(mp-entr-mat.nro-docto),INPUT 16602, INPUT 10, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 410, INPUT i-lin + 400, INPUT "DATA RECEBIMENTO",INPUT 16602, INPUT 5, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 430, INPUT i-lin + 440, INPUT STRING(mp-entr-mat.dt-recebimento,"99/99/9999"),INPUT 16602, INPUT 10, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
  
END PROCEDURE.
