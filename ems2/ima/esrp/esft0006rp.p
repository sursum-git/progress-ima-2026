DEFINE TEMP-TABLE tt-work  NO-UNDO 
       FIELD cod-estabel   LIKE movadm.nota-fiscal.cod-estabel
       FIELD base          AS INT
       FIELD cod-rep       LIKE movadm.nota-fiscal.cod-rep
       FIELD vlr-fat       AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD devolucao     AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD liquidez      AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD fat-liq       AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD comissao      AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD comis-liq     AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD desconto      AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD desc-base-ir  AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD i-renda       AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD adiantamento  AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD emprestimo    AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD liquido       AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD vlr-nf        AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD marcar        AS CHAR.

DEFINE TEMP-TABLE tt-nfs   LIKE movadm.nota-fiscal
       FIELD qt-faturada   AS   DECIMAL
       FIELD comissao      AS   DECIMAL
       FIELD base          AS   INT
       INDEX indice1 IS PRIMARY cod-rep cod-estabel serie nr-nota-fis.

DEFINE TEMP-TABLE tt-calc-repres 
       FIELD cod-rep LIKE repres.cod-rep
       FIELD cod-pai LIKE repres.cod-rep.

DEFINE TEMP-TABLE tt-excessao
       FIELD perc-comis   LIKE repres.comis-direta
       FIELD vlr-fat      AS DECIMAL
       FIELD vlr-desconto AS DECIMAL.

DEFINE TEMP-TABLE tt-digita
       FIELD opcao AS CHAR
       FIELD campo AS CHAR
       FIELD valor AS CHAR.
    
DEF TEMP-TABLE tt-aux
    FIELD cod-rep       LIKE movadm.nota-fiscal.cod-rep
    FIELD uf            LIKE repres.estado
    FIELD vlr-fat       AS DECIMAL FORMAT "->>>,>>>,>>9.99" EXTENT 12.

DEF VAR c-meses AS CHAR INIT "Janeiro,Fevereiro,Mar‡o,Abril,Maio,Junho,Julho,Agosto,Setembro,Outubro,Novembro,Dezembro".
DEF VAR i-mes AS INT.
DEF VAR i-pos AS INT.

/* Variaveis para o Excel */
DEF VAR chExcelApp  AS COM-HANDLE NO-UNDO.
DEF VAR chWorkbook  AS COM-HANDLE NO-UNDO.
DEF VAR chworksheet AS COM-HANDLE NO-UNDO.
DEF VAR c-lin       AS CHAR FORMAT "x(500)".
DEF VAR arq-saida   AS CHAR FORMAT "x(50)".

FOR EACH cm-ext-repres WHERE
         cm-ext-repres.classe <= 2 NO-LOCK.

    FIND FIRST representante WHERE 
               representante.cdn_repr = cm-ext-repres.cod-rep AND
               representante.ind_sit_repres = 'Ativo' NO-LOCK NO-ERROR.

    IF NOT AVAIL representante THEN NEXT.


    FOR EACH cm-hierarquia WHERE
             cm-hierarquia.cod-rep = representante.cdn_rep NO-LOCK.

        CREATE tt-calc-repres.
        ASSIGN tt-calc-repres.cod-pai = cm-hierarquia.cod-depend
               tt-calc-repres.cod-rep = cm-hierarquia.cod-depend.
    END.
END.

DEF VAR da-dt-ini AS DATE.

DEF VAR c-dia AS CHAR.
DEF VAR da-dt-proc-ini AS DATE FORMAT "99/99/9999".
DEF VAR da-dt-proc-fin AS DATE FORMAT "99/99/9999".

DEF VAR c-periodo AS CHAR.
DEF VAR c-cabec AS CHAR FORMAT "x(50)".

RUN esapi/connect-ima-med.p.


ASSIGN da-dt-ini = ADD-INTERVAL(TODAY,-11,"months").
REPEAT WHILE da-dt-ini <= TODAY.
    ASSIGN c-periodo = STRING(MONTH(da-dt-ini),"99") + STRING(YEAR(da-dt-ini),"9999").

    ASSIGN c-cabec = IF c-cabec = ''
                     THEN c-periodo 
                     ELSE c-cabec + ";" + c-periodo.

    ASSIGN da-dt-ini = ADD-INTERVAL(da-dt-ini,1,"months").
END.

DO i-pos = 1 TO 12.

    ASSIGN c-periodo = ENTRY(i-pos,c-cabec,";").

    IF i-pos <= 11 THEN DO.
       RUN esapi/ret-udm.p (INPUT c-periodo, OUTPUT c-dia).
       ASSIGN da-dt-proc-ini = DATE("01"  + c-periodo)
              da-dt-proc-fin = DATE(c-dia + c-periodo).
    END.
    ELSE
        ASSIGN da-dt-proc-ini = ADD-INTERVAL(TODAY, - DAY(TODAY) + 1,"days")
               da-dt-proc-fin = TODAY.
    
    EMPTY TEMP-TABLE tt-work.
    RUN esapi/escm002a.p (INPUT-OUTPUT TABLE tt-work,
                          INPUT-OUTPUT TABLE tt-calc-repres,
                          INPUT-OUTPUT TABLE tt-nfs,
                          INPUT-OUTPUT TABLE tt-digita,
                          INPUT "",
                          INPUT "ZZZZ",
                          INPUT da-dt-proc-ini,
                          INPUT da-dt-proc-fin,
                          INPUT "",
                          INPUT "ZZZZZZZZZZZZ",
                          INPUT "",
                          INPUT "ZZZZZZZZZZZZZZZZ", 
                          INPUT 0,
                          INPUT 99999,
                          INPUT "",
                          INPUT "ZZZZZZZZZZZZZZZZ").

    FOR EACH tt-work BREAK BY tt-work.cod-rep.
        ACCUMULATE tt-work.vlr-fat (TOTAL BY tt-work.cod-rep).

        IF FIRST-OF(tt-work.cod-rep) THEN DO.
           FIND repres WHERE
                repres.cod-rep = tt-work.cod-rep NO-LOCK NO-ERROR.

           CREATE tt-aux.
           ASSIGN tt-aux.cod-rep = tt-work.cod-rep 
                  tt-aux.uf = repres.estado
                  tt-aux.vlr-fat[i-pos] = ACCUM TOTAL BY tt-work.cod-rep tt-work.vlr-fat.
        END.
    END.
END.

DISCONNECT dbaux.


FOR EACH tt-aux.
    DISP tt-aux.
END.
