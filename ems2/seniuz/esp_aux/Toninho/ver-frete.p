DEF TEMP-TABLE tt-work      NO-UNDO
    FIELD visualiza         AS LOG
    FIELD seq-item          AS INT
    FIELD seq-repres        AS INT
    FIELD seq-grupo         AS INT
    FIELD seq-cliente       AS INT
    FIELD seq-regiao        AS INT
    FIELD seq-uf            AS INT
    FIELD seq-nat-oper      AS INT
    FIELD seq-cond-pg       AS INT
    FIELD it-codigo         LIKE ped-item.it-codigo
    FIELD desc-item         LIKE ITEM.desc-item
    FIELD no-ab-reppri      LIKE ped-venda.no-ab-reppri
    FIELD cod-rep           LIKE repres.cod-rep
    FIELD matriz            LIKE it-nota-fisc.aliquota-icm
    FIELD nome-abrev        LIKE ped-venda.nome-abrev
    FIELD cod-emit          LIKE emitente.cod-emit
    FIELD regiao            AS CHAR FORMAT "x(20)"
    FIELD nat-operacao      LIKE natur-oper.nat-operacao
    FIELD aliq-icms         LIKE natur-oper.aliquota-icm
    FIELD vl-icms           LIKE it-nota-fisc.vl-icms-it
    FIELD desc-pratic       AS DEC
    FIELD cond-pagto        AS CHAR
    FIELD uf                AS CHAR
    FIELD lote              AS CHAR
    FIELD Und               AS CHAR
    FIELD qtd               AS DEC
    FIELD qtd-devol         AS DEC
    FIELD vlr               AS DEC
    FIELD vlr-devol         AS DEC
    FIELD vlr-custo         AS DEC
    FIELD preco-medio       AS DEC
    FIELD prazo-medio       AS DEC
    FIELD rentabilidade     AS DEC
    FIELD perc-sobr-total   AS DEC
    INDEX indice1 it-codigo    und lote 
    INDEX indice2 no-ab-reppri und lote 
    INDEX indice3 matriz       und lote 
    INDEX indice4 nome-abrev   und lote 
    INDEX indice5 regiao       und lote uf
    INDEX indice6 nat-operacao und lote 
    INDEX indice7 cond-pagto   und lote.

DEF TEMP-TABLE tt-frete
    FIELD periodo AS CHAR                             LABEL "Periodo"
    FIELD vlr-fat AS DEC FORMAT ">>>,>>>,>>9.99"      LABEL "Vlr Faturado"
    FIELD vlr-frete AS DEC  FORMAT ">>>,>>>,>>9.99"   LABEL "Vlr Frete".

DEF VAR h-essp0190 AS HANDLE.
DEF VAR da-data-ini AS DATE.
DEF VAR da-data-fin AS DATE.
DEF VAR da-data-i  AS DATE.
DEF VAR da-data-f  AS DATE.
DEF VAR da-data    AS DATE.
DEF VAR i-ct       AS INT.
DEF VAR c-mes      AS INT.
DEF VAR c-periodo  AS CHAR.   
DEF VAR c-natur    LIKE docum-est.nat-oper.

FIND FIRST para-ped NO-LOCK NO-ERROR.

ASSIGN da-data-ini = 01.01.2015
       da-data-fin = 09.30.2015.

FOR EACH docum-est WHERE
         docum-est.dt-emis >= da-data-ini AND
         docum-est.dt-emis <= da-data-fin AND
         (docum-est.nat-oper BEGINS "163" OR
          docum-est.nat-oper BEGINS "263") NO-LOCK.

    ASSIGN c-periodo = STRING(MONTH(docum-est.dt-emis),"99") + STRING(YEAR(docum-est.dt-emis),"9999").

    FIND tt-frete WHERE
         tt-frete.periodo = c-periodo NO-ERROR.
    IF NOT AVAIL tt-frete THEN DO.
       CREATE tt-frete.
       ASSIGN tt-frete.periodo = c-periodo.
    END.
    ASSIGN tt-frete.vlr-frete = tt-frete.vlr-frete + docum-est.tot-valor.
END.


ASSIGN c-mes = MONTH(da-data-ini).
DO da-data = da-data-ini TO da-data-fin.

   IF MONTH(da-data) <> c-mes OR da-data = da-data-fin THEN DO.

      ASSIGN da-data-i = DATE("01" + STRING(MONTH(da-data - 1),"99") + STRING(YEAR(da-data - 1),"9999")).
      IF MONTH(da-data - 1) = MONTH(da-data-ini) THEN
         ASSIGN da-data-i = da-data-ini.

      ASSIGN da-data-f = da-data.
      IF MONTH(da-data - 1) <> MONTH(da-data) THEN
         ASSIGN da-data-f = da-data - 1.

      ASSIGN c-periodo = STRING(MONTH(da-data-i),"99") + STRING(YEAR(da-data-i),"9999").

      RUN pi-busca-190 (INPUT para-ped.estab-padrao,
                        INPUT da-data-i,
                        INPUT da-data-f).
   END.
   ASSIGN c-mes = MONTH(da-data).
END.

OUTPUT TO c:\temp\frete.txt.
    FOR EACH tt-frete.
        DISP tt-frete.periodo
             tt-frete.vlr-fat
             tt-frete.vlr-frete.
    END.
OUTPUT CLOSE.


/*---------------- P R O C E D U R E S ----------------*/

PROCEDURE pi-busca-190.
    DEF INPUT PARAMETER p-cod-estab AS CHAR.
    DEF INPUT PARAMETER p-dt-ini AS DATE.
    DEF INPUT PARAMETER p-dt-fin AS DATE.

    RUN esp/essp0190.p PERSISTENT SET h-essp0190.

    EMPTY TEMP-TABLE tt-work.
    RUN pi-retorna-dados IN h-essp0190 (INPUT 2,              /* Tipo 1-A Faturar 2-Faturados 3-Vendidos*/
                                        INPUT p-cod-estab,    /* Estabelecimento */
                                        INPUT TODAY,          /* Data a Faturar */ 
                                        INPUT p-dt-ini,       /* Data Faturados INI */
                                        INPUT p-dt-fin,       /* Data Faturados FIN */
                                        INPUT TODAY,          /* Data Vendido INI */   
                                        INPUT TODAY,          /* Data Vendido FIN */
                                        INPUT "",             /* Item INI */
                                        INPUT "ZZZZZZZZZZZZ", /* Item FIN */
                                        INPUT "",             /* Referencia INI */
                                        INPUT "ZZZZZZZZ",     /* Referencia FIN */
                                        INPUT "",             /* Cliente INI */
                                        INPUT "ZZZZZZZZZZZZ", /* Cliente FIN */
                                        INPUT "",             /* Representante INI */
                                        INPUT "ZZZZZZZZZZZZ", /* Representante FIN */
                                        OUTPUT TABLE tt-work).

    DELETE OBJECT h-essp0190.

    FOR EACH tt-work.
        FIND emitente WHERE
             emitente.nome-abrev = tt-work.nome-abrev NO-LOCK NO-ERROR.
        IF NOT AVAIL emitente THEN NEXT.

        FIND tt-frete WHERE
             tt-frete.periodo = c-periodo NO-ERROR.
        IF NOT AVAIL tt-frete THEN DO.
           CREATE tt-frete.
           ASSIGN tt-frete.periodo = c-periodo.
        END.
        ASSIGN tt-frete.vlr-fat = tt-frete.vlr-fat + tt-work.vlr - tt-work.vlr-devol.
    END.
END PROCEDURE.
