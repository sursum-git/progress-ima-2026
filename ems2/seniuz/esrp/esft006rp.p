
/* include de controle de versÆo */
{include/i-prgvrs.i ESFT006RP 1.00.00.000}
/*{PDFinclude/pdf_inc.i "THIS-PROCEDURE"}*/

/* defini‡Æo das temp-tables para recebimento de parƒmetros */
define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG
    FIELD d-data-ini       AS DATE
    FIELD d-data-fin       AS DATE
    FIELD c-cliente-ini    AS CHAR
    FIELD c-cliente-fin    AS CHAR
    FIELD c-estado-ini     AS CHAR
    FIELD c-estado-fin     AS CHAR
    FIELD l-excel          AS LOG.
    
DEF TEMP-TABLE tt-raw-digita
   	FIELD raw-digita	AS RAW.

DEF TEMP-TABLE tt-cliente 
    FIELD cliente AS CHAR
    FIELD un-fatur AS CHAR
    FIELD cod-cli  AS INT
    FIELD repres   AS CHAR
    FIELD qtd-tot  AS DEC EXTENT 12
    FIELD vlr-tot  AS DEC EXTENT 12
    FIELD estado   AS CHAR
    INDEX idx-cliente cliente un-fatur estado.

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

/* recebimento de parƒmetros */
DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
DEF VAR h-essp0190 AS HANDLE.
DEF VAR h-acomp    AS HANDLE NO-UNDO.
DEF VAR i-lin      AS INT INITIAL 99.
DEF VAR i-pag      AS INT INITIAL  1.
DEF VAR c-estabel  AS CHAR.
DEF VAR cFileName  AS CHAR.
DEF VAR da-data    AS DATE.
DEF VAR da-data-i  AS DATE.
DEF VAR da-data-f  AS DATE.
DEF VAR i-ct       AS INT.
DEF VAR c-mes      AS INT.


/* defini‡Æo de frames do relat¢rio */

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i &STREAM="stream str-rp"}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i &STREAM="str-rp"}

/* bloco principal do programa */
ASSIGN c-programa 	= "espp012rp"
       c-versao 	= "2.06"
       c-revisao	= "00.001"
       c-empresa 	= "IMA"
       c-sistema	= ""
       c-titulo-relat = "Relat¢rio de Vendas por Cliente".

VIEW STREAM str-rp FRAME f-cabec.
VIEW STREAM str-rp FRAME f-rodape.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Imprimindo *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

FIND FIRST para-ped NO-LOCK NO-ERROR.

/*RUN pi-processa.*/

IF MONTH(tt-param.d-data-ini) = MONTH(tt-param.d-data-fin) THEN DO.
   ASSIGN da-data-i = tt-param.d-data-ini
          da-data-f = tt-param.d-data-fin.

   RUN pi-busca-190 (INPUT para-ped.estab-padrao,
                     INPUT d-data-i,
                     INPUT d-data-f).
END.
ELSE DO. /* Processa Mˆs a Mˆs */
   ASSIGN c-mes = MONTH(tt-param.d-data-ini).
   DO da-data = tt-param.d-data-ini TO tt-param.d-data-fin.
    
      IF MONTH(da-data) <> c-mes OR 
         da-data = tt-param.d-data-fin THEN DO.
    
         ASSIGN da-data-i = DATE("01" + STRING(MONTH(da-data - 1),"99") + STRING(YEAR(da-data - 1),"9999")).
         IF MONTH(da-data - 1) = MONTH(tt-param.d-data-ini) THEN
            ASSIGN da-data-i = tt-param.d-data-ini.
    
         ASSIGN da-data-f = da-data.
         IF MONTH(da-data - 1) <> MONTH(da-data) THEN
            ASSIGN da-data-f = da-data - 1.

         RUN pi-busca-190 (INPUT para-ped.estab-padrao,
                           INPUT da-data-i,
                           INPUT da-data-f).
      END.
      ASSIGN c-mes = MONTH(da-data).
   END.
END.
RUN pi-gera-excel.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i &STREAM="stream str-rp"}
RUN pi-finalizar in h-acomp.
RETURN "OK":U.


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

        FIND repres WHERE
             repres.cod-rep = emitente.cod-rep NO-LOCK NO-ERROR.

        FIND tt-cliente WHERE
             tt-cliente.cliente = tt-work.nome-abrev AND
             tt-cliente.un-fatur = tt-work.und
             NO-ERROR.

        IF NOT AVAIL tt-cliente THEN DO.
           CREATE tt-cliente.
           ASSIGN tt-cliente.cliente = tt-work.nome-abrev
                  tt-cliente.un-fatur = tt-work.und
                  tt-cliente.estado = emitente.estado
                  tt-cliente.repres = IF AVAIL repres 
                                      THEN repres.nome-abrev ELSE ''
                  tt-cliente.cod-cli = tt-work.cod-emit.
        END.
        ASSIGN tt-cliente.qtd-tot[MONTH(p-dt-ini)] = tt-cliente.qtd-tot[MONTH(p-dt-ini)] + tt-work.qtd - tt-work.qtd-devol
               tt-cliente.vlr-tot[MONTH(p-dt-ini)] = tt-cliente.vlr-tot[MONTH(p-dt-ini)] + tt-work.vlr - tt-work.vlr-devol.
    END.
END PROCEDURE.

/*  Gerando relat¢rio em Excel  */
PROCEDURE pi-gera-excel.

    DEF VAR chExcelApp  AS COM-HANDLE NO-UNDO.
    DEF VAR chWorkbook  AS COM-HANDLE NO-UNDO.
    DEF var chWorksheet AS COM-HANDLE NO-UNDO.
    
    DEF VAR i-lin          AS INT.
    DEF VAR i-ct           AS INT.
    
    CREATE "Excel.Application" chExcelApp.

    /* Cria a Planilha */
    ASSIGN chExcelApp:VISIBLE     = TRUE  /* A Planilha Ficar  Visivel  TRUE / FALSE */
           chWorkbook             = chExcelApp:Workbooks:ADD() /* Cria Planilha */
           chworksheet            = chExcelapp:sheets:ITEM(1).

    
    ASSIGN chWorkSheet:PageSetup:Orientation = 2.      /* Define Orienta‡Æo da P gina com Formato Paisagem */
    
    chWorkbook:Worksheets(1):Activate.                 /* Incluir Uma Nova Planilha */
    chWorksheet = chWorkbook:Worksheets(1).            /* Selecionar a Nova Planilha Criada */ 
    chWorkSheet:PageSetup:PrintTitleRows  = "$1:$2".   /* Adiciona o cabe‡alho em todas as paginas de Impressao */

    /* Altera formato da Celula */
    ASSIGN i-lin = 1.
    ASSIGN chWorksheet:range("1:" + STRING(i-lin)):FONT:ColorIndex = 2
           chWorksheet:range("1:" + STRING(i-lin)):FONT:NAME = "Courrier New"
           chWorksheet:range("1:" + STRING(i-lin)):FONT:bold = TRUE
           chWorksheet:range("1:" + STRING(i-lin)):FONT:SIZE = 14.
    
    chWorksheet:range("A" + STRING(i-lin) + ":" + "J" + STRING(i-lin)):Interior:ColorIndex = 25.
    chWorksheet:range("A1"):VALUE = cFileName.                   /* "Relat¢rio de Vendas por Cliente". */             /* Cabe‡alho T¡tulo */
    chWorkSheet:range("A1"):HorizontalAlignment = 3.             /* Centraliza */
    chWorksheet:range("A1:AC1"):MergeCells = 1.                  /* Mesclando C‚lulas no Cabe‡alho da Planilha Selecionada*/
    chWorkSheet:range("A1"):VerticalAlignment = 3.               /* Centraliza */

    ASSIGN i-lin = i-lin + 1.

    chWorksheet:range("F" + STRING(i-lin)):VALUE = "Jan".
    chWorksheet:range("F2:G2"):MergeCells = 1.         /* Mesclando C‚lulas */
    chWorkSheet:range("F2"):HorizontalAlignment = 3.   /* Centraliza */
    chWorkSheet:range("G2"):VerticalAlignment = 3.     /* Centraliza */
    chWorksheet:range("F2"):Interior:ColorIndex = 15.

    chWorksheet:range("H" + STRING(i-lin)):VALUE = "Fev".
    chWorksheet:range("H2:I2"):MergeCells = 1.       /* Mesclando C‚lulas */
    chWorkSheet:range("H2"):HorizontalAlignment = 3.   /* Centraliza */
    chWorkSheet:range("I2"):VerticalAlignment = 3.   /* Centraliza */
    chWorksheet:range("H2"):Interior:ColorIndex = 16.
    
    chWorksheet:range("J" + STRING(i-lin)):VALUE = "Mar".
    chWorksheet:range("J2:K2"):MergeCells = 1.       /* Mesclando C‚lulas */
    chWorkSheet:range("J2"):HorizontalAlignment = 3. /* Centraliza */
    chWorkSheet:range("K2"):VerticalAlignment = 3.   /* Centraliza */
    chWorksheet:range("J2"):Interior:ColorIndex = 15.
    
    chWorksheet:range("L" + STRING(i-lin)):VALUE = "Abr".
    chWorksheet:range("L2:M2"):MergeCells = 1.       /* Mesclando C‚lulas */
    chWorkSheet:range("L2"):HorizontalAlignment = 3. /* Centraliza */
    chWorkSheet:range("M2"):VerticalAlignment = 3.   /* Centraliza */
    chWorksheet:range("L2"):Interior:ColorIndex = 16.
    
    chWorksheet:range("N" + STRING(i-lin)):VALUE = "Mai".
    chWorksheet:range("N2:O2"):MergeCells = 1.       /* Mesclando C‚lulas */
    chWorkSheet:range("N2"):HorizontalAlignment = 3. /* Centraliza */
    chWorkSheet:range("O2"):VerticalAlignment = 3.   /* Centraliza */
    chWorksheet:range("N2"):Interior:ColorIndex = 15.
    
    chWorksheet:range("P" + STRING(i-lin)):VALUE = "Jun".
    chWorksheet:range("P2:Q2"):MergeCells = 1.       /* Mesclando C‚lulas */
    chWorkSheet:range("P2"):HorizontalAlignment = 3. /* Centraliza */
    chWorkSheet:range("Q2"):VerticalAlignment = 3.   /* Centraliza */
    chWorksheet:range("P2"):Interior:ColorIndex = 16.
    
    chWorksheet:range("R" + STRING(i-lin)):VALUE = "Jul".
    chWorksheet:range("R2:S2"):MergeCells = 1.       /* Mesclando C‚lulas */
    chWorkSheet:range("R2"):HorizontalAlignment = 3. /* Centraliza */
    chWorkSheet:range("S2"):VerticalAlignment = 3.   /* Centraliza */
    chWorksheet:range("R2"):Interior:ColorIndex = 15.
    
    chWorksheet:range("T" + STRING(i-lin)):VALUE = "Ago".
    chWorksheet:range("T2:U2"):MergeCells = 1.       /* Mesclando C‚lulas */
    chWorkSheet:range("T2"):HorizontalAlignment = 3. /* Centraliza */
    chWorkSheet:range("U2"):VerticalAlignment = 3.   /* Centraliza */
    chWorksheet:range("T2"):Interior:ColorIndex = 16.
    
    chWorksheet:range("V" + STRING(i-lin)):VALUE = "Set".
    chWorksheet:range("V2:W2"):MergeCells = 1.       /* Mesclando C‚lulas */
    chWorkSheet:range("V2"):HorizontalAlignment = 3. /* Centraliza */
    chWorkSheet:range("W2"):VerticalAlignment = 3.   /* Centraliza */
    chWorksheet:range("V2"):Interior:ColorIndex = 15.
    
    chWorksheet:range("X" + STRING(i-lin)):VALUE = "Out".
    chWorksheet:range("X2:Y2"):MergeCells = 1.       /* Mesclando C‚lulas */
    chWorkSheet:range("X2"):HorizontalAlignment = 3. /* Centraliza */
    chWorkSheet:range("Y2"):VerticalAlignment = 3.   /* Centraliza */
    chWorksheet:range("X2"):Interior:ColorIndex = 16.
    
    chWorksheet:range("Z" + STRING(i-lin)):VALUE = "Nov".
    chWorksheet:range("Z2:AA2"):MergeCells = 1.       /* Mesclando C‚lulas */
    chWorkSheet:range("Z2"):HorizontalAlignment = 3. /* Centraliza */
    chWorkSheet:range("AA2"):VerticalAlignment = 3.   /* Centraliza */
    chWorksheet:range("Z2"):Interior:ColorIndex = 15.
    
    chWorksheet:range("AB" + STRING(i-lin)):VALUE = "Dez".
    chWorksheet:range("AB2:AC2"):MergeCells = 1.       /* Mesclando C‚lulas */
    chWorkSheet:range("AB2"):HorizontalAlignment = 3. /* Centraliza */
    chWorkSheet:range("AC2"):VerticalAlignment = 3.   /* Centraliza */
    chWorksheet:range("AB2"):Interior:ColorIndex = 16.
    
    ASSIGN i-lin = i-lin + 1.

    chWorksheet:range("E" + STRING(i-lin) + ":I" + STRING(i-lin)):Interior:ColorIndex = 15.

    chWorksheet:range("A" + STRING(i-lin)):VALUE = "Cliente".
    chWorksheet:range("A" + STRING(i-lin)):Interior:ColorIndex = 16.
    chWorksheet:range("B" + STRING(i-lin)):VALUE = "Cod. Cliente".
    chWorksheet:range("B" + STRING(i-lin)):Interior:ColorIndex = 15.
    chWorksheet:range("C" + STRING(i-lin)):VALUE = "Representante".
    chWorksheet:range("C" + STRING(i-lin)):Interior:ColorIndex = 16.
    chWorksheet:range("D" + STRING(i-lin)):VALUE = "Estado".
    chWorksheet:range("D" + STRING(i-lin)):Interior:ColorIndex = 15.
    chWorksheet:range("E" + STRING(i-lin)):VALUE = "UND.".
    chWorksheet:range("E" + STRING(i-lin)):Interior:ColorIndex = 16. 
    chWorksheet:range("F" + STRING(i-lin)):VALUE = "Quantidade". /* JAN */
    chWorksheet:range("F" + STRING(i-lin)):Interior:ColorIndex = 15.
    chWorksheet:range("G" + STRING(i-lin)):VALUE = "Valor".      /* JAN */
    chWorksheet:range("G" + STRING(i-lin)):Interior:ColorIndex = 15. 
    chWorksheet:range("H" + STRING(i-lin)):VALUE = "Quantidade". /* FEV */
    chWorksheet:range("H" + STRING(i-lin)):Interior:ColorIndex = 16.
    chWorksheet:range("I" + STRING(i-lin)):VALUE = "Valor".      /* FEV */
    chWorksheet:range("I" + STRING(i-lin)):Interior:ColorIndex = 16.
    chWorksheet:range("J" + STRING(i-lin)):VALUE = "Quantidade". /* MAR */
    chWorksheet:range("J" + STRING(i-lin)):Interior:ColorIndex = 15.
    chWorksheet:range("K" + STRING(i-lin)):VALUE = "Valor".      /* MAR */
    chWorksheet:range("K" + STRING(i-lin)):Interior:ColorIndex = 15.
    chWorksheet:range("L" + STRING(i-lin)):VALUE = "Quantidade". /* ABR */
    chWorksheet:range("L" + STRING(i-lin)):Interior:ColorIndex = 16.
    chWorksheet:range("M" + STRING(i-lin)):VALUE = "Valor".      /* ABR */
    chWorksheet:range("M" + STRING(i-lin)):Interior:ColorIndex = 16.
    chWorksheet:range("N" + STRING(i-lin)):VALUE = "Quantidade". /* MAI */
    chWorksheet:range("N" + STRING(i-lin)):Interior:ColorIndex = 15.
    chWorksheet:range("O" + STRING(i-lin)):VALUE = "Valor".      /* MAI */
    chWorksheet:range("O" + STRING(i-lin)):Interior:ColorIndex = 15.
    chWorksheet:range("P" + STRING(i-lin)):VALUE = "Quantidade". /* JUN */
    chWorksheet:range("P" + STRING(i-lin)):Interior:ColorIndex = 16.
    chWorksheet:range("Q" + STRING(i-lin)):VALUE = "Valor".      /* JUN */
    chWorksheet:range("Q" + STRING(i-lin)):Interior:ColorIndex = 16.
    chWorksheet:range("R" + STRING(i-lin)):VALUE = "Quantidade". /* JUL */
    chWorksheet:range("R" + STRING(i-lin)):Interior:ColorIndex = 15.
    chWorksheet:range("S" + STRING(i-lin)):VALUE = "Valor".      /* JUL */
    chWorksheet:range("S" + STRING(i-lin)):Interior:ColorIndex = 15.
    chWorksheet:range("T" + STRING(i-lin)):VALUE = "Quantidade". /* AGO */
    chWorksheet:range("T" + STRING(i-lin)):Interior:ColorIndex = 16.
    chWorksheet:range("U" + STRING(i-lin)):VALUE = "Valor".      /* AGO */
    chWorksheet:range("U" + STRING(i-lin)):Interior:ColorIndex = 16.
    chWorksheet:range("V" + STRING(i-lin)):VALUE = "Quantidade". /* SET */
    chWorksheet:range("V" + STRING(i-lin)):Interior:ColorIndex = 15.
    chWorksheet:range("W" + STRING(i-lin)):VALUE = "Valor".      /* SET */
    chWorksheet:range("W" + STRING(i-lin)):Interior:ColorIndex = 15.
    chWorksheet:range("X" + STRING(i-lin)):VALUE = "Quantidade". /* OUT */
    chWorksheet:range("X" + STRING(i-lin)):Interior:ColorIndex = 16.
    chWorksheet:range("Y" + STRING(i-lin)):VALUE = "Valor".      /* OUT */
    chWorksheet:range("Y" + STRING(i-lin)):Interior:ColorIndex = 16.
    chWorksheet:range("Z" + STRING(i-lin)):VALUE = "Quantidade". /* NOV */
    chWorksheet:range("Z" + STRING(i-lin)):Interior:ColorIndex = 15.
    chWorksheet:range("AA" + STRING(i-lin)):VALUE = "Valor".      /* NOV */
    chWorksheet:range("AA" + STRING(i-lin)):Interior:ColorIndex = 15.
    chWorksheet:range("AB" + STRING(i-lin)):VALUE = "Quantidade". /* DEZ */
    chWorksheet:range("AB" + STRING(i-lin)):Interior:ColorIndex = 16.
    chWorksheet:range("AC" + STRING(i-lin)):VALUE = "Valor".     /* DEZ */
    chWorksheet:range("AC" + STRING(i-lin)):Interior:ColorIndex = 16.
    
    ASSIGN i-lin = i-lin + 1.
    
    FOR EACH tt-cliente NO-LOCK.
        chWorksheet:range("A" + STRING(i-lin)):VALUE = tt-cliente.cliente.
        chWorksheet:range("B" + STRING(i-lin)):VALUE = tt-cliente.cod-cli.
        chWorksheet:range("C" + STRING(i-lin)):VALUE = tt-cliente.repres.
        chWorksheet:range("D" + STRING(i-lin)):VALUE = tt-cliente.estado.
        chWorksheet:range("E" + STRING(i-lin)):VALUE = tt-cliente.un-fatur.

        chWorksheet:range("F" + STRING(i-lin)):VALUE = tt-cliente.qtd-tot[1].
        chWorksheet:range("G" + STRING(i-lin)):NumberFormat = "R$ #.###.##0,00".  /* Formato de MOEDA */
        chWorksheet:range("G" + STRING(i-lin)):VALUE = tt-cliente.vlr-tot[1].

        chWorksheet:range("H" + STRING(i-lin)):VALUE = tt-cliente.qtd-tot[2].
        chWorksheet:range("I" + STRING(i-lin)):NumberFormat = "R$ #.###.##0,00".  /* Formato de MOEDA */
        chWorksheet:range("I" + STRING(i-lin)):VALUE = tt-cliente.vlr-tot[2].

        chWorksheet:range("J" + STRING(i-lin)):VALUE = tt-cliente.qtd-tot[3].
        chWorksheet:range("K" + STRING(i-lin)):NumberFormat = "R$ #.###.##0,00".  /* Formato de MOEDA */
        chWorksheet:range("K" + STRING(i-lin)):VALUE = tt-cliente.vlr-tot[3].

        chWorksheet:range("L" + STRING(i-lin)):VALUE = tt-cliente.qtd-tot[4].
        chWorksheet:range("M" + STRING(i-lin)):NumberFormat = "R$ #.###.##0,00".  /* Formato de MOEDA */
        chWorksheet:range("M" + STRING(i-lin)):VALUE = tt-cliente.vlr-tot[4].

        chWorksheet:range("N" + STRING(i-lin)):VALUE = tt-cliente.qtd-tot[5].
        chWorksheet:range("O" + STRING(i-lin)):NumberFormat = "R$ #.###.##0,00".  /* Formato de MOEDA */
        chWorksheet:range("O" + STRING(i-lin)):VALUE = tt-cliente.vlr-tot[5].

        chWorksheet:range("P" + STRING(i-lin)):VALUE = tt-cliente.qtd-tot[6].
        chWorksheet:range("Q" + STRING(i-lin)):NumberFormat = "R$ #.###.##0,00".  /* Formato de MOEDA */
        chWorksheet:range("Q" + STRING(i-lin)):VALUE = tt-cliente.vlr-tot[6].

        chWorksheet:range("R" + STRING(i-lin)):VALUE = tt-cliente.qtd-tot[7].
        chWorksheet:range("S" + STRING(i-lin)):NumberFormat = "R$ #.###.##0,00".  /* Formato de MOEDA */
        chWorksheet:range("S" + STRING(i-lin)):VALUE = tt-cliente.vlr-tot[7].

        chWorksheet:range("T" + STRING(i-lin)):VALUE = tt-cliente.qtd-tot[8].
        chWorksheet:range("U" + STRING(i-lin)):NumberFormat = "R$ #.###.##0,00".  /* Formato de MOEDA */
        chWorksheet:range("U" + STRING(i-lin)):VALUE = tt-cliente.vlr-tot[8].

        chWorksheet:range("V" + STRING(i-lin)):VALUE = tt-cliente.qtd-tot[9].
        chWorksheet:range("W" + STRING(i-lin)):NumberFormat = "R$ #.###.##0,00".  /* Formato de MOEDA */
        chWorksheet:range("W" + STRING(i-lin)):VALUE = tt-cliente.vlr-tot[9].

        chWorksheet:range("X" + STRING(i-lin)):VALUE = tt-cliente.qtd-tot[10].
        chWorksheet:range("Y" + STRING(i-lin)):NumberFormat = "R$ #.###.##0,00".  /* Formato de MOEDA */
        chWorksheet:range("Y" + STRING(i-lin)):VALUE = tt-cliente.vlr-tot[10].

        chWorksheet:range("Z" + STRING(i-lin)):VALUE = tt-cliente.qtd-tot[11].
        chWorksheet:range("AA" + STRING(i-lin)):NumberFormat = "R$ #.###.##0,00".  /* Formato de MOEDA */
        chWorksheet:range("AA" + STRING(i-lin)):VALUE = tt-cliente.vlr-tot[11].

        chWorksheet:range("AB" + STRING(i-lin)):VALUE = tt-cliente.qtd-tot[12].
        chWorksheet:range("AC" + STRING(i-lin)):NumberFormat = "R$ #.###.##0,00".  /* Formato de MOEDA */
        chWorksheet:range("AC" + STRING(i-lin)):VALUE = tt-cliente.vlr-tot[12].


        CASE tt-cliente.un-fatur.
            WHEN "m" THEN 
                chWorksheet:range("A" + STRING(i-lin) + ":" + "AC" + STRING(i-lin)):Interior:ColorIndex = 2.
            WHEN "kg" THEN 
                chWorksheet:range("A" + STRING(i-lin) + ":" + "AC" + STRING(i-lin)):Interior:ColorIndex = 45.
            WHEN "un" THEN 
                chWorksheet:range("A" + STRING(i-lin) + ":" + "AC" + STRING(i-lin)):Interior:ColorIndex = 39.
        END CASE.
        
        IF AVAIL tt-cliente THEN
           ASSIGN i-lin = i-lin + 1.
    END.

    /* ActiveCell.FormulaR1C1 = "=SUM(R[-2714]C:R[-1]C)" */
    chWorksheet:range("G" + STRING(i-lin) + ":" + "AC" + STRING(i-lin)):Interior:ColorIndex = 37.
    chWorksheet:range("G" + STRING(i-lin)):Formula = "=SUM(G4:G" + STRING(i-lin - 1) + ")".
    chWorksheet:range("H" + STRING(i-lin)):Formula = "=SUM(H4:H" + STRING(i-lin - 1) + ")".
    chWorksheet:range("I" + STRING(i-lin)):Formula = "=SUM(I4:I" + STRING(i-lin - 1) + ")".
    chWorksheet:range("J" + STRING(i-lin)):Formula = "=SUM(J4:J" + STRING(i-lin - 1) + ")".
    chWorksheet:range("K" + STRING(i-lin)):Formula = "=SUM(K4:K" + STRING(i-lin - 1) + ")".
    chWorksheet:range("L" + STRING(i-lin)):Formula = "=SUM(L4:L" + STRING(i-lin - 1) + ")".
    chWorksheet:range("M" + STRING(i-lin)):Formula = "=SUM(M4:M" + STRING(i-lin - 1) + ")".
    chWorksheet:range("N" + STRING(i-lin)):Formula = "=SUM(N4:N" + STRING(i-lin - 1) + ")".
    chWorksheet:range("O" + STRING(i-lin)):Formula = "=SUM(O4:O" + STRING(i-lin - 1) + ")".
    chWorksheet:range("P" + STRING(i-lin)):Formula = "=SUM(P4:P" + STRING(i-lin - 1) + ")".
    chWorksheet:range("Q" + STRING(i-lin)):Formula = "=SUM(Q4:Q" + STRING(i-lin - 1) + ")".
    chWorksheet:range("R" + STRING(i-lin)):Formula = "=SUM(R4:R" + STRING(i-lin - 1) + ")".
    chWorksheet:range("S" + STRING(i-lin)):Formula = "=SUM(S4:S" + STRING(i-lin - 1) + ")".
    chWorksheet:range("T" + STRING(i-lin)):Formula = "=SUM(T4:T" + STRING(i-lin - 1) + ")".
    chWorksheet:range("U" + STRING(i-lin)):Formula = "=SUM(U4:U" + STRING(i-lin - 1) + ")".
    chWorksheet:range("V" + STRING(i-lin)):Formula = "=SUM(V4:V" + STRING(i-lin - 1) + ")".
    chWorksheet:range("W" + STRING(i-lin)):Formula = "=SUM(W4:W" + STRING(i-lin - 1) + ")".
    chWorksheet:range("X" + STRING(i-lin)):Formula = "=SUM(X4:X" + STRING(i-lin - 1) + ")".
    chWorksheet:range("Y" + STRING(i-lin)):Formula = "=SUM(Y4:Y" + STRING(i-lin - 1) + ")".
    chWorksheet:range("Z" + STRING(i-lin)):Formula = "=SUM(Z4:Z" + STRING(i-lin - 1) + ")".
    chWorksheet:range("AA" + STRING(i-lin)):Formula = "=SUM(AA4:AA" + STRING(i-lin - 1) + ")".
    chWorksheet:range("AB" + STRING(i-lin)):Formula = "=SUM(AB4:AB" + STRING(i-lin - 1) + ")".
    chWorksheet:range("AC" + STRING(i-lin)):Formula = "=SUM(AC4:AC" + STRING(i-lin - 1) + ")".
    chWorkSheet:COLUMNS("A:AC"):AutoFit.  /* Redimenciona automaticamente as colunas de A ¹ Z */

    /* Salva e Fecha a Planilha */
    chExcelApp:DisplayAlerts = FALSE.

    /*
    chWorkBook:Save().
    chWorkBook:SaveAs(cFileName,,,,,,,).
    */
    
    /*  chWorkBook:CLOSE().
    chExcelApp:QUIT().   */
    RELEASE OBJECT chworkBook. 
    RELEASE OBJECT chworksheet.
END PROCEDURE.

 


     



