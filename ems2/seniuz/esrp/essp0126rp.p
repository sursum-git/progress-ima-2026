/* Programa: ESSP0126.W
** Modulo..: Controle de AlgodÆo
** Objetivo: Listar Resultado de Testes em Fibrografo
** Autor...: Fábio Coelho Lanza - Julho/2006
** Obs.....: Especifico da TEAR TEXTIL IND.COM.LTDA.
**
*/
/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0126RP 2.04.00.000}

define TEMP-TABLE tt-param   NO-UNDO
       FIELD destino            AS INTEGER
       FIELD arquivo            AS CHAR format "x(35)"
       FIELD usuario            AS CHAR format "x(12)"
       FIELD data-exec          AS DATE
       FIELD hora-exec          AS INTEGER
       FIELD classifica         AS INTEGER
       FIELD desc-classifica    AS CHAR format "x(40)"
       FIELD cod-emit-ini       LIKE mp-entr-mat.cod-emit
       FIELD cod-emit-fin       LIKE mp-entr-mat.cod-emit
       FIELD padrao-ini         LIKE mp-fardo.padrao
       FIELD padrao-fin         LIKE mp-fardo.padrao
       FIELD dt-recebimento-ini LIKE mp-entr-mat.dt-recebimento
       FIELD dt-recebimento-fin LIKE mp-entr-mat.dt-recebimento
       FIELD nf-ini             LIKE mp-entr-mat.nro-docto
       FIELD nf-fin             LIKE mp-entr-mat.nro-docto
       FIELD tipo-relatorio     AS INTEGER
       FIELD desc-tipo-relat    AS CHAR FORMAT "x(20)"
       FIELD imp-param          as log.

define temp-table tt-raw-digita
       field raw-digita as raw.


DEF TEMP-TABLE w-work
    FIELD letra          LIKE mp-fardo.letra
    FIELD qtd            AS INT FORMAT ">,>>9"
    INDEX ch-work letra.

DEF TEMP-TABLE w-work1
    FIELD tipo           LIKE mp-tipo.tipo
    FIELD qtd            AS INT FORMAT ">,>>9"
    INDEX ch-work1 tipo.

DEF TEMP-TABLE w-work2
    FIELD tonalidade     LIKE mp-coloracao.tonalidade
    FIELD qtd            AS INT FORMAT ">,>>9"
    INDEX ch-work2 tonalidade.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i}


/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.
def var c-classif    AS CHAR FORMAT "x(15)".
DEF VAR i-tot-geral  AS INT  FORMAT ">,>>9".
DEF VAR de-tot-perc  AS DEC  FORMAT ">>>9.99".
DEF VAR i-tot-fardos AS INT  FORMAT ">>>9".

form 
    "*--------------- Parƒmetros/Sele‡Æo ----------------*" SKIP
    tt-param.cod-emit-ini       LABEL  "Fornecedor "
    "A"  AT 40
    tt-param.cod-emit-fin       NO-LABELS SKIP
    tt-param.padrao-ini         LABEL  "Padrao....."
    "A"  AT 40
    tt-param.padrao-fin         NO-LABELS SKIP
    tt-param.dt-recebimento-ini LABEL  "Dt Receb.  "
    "A"  AT 40                  
    tt-param.dt-recebimento-fin NO-LABELS SKIP
    tt-param.nf-ini             LABEL  "Nota Fiscal"
    "A"  AT 40                    
    tt-param.nf-fin             NO-LABELS SKIP
    WITH FRAME f-param SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.

form
    mp-fardo.nr-fardo                      LABEL "No Fardo"     
    mp-fardo.padrao                        LABEL "PadrÆo" 
    mp-fardo.letra                         LABEL "Cls"
    mp-tipo.tipo                           LABEL "Tipo"
    mp-coloracao.tonalidade                LABEL "Tonalidade"
    c-classif                              LABEL "Compr. (mm)"
    mp-fardo.sl1                           LABEL "SL#1"
    mp-fardo.sl2                           LABEL "SL#2" 
    mp-fardo.ur                            LABEL "UR%" 
    with no-box NO-LABEL 55 down WIDTH 132 STREAM-IO frame f-detalhe.

FORM
    "Fornecedor : " emitente.nome-emit
    "Nota Fiscal: " mp-entr-mat.nro-docto SKIP
    "Procedˆncia: " mp-entr-mat.procedencia
    " Contrato: " mp-entr-mat.nro-contrato 
    " Quantidade Fardos: " i-tot-fardos
    WITH NO-LABEL DOWN NO-BOX NO-ATTR-SPACE WIDTH 132 STREAM-IO FRAME f-dados-nf.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i ESPECÖFICOS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Resultado_de_Testes_no_Fibrografo * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-dados-nf.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).
 
FOR EACH mp-entr-mat WHERE mp-entr-mat.cod-emit        >= tt-param.cod-emit-ini    
                       AND mp-entr-mat.cod-emit        <= tt-param.cod-emit-fin
                       AND mp-entr-mat.dt-recebimento  >= tt-param.dt-recebimento-ini    
                       AND mp-entr-mat.dt-recebimento  <= tt-param.dt-recebimento-fin
                       AND mp-entr-mat.nro-docto       >= tt-param.nf-ini  
                       AND mp-entr-mat.nro-docto       <= tt-param.nf-fin
                   NO-LOCK,
    FIRST emitente   WHERE emitente.cod-emit = mp-entr-mat.cod-emit NO-LOCK,    
    EACH mp-fardo    WHERE mp-fardo.nr-cdr = mp-entr-mat.nr-cdr
                       AND mp-fardo.padrao >= tt-param.padrao-ini
                       AND mp-fardo.padrao <= tt-param.padrao-fin
                   NO-LOCK
    BREAK BY mp-fardo.nr-cdr
          BY mp-fardo.nr-fardo:

    RUN pi-acompanhar IN h-acomp (INPUT "PadrÆo: "  + mp-fardo.padrao +
                                        " Fardo: " + STRING (mp-fardo.nr-fardo)).

    FIND w-work WHERE w-work.letra  = mp-fardo.letra NO-ERROR.
    IF NOT AVAIL w-work THEN DO:
       CREATE w-work.
       ASSIGN w-work.letra = mp-fardo.letra
              w-work.qtd   = 0.
    END.
    ASSIGN w-work.qtd  = w-work.qtd + 1.

    FIND mp-tipo WHERE
         mp-tipo.codigo = mp-fardo.cd-tipo NO-LOCK NO-ERROR.
    FIND w-work1 WHERE w-work1.tipo = mp-tipo.tipo NO-ERROR.
    IF NOT AVAIL w-work1 THEN DO:
       CREATE w-work1.
       ASSIGN w-work1.tipo    = mp-tipo.tipo
              w-work1.qtd     = 0.
    END.
    ASSIGN w-work1.qtd  = w-work1.qtd + 1.

    FIND mp-coloracao WHERE
         mp-coloracao.codigo = mp-fardo.cd-coloracao NO-LOCK NO-ERROR.
    FIND w-work2 WHERE w-work2.tonalidade = mp-coloracao.tonalidade NO-ERROR.
    IF NOT AVAIL w-work2 THEN DO:
       CREATE w-work2.
       ASSIGN w-work2.tonalidade = mp-coloracao.tonalidade
              w-work2.qtd        = 0.
    END.
    ASSIGN w-work2.qtd  = w-work2.qtd + 1.

    FIND mp-classificacao WHERE
         mp-classificacao.codigo = mp-fardo.cd-coloracao NO-LOCK NO-ERROR.
    IF AVAIL mp-classificacao THEN
       ASSIGN c-classif = STRING(mp-classificacao.compr-min, ">>9.99") + " A " + STRING(mp-classificacao.compr-max, ">>9.99").

    IF FIRST-OF(mp-fardo.nr-cdr) THEN DO:
        ASSIGN i-tot-fardos = mp-entr-mat.qtd-fardos[1] + mp-entr-mat.qtd-fardos[2] +
                              mp-entr-mat.qtd-fardos[3] + mp-entr-mat.qtd-fardos[4] +
                              mp-entr-mat.qtd-fardos[5].
        DISPLAY mp-entr-mat.nro-docto      
                emitente.nome-emit 
                mp-entr-mat.procedencia
                mp-entr-mat.nro-contrato
                i-tot-fardos
                WITH FRAME f-dados-nf.
        DOWN 2 WITH FRAME f-dados-nf.
    END.

    ACCUMULATE mp-fardo.sl1 (AVERAGE BY mp-fardo.nr-cdr).
    ACCUMULATE mp-fardo.sl2 (AVERAGE BY mp-fardo.nr-cdr).
    ACCUMULATE mp-fardo.ur  (AVERAGE BY mp-fardo.nr-cdr).

    IF tt-param.tipo-relatorio = 2 THEN do: /* ANALITICO */
       DISPLAY mp-fardo.nr-fardo          
               mp-fardo.padrao           
               mp-fardo.letra 
               mp-tipo.tipo
               mp-coloracao.tonalidade
               c-classif
               mp-fardo.sl1              
               mp-fardo.sl2   
               mp-fardo.ur                
               WITH FRAME f-detalhe.
       DOWN WITH FRAME f-detalhe.
    END.
    IF LAST-OF(mp-fardo.nr-cdr) THEN DO:
        IF tt-param.tipo-relatorio = 2 THEN DO: /* ANALITICO */
           DISPLAY "M‚dias ......." @ c-classif      
                   (ACCUM AVERAGE BY mp-fardo.nr-cdr mp-fardo.sl1) @ mp-fardo.sl1 
                   (ACCUM AVERAGE BY mp-fardo.nr-cdr mp-fardo.sl2) @ mp-fardo.sl2 
                   (ACCUM AVERAGE BY mp-fardo.nr-cdr mp-fardo.ur)  @ mp-fardo.ur 
           WITH FRAME f-detalhe.
           DOWN 2 WITH FRAME f-detalhe.
           PAGE.
           DISPLAY emitente.nome-emit          
                   mp-entr-mat.nro-docto           
                   mp-entr-mat.procedencia 
                   mp-entr-mat.nro-contrato
                   i-tot-fardos
                   WITH FRAME f-dados-nf.
           DOWN 2 WITH FRAME f-dados-nf.
        END.
        PUT FILL("-",132)  AT  1 FORMAT "x(132)" SKIP(2).
        PUT "* * *      R  E  S  U  M  O  S     * * *" SKIP(3).
        PUT "CLASSIFICACAO            TOTAL       %"   AT 1 
            "----------------------------------------" AT 1.

        ASSIGN i-tot-geral = 0
               de-tot-perc = 0.
        FOR EACH w-work NO-LOCK.
           ASSIGN i-tot-geral = i-tot-geral + w-work.qtd.
        END.
        FOR EACH w-work NO-LOCK.
            PUT w-work.letra               AT 06
                w-work.qtd                 AT 26
                w-work.qtd / i-tot-geral * 100  AT 31.
            ASSIGN de-tot-perc = de-tot-perc + w-work.qtd / i-tot-geral * 100.
        END.
        PUT "-----    ------" AT 26 SKIP
            "TOTAL.........:" AT 1
            i-tot-geral       AT 26
            de-tot-perc       AT 35 SKIP(4).

        PUT "TIPO                     TOTAL       %"   AT 1 
            "----------------------------------------" AT 1.

        ASSIGN i-tot-geral = 0
               de-tot-perc = 0.
        FOR EACH w-work1 NO-LOCK.
           ASSIGN i-tot-geral = i-tot-geral + w-work1.qtd.
        END.
        FOR EACH w-work1 NO-LOCK.
            PUT w-work1.tipo                AT 01
                w-work1.qtd                 AT 26
                w-work1.qtd / i-tot-geral * 100  AT 31.
            ASSIGN de-tot-perc = de-tot-perc + w-work1.qtd / i-tot-geral * 100.
        END.
        PUT "-----    ------" AT 26 SKIP
            "TOTAL.........:" AT 1
            i-tot-geral       AT 26
            de-tot-perc       AT 35 SKIP(4).



        PUT "TONALIDADE               TOTAL       %"   AT 1 
            "----------------------------------------" AT 1.

        ASSIGN i-tot-geral = 0
               de-tot-perc = 0.
        FOR EACH w-work2 NO-LOCK.
           ASSIGN i-tot-geral = i-tot-geral + w-work2.qtd.
        END.
        FOR EACH w-work2 NO-LOCK.
            PUT w-work2.tonalidade          AT 01
                w-work2.qtd                 AT 26
                w-work2.qtd / i-tot-geral * 100  AT 31.
            ASSIGN de-tot-perc = de-tot-perc + w-work2.qtd / i-tot-geral * 100.
        END.
        PUT "-----    ------" AT 26 SKIP
            "TOTAL.........:" AT 1
            i-tot-geral       AT 26
            de-tot-perc       AT 35 SKIP(2).

        PAGE.
        FOR EACH w-work.
            DELETE w-work.
        END.
        FOR EACH w-work1.
            DELETE w-work1.
        END.
        FOR EACH w-work2.
            DELETE w-work2.
        END.

    END. 
END.

IF tt-param.imp-param THEN DO:
   PAGE.
   DISPLAY tt-param.cod-emit-ini
           tt-param.cod-emit-fin
           tt-param.padrao-ini
           tt-param.padrao-fin
           tt-param.dt-recebimento-ini
           tt-param.dt-recebimento-fin
           tt-param.nf-ini
           tt-param.nf-fin
           WITH FRAME f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
RUN pi-finalizar IN h-acomp.
RETURN "OK":U.

