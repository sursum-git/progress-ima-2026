/* Programa: ESSP0061RP.P (Chamado pelo programa ESSP0061.W)
** Objetivo: Imprimir o relat¢rio de Testes de Pick-up
**           (tabela: teste-pkup)
** Sistema.: Espec¡ficos
** M¢dulo..: Espec¡ficos
** Autor...: F bio Coelho Lanza - Dezembro/2003
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0061RP 2.04.00.000} 

/* defini‡Æo das temp-tables para recebimento de parƒmetros */
define temp-table tt-param no-undo
       FIELD destino             AS INTEGER
       FIELD arquivo             AS CHAR format "x(35)"
       FIELD usuario             AS CHAR format "x(12)"
       FIELD data-exec           AS DATE
       FIELD hora-exec           AS INTEGER
       FIELD classifica          AS INTEGER
       FIELD desc-classifica     AS CHAR FORMAT "x(40)"
       FIELD fi-ini-cod-maq      LIKE teste-pkup.cod-maq
       FIELD fi-fin-cod-maq      LIKE teste-pkup.cod-maq
       FIELD fi-ini-data-teste   LIKE teste-pkup.data-teste
       FIELD fi-fin-data-teste   LIKE teste-pkup.data-teste
       FIELD fi-ini-pre-esqrd    LIKE teste-pkup.pre-esqrd  
       FIELD fi-fin-pre-esqrd    LIKE teste-pkup.pre-esqrd  
       FIELD fi-ini-pre-centro   LIKE teste-pkup.pre-centro 
       FIELD fi-fin-pre-centro   LIKE teste-pkup.pre-centro
       FIELD fi-ini-pre-direita  LIKE teste-pkup.pre-direita
       FIELD fi-fin-pre-direita  LIKE teste-pkup.pre-direita
       FIELD fi-ini-res-esqrd    LIKE teste-pkup.res-esqrd    
       FIELD fi-fin-res-esqrd    LIKE teste-pkup.res-esqrd    
       FIELD fi-ini-res-centro   LIKE teste-pkup.res-centro    
       FIELD fi-fin-res-centro   LIKE teste-pkup.res-centro
       FIELD fi-ini-res-direita  LIKE teste-pkup.res-direita
       FIELD fi-fin-res-direita  LIKE teste-pkup.res-direita
       FIELD fi-ini-apr-ll       AS DEC
       FIELD fi-fin-apr-ll       AS DEC 
       FIELD fi-ini-apr-ldc      AS DEC 
       FIELD fi-fin-apr-ldc      AS DEC 
       FIELD tipo-relatorio      AS INTEGER
       FIELD desc-tipo-relat     AS CHAR FORMAT "x(20)"
       FIELD imp-param           AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

/* recebimento de parƒmetros */
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.
def var de-tot-pre-esq          as dec.
def var de-tot-pre-cen          as dec.
def var de-tot-pre-dir          as dec.
def var de-tot-res-esq          as dec.
def var de-tot-res-cen          as dec.
def var de-tot-res-dir          as dec.
def var de-tot-dif-ll           as dec.
def var de-tot-dif-ldc          as dec.
def var de-tot-dif-lec          as dec.
def var de-dif-ll               as dec. format ">9.99".
def var de-dif-ldc              as dec. format ">9.99".
def var de-dif-lec              as dec. format ">9.99".
def var i-cont-maq              as int  format ">>>9".
def var i-cont-dat              as int  format ">>>9".
def var i-cont-ger              as int.
def var i-cont-apr-ll           as int.
def var i-cont-rep-ll           as int.
def var i-cont-apr-lec          as int.
def var i-cont-rep-lec          as int.
def var i-cont-apr-ldc          as int.
def var i-cont-rep-ldc          as int.

/* defini‡Æo de frames do relat¢rio */
FORM 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
   tt-param.desc-classifica     LABEL "Classifica‡Æo....."  SKIP
   tt-param.fi-ini-cod-maq      LABEL "Maquina..........."
   "A"  AT 35
   tt-param.fi-fin-cod-maq      NO-LABELS             SKIP
   tt-param.fi-ini-data-teste   LABEL "Data.............."
   "A"  AT 35
   tt-param.fi-fin-data-teste   NO-LABELS             SKIP
   tt-param.fi-ini-pre-esqrd    LABEL "PressÆo Esquerda.."
   "A"  AT 35
   tt-param.fi-fin-pre-esqrd    NO-LABELS             SKIP
   tt-param.fi-ini-pre-centro   LABEL "PressÆo Centro...."
   "A"  AT 35
   tt-param.fi-fin-pre-centro   NO-LABELS             SKIP
   tt-param.fi-ini-pre-direita  LABEL "PressÆo Direita..."
   "A"  AT 35
   tt-param.fi-fin-pre-direita  NO-LABELS             SKIP
   tt-param.fi-ini-res-esqrd    LABEL "Resultado Esquerda"
   "A"  AT 35
   tt-param.fi-fin-res-esqrd    NO-LABELS             SKIP 
   tt-param.fi-ini-res-centro   LABEL "Resultado Centro.."
   "A"  AT 35
   tt-param.fi-fin-res-centro   NO-LABELS             SKIP 
   tt-param.fi-ini-res-direita  LABEL "Resultado Direita."
   "A"  AT 35
   tt-param.fi-fin-res-direita  NO-LABELS             SKIP 
   tt-param.fi-ini-apr-ll       LABEL "Aprovado LL......."
   "A"  AT 35
   tt-param.fi-fin-apr-ll       NO-LABELS             SKIP
   tt-param.fi-ini-apr-ldc      LABEL "Aprovado LDC/LEC.."
   "A"  AT 35
   tt-param.fi-fin-apr-ldc      NO-LABELS             SKIP
   tt-param.desc-tipo-rel       LABEL "Tipo Relat"
   with FRAME f-param side-labels no-box WIDTH 133 STREAM-IO.

FORM   
   maq-benef.codigo           label "Maquina"
   maq-benef.descricao        label "Descricao"
   teste-pkup.data-teste      label "Data-Teste"
   teste-pkup.num-seq         label "NS"
   teste-pkup.pre-esqrd       label "P-Esq"
   teste-pkup.pre-centro      label "P-Cen"
   teste-pkup.pre-direita     label "P-Dir"
   teste-pkup.res-esqrd       label "Res.Esq"
   teste-pkup.res-centro      label "Res.Cen"
   teste-pkup.res-direita     label "Res.Dir"
   de-dif-ll                  label "Dif.LL"
   de-dif-ldc                 label "Dif.LDC"
   de-dif-lec                 label "Dif.LEC" 
   i-cont-maq                 label "Tsts"
   with frame f-analitico-maq NO-LABEL WIDTH 133 down stream-io.

FORM   
    maq-benef.codigo          label "Maquina"
    maq-benef.descricao       label "Descricao"
    teste-pkup.data-teste     label "Data-Teste"
    teste-pkup.pre-esqrd      label "P-Esq"
    teste-pkup.pre-centro     label "P-Cen"
    teste-pkup.pre-direita    label "P-Dir"
    teste-pkup.res-esqrd      label "Res.Esq"
    teste-pkup.res-centro     label "Res.Cen"
    teste-pkup.res-direita    label "Res.Dir"
    de-dif-ll                 label "Dif.LL"
    de-dif-ldc                label "Dif.LDC"
    de-dif-lec                label "Dif.LEC" 
    i-cont-maq                label "Tsts"
    with frame f-sintetico-maq NO-LABEL WIDTH 133 down stream-io.

FORM
    teste-pkup.data-teste      label "Data-Teste"
    teste-pkup.cod-maq         label "Maquina"
    maq-benef.descricao        label "Descricao"
    teste-pkup.num-seq         label "NS"
    teste-pkup.pre-esqrd       label "P-Esq"
    teste-pkup.pre-centro      label "P-Cen"
    teste-pkup.pre-direita     label "P-Dir"
    teste-pkup.res-esqrd       label "Res.Esq"
    teste-pkup.res-centro      label "Res.Cen"
    teste-pkup.res-direita     label "Res.Dir"
    de-dif-ll                  label "Dif.LL"
    de-dif-ldc                 label "Dif.LDC"
    de-dif-lec                 label "Dif.LEC" 
    i-cont-dat                 label "Tsts"
    with frame f-analitico-dat NO-LABEL WIDTH 133 down stream-io.

FORM   
    teste-pkup.data-teste     label "Data-Teste"
    teste-pkup.cod-maq        label "Maquina"
    maq-benef.descricao       label "Descricao"
    teste-pkup.pre-esqrd      label "P-Esq"
    teste-pkup.pre-centro     label "P-Cen"
    teste-pkup.pre-direita    label "P-Dir"
    teste-pkup.res-esqrd      label "Res.Esq"
    teste-pkup.res-centro     label "Res.Cen"
    teste-pkup.res-direita    label "Res.Dir"
    de-dif-ll                 label "Dif.LL"
    de-dif-ldc                label "Dif.LDC"
    de-dif-lec                label "Dif.LEC" 
    i-cont-dat                label "Tsts"
   with frame f-sintetico-dat NO-LABEL WIDTH 133 down stream-io.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i &STREAM="stream str-rp"}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i &STREAM="str-rp"}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i ESPECÖFICOS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Testes_de_Pick-Up * r}
assign c-titulo-relat = trim(return-value).

view stream str-rp frame f-cabec.
view stream str-rp frame f-rodape.
run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

if tt-param.classific = 1 then do: /* Por Maquina */
   FOR each teste-pkup 
      where teste-pkup.cod-maq      >= tt-param.fi-ini-cod-maq    
        AND teste-pkup.cod-maq      <= tt-param.fi-fin-cod-maq    
        AND teste-pkup.data-teste   >= tt-param.fi-ini-data-teste
        AND teste-pkup.data-teste   <= tt-param.fi-fin-data-teste
        AND teste-pkup.pre-esqrd    >= tt-param.fi-ini-pre-esqrd   
        AND teste-pkup.pre-esqrd    <= tt-param.fi-fin-pre-esqrd   
        AND teste-pkup.pre-centro   >= tt-param.fi-ini-pre-centro
        AND teste-pkup.pre-centro   <= tt-param.fi-fin-pre-centro
        AND teste-pkup.pre-direita  >= tt-param.fi-ini-pre-direita
        AND teste-pkup.pre-direita  <= tt-param.fi-fin-pre-direita
        AND teste-pkup.res-esqrd    >= tt-param.fi-ini-res-esqrd     
        AND teste-pkup.res-esqrd    <= tt-param.fi-fin-res-esqrd  
        AND teste-pkup.res-centro   >= tt-param.fi-ini-res-centro   
        AND teste-pkup.res-centro   <= tt-param.fi-fin-res-centro    
        AND teste-pkup.res-direita  >= tt-param.fi-ini-res-direita  
        AND teste-pkup.res-direita  <= tt-param.fi-fin-res-direita     
        NO-LOCK,
   each maq-benef where maq-benef.codigo = teste-pkup.cod-maq
                  no-lock      
                  BREAK BY maq-benef.codigo
      on stop undo,leave:

      run pi-acompanhar in h-acomp (input string(teste-pkup.cod-maq)).
      
      assign de-dif-ll      = round(abs(teste-pkup.res-esqrd - 
                                  teste-pkup.res-direita),2)
             de-dif-ldc     = round(abs(teste-pkup.res-direita -
                                  teste-pkup.res-centro),2)
             de-dif-lec     = round(abs(teste-pkup.res-esqrd -
                                  teste-pkup.res-centro),2).

      IF tt-param.tipo-relatorio = 2 THEN DO: /* ANALITICO */
         DISPLAY STREAM str-rp
                 maq-benef.codigo    WHEN FIRST-OF(maq-benef.codigo)
                 maq-benef.descricao WHEN FIRST-OF(maq-benef.codigo)
                 teste-pkup.data-teste
                 teste-pkup.num-seq
                 teste-pkup.pre-esqrd
                 teste-pkup.pre-centro
                 teste-pkup.pre-direita
                 teste-pkup.res-esqrd
                 teste-pkup.res-centro
                 teste-pkup.res-direita
                 de-dif-ll
                 de-dif-ldc
                 de-dif-lec
                 with frame f-analitico-maq.
         down stream str-rp with frame f-analitico-maq.
      END.
      assign i-cont-maq     = i-cont-maq + 1
             de-tot-pre-esq = de-tot-pre-esq + teste-pkup.pre-esqrd
             de-tot-pre-cen = de-tot-pre-cen + teste-pkup.pre-centro
             de-tot-pre-dir = de-tot-pre-dir + teste-pkup.pre-direita
             de-tot-res-esq = de-tot-res-esq + teste-pkup.res-esqrd
             de-tot-res-cen = de-tot-res-cen + teste-pkup.res-centro
             de-tot-res-dir = de-tot-res-dir + teste-pkup.res-direita
             de-tot-dif-ll  = de-tot-dif-ll  + de-dif-ll
             de-tot-dif-ldc = de-tot-dif-ldc + de-dif-ldc
             de-tot-dif-lec = de-tot-dif-lec + de-dif-lec.

      /* Acumula para Estatisticas */ 
     
      assign i-cont-ger = i-cont-ger + 1.
      if de-dif-ll >= tt-param.fi-ini-apr-ll AND de-dif-ll <= tt-param.fi-fin-apr-ll then
         assign i-cont-apr-ll = i-cont-apr-ll + 1.
      else
         assign i-cont-rep-ll = i-cont-rep-ll + 1.  
      if de-dif-ldc >= tt-param.fi-ini-apr-ldc AND de-dif-ldc <= tt-param.fi-fin-apr-ldc then
         assign i-cont-apr-ldc = i-cont-apr-ldc + 1.
      else
         assign i-cont-rep-ldc = i-cont-rep-ldc + 1.  
      if de-dif-lec >= tt-param.fi-ini-apr-ldc AND de-dif-lec <= tt-param.fi-fin-apr-ldc then
         assign i-cont-apr-lec = i-cont-apr-lec + 1.
      else
         assign i-cont-rep-lec = i-cont-rep-lec + 1.  

      IF LAST-OF(maq-benef.codigo) THEN DO:
          IF tt-param.tipo-relatorio = 2 THEN DO: /* ANALITICO */
             DISPLAY STREAM str-rp
                     "MEDIA"                     @ teste-pkup.data-teste
                     de-tot-pre-esq / i-cont-maq @ teste-pkup.pre-esqrd
                     de-tot-pre-cen / i-cont-maq @ teste-pkup.pre-centro
                     de-tot-pre-dir / i-cont-maq @ teste-pkup.pre-direita
                     de-tot-res-esq / i-cont-maq @ teste-pkup.res-esqrd
                     de-tot-res-cen / i-cont-maq @ teste-pkup.res-cen
                     de-tot-res-dir / i-cont-maq @ teste-pkup.res-dir
                     de-tot-dif-ll  / i-cont-maq @ de-dif-ll
                     de-tot-dif-ldc / i-cont-maq @ de-dif-ldc
                     de-tot-dif-lec / i-cont-maq @ de-dif-lec
                     i-cont-maq
                     WITH frame f-analitico-maq.
             DOWN STREAM str-rp WITH FRAME f-analitico-maq.
          END.
          ELSE DO: /* SINTTICO */
              DISPLAY STREAM str-rp
                      maq-benef.codigo
                      maq-benef.descricao
                      "MEDIA"                     @ teste-pkup.data-teste
                      de-tot-pre-esq / i-cont-maq @ teste-pkup.pre-esqrd
                      de-tot-pre-cen / i-cont-maq @ teste-pkup.pre-centro
                      de-tot-pre-dir / i-cont-maq @ teste-pkup.pre-direita
                      de-tot-res-esq / i-cont-maq @ teste-pkup.res-esqrd
                      de-tot-res-cen / i-cont-maq @ teste-pkup.res-cen
                      de-tot-res-dir / i-cont-maq @ teste-pkup.res-dir
                      de-tot-dif-ll  / i-cont-maq @ de-dif-ll
                      de-tot-dif-ldc / i-cont-maq @ de-dif-ldc
                      de-tot-dif-lec / i-cont-maq @ de-dif-lec
                      i-cont-maq
                      WITH frame f-analitico-maq.
              DOWN STREAM str-rp WITH FRAME f-analitico-maq.
          END.
          ASSIGN i-cont-maq     = 0
                 de-tot-pre-esq = 0
                 de-tot-pre-cen = 0
                 de-tot-pre-dir = 0
                 de-tot-res-esq = 0
                 de-tot-res-cen = 0
                 de-tot-res-dir = 0
                 de-tot-dif-ll  = 0
                 de-tot-dif-ldc = 0
                 de-tot-dif-lec = 0.
      END.
   END.
END.
else do: /* POR DATA */
   FOR EACH teste-pkup NO-LOCK
       WHERE teste-pkup.cod-maq      >= tt-param.fi-ini-cod-maq    
         AND teste-pkup.cod-maq      <= tt-param.fi-fin-cod-maq    
         AND teste-pkup.data-teste   >= tt-param.fi-ini-data-teste
         AND teste-pkup.data-teste   <= tt-param.fi-fin-data-teste
         AND teste-pkup.pre-esqrd    >= tt-param.fi-ini-pre-esqrd   
         AND teste-pkup.pre-esqrd    <= tt-param.fi-fin-pre-esqrd   
         AND teste-pkup.pre-centro   >= tt-param.fi-ini-pre-centro
         AND teste-pkup.pre-centro   <= tt-param.fi-fin-pre-centro
         AND teste-pkup.pre-direita  >= tt-param.fi-ini-pre-direita
         AND teste-pkup.pre-direita  <= tt-param.fi-fin-pre-direita
         AND teste-pkup.res-esqrd    >= tt-param.fi-ini-res-esqrd     
         AND teste-pkup.res-esqrd    <= tt-param.fi-fin-res-esqrd  
         AND teste-pkup.res-centro   >= tt-param.fi-ini-res-centro   
         AND teste-pkup.res-centro   <= tt-param.fi-fin-res-centro    
         AND teste-pkup.res-direita  >= tt-param.fi-ini-res-direita  
         AND teste-pkup.res-direita  <= tt-param.fi-fin-res-direita     
       BREAK BY teste-pkup.data-teste
             BY teste-pkup.cod-maq
             on stop undo,leave:
       run pi-acompanhar in h-acomp (input string(teste-pkup.data-teste)).
  
       assign de-dif-ll  = round(abs(teste-pkup.res-esqrd - 
                               teste-pkup.res-direita),2)
              de-dif-ldc = round(abs(teste-pkup.res-direita -
                               teste-pkup.res-centro),2)
              de-dif-lec = round(abs(teste-pkup.res-esqrd -
                               teste-pkup.res-centro),2).

       find maq-benef where maq-benef.codigo = teste-pkup.cod-maq
                      no-lock no-error.

       IF tt-param.tipo-relatorio = 2 THEN DO: /* ANALITICO */
          DISPLAY STREAM str-rp
                  teste-pkup.data-teste   WHEN FIRST-OF(teste-pkup.data-teste)
                  teste-pkup.cod-maq                                       
                  maq-benef.descricao     
                  teste-pkup.num-seq
                  teste-pkup.pre-esqrd
                  teste-pkup.pre-centro
                  teste-pkup.pre-direita
                  teste-pkup.res-esqrd
                  teste-pkup.res-centro
                  teste-pkup.res-direita
                  de-dif-ll
                  de-dif-ldc
                  de-dif-lec
                  with frame f-analitico-dat.
          down stream str-rp with frame f-analitico-dat.
       END.
       assign i-cont-dat     = i-cont-dat + 1
              de-tot-pre-esq = de-tot-pre-esq + teste-pkup.pre-esqrd
              de-tot-pre-cen = de-tot-pre-cen + teste-pkup.pre-centro
              de-tot-pre-dir = de-tot-pre-dir + teste-pkup.pre-direita
              de-tot-res-esq = de-tot-res-esq + teste-pkup.res-esqrd
              de-tot-res-cen = de-tot-res-cen + teste-pkup.res-centro
              de-tot-res-dir = de-tot-res-dir + teste-pkup.res-direita
              de-tot-dif-ll  = de-tot-dif-ll  + de-dif-ll
              de-tot-dif-ldc = de-tot-dif-ldc + de-dif-ldc
              de-tot-dif-lec = de-tot-dif-lec + de-dif-lec.

       /* Acumula para Estatisticas */ 
       assign i-cont-ger = i-cont-ger + 1.
       if de-dif-ll >= tt-param.fi-ini-apr-ll AND de-dif-ll <= tt-param.fi-fin-apr-ll then
          assign i-cont-apr-ll = i-cont-apr-ll + 1.
       else
          assign i-cont-rep-ll = i-cont-rep-ll + 1.  
       if de-dif-ldc >= tt-param.fi-ini-apr-ldc AND de-dif-ldc <= tt-param.fi-fin-apr-ldc then
          assign i-cont-apr-ldc = i-cont-apr-ldc + 1.
       else
          assign i-cont-rep-ldc = i-cont-rep-ldc + 1.  
       if de-dif-lec >= tt-param.fi-ini-apr-ldc AND de-dif-lec <= tt-param.fi-fin-apr-ldc then
          assign i-cont-apr-lec = i-cont-apr-lec + 1.
       else
          assign i-cont-rep-lec = i-cont-rep-lec + 1.  

       IF LAST-OF(teste-pkup.data-teste) THEN DO:
           IF tt-param.tipo-relatorio = 2 THEN DO: /* ANALITICO */
              DISPLAY STREAM str-rp
                      "MEDIA"                     @ teste-pkup.data-teste
                      de-tot-pre-esq / i-cont-dat @ teste-pkup.pre-esqrd
                      de-tot-pre-cen / i-cont-dat @ teste-pkup.pre-centro
                      de-tot-pre-dir / i-cont-dat @ teste-pkup.pre-direita
                      de-tot-res-esq / i-cont-dat @ teste-pkup.res-esqrd
                      de-tot-res-cen / i-cont-dat @ teste-pkup.res-cen
                      de-tot-res-dir / i-cont-dat @ teste-pkup.res-dir
                      de-tot-dif-ll  / i-cont-dat @ de-dif-ll
                      de-tot-dif-ldc / i-cont-dat @ de-dif-ldc
                      de-tot-dif-lec / i-cont-dat @ de-dif-lec
                      i-cont-dat
                      WITH frame f-analitico-dat.
              DOWN STREAM str-rp WITH FRAME f-analitico-dat.
           END.
           ELSE DO: /* SINTTICO */
               DISPLAY STREAM str-rp
                       teste-pkup.data-teste
                       teste-pkup.cod-maq
                       maq-benef.descricao
                       teste-pkup.data-teste
                       de-tot-pre-esq / i-cont-dat @ teste-pkup.pre-esqrd
                       de-tot-pre-cen / i-cont-dat @ teste-pkup.pre-centro
                       de-tot-pre-dir / i-cont-dat @ teste-pkup.pre-direita
                       de-tot-res-esq / i-cont-dat @ teste-pkup.res-esqrd
                       de-tot-res-cen / i-cont-dat @ teste-pkup.res-cen
                       de-tot-res-dir / i-cont-dat @ teste-pkup.res-dir
                       de-tot-dif-ll  / i-cont-dat @ de-dif-ll
                       de-tot-dif-ldc / i-cont-dat @ de-dif-ldc
                       de-tot-dif-lec / i-cont-dat @ de-dif-lec
                       i-cont-dat
                       WITH frame f-analitico-dat.
               DOWN STREAM str-rp WITH FRAME f-analitico-dat.
           END.
           ASSIGN i-cont-dat     = 0
                  de-tot-pre-esq = 0
                  de-tot-pre-cen = 0
                  de-tot-pre-dir = 0
                  de-tot-res-esq = 0
                  de-tot-res-cen = 0
                  de-tot-res-dir = 0
                  de-tot-dif-ll  = 0
                  de-tot-dif-ldc = 0
                  de-tot-dif-lec = 0.
       END.
   END.
END.

/* Estatisticas */
IF LINE-COUNT > 58 THEN
   PAGE STREAM str-rp.
PUT STREAM str-rp SKIP(1).
PUT STREAM str-rp "RESULTADOS ESTATISTICOS - "
"TESTES LISTADOS: " i-cont-ger    FORMAT ">>>9" SKIP(1)
"POS. CLASSIFICACAO         Qtde.     %"             AT   1
"POS. CLASSIFICACAO         Qtde.     %"             AT  42
"POS. CLASSIFICACAO         Qtde.     %"             AT  83 SKIP
"LL   APROV"                                         AT   1
tt-param.fi-ini-apr-ll            FORMAT ">>9.99"    AT  12
" - "
tt-param.fi-fin-apr-ll            FORMAT ">>9.99" 
i-cont-apr-ll                     FORMAT ">>>9"      AT  29
i-cont-apr-ll / i-cont-ger * 100  FORMAT ">>9.9"     AT  35
"LDC  APROV"                                         AT  41
tt-param.fi-ini-apr-ldc           FORMAT ">>9.99"    AT  52
" - "
tt-param.fi-fin-apr-ldc           FORMAT ">>9.99" 
i-cont-apr-ldc                    FORMAT ">>>9"      AT  69
i-cont-apr-ldc / i-cont-ger * 100 FORMAT ">>9.9"     AT  76
"LEC  Aprov"                                         AT  82
tt-param.fi-ini-apr-ldc           FORMAT ">>9.99"    AT  93
" - "
tt-param.fi-fin-apr-ldc           FORMAT ">>9.99" 
i-cont-apr-lec                    FORMAT ">>>9"      AT 110
i-cont-apr-lec / i-cont-ger * 100 FORMAT ">>9.9"     AT 116 SKIP
"REPROV (Demais)"                                    AT   1
i-cont-rep-ll                     FORMAT ">>>9"      AT  29
i-cont-rep-ll / i-cont-ger * 100  FORMAT ">>9.9"     AT  35
"REPROV (Demais)"                                    AT  41
i-cont-rep-ldc                    FORMAT ">>>9"      AT  69
i-cont-rep-ldc / i-cont-ger * 100 FORMAT ">>9.9"     AT  76
"REPROV (Demais)"                                    AT  82
i-cont-rep-lec                    FORMAT ">>>9"      AT 110
i-cont-rep-lec / i-cont-ger * 100 FORMAT ">>9.9"     AT 116
SKIP(1).
       
/* Parametros de Selecao */ 
IF tt-param.imp-param THEN
   DISPLAY STREAM str-rp
                  tt-param.desc-classifica
                  tt-param.fi-ini-cod-maq              
                  tt-param.fi-fin-cod-maq              
                  tt-param.fi-ini-data-teste
                  tt-param.fi-fin-data-teste
                  tt-param.fi-ini-pre-esqrd      
                  tt-param.fi-fin-pre-esqrd      
                  tt-param.fi-ini-pre-centro    
                  tt-param.fi-fin-pre-centro   
                  tt-param.fi-ini-pre-direita  
                  tt-param.fi-fin-pre-direita  
                  tt-param.fi-ini-res-esqrd        
                  tt-param.fi-fin-res-esqrd        
                  tt-param.fi-ini-res-centro       
                  tt-param.fi-fin-res-centro   
                  tt-param.fi-ini-res-direita  
                  tt-param.fi-fin-res-direita
                  tt-param.fi-ini-apr-ll    
                  tt-param.fi-fin-apr-ll 
                  tt-param.fi-ini-apr-ldc 
                  tt-param.fi-fin-apr-ldc 
                  tt-param.desc-tipo-rel
                  with frame f-param.           


/* fechamento do output do relat¢rio  */
{include/i-rpclo.i &STREAM="stream str-rp"}
run pi-finalizar in h-acomp.
return "OK":U.

