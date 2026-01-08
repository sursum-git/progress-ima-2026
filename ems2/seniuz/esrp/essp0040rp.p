/* Programa: ESSP0040RP.P (Chamado pelo programa ESSP0040.W)
** Objetivo: Imprimir o relat¢rio de Amostras por Fam¡lia 
**           (tabela: amostra-fam)
** Sistema.: Espec¡ficos
** M¢dulo..: Espec¡ficos
** Autor...: Gilvando Souza Araujo - Novembro/2003
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0040RP 2.04.00.000}
/* defini‡Æo das temp-tables para recebimento de parƒmetros */

DEFINE TEMP-TABLE tt-param NO-UNDO
       FIELD destino             AS INTEGER
       FIELD arquivo             AS CHAR FORMAT "x(35)"
       FIELD usuario             AS CHAR FORMAT "x(12)"
       FIELD data-exec           AS DATE
       FIELD hora-exec           AS INTEGER
       FIELD classifica          AS INTEGER
       FIELD desc-classifica     AS CHAR FORMAT "x(40)"
       FIELD fi-ini-fm-codigo    LIKE amostra-fam.fm-codigo
       FIELD fi-fin-fm-codigo    LIKE amostra-fam.fm-codigo
       FIELD fi-ini-it-codigo    LIKE amostra-fam.it-codigo
       FIELD fi-fin-it-codigo    LIKE amostra-fam.it-codigo
       FIELD fi-ini-ano-mes      LIKE amostra-fam.ano-mes
       FIELD fi-fin-ano-mes      LIKE amostra-fam.ano-mes
       FIELD fi-ini-tipo-tear    LIKE amostra-fam.tipo-tear
       FIELD fi-fin-tipo-tear    LIKE amostra-fam.tipo-tear
       FIELD acabamento          AS INTEGER
       FIELD desc-acabamento     AS CHAR FORMAT "x(20)"
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

DEF VAR i-cont-per      AS INT.
DEF VAR i-cont-fam      AS INT.
DEF VAR i-cont-ger      AS INT.
DEF VAR de-larg-ini-per AS DEC.
DEF VAR de-larg-ini-fam AS DEC.
DEF VAR de-larg-ini-ger AS DEC.
DEF VAR de-enc-urd-per  AS DEC.
DEF VAR de-enc-urd-fam  AS DEC.
DEF VAR de-enc-urd-ger  AS DEC.
DEF VAR de-enc-tra-per  AS DEC.
DEF VAR de-enc-tra-fam  AS DEC.
DEF VAR de-enc-tra-ger  AS DEC.
DEF VAR de-larg-fin-per AS DEC.
DEF VAR de-larg-fin-fam AS DEC.
DEF VAR de-larg-fin-ger AS DEC.

/* defini‡Æo de frames do relat¢rio */

FORM 
   "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
   tt-param.desc-classifica    LABEL "Classific."    SKIP
   tt-param.fi-ini-fm-codigo   label "Familia..."
   "A"  AT 30
   tt-param.fi-fin-fm-codigo   NO-LABELS             SKIP
   tt-param.fi-ini-it-codigo   label "Item......"
   "A"  AT 30
   tt-param.fi-fin-it-codigo   NO-LABELS             SKIP
   tt-param.fi-ini-ano-mes     label "Data......"
   "A"  AT 30
   tt-param.fi-fin-ano-mes     no-labels             SKIP
   tt-param.fi-ini-tipo-tear   LABEL "Tipo Tear."
   "A"  AT 30
   tt-param.fi-fin-tipo-tear   NO-LABELS             SKIP
   tt-param.desc-acabamento    LABEL "Acabamento"    SKIP
   tt-param.desc-tipo-rel      LABEL "Tipo Relat"
   with FRAME f-param side-labels no-box width 132 STREAM-IO.

FORM   
   amostra-fam.fm-codigo FORMAT "x(6)"    LABEL "Familia"
   amostra-fam.it-codigo FORMAT "x(6)"    LABEL "Item"
   item.desc-item        FORMAT "x(32)"   LABEL "Descricao"
   amostra-fam.ano-mes                    LABEL "Periodo"
   amostra-fam.num-amostra                LABEL "Amos"
   amostra-fam.tipo-tear                  LABEL "Tp-Tear"
   amostra-fam.merceriz                   LABEL "Merc"
   amostra-fam.larg-ini  FORMAT "->>>9.9" LABEL "LarIni"
   amostra-fam.enc-urd   FORMAT "->>>9.9" LABEL "EncUrd"
   amostra-fam.enc-tra   FORMAT "->>>9.9" LABEL "EncTra"
   amostra-fam.larg-fin  FORMAT "->>>9.9" LABEL "LarFin"
   amostra-fam.num-ob    FORMAT "999999"  LABEL "Num.OB"
   amostra-fam.observ    FORMAT "x(14)"   LABEL "Observ"
   with frame f-analitico-fam NO-LABEL WIDTH 132 down stream-io.

FORM   
   amostra-fam.fm-codigo FORMAT "x(6)"    LABEL "Familia"  
   amostra-fam.it-codigo FORMAT "x(6)"    LABEL "Item"     
   item.desc-item        FORMAT "x(32)"   LABEL "Descricao"
   amostra-fam.ano-mes                    LABEL "Periodo"  
   amostra-fam.larg-ini  FORMAT "->>>9.9" LABEL "LarIni"
   amostra-fam.enc-urd   FORMAT "->>>9.9" LABEL "EncUrd"
   amostra-fam.enc-tra   FORMAT "->>>9.9" LABEL "EncTra"
   amostra-fam.larg-fin  FORMAT "->>>9.9" LABEL "LarFin"
   with frame f-sintetico-fam NO-LABEL WIDTH 132 down stream-io.

FORM   
   amostra-fam.it-codigo FORMAT "x(6)"    LABEL "Item"
   item.desc-item        FORMAT "x(32)"   LABEL "Descricao"
   amostra-fam.fm-codigo FORMAT "x(6)"    LABEL "Familia"
   amostra-fam.ano-mes                    LABEL "Periodo"
   amostra-fam.num-amostra                LABEL "Amos"
   amostra-fam.tipo-tear                  LABEL "Tp-Tear"
   amostra-fam.merceriz                   LABEL "Merc"
   amostra-fam.larg-ini  FORMAT "->>>9.9" LABEL "LarIni"
   amostra-fam.enc-urd   FORMAT "->>>9.9" LABEL "EncUrd"
   amostra-fam.enc-tra   FORMAT "->>>9.9" LABEL "EncTra"
   amostra-fam.larg-fin  FORMAT "->>>9.9" LABEL "LarFin"
   amostra-fam.num-ob    FORMAT "999999"  LABEL "Num.OB"
   amostra-fam.observ    FORMAT "x(14)"   LABEL "Observ"
   with frame f-analitico-ite NO-LABEL WIDTH 132 down stream-io.

FORM   
   amostra-fam.it-codigo FORMAT "x(6)"    LABEL "Item"     
   item.desc-item        FORMAT "x(32)"   LABEL "Descricao"
   amostra-fam.fm-codigo FORMAT "x(6)"    LABEL "Familia"  
   amostra-fam.ano-mes                    LABEL "Periodo"  
   amostra-fam.larg-ini  FORMAT "->>>9.9" LABEL "LarIni"
   amostra-fam.enc-urd   FORMAT "->>>9.9" LABEL "EncUrd"
   amostra-fam.enc-tra   FORMAT "->>>9.9" LABEL "EncTra"
   amostra-fam.larg-fin  FORMAT "->>>9.9" LABEL "LarFin"
   with frame f-sintetico-ite NO-LABEL WIDTH 132 down stream-io.

FORM
   amostra-fam.ano-mes                    LABEL "Periodo"
   amostra-fam.fm-codigo FORMAT "x(6)"    LABEL "Familia"  
   amostra-fam.it-codigo FORMAT "x(6)"    LABEL "Item"     
   item.desc-item        FORMAT "x(32)"   LABEL "Descricao"
   amostra-fam.num-amostra                LABEL "Amos"
   amostra-fam.tipo-tear                  LABEL "Tp-Tear"
   amostra-fam.merceriz                   LABEL "Merc"   
   amostra-fam.larg-ini  FORMAT "->>>9.9" LABEL "LarIni"  
   amostra-fam.enc-urd   FORMAT "->>>9.9" LABEL "EncUrd"  
   amostra-fam.enc-tra   FORMAT "->>>9.9" LABEL "EncTra"  
   amostra-fam.larg-fin  FORMAT "->>>9.9" LABEL "LarFin"  
   amostra-fam.num-ob                     LABEL "Num.OB"  
   amostra-fam.observ    FORMAT "x(14)"   LABEL "Observ"  
   with frame f-analitico-dat NO-LABEL WIDTH 132 down stream-io.

FORM   
   amostra-fam.ano-mes                    LABEL "Periodo"   
   amostra-fam.fm-codigo FORMAT "x(6)"    LABEL "Familia"   
   amostra-fam.it-codigo FORMAT "x(6)"    LABEL "Item"      
   item.desc-item        FORMAT "x(32)"   LABEL "Descricao" 
   amostra-fam.larg-ini  FORMAT "->>>9.9" LABEL "LarIni"  
   amostra-fam.enc-urd   FORMAT "->>>9.9" LABEL "EncUrd"  
   amostra-fam.enc-tra   FORMAT "->>>9.9" LABEL "EncTra"  
   amostra-fam.larg-fin  FORMAT "->>>9.9" LABEL "LarFin"  
   with frame f-sintetico-dat NO-LABEL WIDTH 132 down stream-io.

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
{utp/ut-liter.i Amostras_por_Fam¡lia * r}
assign c-titulo-relat = trim(return-value).

view stream str-rp frame f-cabec.
view stream str-rp frame f-rodape.
run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

if tt-param.classific = 1 then do: /* Por Fam¡lia */
   FOR each amostra-fam no-lock
      where amostra-fam.fm-codigo >= tt-param.fi-ini-fm-codigo
        AND amostra-fam.fm-codigo <= tt-param.fi-fin-fm-codigo
        AND amostra-fam.it-codigo >= tt-param.fi-ini-it-codigo
        AND amostra-fam.it-codigo <= tt-param.fi-fin-it-codigo
        AND amostra-fam.ano-mes   >= tt-param.fi-ini-ano-mes
        AND amostra-fam.ano-mes   <= tt-param.fi-fin-ano-mes
        AND amostra-fam.tipo-tear >= tt-param.fi-ini-tipo-tear
        AND amostra-fam.tipo-tear <= tt-param.fi-fin-tipo-tear
        AND ((amostra-fam.merceriz = YES AND tt-param.acabamento = 1) OR
             (amostra-fam.merceriz = NO  AND tt-param.acabamento = 2) OR
                                             tt-param.acabamento = 3)
      BREAK BY amostra-fam.fm-codigo
            BY amostra-fam.ano-mes
      on stop undo,leave:
      run pi-acompanhar in h-acomp (input string(amostra-fam.fm-codigo)).
      
      ASSIGN i-cont-per = i-cont-per + 1
             i-cont-fam = i-cont-fam + 1
             i-cont-ger = i-cont-ger + 1
             de-larg-ini-per = de-larg-ini-per + amostra-fam.larg-ini
             de-larg-ini-fam = de-larg-ini-fam + amostra-fam.larg-ini
             de-larg-ini-ger = de-larg-ini-ger + amostra-fam.larg-ini
             de-enc-urd-per  = de-enc-urd-per  + amostra-fam.enc-urd
             de-enc-urd-fam  = de-enc-urd-fam  + amostra-fam.enc-urd
             de-enc-urd-ger  = de-enc-urd-ger  + amostra-fam.enc-urd
             de-enc-tra-per  = de-enc-tra-per  + amostra-fam.enc-tra
             de-enc-tra-fam  = de-enc-tra-fam  + amostra-fam.enc-tra
             de-enc-tra-ger  = de-enc-tra-ger  + amostra-fam.enc-tra
             de-larg-fin-per = de-larg-fin-per + amostra-fam.larg-fin
             de-larg-fin-fam = de-larg-fin-fam + amostra-fam.larg-fin
             de-larg-fin-ger = de-larg-fin-ger + amostra-fam.larg-fin.

      IF tt-param.tipo-relatorio = 1 THEN do: /* Sint‚tico */
         IF FIRST-OF(amostra-fam.fm-codigo) THEN DO:
            FIND familia WHERE familia.fm-codigo = amostra-fam.fm-codigo
                         NO-LOCK NO-ERROR.
            FIND item WHERE ITEM.it-codigo = amostra-fam.it-codigo
                      NO-LOCK NO-ERROR.
            DISPLAY STREAM str-rp
                    amostra-fam.fm-codigo
                    amostra-fam.it-codigo
                    item.desc-item WHEN AVAIL ITEM
                    WITH FRAME f-sintetico-fam.
         END.
         
         IF LAST-OF(amostra-fam.ano-mes) THEN DO:
            DISPLAY STREAM str-rp
                    amostra-fam.ano-mes
                    de-larg-ini-per / i-cont-per @ amostra-fam.larg-ini
                    de-enc-urd-per  / i-cont-per @ amostra-fam.enc-urd
                    de-enc-tra-per  / i-cont-per @ amostra-fam.enc-tra
                    de-larg-fin-per / i-cont-per @ amostra-fam.larg-fin
                    WITH FRAME f-sintetico-fam.
            DOWN STREAM str-rp WITH FRAME f-sintetico-fam.
            ASSIGN i-cont-per      = 0
                   de-larg-ini-per = 0
                   de-enc-urd-per  = 0
                   de-enc-tra-per  = 0
                   de-larg-fin-per = 0.
         END.

         IF LAST-OF(amostra-fam.fm-codigo) THEN DO:
            DISPLAY STREAM str-rp
                    "M‚dia"                      @ amostra-fam.fm-codigo
                    de-larg-ini-fam / i-cont-fam @ amostra-fam.larg-ini
                    de-enc-urd-fam  / i-cont-fam @ amostra-fam.enc-urd
                    de-enc-tra-fam  / i-cont-fam @ amostra-fam.enc-tra
                    de-larg-fin-fam / i-cont-fam @ amostra-fam.larg-fin
                    WITH FRAME f-sintetico-fam.
            DOWN 2 STREAM str-rp WITH FRAME f-sintetico-fam.
            ASSIGN i-cont-fam      = 0
                   de-larg-ini-fam = 0
                   de-enc-urd-fam  = 0
                   de-enc-tra-fam  = 0
                   de-larg-fin-fam = 0.
         END.
      END.
      ELSE DO: /*Anal¡tico */
         FIND familia WHERE familia.fm-codigo = amostra-fam.fm-codigo
                      NO-LOCK NO-ERROR.
         FIND item WHERE item.it-codigo = amostra-fam.it-codigo
                   NO-LOCK NO-ERROR.
   
         display stream str-rp
                 amostra-fam.fm-codigo   
                 amostra-fam.it-codigo
                 item.desc-item WHEN AVAIL ITEM
                 amostra-fam.ano-mes
                 amostra-fam.num-amostra
                 amostra-fam.tipo-tear 
                 amostra-fam.merceriz 
                 amostra-fam.larg-ini 
                 amostra-fam.enc-urd
                 amostra-fam.enc-tra 
                 amostra-fam.larg-fin
                 amostra-fam.num-ob
                 amostra-fam.observ
                 with frame f-analitico-fam.
         down stream str-rp with frame f-analitico-fam.
         
         IF LAST-OF(amostra-fam.ano-mes) THEN DO:
            DISPLAY STREAM str-rp
                    "M‚dia"                      @ amostra-fam.ano-mes
                    de-larg-ini-per / i-cont-per @ amostra-fam.larg-ini
                    de-enc-urd-per  / i-cont-per @ amostra-fam.enc-urd
                    de-enc-tra-per  / i-cont-per @ amostra-fam.enc-tra
                    de-larg-fin-per / i-cont-per @ amostra-fam.larg-fin
                    WITH FRAME f-analitico-fam.
            DOWN STREAM str-rp WITH FRAME f-analitico-fam.
            ASSIGN i-cont-per      = 0
                   de-larg-ini-per = 0
                   de-enc-urd-per  = 0
                   de-enc-tra-per  = 0
                   de-larg-fin-per = 0.
         END.
         IF LAST-OF(amostra-fam.fm-codigo) THEN DO:
            DISPLAY STREAM str-rp
                    "M‚dia"                      @ amostra-fam.fm-codigo
                    de-larg-ini-fam / i-cont-fam @ amostra-fam.larg-ini
                    de-enc-urd-fam  / i-cont-fam @ amostra-fam.enc-urd
                    de-enc-tra-fam  / i-cont-fam @ amostra-fam.enc-tra
                    de-larg-fin-fam / i-cont-fam @ amostra-fam.larg-fin
                    WITH FRAME f-analitico-fam.
            DOWN 2 STREAM str-rp WITH FRAME f-analitico-fam.
            ASSIGN i-cont-fam      = 0
                   de-larg-ini-fam = 0
                   de-enc-urd-fam  = 0
                   de-enc-tra-fam  = 0
                   de-larg-fin-fam = 0.
         END.
      END.
   END.
   IF tt-param.tipo-relatorio = 1 THEN DO: /* Sint‚tico */
      DISPLAY STREAM str-rp
                     "MDIA"                      @ amostra-fam.fm-codigo
                     de-larg-ini-ger / i-cont-ger @ amostra-fam.larg-ini
                     de-enc-urd-ger  / i-cont-ger @ amostra-fam.enc-urd
                     de-enc-tra-ger  / i-cont-ger @ amostra-fam.enc-tra
                     de-larg-fin-ger / i-cont-ger @ amostra-fam.larg-fin
                     WITH FRAME f-sintetico-fam.
      DOWN 2 STREAM str-rp WITH FRAME f-sintetico-fam.
   END.
   ELSE DO: /* Anal¡tico */
      DISPLAY STREAM str-rp
                     "MDIA"                      @ amostra-fam.fm-codigo
                     de-larg-ini-ger / i-cont-ger @ amostra-fam.larg-ini
                     de-enc-urd-ger  / i-cont-ger @ amostra-fam.enc-urd
                     de-enc-tra-ger  / i-cont-ger @ amostra-fam.enc-tra
                     de-larg-fin-ger / i-cont-ger @ amostra-fam.larg-fin
                     WITH FRAME f-analitico-fam.
      DOWN 2 STREAM str-rp WITH FRAME f-analitico-fam.
   END.
end.
ELSE
IF tt-param.classific = 2 THEN DO: /* Por Ötem */
   FOR each amostra-fam no-lock
      where amostra-fam.fm-codigo >= tt-param.fi-ini-fm-codigo
        AND amostra-fam.fm-codigo <= tt-param.fi-fin-fm-codigo
        AND amostra-fam.it-codigo >= tt-param.fi-ini-it-codigo
        AND amostra-fam.it-codigo <= tt-param.fi-fin-it-codigo
        AND amostra-fam.ano-mes   >= tt-param.fi-ini-ano-mes
        AND amostra-fam.ano-mes   <= tt-param.fi-fin-ano-mes
        AND amostra-fam.tipo-tear >= tt-param.fi-ini-tipo-tear
        AND amostra-fam.tipo-tear <= tt-param.fi-fin-tipo-tear
        AND ((amostra-fam.merceriz = YES AND tt-param.acabamento = 1) OR
             (amostra-fam.merceriz = NO  AND tt-param.acabamento = 2) OR
                                             tt-param.acabamento = 3)
      BREAK BY amostra-fam.it-codigo
            BY amostra-fam.fm-codigo
            BY amostra-fam.ano-mes
      on stop undo,leave:
      run pi-acompanhar in h-acomp (input string(amostra-fam.fm-codigo)).

      ASSIGN i-cont-per = i-cont-per + 1
             i-cont-fam = i-cont-fam + 1
             i-cont-ger = i-cont-ger + 1
             de-larg-ini-per = de-larg-ini-per + amostra-fam.larg-ini
             de-larg-ini-fam = de-larg-ini-fam + amostra-fam.larg-ini
             de-larg-ini-ger = de-larg-ini-ger + amostra-fam.larg-ini
             de-enc-urd-per  = de-enc-urd-per  + amostra-fam.enc-urd
             de-enc-urd-fam  = de-enc-urd-fam  + amostra-fam.enc-urd
             de-enc-urd-ger  = de-enc-urd-ger  + amostra-fam.enc-urd
             de-enc-tra-per  = de-enc-tra-per  + amostra-fam.enc-tra
             de-enc-tra-fam  = de-enc-tra-fam  + amostra-fam.enc-tra
             de-enc-tra-ger  = de-enc-tra-ger  + amostra-fam.enc-tra
             de-larg-fin-per = de-larg-fin-per + amostra-fam.larg-fin
             de-larg-fin-fam = de-larg-fin-fam + amostra-fam.larg-fin
             de-larg-fin-ger = de-larg-fin-ger + amostra-fam.larg-fin.

      IF tt-param.tipo-relatorio = 1 THEN do: /* Sint‚tico */
         IF FIRST-OF(amostra-fam.it-codigo) THEN DO:
            FIND familia WHERE familia.fm-codigo = amostra-fam.fm-codigo
                         NO-LOCK NO-ERROR.
            FIND item WHERE ITEM.it-codigo = amostra-fam.it-codigo
                      NO-LOCK NO-ERROR.
            DISPLAY STREAM str-rp
                    amostra-fam.fm-codigo
                    amostra-fam.it-codigo
                    item.desc-item WHEN AVAIL ITEM
                    WITH FRAME f-sintetico-ite.
         END.

         IF LAST-OF(amostra-fam.ano-mes) THEN DO:
            DISPLAY STREAM str-rp
                    amostra-fam.ano-mes
                    de-larg-ini-per / i-cont-per @ amostra-fam.larg-ini
                    de-enc-urd-per  / i-cont-per @ amostra-fam.enc-urd
                    de-enc-tra-per  / i-cont-per @ amostra-fam.enc-tra
                    de-larg-fin-per / i-cont-per @ amostra-fam.larg-fin
                    WITH FRAME f-sintetico-ite.
            DOWN STREAM str-rp WITH FRAME f-sintetico-ite.
            ASSIGN i-cont-per      = 0
                   de-larg-ini-per = 0
                   de-enc-urd-per  = 0
                   de-enc-tra-per  = 0
                   de-larg-fin-per = 0.
         END.

         IF LAST-OF(amostra-fam.it-codigo) THEN DO:
            DISPLAY STREAM str-rp
                    "M‚dia"                      @ amostra-fam.fm-codigo
                    de-larg-ini-fam / i-cont-fam @ amostra-fam.larg-ini
                    de-enc-urd-fam  / i-cont-fam @ amostra-fam.enc-urd
                    de-enc-tra-fam  / i-cont-fam @ amostra-fam.enc-tra
                    de-larg-fin-fam / i-cont-fam @ amostra-fam.larg-fin
                    WITH FRAME f-sintetico-ite.
            DOWN 2 STREAM str-rp WITH FRAME f-sintetico-ite.
            ASSIGN i-cont-fam      = 0
                   de-larg-ini-fam = 0
                   de-enc-urd-fam  = 0
                   de-enc-tra-fam  = 0
                   de-larg-fin-fam = 0.
         END.
      END.
      ELSE DO: /*Anal¡tico */
         FIND familia WHERE familia.fm-codigo = amostra-fam.fm-codigo
                      NO-LOCK NO-ERROR.
         FIND item WHERE item.it-codigo = amostra-fam.it-codigo
                   NO-LOCK NO-ERROR.

         display stream str-rp
                 amostra-fam.fm-codigo   
                 amostra-fam.it-codigo
                 item.desc-item WHEN AVAIL ITEM
                 amostra-fam.ano-mes
                 amostra-fam.num-amostra
                 amostra-fam.tipo-tear 
                 amostra-fam.merceriz 
                 amostra-fam.larg-ini 
                 amostra-fam.enc-urd
                 amostra-fam.enc-tra 
                 amostra-fam.larg-fin
                 amostra-fam.num-ob
                 amostra-fam.observ
                 with frame f-analitico-ite.
         down stream str-rp with frame f-analitico-ite.

         IF LAST-OF(amostra-fam.ano-mes) THEN DO:
            DISPLAY STREAM str-rp
                    "M‚dia"                      @ amostra-fam.ano-mes
                    de-larg-ini-per / i-cont-per @ amostra-fam.larg-ini
                    de-enc-urd-per  / i-cont-per @ amostra-fam.enc-urd
                    de-enc-tra-per  / i-cont-per @ amostra-fam.enc-tra
                    de-larg-fin-per / i-cont-per @ amostra-fam.larg-fin
                    WITH FRAME f-analitico-fam.
            DOWN STREAM str-rp WITH FRAME f-analitico-ite.
            ASSIGN i-cont-per      = 0
                   de-larg-ini-per = 0
                   de-enc-urd-per  = 0
                   de-enc-tra-per  = 0
                   de-larg-fin-per = 0.
         END.
         IF LAST-OF(amostra-fam.fm-codigo) THEN DO:
            DISPLAY STREAM str-rp
                    "M‚dia"                      @ amostra-fam.fm-codigo
                    de-larg-ini-fam / i-cont-fam @ amostra-fam.larg-ini
                    de-enc-urd-fam  / i-cont-fam @ amostra-fam.enc-urd
                    de-enc-tra-fam  / i-cont-fam @ amostra-fam.enc-tra
                    de-larg-fin-fam / i-cont-fam @ amostra-fam.larg-fin
                    WITH FRAME f-analitico-ite.
            DOWN 2 STREAM str-rp WITH FRAME f-analitico-ite.
            ASSIGN i-cont-fam      = 0
                   de-larg-ini-fam = 0
                   de-enc-urd-fam  = 0
                   de-enc-tra-fam  = 0
                   de-larg-fin-fam = 0.
         END.
      END.
   END.
   IF tt-param.tipo-relatorio = 1 THEN DO: /* Sint‚tico */
      DISPLAY STREAM str-rp
                     "MDIA"                      @ amostra-fam.fm-codigo
                     de-larg-ini-ger / i-cont-ger @ amostra-fam.larg-ini
                     de-enc-urd-ger  / i-cont-ger @ amostra-fam.enc-urd
                     de-enc-tra-ger  / i-cont-ger @ amostra-fam.enc-tra
                     de-larg-fin-ger / i-cont-ger @ amostra-fam.larg-fin
                     WITH FRAME f-sintetico-ite.
      DOWN 2 STREAM str-rp WITH FRAME f-sintetico-ite.
   END.
   ELSE DO: /* Anal¡tico */
      DISPLAY STREAM str-rp
                     "MDIA"                      @ amostra-fam.fm-codigo
                     de-larg-ini-ger / i-cont-ger @ amostra-fam.larg-ini
                     de-enc-urd-ger  / i-cont-ger @ amostra-fam.enc-urd
                     de-enc-tra-ger  / i-cont-ger @ amostra-fam.enc-tra
                     de-larg-fin-ger / i-cont-ger @ amostra-fam.larg-fin
                     WITH FRAME f-analitico-ite.
      DOWN 2 STREAM str-rp WITH FRAME f-analitico-ite.
   END.
END.
else do: /* Por Data */
   FOR each amostra-fam no-lock
       where amostra-fam.fm-codigo >= tt-param.fi-ini-fm-codigo
         AND amostra-fam.fm-codigo <= tt-param.fi-fin-fm-codigo
         AND amostra-fam.it-codigo >= tt-param.fi-ini-it-codigo
         AND amostra-fam.it-codigo <= tt-param.fi-fin-it-codigo
         AND amostra-fam.ano-mes   >= tt-param.fi-ini-ano-mes
         AND amostra-fam.ano-mes   <= tt-param.fi-fin-ano-mes
         AND amostra-fam.tipo-tear >= tt-param.fi-ini-tipo-tear
         AND amostra-fam.tipo-tear <= tt-param.fi-fin-tipo-tear
         AND ((amostra-fam.merceriz = YES AND tt-param.acabamento = 1) OR
              (amostra-fam.merceriz = NO  AND tt-param.acabamento = 2) OR
                                              tt-param.acabamento = 3)
   BREAK BY amostra-fam.ano-mes
         BY amostra-fam.fm-codigo
   on stop undo,leave:
      run pi-acompanhar in h-acomp (input string(amostra-fam.fm-codigo)).

      ASSIGN i-cont-per = i-cont-per + 1
             i-cont-fam = i-cont-fam + 1
             i-cont-ger = i-cont-ger + 1
             de-larg-ini-per = de-larg-ini-per + amostra-fam.larg-ini
             de-larg-ini-fam = de-larg-ini-fam + amostra-fam.larg-ini
             de-larg-ini-ger = de-larg-ini-ger + amostra-fam.larg-ini
             de-enc-urd-per  = de-enc-urd-per  + amostra-fam.enc-urd
             de-enc-urd-fam  = de-enc-urd-fam  + amostra-fam.enc-urd
             de-enc-urd-ger  = de-enc-urd-ger  + amostra-fam.enc-urd
             de-enc-tra-per  = de-enc-tra-per  + amostra-fam.enc-tra
             de-enc-tra-fam  = de-enc-tra-fam  + amostra-fam.enc-tra
             de-enc-tra-ger  = de-enc-tra-ger  + amostra-fam.enc-tra
             de-larg-fin-per = de-larg-fin-per + amostra-fam.larg-fin
             de-larg-fin-fam = de-larg-fin-fam + amostra-fam.larg-fin
             de-larg-fin-ger = de-larg-fin-ger + amostra-fam.larg-fin.

      IF tt-param.tipo-relatorio = 1 THEN do: /* Sint‚tico */
         IF FIRST-OF(amostra-fam.ano-mes) THEN
            DISPLAY STREAM str-rp
                    amostra-fam.ano-mes
                    WITH FRAME f-sintetico-dat.
         
         IF LAST-OF(amostra-fam.fm-codigo) THEN DO:
             FIND familia WHERE familia.fm-codigo = amostra-fam.fm-codigo
                          NO-LOCK NO-ERROR.
             FIND item WHERE item.it-codigo = amostra-fam.it-codigo
                       NO-LOCK NO-ERROR.

             DISPLAY STREAM str-rp
                    amostra-fam.fm-codigo
                    amostra-fam.it-codigo
                    item.desc-item WHEN AVAIL ITEM
                    de-larg-ini-fam / i-cont-fam @ amostra-fam.larg-ini
                    de-enc-urd-fam  / i-cont-fam @ amostra-fam.enc-urd
                    de-enc-tra-fam  / i-cont-fam @ amostra-fam.enc-tra
                    de-larg-fin-fam / i-cont-fam @ amostra-fam.larg-fin
                    WITH FRAME f-sintetico-dat.
            DOWN STREAM str-rp WITH FRAME f-sintetico-dat.
            ASSIGN i-cont-fam      = 0
                   de-larg-ini-fam = 0
                   de-enc-urd-fam  = 0
                   de-enc-tra-fam  = 0
                   de-larg-fin-fam = 0.
         END.

         IF LAST-OF(amostra-fam.ano-mes) THEN DO:
            DISPLAY STREAM str-rp
                    "M‚dia"                      @ amostra-fam.ano-mes
                    de-larg-ini-per / i-cont-per @ amostra-fam.larg-ini
                    de-enc-urd-per  / i-cont-per @ amostra-fam.enc-urd
                    de-enc-tra-per  / i-cont-per @ amostra-fam.enc-tra
                    de-larg-fin-per / i-cont-per @ amostra-fam.larg-fin
                    WITH FRAME f-sintetico-dat.
            DOWN 2 STREAM str-rp WITH FRAME f-sintetico-dat.
            ASSIGN i-cont-per      = 0
                   de-larg-ini-per = 0
                   de-enc-urd-per  = 0
                   de-enc-tra-per  = 0
                   de-larg-fin-per = 0.
         END.
      END.
      ELSE DO: /*Anal¡tico */
         FIND familia WHERE familia.fm-codigo = amostra-fam.fm-codigo
                      NO-LOCK NO-ERROR.
   
         display stream str-rp
                 amostra-fam.ano-mes 
                 amostra-fam.fm-codigo 
                 amostra-fam.it-codigo
                 item.desc-item WHEN AVAIL ITEM
                 amostra-fam.num-amostra
                 amostra-fam.tipo-tear 
                 amostra-fam.merceriz 
                 amostra-fam.larg-ini 
                 amostra-fam.enc-urd
                 amostra-fam.enc-tra 
                 amostra-fam.larg-fin
                 amostra-fam.num-ob
                 amostra-fam.observ
                 with frame f-analitico-dat.
         down stream str-rp with frame f-analitico-dat.
         
         IF LAST-OF(amostra-fam.fm-codigo) THEN DO:
            DISPLAY STREAM str-rp
                    "M‚dia"                      @ amostra-fam.fm-codigo
                    de-larg-ini-fam / i-cont-fam @ amostra-fam.larg-ini
                    de-enc-urd-fam  / i-cont-fam @ amostra-fam.enc-urd
                    de-enc-tra-fam  / i-cont-fam @ amostra-fam.enc-tra
                    de-larg-fin-fam / i-cont-fam @ amostra-fam.larg-fin
                    WITH FRAME f-analitico-dat.
            DOWN STREAM str-rp WITH FRAME f-analitico-dat.
            ASSIGN i-cont-fam      = 0
                   de-larg-ini-fam = 0
                   de-enc-urd-fam  = 0
                   de-enc-tra-fam  = 0
                   de-larg-fin-fam = 0.
         END.

         IF LAST-OF(amostra-fam.ano-mes) THEN DO:
            DISPLAY STREAM str-rp
                    "M‚dia"                      @ amostra-fam.ano-mes
                    de-larg-ini-per / i-cont-per @ amostra-fam.larg-ini
                    de-enc-urd-per  / i-cont-per @ amostra-fam.enc-urd
                    de-enc-tra-per  / i-cont-per @ amostra-fam.enc-tra
                    de-larg-fin-per / i-cont-per @ amostra-fam.larg-fin
                    WITH FRAME f-analitico-dat.
            DOWN 2 STREAM str-rp WITH FRAME f-analitico-dat.
            ASSIGN i-cont-per      = 0
                   de-larg-ini-per = 0
                   de-enc-urd-per  = 0
                   de-enc-tra-per  = 0
                   de-larg-fin-per = 0.
         END.
      END.
   END.
   IF tt-param.tipo-relatorio = 1 THEN do: /* Sint‚tico */
      DISPLAY STREAM str-rp
                     "MDIA"                      @ amostra-fam.ano-mes
                     de-larg-ini-ger / i-cont-ger @ amostra-fam.larg-ini
                     de-enc-urd-ger  / i-cont-ger @ amostra-fam.enc-urd
                     de-enc-tra-ger  / i-cont-ger @ amostra-fam.enc-tra
                     de-larg-fin-ger / i-cont-ger @ amostra-fam.larg-fin
                     WITH FRAME f-sintetico-dat.
      DOWN 2 STREAM str-rp WITH FRAME f-sintetico-dat.
   END.
   ELSE do: /* Anal¡tico */
      DISPLAY STREAM str-rp
                     "MDIA"                      @ amostra-fam.ano-mes
                     de-larg-ini-ger / i-cont-ger @ amostra-fam.larg-ini
                     de-enc-urd-ger  / i-cont-ger @ amostra-fam.enc-urd
                     de-enc-tra-ger  / i-cont-ger @ amostra-fam.enc-tra
                     de-larg-fin-ger / i-cont-ger @ amostra-fam.larg-fin
                     WITH FRAME f-analitico-dat.
      DOWN 2 STREAM str-rp WITH FRAME f-analitico-dat.
   END.
END.

IF tt-param.imp-param THEN
   display STREAM str-rp
           tt-param.desc-classifica
           tt-param.fi-ini-fm-codigo              
           tt-param.fi-fin-fm-codigo 
           tt-param.fi-ini-it-codigo
           tt-param.fi-fin-it-codigo
           tt-param.fi-ini-ano-mes                
           tt-param.fi-fin-ano-mes
           tt-param.fi-ini-tipo-tear
           tt-param.fi-fin-tipo-tear
           tt-param.desc-acabamento
           tt-param.desc-tipo-rel
           with frame f-param.           

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i &STREAM="stream str-rp"}
run pi-finalizar in h-acomp.
return "OK":U.
