/* Programa: ESSP0067RP.P (Chamado pelo programa ESSP0067.W)
** Objetivo: Imprimir o relat¢rio de Testes de ASTM
**           (tabela: teste-astm)
** Sistema.: Espec¡ficos
** M¢dulo..: Espec¡ficos
** Autor...: Fabio Coelho Lanza - Janeiro/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0067RP 2.04.00.000} 
/* defini‡Æo das temp-tables para recebimento de parƒmetros */

DEFINE TEMP-TABLE tt-param NO-UNDO
       FIELD destino             AS INTEGER
       FIELD arquivo             AS CHAR FORMAT "x(35)"
       FIELD usuario             AS CHAR FORMAT "x(12)"
       FIELD data-exec           AS DATE
       FIELD hora-exec           AS INTEGER
       FIELD classifica          AS INTEGER
       FIELD desc-classifica     AS CHAR FORMAT "x(40)"
       FIELD fi-ini-fm-codigo    LIKE teste-astm.fm-codigo
       FIELD fi-fin-fm-codigo    LIKE teste-astm.fm-codigo
       FIELD fi-ini-it-codigo    LIKE teste-astm.it-codigo
       FIELD fi-fin-it-codigo    LIKE teste-astm.it-codigo
       FIELD fi-ini-ano-mes      LIKE teste-astm.ano-mes    
       FIELD fi-fin-ano-mes      LIKE teste-astm.ano-mes    
       FIELD fi-ini-tipo-tear    LIKE teste-astm.tipo-tear
       FIELD fi-fin-tipo-tear    LIKE teste-astm.tipo-tear
       FIELD fi-ini-num-ob       LIKE teste-astm.num-ob
       FIELD fi-fin-num-ob       LIKE teste-astm.num-ob
       FIELD fi-ini-enc-tra      LIKE teste-astm.enc-tra     
       FIELD fi-fin-enc-tra      LIKE teste-astm.enc-tra
       FIELD fi-ini-enc-urd      LIKE teste-astm.enc-urd
       FIELD fi-fin-enc-urd      LIKE teste-astm.enc-urd
       FIELD fi-ini-dentra-ini   LIKE teste-astm.dentra-ini
       FIELD fi-fin-dentra-ini   LIKE teste-astm.dentra-ini
       FIELD fi-ini-dentra-fin   LIKE teste-astm.dentra-fin
       FIELD fi-fin-dentra-fin   LIKE teste-astm.dentra-fin
       FIELD tipo-acabamento     AS INTEGER
       FIELD desc-tipo-acab      AS CHAR FORMAT "x(15)"
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
DEF VAR c-desc-item             LIKE ITEM.desc-item.
def var de-tot-enc-tra          as dec.
def var de-tot-enc-urd          as dec.
def var de-tot-dentra-ini       as dec.
def var de-tot-dentra-fin       as dec.
def var i-cont-fam              as int  format ">>>9".
def var i-cont-dat              as int  format ">>>9".
def var i-cont-ger              as int.

/* defini‡Æo de frames do relat¢rio */
FORM 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
   tt-param.desc-classifica     LABEL "Classificacao....."  SKIP
   tt-param.fi-ini-fm-codigo    LABEL "Familia..........."
   "A"  AT 32
   tt-param.fi-fin-fm-codigo    NO-LABELS             SKIP
   tt-param.fi-ini-it-codigo    LABEL "Item.............."
   "A"  AT 32
   tt-param.fi-fin-it-codigo    NO-LABELS             SKIP
   tt-param.fi-ini-ano-mes      LABEL "Periodo..........."
   "A"  AT 32
   tt-param.fi-fin-ano-mes      NO-LABELS             SKIP
   tt-param.fi-ini-tipo-tear    LABEL "Tipo Tear........."
   "A"  AT 32
   tt-param.fi-fin-tipo-tear    NO-LABELS             SKIP
   tt-param.fi-ini-num-ob       LABEL "Numero da OB......"
   "A"  AT 32
   tt-param.fi-fin-num-ob       NO-LABELS             SKIP
   tt-param.fi-ini-enc-tra      LABEL "Encolh. da Trama.."
   "A"  AT 32
   tt-param.fi-fin-enc-tra      NO-LABELS             SKIP
   tt-param.fi-ini-enc-urd      LABEL "Encolh. Urdideira."
   "A"  AT 32
   tt-param.fi-fin-enc-urd      NO-LABELS             SKIP 
   tt-param.fi-ini-dentra-ini   LABEL "Dens.Trama Inicial"
   "A"  AT 32
   tt-param.fi-fin-dentra-ini   NO-LABELS             SKIP 
   tt-param.fi-ini-dentra-fin   LABEL "Dens.Trama Final.."
   "A"  AT 32
   tt-param.fi-fin-dentra-fin   NO-LABELS             SKIP 
   tt-param.desc-tipo-rel       LABEL "Tipo do Relatorio."  SKIP
   tt-param.desc-tipo-acab      LABEL "Acabamento........"
   with FRAME f-param side-labels no-box WIDTH 133 STREAM-IO.

FORM   
   teste-astm.fm-codigo                  LABEL "Familia"
   teste-astm.it-codigo   FORMAT "x(6)"  LABEL "Item"
   ITEM.desc-item         FORMAT "x(32)" LABEL "Descricao"
   teste-astm.ano-mes                    LABEL "Per¡odo"
   teste-astm.num-amostra                LABEL "NS"
   teste-astm.tipo-tear                  LABEL "Tp-Tear"
   teste-astm.num-ob                     LABEL "Num.OB"
   teste-astm.merceriz                   LABEL "Merc"
   teste-astm.observ                     LABEL "Observacao"
   teste-astm.dentra-ini                 LABEL "DenIni"
   teste-astm.enc-tra                    LABEL "EncTra"
   teste-astm.enc-urd                    LABEL "EncUrd"
   teste-astm.dentra-fin                 LABEL "DenFin"
   i-cont-fam                            LABEL "Tsts"
   with frame f-analitico-fam NO-LABEL WIDTH 133 down stream-io.

FORM   
   teste-astm.it-codigo   FORMAT "x(6)"  LABEL "Item"
   ITEM.desc-item         FORMAT "x(32)" LABEL "Descricao"
   teste-astm.fm-codigo                  LABEL "Familia"
   teste-astm.ano-mes                    LABEL "Per¡odo"
   teste-astm.num-amostra                LABEL "NS"
   teste-astm.tipo-tear                  LABEL "Tp-Tear"
   teste-astm.num-ob                     LABEL "Num.OB"
   teste-astm.merceriz                   LABEL "Merc"
   teste-astm.observ                     LABEL "Observacao"
   teste-astm.dentra-ini                 LABEL "DenIni"
   teste-astm.enc-tra                    LABEL "EncTra"
   teste-astm.enc-urd                    LABEL "EncUrd"
   teste-astm.dentra-fin                 LABEL "DenFin"
   i-cont-fam                            LABEL "Tsts"
   with frame f-analitico-ite NO-LABEL WIDTH 133 down stream-io.

FORM   
   teste-astm.fm-codigo                 LABEL "Familia"
   teste-astm.it-codigo  FORMAT "x(6)"  LABEL "Item"
   ITEM.desc-item        FORMAT "x(32)" LABEL "Descricao"
   teste-astm.ano-mes                   LABEL "Per¡odo"
   teste-astm.tipo-tear                 LABEL "Maq."
   teste-astm.num-ob                    LABEL "Num.OB"
   teste-astm.merceriz                  LABEL "Merc"
   teste-astm.observ                    LABEL "Observacao"
   teste-astm.dentra-ini                LABEL "DenIni"
   teste-astm.enc-tra                   LABEL "EncTra"
   teste-astm.enc-urd                   LABEL "EncUrd"
   teste-astm.dentra-fin                LABEL "DenFin"
   i-cont-fam                           LABEL "Tsts"
   with frame f-sintetico-fam NO-LABEL WIDTH 133 down stream-io.

FORM   
   teste-astm.it-codigo  FORMAT "x(6)"  LABEL "Item"
   ITEM.desc-item        FORMAT "x(32)" LABEL "Descricao"
   teste-astm.fm-codigo                 LABEL "Familia"
   teste-astm.ano-mes                   LABEL "Per¡odo"
   teste-astm.tipo-tear                 LABEL "Maq."
   teste-astm.num-ob                    LABEL "Num.OB"
   teste-astm.merceriz                  LABEL "Merc"
   teste-astm.observ                    LABEL "Observacao"
   teste-astm.dentra-ini                LABEL "DenIni"
   teste-astm.enc-tra                   LABEL "EncTra"
   teste-astm.enc-urd                   LABEL "EncUrd"
   teste-astm.dentra-fin                LABEL "DenFin"
   i-cont-fam                           LABEL "Tsts"
   with frame f-sintetico-ite NO-LABEL WIDTH 133 down stream-io.

FORM
   teste-astm.ano-mes                    LABEL "Per¡odo"
   teste-astm.fm-codigo                  LABEL "Familia"
   teste-astm.it-codigo   FORMAT "x(6)"  LABEL "Item"
   item.desc-item         FORMAT "x(32)" LABEL "Descricao"
   teste-astm.num-amostra                LABEL "NS"
   teste-astm.tipo-tear                  LABEL "Maq."
   teste-astm.num-ob                     LABEL "Num.OB"
   teste-astm.merceriz                   LABEL "Merc"
   teste-astm.observ                     LABEL "Observacao"
   teste-astm.dentra-ini                 LABEL "DenIni"
   teste-astm.enc-tra                    LABEL "EncTra"
   teste-astm.enc-urd                    LABEL "EncUrd"
   teste-astm.dentra-fin                 LABEL "DenFin"
   i-cont-dat                            LABEL "Tsts"
   with frame f-analitico-dat NO-LABEL WIDTH 133 down stream-io.

FORM   
   teste-astm.ano-mes                   LABEL "Per¡odo"
   teste-astm.fm-codigo                 LABEL "Familia"
   teste-astm.it-codigo  FORMAT "x(6)"  LABEL "Item"
   item.desc-item        FORMAT "x(32)" LABEL "Descricao"
   teste-astm.tipo-tear                 LABEL "Tp-Tear"
   teste-astm.num-ob                    LABEL "Num.OB"
   teste-astm.merceriz                  LABEL "Merc"
   teste-astm.observ                    LABEL "Observacao"
   teste-astm.dentra-ini                LABEL "DenIni"
   teste-astm.enc-tra                   LABEL "EncTra"
   teste-astm.enc-urd                   LABEL "EncUrd"
   teste-astm.dentra-fin                LABEL "DenFin"
   i-cont-dat                           LABEL "Tsts"
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
{utp/ut-liter.i Testes_de_ASTM * r}
assign c-titulo-relat = trim(return-value).

view stream str-rp frame f-cabec.
view stream str-rp frame f-rodape.
run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

if tt-param.classific = 1 then do: /* Por Familia */
   FOR EACH teste-astm 
      WHERE teste-astm.fm-codigo    >= tt-param.fi-ini-fm-codigo  
        AND teste-astm.fm-codigo    <= tt-param.fi-fin-fm-codigo  
        AND teste-astm.it-codigo    >= tt-param.fi-ini-it-codigo
        AND teste-astm.it-codigo    <= tt-param.fi-fin-it-codigo
        AND teste-astm.ano-mes      >= tt-param.fi-ini-ano-mes   
        AND teste-astm.ano-mes      <= tt-param.fi-fin-ano-mes    
        AND teste-astm.tipo-tear    >= tt-param.fi-ini-tipo-tear     
        AND teste-astm.tipo-tear    <= tt-param.fi-fin-tipo-tear     
        AND teste-astm.num-ob       >= tt-param.fi-ini-num-ob     
        AND teste-astm.num-ob       <= tt-param.fi-fin-num-ob     
        AND teste-astm.enc-tra      >= tt-param.fi-ini-enc-tra     
        AND teste-astm.enc-tra      <= tt-param.fi-fin-enc-tra     
        AND teste-astm.enc-urd      >= tt-param.fi-ini-enc-urd       
        AND teste-astm.enc-urd      <= tt-param.fi-fin-enc-urd    
        AND teste-astm.dentra-ini   >= tt-param.fi-ini-dentra-ini   
        AND teste-astm.dentra-ini   <= tt-param.fi-fin-dentra-ini    
        AND teste-astm.dentra-fin   >= tt-param.fi-ini-dentra-fin   
        AND teste-astm.dentra-fin   <= tt-param.fi-fin-dentra-fin 
        AND ((teste-astm.merceriz = YES AND tipo-acabamento = 1) OR
             (teste-astm.merceriz = NO  AND tipo-acabamento = 2) OR
             (tipo-acabamento = 3))
      BREAK BY teste-astm.fm-codigo
      on stop undo,leave:
      run pi-acompanhar in h-acomp (input string(teste-astm.fm-codigo)).

      FIND item WHERE item.it-codigo = teste-astm.it-codigo NO-LOCK NO-ERROR.
      FIND familia WHERE familia.fm-codigo = teste-astm.fm-codigo NO-LOCK NO-ERROR.
      IF teste-astm.it-codigo = "" THEN DO:
         IF AVAIL familia THEN
            ASSIGN c-desc-item = familia.descricao.
         ELSE
            ASSIGN c-desc-item = "".
      END.
      ELSE DO:
         IF AVAIL ITEM THEN
            ASSIGN c-desc-item = ITEM.desc-item.
         ELSE
            ASSIGN c-desc-item = "".
      END.

      IF tt-param.tipo-relatorio = 2 THEN DO: /* ANALITICO */
         DISPLAY STREAM str-rp
                 teste-astm.fm-codigo WHEN FIRST-OF(teste-astm.fm-codigo)
                 teste-astm.it-codigo WHEN AVAIL ITEM
                 item.desc-item       WHEN AVAIL ITEM
                 c-desc-item          @ item.desc-item 
                 teste-astm.ano-mes         
                 teste-astm.num-amostra     
                 teste-astm.tipo-tear       
                 teste-astm.num-ob          
                 teste-astm.merceriz        
                 teste-astm.observ          
                 teste-astm.dentra-ini      
                 teste-astm.enc-tra         
                 teste-astm.enc-urd         
                 teste-astm.dentra-fin      
                 with frame f-analitico-fam.
         down stream str-rp with frame f-analitico-fam.
      END.
      assign i-cont-fam        = i-cont-fam + 1
             de-tot-enc-tra    = de-tot-enc-tra    + teste-astm.enc-tra    
             de-tot-enc-urd    = de-tot-enc-urd    + teste-astm.enc-urd     
             de-tot-dentra-ini = de-tot-dentra-ini + teste-astm.dentra-ini  
             de-tot-dentra-fin = de-tot-dentra-fin + teste-astm.dentra-fin.

      IF LAST-OF(teste-astm.fm-codigo) THEN DO:
          IF tt-param.tipo-relatorio = 2 THEN DO: /* ANALITICO */
             DISPLAY STREAM str-rp
                     "MEDIA"                        @ teste-astm.ano-mes
                     de-tot-enc-tra    / i-cont-fam @ teste-astm.enc-tra
                     de-tot-enc-urd    / i-cont-fam @ teste-astm.enc-urd
                     de-tot-dentra-ini / i-cont-fam @ teste-astm.dentra-ini
                     de-tot-dentra-fin / i-cont-fam @ teste-astm.dentra-fin
                     i-cont-fam
                     WITH frame f-analitico-fam.
             DOWN STREAM str-rp WITH FRAME f-analitico-fam.
          END.
          ELSE DO: /* SINTTICO */
              DISPLAY STREAM str-rp
                      teste-astm.fm-codigo
                      teste-astm.it-codigo
                      c-desc-item @ item.desc-item
                      "MEDIA"                        @ teste-astm.ano-mes
                      de-tot-enc-tra    / i-cont-fam @ teste-astm.enc-tra
                      de-tot-enc-urd    / i-cont-fam @ teste-astm.enc-urd
                      de-tot-dentra-ini / i-cont-fam @ teste-astm.dentra-ini
                      de-tot-dentra-fin / i-cont-fam @ teste-astm.dentra-fin
                      i-cont-fam
                      WITH frame f-analitico-fam.
              DOWN STREAM str-rp WITH FRAME f-analitico-fam.
          END.
          ASSIGN i-cont-fam        = 0
                 de-tot-enc-tra    = 0 
                 de-tot-enc-urd    = 0 
                 de-tot-dentra-ini = 0 
                 de-tot-dentra-fin = 0. 
      END.
   END.
END.
ELSE
if tt-param.classific = 2 then do: /* Por Ötem */
   FOR EACH teste-astm 
      WHERE teste-astm.fm-codigo    >= tt-param.fi-ini-fm-codigo  
        AND teste-astm.fm-codigo    <= tt-param.fi-fin-fm-codigo  
        AND teste-astm.it-codigo    >= tt-param.fi-ini-it-codigo
        AND teste-astm.it-codigo    <= tt-param.fi-fin-it-codigo
        AND teste-astm.ano-mes      >= tt-param.fi-ini-ano-mes   
        AND teste-astm.ano-mes      <= tt-param.fi-fin-ano-mes    
        AND teste-astm.tipo-tear    >= tt-param.fi-ini-tipo-tear     
        AND teste-astm.tipo-tear    <= tt-param.fi-fin-tipo-tear     
        AND teste-astm.num-ob       >= tt-param.fi-ini-num-ob     
        AND teste-astm.num-ob       <= tt-param.fi-fin-num-ob     
        AND teste-astm.enc-tra      >= tt-param.fi-ini-enc-tra     
        AND teste-astm.enc-tra      <= tt-param.fi-fin-enc-tra     
        AND teste-astm.enc-urd      >= tt-param.fi-ini-enc-urd       
        AND teste-astm.enc-urd      <= tt-param.fi-fin-enc-urd    
        AND teste-astm.dentra-ini   >= tt-param.fi-ini-dentra-ini   
        AND teste-astm.dentra-ini   <= tt-param.fi-fin-dentra-ini    
        AND teste-astm.dentra-fin   >= tt-param.fi-ini-dentra-fin   
        AND teste-astm.dentra-fin   <= tt-param.fi-fin-dentra-fin 
        AND ((teste-astm.merceriz = YES AND tipo-acabamento = 1) OR
             (teste-astm.merceriz = NO  AND tipo-acabamento = 2) OR
             (tipo-acabamento = 3))
      BREAK BY teste-astm.it-codigo
            BY teste-astm.fm-codigo
      on stop undo,leave:
      run pi-acompanhar in h-acomp (input string(teste-astm.fm-codigo)).

      FIND item WHERE item.it-codigo = teste-astm.it-codigo NO-LOCK NO-ERROR.
      FIND familia WHERE familia.fm-codigo = teste-astm.fm-codigo NO-LOCK NO-ERROR.
      IF teste-astm.it-codigo = "" THEN DO:
         IF AVAIL familia THEN
            ASSIGN c-desc-item = familia.descricao.
         ELSE
            ASSIGN c-desc-item = "".
      END.
      ELSE DO:
         IF AVAIL ITEM THEN
            ASSIGN c-desc-item = ITEM.desc-item.
         ELSE
            ASSIGN c-desc-item = "".
      END.

      IF tt-param.tipo-relatorio = 2 THEN DO: /* ANALITICO */
         DISPLAY STREAM str-rp
                 teste-astm.fm-codigo WHEN FIRST-OF(teste-astm.fm-codigo)
                 teste-astm.it-codigo WHEN AVAIL ITEM
                 item.desc-item       WHEN AVAIL ITEM
                 c-desc-item          @ item.desc-item 
                 teste-astm.ano-mes         
                 teste-astm.num-amostra     
                 teste-astm.tipo-tear       
                 teste-astm.num-ob          
                 teste-astm.merceriz        
                 teste-astm.observ          
                 teste-astm.dentra-ini      
                 teste-astm.enc-tra         
                 teste-astm.enc-urd         
                 teste-astm.dentra-fin      
                 with frame f-analitico-ite.
         down stream str-rp with frame f-analitico-ite.
      END.
      assign i-cont-fam        = i-cont-fam + 1
             de-tot-enc-tra    = de-tot-enc-tra    + teste-astm.enc-tra    
             de-tot-enc-urd    = de-tot-enc-urd    + teste-astm.enc-urd     
             de-tot-dentra-ini = de-tot-dentra-ini + teste-astm.dentra-ini  
             de-tot-dentra-fin = de-tot-dentra-fin + teste-astm.dentra-fin.

      IF LAST-OF(teste-astm.fm-codigo) THEN DO:
          IF tt-param.tipo-relatorio = 2 THEN DO: /* ANALITICO */
             DISPLAY STREAM str-rp
                     "MEDIA"                        @ teste-astm.ano-mes
                     de-tot-enc-tra    / i-cont-fam @ teste-astm.enc-tra
                     de-tot-enc-urd    / i-cont-fam @ teste-astm.enc-urd
                     de-tot-dentra-ini / i-cont-fam @ teste-astm.dentra-ini
                     de-tot-dentra-fin / i-cont-fam @ teste-astm.dentra-fin
                     i-cont-fam
                     WITH frame f-analitico-ite.
             DOWN STREAM str-rp WITH FRAME f-analitico-ite.
          END.
          ELSE DO: /* SINTTICO */
              DISPLAY STREAM str-rp
                      teste-astm.fm-codigo
                      teste-astm.it-codigo
                      c-desc-item @ item.desc-item
                      "MEDIA"                        @ teste-astm.ano-mes
                      de-tot-enc-tra    / i-cont-fam @ teste-astm.enc-tra
                      de-tot-enc-urd    / i-cont-fam @ teste-astm.enc-urd
                      de-tot-dentra-ini / i-cont-fam @ teste-astm.dentra-ini
                      de-tot-dentra-fin / i-cont-fam @ teste-astm.dentra-fin
                      i-cont-fam
                      WITH frame f-analitico-ite.
              DOWN STREAM str-rp WITH FRAME f-analitico-ite.
          END.
          ASSIGN i-cont-fam        = 0
                 de-tot-enc-tra    = 0 
                 de-tot-enc-urd    = 0 
                 de-tot-dentra-ini = 0 
                 de-tot-dentra-fin = 0. 
      END.
   END.
END.
else do: /* POR PERÖODO */
   FOR EACH  teste-astm NO-LOCK
       where teste-astm.ano-mes      >= tt-param.fi-ini-ano-mes    
         AND teste-astm.ano-mes      <= tt-param.fi-fin-ano-mes    
         AND teste-astm.fm-codigo    >= tt-param.fi-ini-fm-codigo 
         AND teste-astm.fm-codigo    <= tt-param.fi-fin-fm-codigo 
         AND teste-astm.it-codigo    >= tt-param.fi-ini-it-codigo
         AND teste-astm.it-codigo    <= tt-param.fi-fin-it-codigo
         AND teste-astm.tipo-tear    >= tt-param.fi-ini-tipo-tear     
         AND teste-astm.tipo-tear    <= tt-param.fi-fin-tipo-tear     
         AND teste-astm.num-ob       >= tt-param.fi-ini-num-ob     
         AND teste-astm.num-ob       <= tt-param.fi-fin-num-ob     
         AND teste-astm.enc-tra      >= tt-param.fi-ini-enc-tra     
         AND teste-astm.enc-tra      <= tt-param.fi-fin-enc-tra     
         AND teste-astm.enc-urd      >= tt-param.fi-ini-enc-urd       
         AND teste-astm.enc-urd      <= tt-param.fi-fin-enc-urd    
         AND teste-astm.dentra-ini   >= tt-param.fi-ini-dentra-ini   
         AND teste-astm.dentra-ini   <= tt-param.fi-fin-dentra-ini    
         AND teste-astm.dentra-fin   >= tt-param.fi-ini-dentra-fin   
         AND teste-astm.dentra-fin   <= tt-param.fi-fin-dentra-fin 
         AND ((teste-astm.merceriz = YES AND tipo-acabamento = 1) OR
              (teste-astm.merceriz = NO  AND tipo-acabamento = 2) OR
              (tipo-acabamento = 3))
       BREAK BY teste-astm.ano-mes   
             BY teste-astm.fm-codigo
             on stop undo,leave:
       run pi-acompanhar in h-acomp (input string(teste-astm.ano-mes)).

       FIND item WHERE item.it-codigo = teste-astm.it-codigo NO-LOCK NO-ERROR.
       FIND familia WHERE familia.fm-codigo = teste-astm.fm-codigo NO-LOCK NO-ERROR.
       IF teste-astm.it-codigo = "" THEN DO:
          IF AVAIL familia THEN
             ASSIGN c-desc-item = familia.descricao.
          ELSE
             ASSIGN c-desc-item = "".
       END.
       ELSE DO:
          IF AVAIL ITEM THEN
             ASSIGN c-desc-item = ITEM.desc-item.
          ELSE
             ASSIGN c-desc-item = "".
       END.

       IF tt-param.tipo-relatorio = 2 THEN DO: /* ANALITICO */
          DISPLAY STREAM str-rp
                  teste-astm.ano-mes    WHEN FIRST-OF(teste-astm.ano-mes)
                  teste-astm.fm-codigo       
                  teste-astm.it-codigo       WHEN AVAIL ITEM
                  c-desc-item                @ item.desc-item
                  teste-astm.num-amostra     
                  teste-astm.tipo-tear       
                  teste-astm.num-ob          
                  teste-astm.merceriz        
                  teste-astm.observ          
                  teste-astm.dentra-ini      
                  teste-astm.enc-tra         
                  teste-astm.enc-urd         
                  teste-astm.dentra-fin      
                  with frame f-analitico-dat.
          down stream str-rp with frame f-analitico-dat.
       END.
       assign i-cont-dat      = i-cont-dat + 1
              de-tot-enc-tra    = de-tot-enc-tra    + teste-astm.enc-tra    
              de-tot-enc-urd    = de-tot-enc-urd    + teste-astm.enc-urd     
              de-tot-dentra-ini = de-tot-dentra-ini + teste-astm.dentra-ini  
              de-tot-dentra-fin = de-tot-dentra-fin + teste-astm.dentra-fin.

       IF LAST-OF(teste-astm.ano-mes) THEN DO:
           IF tt-param.tipo-relatorio = 2 THEN DO: /* ANALITICO */
              DISPLAY STREAM str-rp
                      "MEDIA"                        @ teste-astm.ano-mes
                      de-tot-enc-tra    / i-cont-dat @ teste-astm.enc-tra
                      de-tot-enc-urd    / i-cont-dat @ teste-astm.enc-urd
                      de-tot-dentra-ini / i-cont-dat @ teste-astm.dentra-ini
                      de-tot-dentra-fin / i-cont-dat @ teste-astm.dentra-fin
                      i-cont-dat
                      WITH frame f-analitico-dat.
              DOWN STREAM str-rp WITH FRAME f-analitico-dat.
           END.
           ELSE DO: /* SINTTICO */
               DISPLAY STREAM str-rp
                       teste-astm.ano-mes
                       de-tot-enc-tra    / i-cont-dat @ teste-astm.enc-tra
                       de-tot-enc-urd    / i-cont-dat @ teste-astm.enc-urd
                       de-tot-dentra-ini / i-cont-dat @ teste-astm.dentra-ini
                       de-tot-dentra-fin / i-cont-dat @ teste-astm.dentra-fin
                       i-cont-dat
                       WITH frame f-analitico-dat.
               DOWN STREAM str-rp WITH FRAME f-analitico-dat.
           END.
           ASSIGN i-cont-dat      = 0
                de-tot-enc-tra    = 0 
                de-tot-enc-urd    = 0 
                de-tot-dentra-ini = 0 
                de-tot-dentra-fin = 0. 
       END.
   END.
END.

/* Parametros de Selecao */ 
IF tt-param.imp-param THEN
   DISPLAY STREAM str-rp
                  tt-param.desc-classifica
                  tt-param.fi-ini-fm-codigo            
                  tt-param.fi-fin-fm-codigo 
                  tt-param.fi-ini-it-codigo
                  tt-param.fi-fin-it-codigo
                  tt-param.fi-ini-ano-mes
                  tt-param.fi-fin-ano-mes
                  tt-param.fi-ini-tipo-tear       
                  tt-param.fi-fin-tipo-tear      
                  tt-param.fi-ini-num-ob       
                  tt-param.fi-fin-num-ob       
                  tt-param.fi-ini-enc-tra          
                  tt-param.fi-fin-enc-tra          
                  tt-param.fi-ini-enc-urd          
                  tt-param.fi-fin-enc-urd      
                  tt-param.fi-ini-dentra-ini     
                  tt-param.fi-fin-dentra-ini    
                  tt-param.fi-ini-dentra-fin  
                  tt-param.fi-fin-dentra-fin
                  tt-param.desc-tipo-rel
                  tt-param.desc-tipo-acab
                  with frame f-param.           


/* fechamento do output do relat¢rio  */
{include/i-rpclo.i &STREAM="stream str-rp"}
run pi-finalizar in h-acomp.
return "OK":U.

