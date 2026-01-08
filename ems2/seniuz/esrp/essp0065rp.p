/* Programa: ESSP0065RP.P (Chamado pelo programa ESSP0065.W)
** Objetivo: Imprimir o relat¢rio de Testes de Amostra
**           (tabela: teste-amos)
** Sistema.: Espec¡ficos
** M¢dulo..: Espec¡ficos
** Autor...: Fabio Coelho Lanza - Janeiro/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0065RP 2.04.00.000} 

/* defini‡Æo das temp-tables para recebimento de parƒmetros */
define temp-table tt-param no-undo
       FIELD destino             AS INTEGER
       FIELD arquivo             AS CHAR format "x(35)"
       FIELD usuario             AS CHAR format "x(12)"
       FIELD data-exec           AS DATE
       FIELD hora-exec           AS INTEGER
       FIELD classifica          AS INTEGER
       FIELD desc-classifica     AS CHAR FORMAT "x(40)"
       FIELD fi-ini-fm-codigo    LIKE teste-amos.fm-codigo
       FIELD fi-fin-fm-codigo    LIKE teste-amos.fm-codigo 
       FIELD fi-ini-data-teste   LIKE teste-amos.data-teste
       FIELD fi-fin-data-teste   LIKE teste-amos.data-teste
       FIELD fi-ini-cod-maq      LIKE teste-amos.cod-maq
       FIELD fi-fin-cod-maq      LIKE teste-amos.cod-maq
       FIELD fi-ini-num-ob       LIKE teste-amos.num-ob     
       FIELD fi-fin-num-ob       LIKE teste-amos.num-ob     
       FIELD fi-ini-enc-tra      LIKE teste-amos.enc-tra    
       FIELD fi-fin-enc-tra      LIKE teste-amos.enc-tra     
       FIELD fi-ini-enc-urd      LIKE teste-amos.enc-urd     
       FIELD fi-fin-enc-urd      LIKE teste-amos.enc-urd     
       FIELD fi-ini-larg-ent     LIKE teste-amos.larg-ent     
       FIELD fi-fin-larg-ent     LIKE teste-amos.larg-ent     
       FIELD fi-ini-larg-sai     LIKE teste-amos.larg-sai      
       FIELD fi-fin-larg-sai     LIKE teste-amos.larg-sai  
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
def var de-tot-enc-tra          as dec.
def var de-tot-enc-urd          as dec.
def var de-tot-larg-ent         as dec.
def var de-tot-larg-sai         as dec.
def var i-cont-fam              as int  format ">>>9".
def var i-cont-dat              as int  format ">>>9".
def var i-cont-ger              as int.

/* defini‡Æo de frames do relat¢rio */
FORM 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.desc-classifica     LABEL "Classifica‡Æo....."  SKIP
    tt-param.fi-ini-fm-codigo    LABEL "Familia..........."
    "A"  AT 32
    tt-param.fi-fin-fm-codigo    NO-LABELS             SKIP
    tt-param.fi-ini-data-teste   LABEL "Data.............."
    "A"  AT 32
    tt-param.fi-fin-data-teste   NO-LABELS             SKIP
    tt-param.fi-ini-cod-maq      LABEL "Maquina..........."
    "A"  AT 32
    tt-param.fi-fin-cod-maq      NO-LABELS             SKIP
    tt-param.fi-ini-num-ob       LABEL "Numero da OB......"
    "A"  AT 32
    tt-param.fi-fin-num-ob       NO-LABELS             SKIP
    tt-param.fi-ini-enc-tra      LABEL "Encolh. da Trama.."
    "A"  AT 32
    tt-param.fi-fin-enc-tra      NO-LABELS             SKIP
    tt-param.fi-ini-enc-urd      LABEL "Encolh. Urdideira."
    "A"  AT 32
    tt-param.fi-fin-enc-urd      NO-LABELS             SKIP 
    tt-param.fi-ini-larg-ent     LABEL "Largura de Entrada"
    "A"  AT 32
    tt-param.fi-fin-larg-ent     NO-LABELS             SKIP 
    tt-param.fi-ini-larg-sai     LABEL "Largura da Saida.."
    "A"  AT 32
    tt-param.fi-fin-larg-sai     NO-LABELS             SKIP 
    tt-param.desc-tipo-rel       LABEL "Tipo do Relatorio."
    with FRAME f-param side-labels no-box WIDTH 133 STREAM-IO.

FORM   
    teste-amos.fm-codigo       label "Familia"
    familia.descricao          label "Descricao"
    teste-amos.data-teste      label "Data-Teste"
    teste-amos.num-seq         label "NS"
    teste-amos.num-ob          label "Num.OB"
    teste-amos.enc-tra         label "Enc.Trama"
    teste-amos.enc-urd         label "Enc.Urd."
    teste-amos.larg-ent        label "Larg.Ent"
    teste-amos.larg-sai        label "Larg.Sai"
    i-cont-fam                 label "Tsts"
    with frame f-analitico-fam NO-LABEL WIDTH 133 down stream-io.

FORM   
    teste-amos.data-teste     label "Data-Teste"
    teste-amos.fm-codigo      label "Familia"
    familia.descricao         label "Descricao"
    teste-amos.num-ob         label "Num.OB"
    teste-amos.enc-tra        label "Enc.Trama"
    teste-amos.enc-urd        label "Enc.Urd"
    teste-amos.larg-ent       label "Larg-Ent"
    teste-amos.larg-sai       label "Larg-Sai"
    i-cont-dat                label "Tsts"
    with frame f-sintetico-dat NO-LABEL WIDTH 133 down stream-io.

FORM
    teste-amos.data-teste     label "Data-Teste"
    teste-amos.fm-codigo      label "Familia"
    familia.descricao         label "Descricao"
    teste-amos.num-seq        label "NS"
    teste-amos.num-ob         label "Num.OB"
    teste-amos.enc-tra        label "Enc.Trama"
    teste-amos.enc-urd        label "Enc.Urd"
    teste-amos.larg-ent       label "Larg-Ent"
    teste-amos.larg-sai       label "Larg-Sai"
    i-cont-dat                label "Tsts"
    with frame f-analitico-dat NO-LABEL WIDTH 133 down stream-io.

FORM   
    teste-amos.fm-codigo       label "Familia"
    familia.descricao          label "Descricao"
    teste-amos.data-teste      label "Data-Teste"
    teste-amos.num-ob          label "Num.OB"
    teste-amos.enc-tra         label "Enc.Trama"
    teste-amos.enc-urd         label "Enc.Urd."
    teste-amos.larg-ent        label "Larg.Ent"
    teste-amos.larg-sai        label "Larg.Sai"
    i-cont-fam                 label "Tsts"
    with frame f-sintetico-fam NO-LABEL WIDTH 133 down stream-io.

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
{utp/ut-liter.i Testes_de_Amostras * r}
assign c-titulo-relat = trim(return-value).

view stream str-rp frame f-cabec.
view stream str-rp frame f-rodape.
run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

if tt-param.classific = 1 then do: /* Por Familia */
   FOR each teste-amos 
      where teste-amos.fm-codigo    >= tt-param.fi-ini-fm-codigo  
        AND teste-amos.fm-codigo    <= tt-param.fi-fin-fm-codigo  
        AND teste-amos.data-teste   >= tt-param.fi-ini-data-teste
        AND teste-amos.data-teste   <= tt-param.fi-fin-data-teste
        AND teste-amos.cod-maq      >= tt-param.fi-ini-cod-maq     
        AND teste-amos.cod-maq      <= tt-param.fi-fin-cod-maq     
        AND teste-amos.num-ob       >= tt-param.fi-ini-num-ob     
        AND teste-amos.num-ob       <= tt-param.fi-fin-num-ob     
        AND teste-amos.enc-tra      >= tt-param.fi-ini-enc-tra     
        AND teste-amos.enc-tra      <= tt-param.fi-fin-enc-tra     
        AND teste-amos.enc-urd      >= tt-param.fi-ini-enc-urd       
        AND teste-amos.enc-urd      <= tt-param.fi-fin-enc-urd    
        AND teste-amos.larg-ent     >= tt-param.fi-ini-larg-ent     
        AND teste-amos.larg-ent     <= tt-param.fi-fin-larg-ent      
        AND teste-amos.larg-sai     >= tt-param.fi-ini-larg-sai     
        AND teste-amos.larg-sai     <= tt-param.fi-fin-larg-sai        
      BREAK BY teste-amos.fm-codigo
      on stop undo,leave:
      run pi-acompanhar in h-acomp (input string(teste-amos.fm-codigo)).

      find familia where familia.fm-codigo = teste-amos.fm-codigo
                     no-lock no-error.

      IF tt-param.tipo-relatorio = 2 THEN DO: /* ANALITICO */
         DISPLAY STREAM str-rp
                 teste-amos.fm-codigo WHEN FIRST-OF(teste-amos.fm-codigo)
                 familia.descricao    
                 teste-amos.data-teste
                 teste-amos.num-seq
                 teste-amos.num-ob
                 teste-amos.enc-tra    
                 teste-amos.enc-urd      
                 teste-amos.larg-ent   
                 teste-amos.larg-sai    
                 with frame f-analitico-fam.
         down stream str-rp with frame f-analitico-fam.
      END.
      assign i-cont-fam      = i-cont-fam + 1
             de-tot-enc-tra  = de-tot-enc-tra  + teste-amos.enc-tra    
             de-tot-enc-urd  = de-tot-enc-urd  + teste-amos.enc-urd     
             de-tot-larg-ent = de-tot-larg-ent + teste-amos.larg-ent    
             de-tot-larg-sai = de-tot-larg-sai + teste-amos.larg-sai. 


      IF LAST-OF(teste-amos.fm-codigo) THEN DO:
          IF tt-param.tipo-relatorio = 2 THEN DO: /* ANALITICO */
             DISPLAY STREAM str-rp
                     "MEDIA"                     @ teste-amos.data-teste
                     de-tot-enc-tra  / i-cont-fam @ teste-amos.enc-tra
                     de-tot-enc-urd  / i-cont-fam @ teste-amos.enc-urd
                     de-tot-larg-ent / i-cont-fam @ teste-amos.larg-ent
                     de-tot-larg-sai / i-cont-fam @ teste-amos.larg-sai
                     i-cont-fam
                     WITH frame f-analitico-fam.
             DOWN STREAM str-rp WITH FRAME f-analitico-fam.
          END.
          ELSE DO: /* SINTTICO */
              DISPLAY STREAM str-rp
                      teste-amos.fm-codigo
                      familia.descricao
                      "MEDIA"                      @ teste-amos.data-teste
                      de-tot-enc-tra  / i-cont-fam @ teste-amos.enc-tra
                      de-tot-enc-urd  / i-cont-fam @ teste-amos.enc-urd
                      de-tot-larg-ent / i-cont-fam @ teste-amos.larg-ent
                      de-tot-larg-sai / i-cont-fam @ teste-amos.larg-sai
                      i-cont-fam
                      WITH frame f-analitico-fam.
              DOWN STREAM str-rp WITH FRAME f-analitico-fam.
          END.
          ASSIGN i-cont-fam      = 0
                 de-tot-enc-tra  = 0 
                 de-tot-enc-urd  = 0 
                 de-tot-larg-ent = 0 
                 de-tot-larg-sai = 0. 
      END.
   END.
END.
else do: /* POR DATA */
   FOR EACH  teste-amos NO-LOCK
       WHERE teste-amos.data-teste   >= tt-param.fi-ini-data-teste
         AND teste-amos.data-teste   <= tt-param.fi-fin-data-teste
         AND teste-amos.fm-codigo    >= tt-param.fi-ini-fm-codigo  
         AND teste-amos.fm-codigo    <= tt-param.fi-fin-fm-codigo  
         AND teste-amos.cod-maq      >= tt-param.fi-ini-cod-maq     
         AND teste-amos.cod-maq      <= tt-param.fi-fin-cod-maq     
         AND teste-amos.num-ob       >= tt-param.fi-ini-num-ob     
         AND teste-amos.num-ob       <= tt-param.fi-fin-num-ob     
         AND teste-amos.enc-tra      >= tt-param.fi-ini-enc-tra     
         AND teste-amos.enc-tra      <= tt-param.fi-fin-enc-tra     
         AND teste-amos.enc-urd      >= tt-param.fi-ini-enc-urd       
         AND teste-amos.enc-urd      <= tt-param.fi-fin-enc-urd    
         AND teste-amos.larg-ent     >= tt-param.fi-ini-larg-ent     
         AND teste-amos.larg-ent     <= tt-param.fi-fin-larg-ent      
         AND teste-amos.larg-sai     >= tt-param.fi-ini-larg-sai     
         AND teste-amos.larg-sai     <= tt-param.fi-fin-larg-sai        
       BREAK BY teste-amos.data-teste
             BY teste-amos.fm-codigo
             on stop undo,leave:
       run pi-acompanhar in h-acomp (input string(teste-amos.data-teste)).

       find familia where familia.fm-codigo = teste-amos.fm-codigo
                      no-lock no-error.

       IF tt-param.tipo-relatorio = 2 THEN DO: /* ANALITICO */
          DISPLAY STREAM str-rp
                  teste-amos.data-teste   WHEN FIRST-OF(teste-amos.data-teste)
                  teste-amos.fm-codigo 
                  familia.descricao    
                  teste-amos.num-seq
                  teste-amos.num-ob
                  teste-amos.enc-tra    
                  teste-amos.enc-urd      
                  teste-amos.larg-ent   
                  teste-amos.larg-sai    
                  with frame f-analitico-dat.
          down stream str-rp with frame f-analitico-dat.
       END.
       assign i-cont-dat      = i-cont-dat + 1
              de-tot-enc-tra  = de-tot-enc-tra  + teste-amos.enc-tra    
              de-tot-enc-urd  = de-tot-enc-urd  + teste-amos.enc-urd     
              de-tot-larg-ent = de-tot-larg-ent + teste-amos.larg-ent    
              de-tot-larg-sai = de-tot-larg-sai + teste-amos.larg-sai. 

       IF LAST-OF(teste-amos.data-teste) THEN DO:
           IF tt-param.tipo-relatorio = 2 THEN DO: /* ANALITICO */
              DISPLAY STREAM str-rp
                      "MEDIA"                     @ teste-amos.data-teste
                      de-tot-enc-tra  / i-cont-dat @ teste-amos.enc-tra
                      de-tot-enc-urd  / i-cont-dat @ teste-amos.enc-urd
                      de-tot-larg-ent / i-cont-dat @ teste-amos.larg-ent
                      de-tot-larg-sai / i-cont-dat @ teste-amos.larg-sai
                      i-cont-dat
                      WITH frame f-analitico-dat.
              DOWN STREAM str-rp WITH FRAME f-analitico-dat.
           END.
           ELSE DO: /* SINTTICO */
               DISPLAY STREAM str-rp
                       teste-amos.data-teste
                       de-tot-enc-tra  / i-cont-dat @ teste-amos.enc-tra
                       de-tot-enc-urd  / i-cont-dat @ teste-amos.enc-urd
                       de-tot-larg-ent / i-cont-dat @ teste-amos.larg-ent
                       de-tot-larg-sai / i-cont-dat @ teste-amos.larg-sai
                       i-cont-dat
                       WITH frame f-analitico-dat.
               DOWN STREAM str-rp WITH FRAME f-analitico-dat.
           END.
           ASSIGN i-cont-dat      = 0
                  de-tot-enc-tra  = 0 
                  de-tot-enc-urd  = 0 
                  de-tot-larg-ent = 0 
                  de-tot-larg-sai = 0. 
       END.
   END.
END.

/* Parametros de Selecao */ 
IF tt-param.imp-param THEN
   DISPLAY STREAM str-rp
                  tt-param.desc-classifica
                  tt-param.fi-ini-fm-codigo            
                  tt-param.fi-fin-fm-codigo            
                  tt-param.fi-ini-data-teste
                  tt-param.fi-fin-data-teste
                  tt-param.fi-ini-cod-maq       
                  tt-param.fi-fin-cod-maq      
                  tt-param.fi-ini-num-ob       
                  tt-param.fi-fin-num-ob       
                  tt-param.fi-ini-enc-tra          
                  tt-param.fi-fin-enc-tra          
                  tt-param.fi-ini-enc-urd          
                  tt-param.fi-fin-enc-urd      
                  tt-param.fi-ini-larg-ent     
                  tt-param.fi-fin-larg-ent    
                  tt-param.fi-ini-larg-sai  
                  tt-param.fi-fin-larg-sai
                  tt-param.desc-tipo-rel
                  with frame f-param.           


/* fechamento do output do relat¢rio  */
{include/i-rpclo.i &STREAM="stream str-rp"}
run pi-finalizar in h-acomp.
return "OK":U.

