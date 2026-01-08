/* Programa: ESSP0147.W
** Modulo..: Controle de Expedi‡Æo
** Objetivo: Listar a Consulta Itens X Etiquetas
** Autor...: Fábio Coelho Lanza - Janeiro/2007
** Obs.....: Especifico da TEAR TEXTIL IND.COM.LTDA.
**
*/
/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0147RP 2.04.00.000}

DEF BUFFER empresa FOR mgadm.empresa.

DEFINE TEMP-TABLE tt-param   NO-UNDO
       FIELD destino            AS INTEGER
       FIELD arquivo            AS CHAR FORMAT "x(35)"
       FIELD usuario            AS CHAR FORMAT "x(12)"
       FIELD data-exec          AS DATE
       FIELD hora-exec          AS INTEGER
       FIELD classifica         AS INTEGER
       FIELD desc-classifica    AS CHAR FORMAT "x(40)"
       FIELD cod-estabel-ini    LIKE ob-etiqueta.cod-estabel
       FIELD cod-estabel-fin    LIKE ob-etiqueta.cod-estabel
       FIELD localizacao-ini    LIKE ob-etiqueta.localizacao
       FIELD localizacao-fin    LIKE ob-etiqueta.localizacao
       FIELD dt-emissao-ini     LIKE ob-etiqueta.dt-emissao 
       FIELD dt-emissao-fin     LIKE ob-etiqueta.dt-emissao 
       FIELD nr-lote-ini        LIKE ob-etiqueta.nr-lote     
       FIELD nr-lote-fin        LIKE ob-etiqueta.nr-lote 
       FIELD fi-ini-it-codigo   like ob-etiqueta.it-codigo
       FIELD fi-fin-it-codigo   like ob-etiqueta.it-codigo
       FIELD fi-ini-cod-refer   like ob-etiqueta.cod-refer
       FIELD fi-fin-cod-refer   like ob-etiqueta.cod-refer
       FIELD fi-desenho         AS CHAR FORMAT "x(4)"
       FIELD l-inc-exc          AS LOG FORMAT "Inclusive/Exclusive"
       FIELD i-situacao         AS INT
       FIELD opc-artigo         AS CHAR FORMAT "x"
       FIELD enviar-e-mail      AS LOG FORMAT "Sim/NÆo"
       FIELD subject-e-mail     AS CHAR FORMAT "x(40)"
       FIELD texto-e-mail       AS CHAR FORMAT "x(2000)"
       FIELD l-batch            AS LOG
       FIELD imp-param          AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

DEF TEMP-TABLE tt-work
    FIELD cod-estabel      LIKE ob-etiqueta.cod-estabel
    FIELD dt-emissao       LIKE ob-etiqueta.dt-emissao
    FIELD doca             LIKE ob-etiqueta.localizacao
    FIELD num-etiqueta     LIKE ob-etiqueta.num-etiqueta
    FIELD nuance           LIKE ob-etiqueta.nuance
    FIELD acondicionamento AS CHAR FORMAT "x(10)"
    FIELD qualidade        AS CHAR FORMAT "x(11)"
    FIELD it-codigo        AS CHAR FORMAT "x(38)"
    FIELD cod-refer        AS CHAR FORMAT "x(6)"
    FIELD nr-lote          AS CHAR FORMAT "x(5)"
    FIELD quantidade       LIKE ob-etiqueta.quantidade
    FIELD situacao         AS CHAR FORMAT "x(10)"
    FIELD un               LIKE ITEM.un
    INDEX indice cod-estabel num-etiqueta.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i}

/* defini‡Æo de vari veis  */
DEF STREAM email.

DEF VAR h-acomp        AS HANDLE NO-UNDO.
DEF VAR c-situacao     AS CHAR FORMAT "x(10)".
DEF VAR i-sit-ini      AS INT.
DEF VAR i-sit-fin      AS INT.
DEF VAR c-refer        AS CHAR FORMAT "x(12)".
DEF VAR c-un           AS CHAR.
DEF VAR c-qualid       AS CHAR FORMAT "x(11)".
DEF VAR c-corte-comerc AS CHAR FORMAT "x(15)".
DEF VAR c-aux-e-mail   AS CHAR.
DEF VAR c-destinatar   AS CHAR.
DEF VAR i-qt-rl-doca   AS INTEGER     NO-UNDO.
DEF VAR i-qt-rl-geral  AS INTEGER     NO-UNDO.
DEF VAR de-tot-doca-kg AS DECIMAL     NO-UNDO.
DEF VAR de-tot-doca-mt AS DECIMAL     NO-UNDO.
DEF VAR de-tot-ger-kg AS DECIMAL     NO-UNDO.
DEF VAR de-tot-ger-mt AS DECIMAL     NO-UNDO.

form 
    "*--------------- Parƒmetros/Sele‡Æo ----------------*" SKIP
    tt-param.cod-estabel-ini  LABEL  "Estabelec...."
    "A"  AT 40
    tt-param.cod-estabel-fin  NO-LABEL SKIP
    tt-param.localizacao-ini  LABEL  "Doca........."
    "A"  AT 40                
    tt-param.localizacao-fin  NO-LABELS SKIP
    tt-param.dt-emissao-ini   LABEL  "Data EmissÆo."
    "A"  AT 40                
    tt-param.dt-emissao-fin   NO-LABELS SKIP
    tt-param.nr-lote-ini      LABEL  "Lote........."
    "A"  AT 40                     
    tt-param.nr-lote-fin      NO-LABELS SKIP
    tt-param.fi-ini-it-codigo LABEL  "Item........."
    "A"  AT 40                     
    tt-param.fi-fin-it-codigo NO-LABEL SKIP
    tt-param.fi-ini-cod-refer LABEL  "Referˆncia..."
    "A"  AT 40                     
    tt-param.fi-fin-cod-refer NO-LABEL            
    tt-param.fi-desenho       LABEL  "Desenho......"
    "A"  AT 40                     
    tt-param.l-inc-exc        LABEL  "Doca........."
    "A"  AT 40                     
    tt-param.opc-artigo       LABEL  "Inclus/Exclus"
    WITH FRAME f-param SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.

form
    tt-work.cod-estabel FORMAT "x(3)"        LABEL "Est"
    tt-work.doca FORMAT "XXX/XXX"            LABEL "Doca" 
    tt-work.num-etiqueta FORMAT ">>>>>>>>9"  LABEL "Etiqueta"
    tt-work.nuance                           LABEL "Nu"
    tt-work.dt-emissao   FORMAT "99/99/9999" LABEL "Dt-EmissÆo"
    tt-work.qualidade                        LABEL "Qualidade"
    tt-work.it-codigo                        LABEL "Item"
    tt-work.cod-refer                        LABEL "Refer." 
    tt-work.nr-lote                          LABEL "Lote" 
    tt-work.quantidade                       LABEL "Quantidade"
    tt-work.situacao                         LABEL "Situacao"
    tt-work.un                               LABEL "UN"
    WITH NO-BOX NO-LABEL 55 DOWN WIDTH 132 STREAM-IO FRAME f-detalhe.

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
{utp/ut-liter.i Estoque_de_Doca_X_Etiqueta * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

/* Altera parƒmetros, quando execu‡Æo for em batch */
IF tt-param.l-batch THEN DO:
   assign tt-param.usuario          = "super"
          tt-param.destino          = 2 /* Arquivo */
          tt-param.arquivo          = "ESSP0147.LST"
          tt-param.data-exec        = TODAY
          tt-param.hora-exec        = TIME
          tt-param.classifica       = 1
          tt-param.desc-classifica  = "Padrao"
          tt-param.cod-estabel-ini  = "1"
          tt-param.cod-estabel-fin  = "999"
          tt-param.localizacao-ini  = ""
          tt-param.localizacao-fin  = ""
          tt-param.dt-emissao-ini   = 01/01/0001
          tt-param.dt-emissao-fin   = TODAY
          tt-param.nr-lote-ini      = ""
          tt-param.nr-lote-fin      = "ZZ"
          tt-param.fi-ini-it-codigo = ""
          tt-param.fi-fin-it-codigo = "ZZZZZZZZZZZZZZZZ"
          tt-param.fi-ini-cod-refer = ""
          tt-param.fi-fin-cod-refer = "ZZZZZZZZ"
          tt-param.fi-desenho       = ""
          tt-param.l-inc-exc        = YES
          tt-param.opc-artigo       = "A"
          tt-param.enviar-e-mail    = YES
          tt-param.subject-e-mail   = "Estoque da Expedicao sem Docas - #PER-INI a #PER-FIN"
          tt-param.texto-e-mail     = "Segue anexo Rela‡Æo de Estoque da Expedi‡Æo sem Docas, #PER-INI a #PER-FIN." + CHR(13) +
                                      "Lembrando que o Estoque sem Docas nÆo permite reservas nem faturamento." + CHR(13) + CHR(13) + 
                                      "Atenciosamente," + CHR(13) + CHR(13) +
                                      "TEAR TEXTIL INDUSTRIA E COMERCIO LTDA." + CHR(13) + 
                                      "Setor de PCP de Vendas."
          tt-param.imp-param        = NO.

   /* O programa esta agendado para rodar de Seg a Sex …s 06:00, selecionando
   ** at‚ o dia anterior quando for Ter a Dom e, Sex, Sab e Dom, quando for Seg */
   IF WEEKDAY(TODAY) = 2 THEN /* Seg */
      ASSIGN tt-param.dt-emissao-fin = TODAY - 1.
   ELSE                       /* Ter a Dom */
      ASSIGN tt-param.dt-emissao-fin = TODAY - 1.
END.

ASSIGN c-aux-e-mail = SESSION:TEMP-DIRECTORY + "aux-e-mail.txt".

ASSIGN i-sit-ini = IF tt-param.i-situacao = 2 
                   THEN 4 ELSE 3.
       i-sit-fin = IF tt-param.i-situacao = 1 
                   THEN 3 ELSE 4.

FOR EACH ob-etiqueta WHERE
         ob-etiqueta.situacao                >= i-sit-ini   AND
         ob-etiqueta.situacao                <= i-sit-fin   AND
         ob-etiqueta.cod-estabel             >= tt-param.cod-estabel-ini  AND
         ob-etiqueta.cod-estabel             <= tt-param.cod-estabel-fin  AND
         ob-etiqueta.localizacao             >= tt-param.localizacao-ini  AND 
         ob-etiqueta.localizacao             <= tt-param.localizacao-fin  AND
         ob-etiqueta.dt-emissao              >= tt-param.dt-emissao-ini   AND
         ob-etiqueta.dt-emissao              <= tt-param.dt-emissao-fin   AND
         SUBSTR(ob-etiqueta.nr-lote,1,2)     >= tt-param.nr-lote-ini      AND
         SUBSTR(ob-etiqueta.nr-lote,1,2)     <= tt-param.nr-lote-fin      AND
         ob-etiqueta.it-codigo               >= tt-param.fi-ini-it-codigo AND
         ob-etiqueta.it-codigo               <= tt-param.fi-fin-it-codigo AND
         ob-etiqueta.cod-refer               >= tt-param.fi-ini-cod-refer AND
         ob-etiqueta.cod-refer               <= tt-param.fi-fin-cod-refer AND
         ((SUBSTR(ob-etiqueta.it-codigo,7,4) =  tt-param.fi-desenho AND tt-param.l-inc-exc = YES) OR
          (SUBSTR(ob-etiqueta.it-codigo,7,4) <> tt-param.fi-desenho AND tt-param.l-inc-exc = NO)  OR
          (tt-param.fi-desenho = ""))
         NO-LOCK,
    EACH item-ext WHERE 
         item-ext.it-codigo = ob-etiqueta.it-codigo 
         NO-LOCK  
    BREAK BY ob-etiqueta.localizacao
          BY ob-etiqueta.num-etiqueta.

    RUN pi-acompanhar IN h-acomp (INPUT "Etiqueta: " + STRING(ob-etiqueta.num-etiqueta)).

    IF ob-etiqueta.quantidade <= 0 THEN NEXT.

    {esinc/i-dsrb.i ob-etiqueta.situacao ob-etiqueta.situacao c-situacao}

    FIND ITEM WHERE 
         ITEM.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

    FIND qualid-tecido WHERE
         qualid-tecido.codigo = ob-etiqueta.cod-qualid NO-LOCK NO-ERROR.
    IF AVAIL qualid-tecido THEN
       ASSIGN c-qualid = qualid-tecido.descricao.

    ASSIGN c-corte-comerc = "".
    FIND corte-comerc WHERE corte-comerc.codigo = ob-etiqueta.corte-comerc NO-LOCK NO-ERROR.
    IF AVAIL corte-comerc  THEN 
       ASSIGN c-corte-comerc = corte-comerc.descricao.
     
    FIND tt-work WHERE
         tt-work.cod-estabel  = ob-etiqueta.cod-estabel AND
         tt-work.num-etiqueta = ob-etiqueta.num-etiqueta no-lock no-error.
    IF NOT AVAIL tt-work THEN DO.
       CREATE tt-work.                 
       ASSIGN tt-work.cod-estabel      = ob-etiqueta.cod-estabel
              tt-work.dt-emissao       = ob-etiqueta.dt-emissao
              tt-work.doca             = ob-etiqueta.localizacao
              tt-work.num-etiqueta     = ob-etiqueta.num-etiqueta
              tt-work.nuance           = ob-etiqueta.nuance
              tt-work.acondicionamento = c-corte-comerc
              tt-work.qualidade        = c-qualid
              tt-work.it-codigo        = ob-etiqueta.it-codigo + "-" + ITEM.desc-item
              tt-work.cod-refer        = ob-etiqueta.cod-refer
              tt-work.nr-lote          = ob-etiqueta.nr-lote
              tt-work.quantidade       = ob-etiqueta.quantidade
              tt-work.situacao         = c-situacao
              tt-work.un               = ITEM.un.
     END.                                 
    
END. 

/* Processa relatorio a partir da TEMP-TABLE */

IF tt-param.enviar-e-mail THEN DO:
   OUTPUT STREAM email TO value(c-aux-e-mail).
   PUT STREAM email
       c-empresa " - "
       " ESTOQUE DA EXPEDICAO SEM DOCAS - PERIODO: " tt-param.dt-emissao-ini FORMAT "99/99/9999" " a "  
       tt-param.dt-emissao-fin FORMAT "99/99/9999"
       " - EMITIDO EM: " TODAY " " STRING(TIME,"HH:MM")
       SKIP(1)
       "Est Dt-Emissao Doca     Etiqueta Nu Acondic    Qualidade   Item              " AT  1
       "                     Refer. Lote Quantidade Situacao   UN"            AT 78
       "--- ---------- ------- --------- -- ---------- ----------- ------------------" AT  1
       "-------------------- ------ ---- ---------- ---------- --"            AT 78
       SKIP.
END.

FOR EACH tt-work BREAK BY tt-work.doca
                       BY tt-work.num-etiqueta.

    ASSIGN i-qt-rl-doca = i-qt-rl-doca + 1
           i-qt-rl-geral = i-qt-rl-geral + 1.

    CASE tt-work.un.
        WHEN 'Kg' THEN ASSIGN de-tot-doca-kg = de-tot-doca-kg + tt-work.quantidade
                              de-tot-ger-kg = de-tot-ger-kg + tt-work.quantidade.
        OTHERWISE ASSIGN de-tot-doca-mt = de-tot-doca-mt + tt-work.quantidade
                              de-tot-ger-mt = de-tot-ger-mt + tt-work.quantidade.
    END.

    DISPLAY tt-work.cod-estabel
            tt-work.doca 
            tt-work.num-etiqueta                                
            tt-work.nuance
            tt-work.dt-emissao
            tt-work.qualidade
            tt-work.it-codigo 
            tt-work.cod-refer
            tt-work.nr-lote
            tt-work.quantidade 
            tt-work.situacao                  
            tt-work.un
            WITH FRAME f-detalhe.

    DOWN WITH FRAME f-detalhe.

    IF LAST-OF(tt-work.doca) THEN DO:
       DISPLAY "Total Doca Mts" @ tt-work.it-codigo
               de-tot-doca-mt @ tt-work.quantidade
               WITH FRAME f-detalhe.
       DOWN WITH FRAME f-detalhe.

       DISPLAY "Total Doca Kgs" @ tt-work.it-codigo
               de-tot-doca-kg @ tt-work.quantidade
               WITH FRAME f-detalhe.
       DOWN WITH FRAME f-detalhe.

       DISPLAY "Total Pe‡as " @ tt-work.it-codigo
               i-qt-rl-doca @ tt-work.quantidade
               WITH FRAME f-detalhe.
       DOWN 2 WITH FRAME f-detalhe.


       ASSIGN de-tot-doca-mt = 0
              de-tot-doca-kg = 0
              i-qt-rl-doca = 0.
    END.
END.

DOWN 1 WITH FRAME f-detalhe.

DISPLAY "Total GERAL Mts" @ tt-work.it-codigo
       de-tot-ger-mt @ tt-work.quantidade
       WITH FRAME f-detalhe.
DOWN WITH FRAME f-detalhe.

DISPLAY "Total GERAL Kgs" @ tt-work.it-codigo
       de-tot-ger-kg @ tt-work.quantidade
       WITH FRAME f-detalhe.
DOWN WITH FRAME f-detalhe.

DISPLAY "Total Pe‡as" @ tt-work.it-codigo
         i-qt-rl-geral @ tt-work.quantidade
        WITH FRAME f-detalhe.
DOWN WITH FRAME f-detalhe.

IF tt-param.imp-param THEN DO:
   PAGE.
   DISPLAY tt-param.cod-estabel-ini
           tt-param.cod-estabel-fin
           tt-param.localizacao-ini FORMAT "XXX/XXX"
           tt-param.localizacao-fin FORMAT "XXX/XXX"
           tt-param.dt-emissao-ini
           tt-param.dt-emissao-fin
           tt-param.nr-lote-ini
           tt-param.nr-lote-fin
           tt-param.fi-ini-it-codigo 
           tt-param.fi-fin-it-codigo 
           tt-param.fi-ini-cod-refer 
           tt-param.fi-fin-cod-refer 
           tt-param.fi-desenho       
           tt-param.l-inc-exc        
           tt-param.opc-artigo       
           WITH FRAME f-param.
END. 

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
RUN pi-finalizar IN h-acomp.
RETURN "OK":U.


