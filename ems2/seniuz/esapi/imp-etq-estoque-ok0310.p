/*
   Imprime Etiquetas de Estoque
*/

DEFINE INPUT PARAMETER p-cod-estabel  AS CHAR.
DEFINE INPUT PARAMETER p-num-etiqueta AS INT.
DEFINE INPUT PARAMETER p-coletor      AS LOG.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF VAR c-form-epl      AS CHAR FORMAT "x(30)".
DEF VAR c-prog-epl      AS CHAR FORMAT "x(50)".
DEF VAR de-peso-liquido LIKE ITEM.peso-liquido.
DEF VAR c-desc-item     AS CHAR FORMAT "x(45)".
DEF VAR c-composicao    AS CHAR EXTENT 3.
DEF VAR i-ct            AS INT.
DEF VAR v-defeito       AS CHAR EXTENT 3.
DEF VAR i-lote          AS INT.
DEF VAR c-comando       AS CHAR.
DEF VAR c-code-ant      AS CHAR.
DEF VAR c-cod-estabel   AS CHAR.
DEF VAR i-nr-seq        LIKE ob-etiqueta.nr-sequencia.
DEF VAR i-num-bar       AS INT.
DEF VAR i-num-bar-ima   AS INT.
DEF VAR c-impressora    AS CHAR.


DEF VAR c-auxiliar-1 AS CHAR.  /* Descriªío do tecido */
DEF VAR c-auxiliar-2 AS CHAR.  /* Largura */
DEF VAR c-auxiliar-3 AS CHAR.  /* Pais Origem */
DEF VAR c-auxiliar-4 AS CHAR.  /* Composiªío */
DEF VAR c-auxiliar-5 AS CHAR.  /* Rendimento */
DEF VAR c-auxiliar-6 AS CHAR.  /* Barras e numero */

DEF STREAM str-rp.
DEF STREAM s-etq.

{esinc/sz-pcl.i}

ASSIGN c-cod-estabel = p-cod-estabel.

ASSIGN c-prog-epl = SESSION:TEMP-DIRECTORY + c-seg-usuario + ".epl".
IF c-cod-estabel = '1' THEN 
   ASSIGN c-form-epl = "M:\ems206\especificos\etiqueta\FORM-ETQ-ori-ima.epl".
ELSE
   ASSIGN c-form-epl = "M:\ems206\especificos\etiqueta\FORM-ETQ-ori-med.epl".

FIND ob-etiqueta WHERE
     ob-etiqueta.cod-estabel = c-cod-estabel AND
     ob-etiqueta.num-etiqueta = p-num-etiqueta
     USE-INDEX indice4 SHARE-LOCK NO-ERROR.

FIND ITEM WHERE
     ITEM.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

IF NOT AVAIL ITEM THEN DO.
   MESSAGE "Erro: Item n∆o Cadastrado para a Etiqueta..." SKIP
           "Etiqueta: " ob-etiqueta.num-etiqueta
           " Item:" ob-etiqueta.it-codigo
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
   RETURN 'ADM-ERROR'.
END.

FIND item-ext WHERE
     item-ext.it-codigo = ITEM.it-codigo NO-LOCK NO-ERROR.

FIND bc-etiqueta WHERE
     bc-etiqueta.progressivo = ob-etiqueta.progressivo NO-LOCK NO-ERROR.

IF AVAIL bc-etiqueta THEN
   ASSIGN c-auxiliar-6 = bc-etiqueta.progressivo. 
ELSE
   ASSIGN c-auxiliar-6 = ob-etiqueta.progressivo. 

ASSIGN c-auxiliar-1 = ENTRY(3,ITEM.narrativa,CHR(10)) NO-ERROR.
ASSIGN c-auxiliar-2 = ENTRY(5,ITEM.narrativa,CHR(10)) NO-ERROR.
ASSIGN c-auxiliar-3 = ENTRY(4,ITEM.narrativa,CHR(10)) NO-ERROR.
ASSIGN c-auxiliar-4 = ENTRY(6,ITEM.narrativa,CHR(10)) NO-ERROR.
ASSIGN c-auxiliar-5 = ENTRY(7,ITEM.narrativa,CHR(10)) NO-ERROR.

ASSIGN c-desc-item = ITEM.desc-item.

ASSIGN i-ct = 0
       v-defeito = "".

IF p-coletor = YES THEN
   ASSIGN v-defeito[3] = " E ". 

FIND qualid-tecido WHERE
     qualid-tecido.codigo = ob-etiqueta.cod-qualid NO-LOCK NO-ERROR.

IF ob-etiqueta.tipo-ordem <> 4 AND
   NOT AVAIL qualid-tecido THEN DO.
   MESSAGE  "Erro: Qualidade do Tecido n∆o Cadastrada..." SKIP
            "Etiqueta: " ob-etiqueta.num-etiqueta
            " Qualidade:" ob-etiqueta.cod-qualid
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
   RETURN 'ADM-ERROR'.
END. 

CASE ob-etiqueta.nr-lote.
    WHEN "RP" THEN ASSIGN i-lote = 1.
    WHEN "PP" THEN ASSIGN i-lote = 2.
    WHEN "RD" THEN ASSIGN i-lote = 3.
    WHEN "PD" THEN ASSIGN i-lote = 4.
END CASE.

ASSIGN i-num-bar = INT(STRING(ob-etiqueta.num-etiqueta) + fn-calc-digito(INPUT STRING(ob-etiqueta.num-etiqueta,"999999999"))).

ASSIGN de-peso-liquido = ITEM.peso-liquido.

OS-COPY VALUE(c-form-epl) VALUE(c-prog-epl).

OUTPUT STREAM s-etq TO VALUE(c-prog-epl) APPEND.

   PUT STREAM s-etq UNFORMATTED 
       "A20,160,0,1,3,4,N," '"' TRIM(ob-etiqueta.it-codigo) '"' SKIP
       "A220,160,0,1,3,4,N," '"' TRIM(ob-etiqueta.cod-refer) '"' SKIP
       "A525,160,0,3,1,2,N," '"' ENTRY(2,c-auxiliar-2,":") '"' SKIP
       "A630,160,0,3,1,2,N," '"' IF AVAIL item-ext
                                 THEN TRIM(STRING(item-ext.gramatura,">>9"))
                                 ELSE '' '"' SKIP
       "A115,219,0,1,1,2,N," '"' TRIM(c-auxiliar-1) '"' SKIP
       IF qualid-tecido.impr-tarja THEN "LE208,217,490,40" ELSE "" SKIP 
       "A232,258,0,2,1,1,N," '"' ENTRY(2,c-auxiliar-4,":") '"' SKIP
       "A425,290,0,2,1,1,N," '"' TRIM(c-auxiliar-5) '"' SKIP 
       "A328,160,0,1,3,4,N," '"' STRING(ob-etiqueta.quantidade, ">>9.99" ) '"' SKIP
       "A258,290,0,2,1,1,N," '"' ENTRY(2,c-auxiliar-3,":") '"' SKIP 
       "A640,300,0,1,3,3,N," '"' TRIM(ob-etiqueta.nuance) '"' SKIP
       "A215,367,0,1,1,3,N," '"' TRIM(c-composicao[1]) '"' SKIP
       "A215,400,0,1,1,3,N," '"' TRIM(c-composicao[2]) '"' SKIP.

   PUT STREAM s-etq UNFORMATTED 
       "A220,395,0,3,3,4,N," '"' STRING(ob-etiqueta.num-etiqueta,"999999999") '"' SKIP
       "B265,320,0,1,3,7,70,N," '"' STRING(i-num-bar,"9999999999") '"' SKIP
       "A720,160,1,1,2,2,N," '"' STRING(c-auxiliar-6,"99999999999") '"' SKIP     
       "B785,110,1,1,3,7,60,N," '"' STRING(c-auxiliar-6,"99999999999") '"' SKIP. 
   
   PUT STREAM s-etq UNFORMATTED 
       "A630,550,0,2,1,1,N," '"' TRIM(v-defeito[1]) '"' SKIP
       "A630,570,0,2,1,1,N," '"' TRIM(v-defeito[2]) '"' SKIP
       "A630,590,0,2,1,1,N," '"' TRIM(v-defeito[3]) '"' SKIP.

   
   //GK apaga, GM grava, GK busca Imagnes

   IF AVAIL item THEN DO. /*Escolhendo a imagem para jogar na etiqueta*/
      CASE item.cod-image:
          WHEN 'A' THEN
             PUT STREAM s-etq UNFORMATTED
                 "GG32,230," '"imag0212"'  SKIP
                 "GG32,270," '"imag0103"'  SKIP 
                 "GG32,320," '"imag0302"'  SKIP 
                 "GG32,360," '"imag0401"'  SKIP 
                 "GG32,410," '"imag0604"'  SKIP. 
          WHEN 'B' THEN
             PUT STREAM s-etq UNFORMATTED
                 "GG32,230," '"imag0210"'  SKIP
                 "GG32,265," '"imag0103"'  SKIP 
                 "GG32,310," '"imag0302"'  SKIP 
                 "GG32,345," '"imag0402"'  SKIP 
                 "GG32,390," '"imag0604"'  SKIP. 
          WHEN 'C' THEN
             PUT STREAM s-etq UNFORMATTED
                 "GG32,230," '"imag0210"'  SKIP
                 "GG32,265," '"imag0103"'  SKIP 
                 "GG32,310," '"imag0302"'  SKIP 
                 "GG32,345," '"imag0401"'  SKIP 
                 "GG32,390," '"imag0604"'  SKIP. 
          WHEN 'D' THEN
             PUT STREAM s-etq UNFORMATTED
                 "GG32,230," '"imag0201"'  SKIP
                 "GG32,265," '"imag0103"'  SKIP 
                 "GG32,310," '"imag0304"'  SKIP 
                 "GG32,345," '"imag0401"'  SKIP 
                 "GG32,390," '"imag0604"'  SKIP. 
          WHEN 'E' THEN
             PUT STREAM s-etq UNFORMATTED
                 "GG32,230," '"imag0210"'  SKIP
                 "GG32,265," '"imag0103"'  SKIP 
                 "GG32,310," '"imag0304"'  SKIP 
                 "GG32,345," '"imag0401"'  SKIP 
                 "GG32,390," '"imag0604"'  SKIP. 
          WHEN 'F' THEN
             PUT STREAM s-etq UNFORMATTED
                 "GG32,230," '"imag0212"'  SKIP
                 "GG32,265," '"imag0103"'  SKIP 
                 "GG32,310," '"imag0304"'  SKIP 
                 "GG32,345," '"imag0401"'  SKIP 
                 "GG32,390," '"imag0604"'  SKIP. 
          WHEN 'G' THEN
             PUT STREAM s-etq UNFORMATTED
                 "GG32,230," '"imag0212"' SKIP
                 "GG32,265," '"imag0103"' SKIP 
                 "GG32,310," '"imag0302"' SKIP 
                 "GG32,345," '"imag0404"' SKIP 
                 "GG32,390," '"imag0604"' SKIP.
           WHEN 'H' THEN
             PUT STREAM s-etq UNFORMATTED
                 "GG32,230," '"imag0204"'  SKIP
                 "GG32,265," '"imag0103"'  SKIP 
                 "GG32,310," '"imag0302"'  SKIP 
                 "GG32,345," '"imag0401"'  SKIP 
                 "GG32,390," '"imag0604"'  SKIP.
           WHEN 'I' THEN
             PUT STREAM s-etq UNFORMATTED
                 "GG32,230," '"imag0210"'  SKIP
                 "GG32,265," '"imag0103"'  SKIP 
                 "GG32,310," '"imag0304"'  SKIP 
                 "GG32,345," '"imag0404"'  SKIP 
                 "GG32,390," '"imag0604"'  SKIP.
          WHEN 'J' THEN
            PUT STREAM s-etq UNFORMATTED
                "GG32,230," '"imag0210"'  SKIP
                "GG32,265," '"imag0103"'  SKIP 
                "GG32,310," '"imag0304"'  SKIP 
                "GG32,345," '"imag0404"'  SKIP 
                "GG32,390," '"imag0604"'  SKIP.
          WHEN 'K' THEN
            PUT STREAM s-etq UNFORMATTED
                "GG32,230," '"imag0210"'  SKIP
                "GG32,265," '"imag0103"'  SKIP 
                "GG32,310," '"imag0304"'  SKIP 
                "GG32,345," '"imag0404"'  SKIP 
                "GG32,390," '"imag0606"'  SKIP.
           WHEN 'Couro' THEN
             PUT STREAM s-etq UNFORMATTED
                 "GG32,230," '"imag0201"' SKIP
                 "GG32,265," '"imag0103"' SKIP 
                 "GG32,310," '"imag0302"' SKIP 
                 "GG32,345," '"imag0404"' SKIP 
                 "GG32,390," '"imag0604"' SKIP. 
           WHEN 'Tecidos' THEN
             PUT STREAM s-etq UNFORMATTED
                 "GG32,230," '"imag0201"' SKIP
                 "GG32,265," '"imag0103"' SKIP 
                 "GG32,310," '"imag0302"' SKIP 
                 "GG32,345," '"imag0401"' SKIP 
                 "GG32,390," '"imag0604"' SKIP. 
      END CASE.
   END.

   PUT STREAM s-etq UNFORMATTED
       "P1" SKIP.

OUTPUT STREAM s-etq CLOSE.
    
FIND imprsor_usuar WHERE 
     imprsor_usuar.nom_impressora = "rabbit" AND 
     imprsor_usuar.cod_usuario = c-seg-usuario NO-LOCK NO-ERROR.

IF AVAIL imprsor_usuar THEN DO:
   ASSIGN c-impressora = imprsor_usuar.nom_disposit_so
          c-comando = "copy /Y /b " + c-prog-epl + " " + c-impressora. 
   
   OS-COMMAND SILENT VALUE(c-comando).
END.
   
FIND CURRENT ob-etiqueta NO-LOCK NO-ERROR.

