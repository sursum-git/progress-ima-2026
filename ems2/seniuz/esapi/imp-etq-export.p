/*
   Imprime Etiquetas de Estoque
*/

DEFINE INPUT PARAMETER p-num-etiqueta AS INT.
DEFINE INPUT PARAMETER p-coletor      AS LOG.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF VAR c-form-epl   AS CHAR FORMAT "x(30)".
DEF VAR c-prog-epl   AS CHAR FORMAT "x(50)".
DEF VAR de-peso-liquido LIKE ITEM.peso-liquido.
DEF VAR c-desc-item  AS CHAR FORMAT "x(33)".
DEF VAR c-composicao LIKE composi.descricao EXTENT 3.
DEF VAR i-ct         AS INT.
DEF VAR v-defeito    AS CHAR EXTENT 3.
DEF VAR i-lote       AS INT.
DEF VAR c-comando    AS CHAR.
DEF VAR c-code-ant   AS CHAR.
DEF VAR c-cod-estabel AS CHAR.
DEF VAR i-nr-seq     LIKE ob-etiqueta.nr-sequencia.
DEF VAR i-num-bar AS INT.

DEF STREAM str-rp.
DEF STREAM s-etq.

{esinc/sz-pcl.i}

ASSIGN c-prog-epl = SESSION:TEMP-DIRECTORY + "etq-exp.epl"
       c-form-epl = "n:\especificos\etiqueta\form-etq-exp.epl".

RUN esapi/busca-estab-usuar.p (INPUT c-seg-usuario,
                               OUTPUT c-cod-estabel).
IF c-cod-estabel = '1' THEN /* Paraopeba */
   ASSIGN c-form-epl = "n:\especificos\etiqueta\form-par.epl".


FIND ob-etiqueta WHERE
     ob-etiqueta.cod-estabel  = c-cod-estabel AND
     ob-etiqueta.num-etiqueta = p-num-etiqueta
     USE-INDEX indice4 SHARE-LOCK NO-ERROR.


FIND ordem-benefic WHERE
     ordem-benefic.cod-estabel = ob-etiqueta.cod-estabel AND
     ordem-benefic.nr-ob       = ob-etiqueta.nr-ob       AND
     ordem-benefic.dt-ob       = ob-etiqueta.dt-ob       AND
     ordem-benefic.nr-carro    = ob-etiqueta.nr-carro NO-LOCK NO-ERROR.
IF NOT AVAIL ordem-benefic THEN DO.
   PUT STREAM str-rp
       "OB nÆo encontrada para essa Sequencia" SKIP
       "Etiqueta: " ob-etiqueta.num-etiqueta
       " Item:" ob-etiqueta.it-codigo
       SKIP(1).
   RETURN 'ADM-ERROR'.
END.

FIND ITEM WHERE
     ITEM.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

IF NOT AVAIL ITEM THEN DO.
   PUT STREAM str-rp
       "Erro: Item nÆo Cadastrado para a Etiqueta..." SKIP
       "Etiqueta: " ob-etiqueta.num-etiqueta
       " Item:" ob-etiqueta.it-codigo
       SKIP(1).
   RETURN 'ADM-ERROR'.
END.

ASSIGN c-desc-item = ITEM.descricao-1 + TRIM(ITEM.descricao-2).

/*
IF ITEM.tipo-con-est = 4 THEN DO.
   FIND referencia WHERE
        referencia.cod-refer = ob-etiqueta.cod-refer NO-LOCK NO-ERROR.

   FIND FIRST ref-item-ext WHERE 
              ref-item-ext.it-codigo = ob-etiqueta.it-codigo AND 
              ref-item-ext.cod-refer = ob-etiqueta.cod-refer NO-LOCK NO-ERROR.

   IF AVAIL ref-item-ext THEN
      ASSIGN c-desc-item = c-desc-item + " " + referencia.descricao.
END.
*/
FIND FIRST item-ext WHERE
           item-ext.it-codigo = ITEM.it-codigo NO-LOCK NO-ERROR.

IF AVAIL item-ext THEN DO.
   ASSIGN c-desc-item = item-ext.descricao.
   FIND FIRST composi WHERE
              composi.cod-composi = item-ext.cod-composi NO-LOCK NO-ERROR.

   ASSIGN c-composicao = "".
   IF AVAIL composi THEN DO.
      /*
      DO i-ct = 1 TO NUM-ENTRIES(composi.descricao).
         ASSIGN c-composicao[INT(i-ct / 2)] = c-composicao[INT(i-ct / 2)] + ENTRY(i-ct,composi.descricao).
      END.
      */
      ASSIGN c-composicao[1] = composi.descricao
             c-composicao[2] = "ORIGEN BRASIL".
   END.
END.

ASSIGN i-ct = 0
       v-defeito = "".
FOR EACH mov-est-acbd WHERE
         mov-est-acbd.cod-estabel  = ob-etiqueta.cod-estabel  AND
         mov-est-acbd.data-mov     = ob-etiqueta.dt-emissao   AND
         mov-est-acbd.num-lote     = ob-etiqueta.nr-ob        AND
         mov-est-acbd.nr-carro     = ob-etiqueta.nr-carro     AND
         mov-est-acbd.acondic      = ob-etiqueta.acondic      AND
         mov-est-acbd.nr-sequencia = ob-etiqueta.nr-sequencia AND
         mov-est-acbd.classif      = "LD" NO-LOCK. 

    ASSIGN i-ct = i-ct + 1.
    IF i-ct > 3 THEN LEAVE.
    IF v-defeito[i-ct] = "" THEN DO.
       ASSIGN v-defeito[i-ct] = mov-est-acbd.cod-tipo-def + "   " + mov-est-acbd.cod-defeito.
    END.
END.
IF p-coletor = YES THEN
   ASSIGN v-defeito[3] = " E ". 

FIND qualid-tecido WHERE
     qualid-tecido.codigo = ob-etiqueta.cod-qualid NO-LOCK NO-ERROR.

IF NOT AVAIL qualid-tecido THEN DO.
   PUT STREAM str-rp
       "Erro: Qualidade do Tecido nÆo Cadastrada..." SKIP
       "Etiqueta: " ob-etiqueta.num-etiqueta
       " Qualidade:" ob-etiqueta.cod-qualid
       SKIP(1).
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
IF SUBSTR(ob-etiqueta.nr-lote,1,2) = 'SC' THEN DO.
   FIND ITEM WHERE
        ITEM.it-codigo = ordem-benefic.it-codigo NO-LOCK NO-ERROR.
   ASSIGN de-peso-liquido = ITEM.peso-liquido.
END.

OS-COPY VALUE(c-form-epl) VALUE(c-prog-epl).

OUTPUT STREAM s-etq TO VALUE(c-prog-epl) APPEND.

   PUT STREAM s-etq UNFORMATTED 
       "A110,160,0,1,3,4,N," '"' TRIM(ob-etiqueta.it-codigo) '"' SKIP
       "A300,160,0,1,2,4,N," '"' TRIM(SUBSTR(ob-etiqueta.cod-refer,1,2)) TRIM(SUBSTR(ob-etiqueta.cod-refer,3,4)) '"' SKIP
       "A450,160,0,1,2,4,N," '"' TRIM(SUBSTR(ob-etiqueta.cod-refer,7,1)) '"' SKIP
       "A490,160,0,1,2,4,N," '"' STRING((de-peso-liquido * 1000),"999.99") '"' SKIP
       IF AVAIL ordem-benefic AND ordem-benefic.cor-etiqueta = 99 THEN "LE106,150,505,58" ELSE "" SKIP 
       "A210,236,0,1,1,2,N," '"' TRIM(c-desc-item) '"' SKIP
       IF qualid-tecido.impr-tarja THEN "LE208,217,490,40" ELSE "" SKIP
       "A220,310,0,2,1,1,N," '"' STRING(ob-etiqueta.nr-ob,">>>>>9") '"' SKIP
       "A340,310,0,2,1,1,N," '"' IF AVAIL item-ext THEN STRING(item-ext.largura,"9.99") ELSE "" '"' SKIP
       "A450,270,0,2,1,1,N," '"' IF SUBSTR(ob-etiqueta.nr-lote,1,2) = "SC" THEN "(kg)" ELSE "(M)" '"' SKIP
       "A415,298,0,3,1,2,N," '"' STRING(ob-etiqueta.quantidade,">>9.99") '"' SKIP
       "A555,310,0,2,1,1,N," '"' STRING(ob-etiqueta.nr-cortes,">9") '"' SKIP
       "A640,300,0,1,3,3,N," '"' TRIM(ob-etiqueta.nuance) '"' SKIP
       IF qualid-tecido.class-qualid = 2 THEN "LE620,130,80,75" ELSE "" SKIP /* Tamanho */
       IF qualid-tecido.class-qualid = 2 THEN "LE610,270,90,65" ELSE "" SKIP /* Nuance */
       "A215,367,0,1,1,3,N," '"' TRIM(c-composicao[1]) '"' SKIP
       "A550,400,0,1,1,3,N," '"' TRIM(c-composicao[2]) '"' SKIP.

   IF SUBSTR(ob-etiqueta.nr-lote,1,2) = "SC" OR 
      SUBSTR(ob-etiqueta.nr-lote,1,2) = "CA" THEN
      PUT STREAM s-etq UNFORMATTED 
          "A622,146,0,3,3,3,R," '"' TRIM(ob-etiqueta.nr-lote) '"' SKIP.
    ELSE 
      PUT STREAM s-etq UNFORMATTED 
          "A620,170,0,2,1,1,N," '"' TRIM(ob-etiqueta.acondic) '"' SKIP.

   PUT STREAM s-etq UNFORMATTED 
       "A235,450,0,4,3,4,N," '"' STRING(ob-etiqueta.num-etiqueta,"999999999") '"' SKIP
       "B305,550,0,1,3,7,60,N," '"' STRING(i-num-bar,"9999999999") '"' SKIP.
   
   PUT STREAM s-etq UNFORMATTED 
       "A630,550,0,2,1,1,N," '"' TRIM(v-defeito[1]) '"' SKIP
       "A630,570,0,2,1,1,N," '"' TRIM(v-defeito[2]) '"' SKIP
       "A630,590,0,2,1,1,N," '"' TRIM(v-defeito[3]) '"' SKIP.

   IF AVAIL item-ext THEN DO. /*Escolhendo a imagem para jogar na etiqueta*/
      CASE item-ext.cod-rlgp:
           WHEN 1 THEN
              PUT STREAM s-etq UNFORMATTED
                  "GG130,280," '"imag0204"' SKIP
                  "GG130,330," '"imag0103"' SKIP 
                  "GG130,390," '"imag0302"' SKIP 
                  "GG130,450," '"imag0402"' SKIP 
                  "GG130,510," '"imag0606"' SKIP. 
           WHEN 2 THEN
              PUT STREAM s-etq UNFORMATTED
                  "GG130,280," '"imag0206"' SKIP
                  "GG130,330," '"imag0103"' SKIP 
                  "GG130,390," '"imag0303"' SKIP 
                  "GG130,450," '"imag0402"' SKIP 
                  "GG130,510," '"imag0606"' SKIP. 
           WHEN 3 THEN 
              PUT STREAM s-etq UNFORMATTED
                  "GG130,280," '"imag0201"' SKIP
                  "GG130,330," '"imag0103"' SKIP 
                  "GG130,390," '"imag0302"' SKIP 
                  "GG130,450," '"imag0404"' SKIP 
                  "GG130,510," '"imag0606"' SKIP. 
          WHEN 4 THEN 
             PUT STREAM s-etq UNFORMATTED
                 "GG130,280," '"imag0204"' SKIP
                 "GG130,330," '"imag0103"' SKIP 
                 "GG130,390," '"imag0302"' SKIP 
                 "GG130,450," '"imag0402"' SKIP 
                 "GG130,510," '"imag0603"' SKIP. 
      END CASE.
   END.

   PUT STREAM s-etq UNFORMATTED
       "P1" SKIP.
OUTPUT STREAM s-etq CLOSE.
ASSIGN c-comando = "copy /Y /b " + c-prog-epl + " lpt1". 
OS-COMMAND SILENT VALUE(c-comando). 

