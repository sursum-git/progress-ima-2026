DEFINE TEMP-TABLE tt-imp-item
       FIELD id           AS   INT
       FIELD it-codigo    LIKE ped-item.it-codigo
       FIELD cod-refer    LIKE ped-item.cod-refer
       FIELD nr-lote      AS   INT
       FIELD quantidade   AS   DEC FORMAT ">>9.9"
       FIELD volume-it    LIKE ped-item-res.volume-ini
       FIELD nr-ob        LIKE ob-etiqueta.nr-ob
       FIELD sequencia    LIKE ob-etiqueta.nr-sequencia
       FIELD lote         AS   CHAR
       FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta
       FIELD erro         AS   CHAR FORMAT "x(50)".

DEF VAR c-linha       AS CHAR NO-UNDO.
DEF BUFFER b-etiqueta FOR ob-etiqueta.
DEF VAR i-nr-seq AS INT INIT 755.
DEF VAR c-qualid AS CHAR.

DEF STREAM s-imp.
DEF STREAM s-etq.


DEF VAR i-num-bar AS INT.

{esinc/sz-pcl.i}


INPUT  FROM n:\coletor\enviar\pecas-dev.txt NO-ECHO.
REPEAT. 
    ASSIGN c-linha = "".
    IMPORT DELIMITER "$&#" c-linha.
    IF c-linha = "" THEN NEXT.

    CREATE tt-imp-item.
    ASSIGN tt-imp-item.it-codigo  = SUBSTR(c-linha,2,6)
           tt-imp-item.cod-refer  = SUBSTR(c-linha,8,7)
           tt-imp-item.quantidade = DEC(SUBSTR(c-linha,15,4)) / 10
           tt-imp-item.nr-ob      = INT(SUBSTR(c-linha,25,5))
           tt-imp-item.sequencia  = INT(SUBSTR(c-linha,30,3))
           tt-imp-item.nr-lote    = INT(SUBSTR(c-linha,33,1)).
           tt-imp-item.lote = IF tt-imp-item.nr-lote = 1 THEN "RP"
                              ELSE IF tt-imp-item.nr-lote = 2 THEN "PP"
                                   ELSE IF tt-imp-item.nr-lote = 3 THEN "RD"
                                        ELSE IF tt-imp-item.nr-lote = 4 THEN "PD"
                                             ELSE "CA".
END.
INPUT CLOSE.


FOR EACH tt-imp-item.
    FIND ob-etiqueta WHERE
         ob-etiqueta.nr-ob = tt-imp-item.nr-ob AND
         ob-etiqueta.nr-seq = tt-imp-item.sequencia NO-ERROR.

    RUN pi-imprime-etq.
END.



PROCEDURE pi-imprime-etq.
    DEF VAR c-form-epl   AS CHAR FORMAT "x(30)".
    DEF VAR c-prog-epl   AS CHAR FORMAT "x(50)".
    DEF VAR c-desc-item  AS CHAR FORMAT "x(33)".
    DEF VAR c-composicao LIKE composi.descricao EXTENT 3.
    DEF VAR i-ct         AS INT.
    DEF VAR v-defeito    AS CHAR EXTENT 3.
    DEF VAR i-lote       AS INT.
    DEF VAR c-comando    AS CHAR.
    DEF VAR c-code-ant   AS CHAR.
    DEF VAR i-nr-seq     LIKE ob-etiqueta.nr-sequencia.

    ASSIGN c-prog-epl = SESSION:TEMP-DIRECTORY + "etq-fin.epl"
           c-form-epl = "n:\especificos\etiqueta\form-etq.epl".

    FIND ITEM WHERE
         ITEM.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

    IF NOT AVAIL ITEM THEN DO.
       MESSAGE 
           "Erro: Item nÆo Cadastrado para a Etiqueta..." SKIP
           "Etiqueta: " ob-etiqueta.num-etiqueta
           " Item:" ob-etiqueta.it-codigo
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       NEXT.
    END.

    ASSIGN c-desc-item = ITEM.descricao-1 + TRIM(ITEM.descricao-2).

    IF ITEM.tipo-con-est = 4 THEN DO.
       FIND referencia WHERE
            referencia.cod-refer = ob-etiqueta.cod-refer NO-LOCK NO-ERROR.

       FIND FIRST ref-item-ext WHERE 
                  ref-item-ext.it-codigo = ob-etiqueta.it-codigo AND 
                  ref-item-ext.cod-refer = ob-etiqueta.cod-refer NO-LOCK NO-ERROR.

       IF AVAIL ref-item-ext THEN
          ASSIGN c-desc-item = c-desc-item + " " + referencia.descricao.
    END.

    FIND FIRST item-ext WHERE
               item-ext.it-codigo = ITEM.it-codigo NO-LOCK NO-ERROR.

    IF AVAIL item-ext THEN DO.
       FIND FIRST composi WHERE
                  composi.cod-composi = item-ext.cod-composi NO-LOCK NO-ERROR.

       ASSIGN c-composicao = "".
       IF AVAIL composi THEN DO.
          DO i-ct = 1 TO NUM-ENTRIES(composi.descricao).
             ASSIGN c-composicao[INT(i-ct / 2)] = c-composicao[INT(i-ct / 2)] + ENTRY(i-ct,composi.descricao).
          END.
       END.
    END.

    ASSIGN i-ct = 0
           v-defeito = "".
    FOR EACH mov-est-acbd WHERE
             mov-est-acbd.data-mov = ob-etiqueta.dt-emissao AND
             mov-est-acbd.num-lote = ob-etiqueta.nr-ob AND
             mov-est-acbd.nr-carro = ob-etiqueta.nr-carro AND
             mov-est-acbd.acondic  = ob-etiqueta.acondic AND
             mov-est-acbd.nr-sequencia = ob-etiqueta.nr-sequencia AND
             mov-est-acbd.classif = "LD" NO-LOCK. 

        ASSIGN i-ct = i-ct + 1.
        IF i-ct > 3 THEN LEAVE.
        IF v-defeito[i-ct] = "" THEN DO.
           ASSIGN v-defeito[i-ct] = mov-est-acbd.cod-tipo-def + "   " + mov-est-acbd.cod-defeito.
        END.
    END.
    ASSIGN v-defeito[3] = " E ".

    FIND qualid-tecido WHERE
         qualid-tecido.codigo = ob-etiqueta.cod-qualid NO-LOCK NO-ERROR.

    IF NOT AVAIL qualid-tecido THEN DO.
        IF SUBSTR(ob-etiqueta.nr-lote,2,1) = "D" THEN 
           ASSIGN c-qualid = 'D'.

        IF SUBSTR(ob-etiqueta.nr-lote,2,1) = "P" THEN DO.
           FIND FIRST mov-est-acbd WHERE
                      mov-est-acbd.data-mov = ob-etiqueta.dt-emissao AND
                      mov-est-acbd.num-lote = ob-etiqueta.nr-ob AND
                      mov-est-acbd.nr-carro = ob-etiqueta.nr-carro AND
                      mov-est-acbd.acondic  = ob-etiqueta.acondic AND
                      mov-est-acbd.nr-sequencia = ob-etiqueta.nr-sequencia AND
                      mov-est-acbd.classif = "RG" NO-LOCK NO-ERROR. 
           IF AVAIL mov-est-acbd THEN
              ASSIGN c-qualid = "C".
           ELSE
              ASSIGN c-qualid = "B".
        END.
        ASSIGN ob-etiqueta.cod-qualid = c-qualid.
    END.

    FIND qualid-tecido WHERE
         qualid-tecido.codigo = ob-etiqueta.cod-qualid NO-LOCK NO-ERROR.

    CASE ob-etiqueta.nr-lote.
        WHEN "RP" THEN ASSIGN i-lote = 1.
        WHEN "PP" THEN ASSIGN i-lote = 2.
        WHEN "RD" THEN ASSIGN i-lote = 3.
        WHEN "PD" THEN ASSIGN i-lote = 4.
    END CASE.

    ASSIGN c-code-ant = TRIM(ob-etiqueta.it-codigo) + 
                        STRING(INT(ob-etiqueta.cod-refer),"9999999") +
                        STRING(ob-etiqueta.quantidade * 10,"9999") +
                        SUBSTR(STRING(ob-etiqueta.nr-ob,"999999"),2,5) + 
                        STRING(ob-etiqueta.nr-sequencia,"999") + 
                        STRING(i-lote,"9").

    ASSIGN i-num-bar = INT(STRING(ob-etiqueta.num-etiqueta) + fn-calc-digito(INPUT STRING(ob-etiqueta.num-etiqueta,"999999999"))).

    OS-COPY VALUE(c-form-epl) VALUE(c-prog-epl).

    OUTPUT STREAM s-etq TO VALUE(c-prog-epl) APPEND.
       PUT STREAM s-etq UNFORMATTED 
           "B180,50,1,2,2,5,75,N," '"' c-code-ant '"' SKIP
           "A220,60,0,1,3,4,N," '"' TRIM(ob-etiqueta.it-codigo) '"' SKIP
           "A420,60,0,1,2,4,N," '"' TRIM(SUBSTR(ob-etiqueta.cod-refer,1,2)) TRIM(SUBSTR(ob-etiqueta.cod-refer,3,4)) '"' SKIP
           "A640,60,0,1,2,4,N," '"' TRIM(SUBSTR(ob-etiqueta.cod-refer,7,1)) '"' SKIP
           "A220,130,0,2,1,1,N," '"' TRIM(SUBSTR(c-desc-item,1,40)) '"' SKIP
           IF qualid-tecido.impr-tarja THEN "LE210,125,480,30" ELSE "" SKIP
           "A220,200,0,2,1,1,N," '"' STRING(ob-etiqueta.nr-ob,">>>>>9") '"' SKIP
           "A340,200,0,2,1,1,N," '"' IF AVAIL item-ext THEN STRING(item-ext.largura,"9.99") ELSE "" '"' SKIP
           "A430,200,0,2,1,1,N," '"' STRING(ob-etiqueta.quantidade,">>9.99") '"' SKIP
           "A550,200,0,2,1,1,N," '"' STRING(ob-etiqueta.nr-cortes,">9") '"' SKIP
           "A640,190,0,1,3,3,N," '"' TRIM(ob-etiqueta.nuance) '"' SKIP
           IF qualid-tecido.class-qualid = 2 THEN "LE610,170,80,135" ELSE "" SKIP
           "A220,265,0,2,1,1,N," '"' TRIM(c-composicao[1]) '"' SKIP
           "A220,285,0,2,1,1,N," '"' TRIM(c-composicao[2]) '"' SKIP
           "A610,270,0,2,1,1,N," '"' TRIM(ob-etiqueta.acondic) '"' SKIP
           "A240,330,0,4,3,4,N," '"' STRING(ob-etiqueta.num-etiqueta,"999999999") '"' SKIP
           "B295,430,0,1,3,7,60,N," '"' STRING(i-num-bar,"9999999999") '"' SKIP
           "A620,520,0,2,1,1,N," '"' TRIM(v-defeito[1]) '"' SKIP
           "A620,540,0,2,1,1,N," '"' TRIM(v-defeito[2]) '"' SKIP
           "A620,570,0,1,2,2,N," '"' TRIM(v-defeito[3]) '"' SKIP.

       IF AVAIL item-ext THEN DO. /*Escolhendo a imagem para jogar na etiqueta*/
          CASE item-ext.cod-rlgp:
               WHEN 1 THEN
                  PUT STREAM s-etq UNFORMATTED
                      "GG260,535," '"imag0204"' SKIP
                      "GG310,530," '"imag0102"' SKIP 
                      "GG370,530," '"imag0302"' SKIP 
                      "GG425,535," '"imag0402"' SKIP 
                      "GG480,530," '"imag0604"' SKIP. 
               WHEN 2 THEN
                  PUT STREAM s-etq UNFORMATTED
                      "GG260,535," '"imag0204"' SKIP
                      "GG310,530," '"imag0102"' SKIP 
                      "GG370,530," '"imag0303"' SKIP 
                      "GG425,535," '"imag0402"' SKIP 
                      "GG480,530," '"imag0604"' SKIP. 
               WHEN 3 THEN 
                  PUT STREAM s-etq UNFORMATTED
                      "GG260,535," '"imag0206"' SKIP
                      "GG310,530," '"imag0102"' SKIP 
                      "GG370,530," '"imag0303"' SKIP 
                      "GG425,535," '"imag0402"' SKIP 
                      "GG480,530," '"imag0604"' SKIP. 
               WHEN 4 THEN                           
                  PUT STREAM s-etq UNFORMATTED
                      "GG260,535," '"imag0203"' SKIP
                      "GG310,530," '"imag0102"' SKIP 
                      "GG370,530," '"imag0301"' SKIP 
                      "GG425,535," '"imag0503"' SKIP 
                      "GG480,530," '"imag0604"' SKIP. 
          END CASE.
       END.

       PUT STREAM s-etq UNFORMATTED
           "P1" SKIP.
    OUTPUT STREAM s-etq CLOSE.

    ASSIGN c-comando = "copy /Y /b " + c-prog-epl + " lpt1". 
    OS-COMMAND SILENT VALUE(c-comando). 

END PROCEDURE.
