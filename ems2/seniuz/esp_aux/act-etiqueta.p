    DEF VAR c-form-epl   AS CHAR FORMAT "x(30)".
    DEF VAR c-prog-epl   AS CHAR FORMAT "x(50)".
    DEF VAR i-ct         AS INT.
    DEF VAR c-desc-item  AS CHAR FORMAT "x(33)".
    DEF VAR c-composicao LIKE composi.descricao EXTENT 3.
    DEF VAR v-defeito    AS CHAR EXTENT 3.
    DEF VAR i-lote       AS INT.
    DEF VAR c-qualid     AS CHAR.
    DEF VAR c-comando    AS CHAR.
    DEF VAR c-code-ant   AS CHAR.
    DEF VAR i-tp-embal   AS INT.

    FIND ob-etiqueta WHERE
         ob-etiqueta.num-etiqueta = 857608.

    IF ob-etiqueta.nr-lote BEGINS "R" THEN
       ASSIGN i-tp-embal = 1.
    ELSE
       ASSIGN i-tp-embal = 2.

    FIND corte-comerc WHERE
         corte-comerc.compr-min <= ob-etiqueta.quantidade AND
         corte-comerc.compr-max >= ob-etiqueta.quantidade AND
         corte-comerc.tp-embalag = i-tp-embal NO-LOCK NO-ERROR.
    
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

    ASSIGN ob-etiqueta.acondic = corte-comerc.descricao
           ob-etiqueta.cod-qualid = c-qualid. 

