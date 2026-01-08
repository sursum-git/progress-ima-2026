def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

OUTPUT TO "c:/lixo/lixo.txt" CONVERT SOURCE "ibm850".

DEF VAR i-tp-embal AS INT.

FOR EACH ob-etiqueta WHERE ob-etiqueta.situacao > 2 AND
                           ob-etiqueta.situacao < 5 NO-LOCK.
    FIND corte-comerc WHERE corte-comerc.codigo = ob-etiqueta.corte-comerc NO-LOCK NO-ERROR.

    IF NOT AVAIL corte-comerc THEN DO:
       ACCUMULATE ob-etiqueta.num-etiqueta(COUNT).

       IF ob-etiqueta.acondic BEGINS "R" THEN
          ASSIGN i-tp-embal = 1.
       ELSE
       IF ob-etiqueta.acondic BEGINS "P" THEN
          ASSIGN i-tp-embal = 2.
       ELSE
       IF ob-etiqueta.acondic BEGINS "C" THEN
          ASSIGN i-tp-embal = 4.
       ELSE
          ASSIGN i-tp-embal = 3.

       FIND corte-comerc WHERE
            corte-comerc.compr-min <= ob-etiqueta.quantidade AND
            corte-comerc.compr-max >= ob-etiqueta.quantidade AND
            corte-comerc.tp-embalag = i-tp-embal NO-LOCK NO-ERROR.

       IF AVAIL corte-comerc THEN
          /*ASSIGN ob-etiqueta.corte-comerc = corte-comerc.codigo.*/
          
       IF ob-etiqueta.acondic <> corte-comerc.descricao OR
          ob-etiqueta.corte-comerc <> corte-comerc.codigo THEN
          DISPLAY ob-etiqueta.num-etiqueta  
                  ob-etiqueta.acondic       
                  ob-etiqueta.quantidade    
                  ob-etiqueta.corte-comerc
                  corte-comerc.codigo
                  corte-comerc.descricao
                  WITH WIDTH 100.
    END.
END.
DISP ACCUM COUNT ob-etiqueta.num-etiqueta.

OUTPUT CLOSE.

run Execute in h-prog(input "wordpad.exe", input "c:\lixo\lixo.txt").
delete procedure h-prog.
