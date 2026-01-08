DEF VAR i-tp-embal AS INT.
DEF VAR c-situacao AS CHAR.

def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

OUTPUT TO "c:/lixo/lixo.csv".
PUT "Numero-OB;Nr-Seq;Dt-Emiss∆o;Num-Etiq;Situaá∆o;Localiz;Quantidade;Item;Refer;Lote;Acondic;Corte" SKIP.

FOR EACH ob-etiqueta WHERE ob-etiqueta.num-etiqueta <> 0 
                       AND ob-etiqueta.situacao <> 5 NO-LOCK.
    IF ob-etiqueta.nr-lote BEGINS "R" THEN
       ASSIGN i-tp-embal = 1.
    ELSE
    IF ob-etiqueta.nr-lote BEGINS "P" THEN
       ASSIGN i-tp-embal = 2.
    ELSE
    IF ob-etiqueta.nr-lote BEGINS "C" THEN
       ASSIGN i-tp-embal = 4.
    ELSE
       ASSIGN i-tp-embal = 3.

    {esinc/i-dsrb.i ob-etiqueta.situacao ob-etiqueta.situacao c-situacao}

    FIND corte-comerc WHERE
         corte-comerc.compr-min <= ob-etiqueta.quantidade AND
         corte-comerc.compr-max >= ob-etiqueta.quantidade AND
         corte-comerc.tp-embalag = i-tp-embal NO-LOCK NO-ERROR.

    IF NOT AVAIL corte-comerc OR 
       ob-etiqueta.acondic <> corte-comerc.descricao THEN DO.
       PUT ob-etiqueta.nr-ob ";"
           ob-etiqueta.nr-sequencia ";"
           ob-etiqueta.dt-emissao ";"
           ob-etiqueta.num-etiqueta ";"
           c-situacao ";"
           ob-etiqueta.localizacao FORMAT "999/999" ";"
           ob-etiqueta.quantidade ";"
           ob-etiqueta.it-codigo ";"
           ob-etiqueta.cod-refer ";"
           ob-etiqueta.nr-lote ";"
           ob-etiqueta.acondic ";"
           IF AVAIL corte-comerc THEN corte-comerc.descricao
                                 ELSE "*Nao existe*;"
           SKIP.
       /*
       IF ob-etiqueta.situacao = 3 AND AVAIL corte-comerc  THEN
          ASSIGN ob-etiqueta.corte-comerc = corte-comerc.codigo
                 ob-etiqueta.acondic = corte-comerc.descricao.
       */
    END.
END.
OUTPUT CLOSE.

run Execute in h-prog(input "excel.exe", input "c:\lixo\lixo.csv").
delete procedure h-prog.
