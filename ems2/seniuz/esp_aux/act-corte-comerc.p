DEF VAR i-tp-embal AS INT.
DEF VAR de-compr-min LIKE corte-comerc.compr-min.

DEF BUFFER b-corte-comerc FOR corte-comerc.

def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

OUTPUT TO "c:/lixo/lixo.txt" CONVERT SOURCE "ibm850".

FOR EACH ped-venda WHERE
         ped-venda.cod-sit-ped <= 5 AND 
         ped-venda.cod-sit-ped <> 3 NO-LOCK.

    FOR EACH ped-item OF ped-venda.
        FIND ped-item-ext OF ped-item SHARE-LOCK NO-ERROR.

        IF NOT AVAIL ped-item-ext THEN NEXT.

        FIND corte-comerc WHERE
             corte-comerc.descricao = ped-item-ext.acondicionamento
             NO-LOCK NO-ERROR.

        IF ped-item-ext.lote BEGINS "R" THEN
           ASSIGN i-tp-embal = 1.
        ELSE
        IF ped-item-ext.lote BEGINS "P" THEN
           ASSIGN i-tp-embal = 2.
        ELSE
        IF ped-item-ext.lote BEGINS "C" THEN
           ASSIGN i-tp-embal = 4.
        ELSE
           ASSIGN i-tp-embal = 3.

        IF ped-item.qt-pedida < 10 THEN
           ASSIGN i-tp-embal = 4.

        ASSIGN de-compr-min = 0.
        FIND corte-comerc WHERE
             corte-comerc.compr-min <= ped-item.qt-pedida AND
             corte-comerc.compr-max >= ped-item.qt-pedida AND
             corte-comerc.tp-embalag = i-tp-embal NO-LOCK NO-ERROR.
    
        IF AVAIL corte-comerc THEN
           ASSIGN de-compr-min = corte-comerc.compr-min.

         /*IF AVAIL corte-comerc THEN
              ASSIGN ped-item-ext.acondic = corte-comerc.descricao.*/
         IF AVAIL corte-comerc AND 
                  corte-comerc.descricao = ped-item-ext.acondic THEN NEXT.

         IF de-compr-min = 0 THEN NEXT.

         FIND b-corte-comerc 
            WHERE b-corte-comerc.descricao = ped-item-ext.acondic NO-LOCK NO-ERROR.
         IF AVAIL b-corte-comerc THEN DO:
            IF ped-item.qt-pedida > b-corte-comerc.compr-max THEN NEXT.
         END.

         DISP ped-item-ext.nome-abrev    LABEL "Cliente"
              ped-item-ext.nr-pedcli     LABEL "Pedido"
              ped-item-ext.nr-sequencia  LABEL "Seq"
              ped-item-ext.it-codigo     LABEL "Item"
              ped-item-ext.cod-refer     LABEL "Refer"
              ped-item.qt-pedida         LABEL "Quantid"
              ped-item-ext.acondic       LABEL "Acondic"
              corte-comerc.descricao     LABEL "Acond-Sugerido"
                 WHEN AVAIL corte-comerc
              WITH WIDTH 132.
    END.
END.

OUTPUT CLOSE.

run Execute in h-prog(input "wordpad.exe", input "c:\lixo\lixo.txt").
delete procedure h-prog.
