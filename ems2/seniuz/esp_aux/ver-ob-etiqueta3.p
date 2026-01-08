DEF VAR c-situacao AS CHAR.
DEF VAR i-cont AS INT.
DEF VAR l-ok AS LOG.
DEF VAR dt-nota LIKE nota-fiscal.dt-emis-nota.
def var h-acomp as handle no-undo.

def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

OUTPUT TO "c:/lixo/lixo.csv" CONVERT SOURCE "ibm850".
PUT "Data-OB;"
    "Num-Ob;"
    "Seq;"
    "Num-Etiqueta;"
    "Localiz;"
    "Situa‡Æo;"
    "Quantidade;"
    "Lote;"
    "Romaneio?;"
    "Reserva?;"
    "Data-NF"
    SKIP.

FOR EACH ob-etiqueta NO-LOCK.
    
    run pi-acompanhar in h-acomp (input "Num-OB/Seq: " + STRING(ob-etiqueta.nr-ob,"999999") + 
                                        "/" + STRING(ob-etiqueta.nr-sequencia,"-999") + 
                                        " Erros: " + STRING(i-cont)). 

    ASSIGN l-ok = NO.

    {esinc/i-dsrb.i ob-etiqueta.situacao ob-etiqueta.situacao c-situacao}
    FIND ped-item-rom WHERE ped-item-rom.nr-ob = ob-etiqueta.nr-ob
                        AND ped-item-rom.nr-seq-etq = ob-etiqueta.nr-sequencia
                      NO-LOCK NO-ERROR.
    IF NOT AVAIL ped-item-rom AND ob-etiqueta.situacao <> 5 THEN  /* NÆo Faturada */
       ASSIGN l-ok = YES.

    FIND ped-item-res WHERE ped-item-res.nome-abrev   = ped-item-rom.nome-abrev
                        AND ped-item-res.nr-pedcli    = ped-item-rom.nr-pedcli
                        AND ped-item-res.nr-sequencia = ped-item-rom.nr-sequencia
                      NO-LOCK NO-ERROR.
    IF NOT AVAIL ped-item-rom AND ob-etiqueta.situacao = 5 AND NOT AVAIL ped-item-res THEN /* Faturada, sem romaneio */
       ASSIGN l-ok = YES.

    IF AVAIL ped-item-rom AND AVAIL ped-item-res THEN DO:
       IF (ob-etiqueta.situacao = 5 AND ped-item-res.faturado) OR
          (ob-etiqueta.situacao <> 5 AND ped-item-res.faturado = NO) THEN
          ASSIGN l-ok = YES.
    END.

    IF ob-etiqueta.situacao = 2 THEN DO:
       IF ob-etiqueta.dt-emissao = TODAY THEN
          ASSIGN l-ok = YES.
       ELSE
          ASSIGN l-ok = NO.
    END.

    IF l-ok THEN 
       NEXT.

    ASSIGN dt-nota = ?.
    IF AVAIL ped-item-res AND ped-item-res.faturado THEN DO:
       FIND nota-fiscal WHERE nota-fiscal.cod-estabel = ped-item-res.cod-estabel
                          AND nota-fiscal.serie       = ped-item-res.serie
                          AND nota-fiscal.nr-nota-fis = STRING(ped-item-res.nr-nota-fis,"9999999")
                        NO-LOCK NO-ERROR.
       IF AVAIL nota-fiscal THEN
          ASSIGN dt-nota = nota-fiscal.dt-emis-nota.
    END.

    PUT ob-etiqueta.dt-emissao ";"
        ob-etiqueta.nr-ob ";"
        ob-etiqueta.nr-sequencia ";"
        ob-etiqueta.num-etiqueta ";"
        ob-etiqueta.localizacao ";"
        c-situacao ";"
        ob-etiqueta.quantidade ";"
        ob-etiqueta.nr-lote ";"
        AVAIL ped-item-rom ";"
        AVAIL ped-item-res ";"
        dt-nota
        SKIP.
    ASSIGN i-cont = i-cont + 1.
END.
MESSAGE "Leia o resultado e pressione Ok..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
run pi-finalizar in h-acomp.

OUTPUT CLOSE.

run Execute in h-prog(input "excel.exe", input "c:\lixo\lixo.csv").
delete procedure h-prog.
