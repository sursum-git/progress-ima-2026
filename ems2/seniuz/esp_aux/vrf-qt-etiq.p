DEF VAR i-ct AS INT.

DEF VAR i-nr-etiq LIKE bc-etiqueta.nr-etiq.
DEF VAR de-qtidade-atu AS DEC.
DEF VAR de-qtd AS DEC.
DEF BUFFER b-etiqueta FOR bc-etiqueta.

OUTPUT TO c:\temp\erro-corte.csv.

FOR EACH ob-etiqueta WHERE
         ob-etiqueta.situacao = 3 NO-LOCK.

    FIND bc-etiqueta WHERE
         bc-etiqueta.progressivo = ob-etiqueta.progressivo NO-LOCK NO-ERROR.
    
    IF NOT AVAIL bc-etiqueta THEN NEXT.

    ASSIGN de-qtd = 0.
    FOR EACH b-etiqueta WHERE
             b-etiqueta.nr-etiq-pai = bc-etiqueta.nr-etiq AND 
             b-etiqueta.dt-criacao >= 01.01.2012  NO-LOCK.
    
        ASSIGN de-qtd = de-qtd + b-etiqueta.qt-item.
    END.
    
    IF de-qtd = 0 THEN NEXT.

    ASSIGN de-qtidade-atu = 0.
    FOR EACH saldo-estoq WHERE
             saldo-estoq.it-codigo = ob-etiqueta.it-codigo AND
             saldo-estoq.cod-refer = ob-etiqueta.cod-refer NO-LOCK.
        ASSIGN de-qtidade-atu = de-qtidade-atu + saldo-estoq.qtidade-atu.
    END.

    DISP bc-etiqueta.progressivo FORMAT "x(15)" 
         ob-etiqueta.num-etiqueta
         bc-etiqueta.qt-item     LABEL "Qtd Orig" 
         de-qtd                  LABEL "Qtd Corte" 
         ob-etiqueta.quantidade  LABEL "Qtd Logistica" 
         de-qtidade-atu          LABEL "Sld Contabil"
         ob-etiqueta.it-codigo
         ob-etiqueta.cod-refer
         ob-etiqueta.localiz
         WITH WIDTH 550.
END.
    
DISP i-ct.
