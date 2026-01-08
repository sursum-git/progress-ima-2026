DEFINE INPUT  PARAMETER pRowid      AS ROWID       NO-UNDO.
DEFINE OUTPUT PARAMETER pArqDanfe   AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER pArqXML     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAnoXML AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSerie  AS CHARACTER   NO-UNDO.
{esp/params.i}
FOR FIRST nota-fiscal FIELDS(cod-estabel serie nr-nota-fis cod-emitente)
    WHERE rowid(nota-fiscal) = pRowid NO-LOCK:
    
    ASSIGN 
    pArqDanfe  =  getDirDanfe(nota-fiscal.cod-estabel)
    pArqXML    =  getDirXML(nota-fiscal.cod-estabel) 
    pArqDanfe  =  pArqDanfe                                      + "\" + 
                  string(YEAR(nota-fiscal.dt-emis-nota))         + "\" + 
                  string(MONTH(nota-fiscal.dt-emis-nota),'99')   + "\" +
                  nota-fiscal.cod-estabel                        + "-" +
                  nota-fiscal.serie                              + "-" + 
                  nota-fiscal.nr-nota-fis                        + ".PDF"
                 .
    IF YEAR(nota-fiscal.dt-emis-nota) < year(TODAY) THEN
    DO:
         ASSIGN cAnoXML  =  STRING(YEAR(nota-fiscal.dt-emis-nota)) + "\"  
                pArqXML  =  pArqXML                                + "\" + 
                            cAnoXML
                .                             
    END.    
    IF int(nota-fiscal.serie) < 10 THEN 
       ASSIGN cSerie = '00' + nota-fiscal.serie .    
    ELSE
      IF int(nota-fiscal.serie) < 100 THEN  
         ASSIGN cSerie = '0' + nota-fiscal.serie .
       ELSE
         ASSIGN cSerie =   nota-fiscal.serie.

    pArqXML   =  pArqXML                        + "\" +
                 nota-fiscal.cod-estabel        +                           
                 cSerie                         +  
                 nota-fiscal.nr-nota-fis        + ".XML"
                 .
     /*MESSAGE pArqXML
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
    
END.
