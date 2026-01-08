DEFINE TEMP-TABLE tt
    FIELD conta AS CHAR FORMAT 'x(30)' 
    FIELD cc    AS CHAR.

DEFINE TEMP-TABLE ttDwf LIKE dwf-cta-ctbl-refer.




DEFINE VARIABLE codEmp AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ano AS INTEGER     NO-UNDO.
UPDATE codEmp ano.
FOR EACH item_lancto_ctbl
    WHERE year(item_lancto_ctbl.dat_lancto_ctbl) = ano
    AND cod_empresa = codEmp
    AND substr(item_lancto_ctbl.cod_cta_ctbl,1,1) > '2' 
    AND substr(item_lancto_ctbl.cod_cta_ctbl,1,1) < '5':
    FIND FIRST tt NO-LOCK
        WHERE tt.conta = item_lancto_ctbl.cod_cta_ctbl
        AND   tt.cc    = item_lancto_ctbl.cod_ccusto NO-ERROR.
    IF NOT AVAIL tt THEN DO:
       CREATE tt.
       ASSIGN tt.conta = item_lancto_ctbl.cod_cta_ctbl
              tt.cc    = item_lancto_ctbl.cod_ccusto .
    END.
END.

FOR EACH tt:
    FIND FIRST dwf-cta-ctbl-refer
        WHERE dwf-cta-ctbl-refer.cod-cta-ctbl = tt.conta NO-LOCK NO-ERROR.
    IF AVAIL dwf-cta-ctbl-refer THEN DO:
       CREATE ttDwf.
       ASSIGN ttDwf.cod-empresa            =   dwf-cta-ctbl-refer.cod-empresa               
              ttDwf.cod-cta-ctbl-refer     =   dwf-cta-ctbl-refer.cod-cta-ctbl-refer       
              ttDwf.cod-ccusto             =   tt.cc   
              ttDwf.dat-inic-period        =   dwf-cta-ctbl-refer.dat-inic-period          
              ttDwf.dat-fim-period         =   dwf-cta-ctbl-refer.dat-fim-period           
              ttDwf.dat-inic-valid         =   dwf-cta-ctbl-refer.dat-inic-valid           
              ttDwf.dat-fim-valid          =   dwf-cta-ctbl-refer.dat-fim-valid            
           /*         cod-livre-1                   
                    cod-livre-2                   
                    log-livre-1                   
                    log-livre-2                   
                    num-livre-1                   
                    num-livre-2                   
                    val-livre-1                   
                    val-livre-2                   
                    dat-livre-1                   
                    dat-livre-2                   */
              ttDwf.cod-estab               =  dwf-cta-ctbl-refer .cod-estab              
              ttDwf.cod-cta-ctbl            = dwf-cta-ctbl-refer.cod-cta-ctbl    
              ttDwf.ind-natur-cta-ctbl      =  dwf-cta-ctbl-refer .ind-natur-cta-ctbl     
              ttDwf.cod-produto             =  dwf-cta-ctbl-refer .cod-produto            
              ttDwf.cod-unid-neg            =  dwf-cta-ctbl-refer .cod-unid-neg .
    END.
END.
OUTPUT TO c:\temp\ttDWF.txt.
FOR EACH ttDWF:
    EXPORT DELIMITER "|" ttDWF .
    FIND FIRST dwf-cta-ctbl-refer OF ttDWF NO-LOCK NO-ERROR.
    IF NOT AVAIL dwf-cta-ctbl-refer THEN DO:
       CREATE dwf-cta-ctbl-refer.
       BUFFER-COPY ttDWF TO dwf-cta-ctbl-refer NO-ERROR.
    END.
END.
OUTPUT CLOSE.

/***************
  10  cod-empresa                      char        im
   20 cod-cta-ctbl-refer               char        m
   30 cod-ccusto                       char        im
   40 dat-inic-period                  date        im
   50 dat-fim-period                   date        im
   60 dat-inic-valid                   date        im
   70 dat-fim-valid                    date
   80 cod-livre-1                      char
   90 cod-livre-2                      char
  100 log-livre-1                      logi
  110 log-livre-2                      logi
  120 num-livre-1                      inte
  130 num-livre-2                      inte
  140 val-livre-1                      deci-10
  150 val-livre-2                      deci-10
  160 dat-livre-1                      date
  170 dat-livre-2                      date
  180 cod-estab                        char
  190 cod-cta-ctbl                     char        im
  200 ind-natur-cta-ctbl               char
  210 cod-produto                      char        i
  220 cod-unid-neg                     char        im
******************************/
