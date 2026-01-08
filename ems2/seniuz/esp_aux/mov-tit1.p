DEF VAR de-sld-titulo AS DEC.
DEF VAR c-mov AS CHAR FORMAT "x(3)".

FOR EACH titulo WHERE titulo.ep-codigo   = 1  
                  AND titulo.cod-estabel = "2"
                  AND titulo.cod-esp     = "dp"    
                  AND titulo.serie       = "1"  
                  AND titulo.nr-docto    = "0098827"   
                  AND titulo.parcela     = "02"
                NO-LOCK.
    ASSIGN de-sld-titulo = 0.
    IF AVAIL titulo THEN DO:
      ASSIGN de-sld-titulo = titulo.vl-original.
       FOR EACH mov-tit OF titulo
          WHERE mov-tit.dt-movto <= 07/31/2006 NO-LOCK:
          IF mov-tit.contabilizou THEN DO:
             IF mov-tit.lancamento = 2 THEN
                ASSIGN de-sld-titulo = de-sld-titulo + mov-tit.vl-baixa.
             ELSE
                ASSIGN de-sld-titulo = de-sld-titulo - mov-tit.vl-baixa.
          END.
          
          {esinc/i-dsrb.i mov-tit.transacao mov-tit.transacao c-mov} 

          DISP mov-tit.lancamento VIEW-AS FILL-IN
               c-mov
               mov-tit.vl-baixa
               mov-tit.dt-baixa
               de-sld-titulo WITH width 300.
       END.
    END.
END.
