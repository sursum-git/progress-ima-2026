DISABLE TRIGGERS FOR LOAD OF tit_acr.
DISABLE TRIGGERS FOR DUMP OF tit_acr.
DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCont2 AS INTEGER     NO-UNDO INIT 999.
DEFINE TEMP-TABLE ttFatduplic LIKE EMS2MED.fat-duplic
       FIELD nota AS CHAR FORMAT 'x(20)'
       FIELD rRowid AS ROWID.
ASSIGN iCont2 = TIME.
//REPEAT iCont2 = 10 TO 320 :


FOR EACH EMS2MED.nota-fiscal NO-LOCK
    WHERE nota-fiscal.dt-emis-nota >= 11.03.2017
    AND   nota-fiscal.dt-emis-nota <= 11.06.2017
    AND nota-fiscal.dt-cancel = ?
    /*BY nota-fiscal.nr-nota-fis DESC*/.
    RUN acertaNota(ROWID(nota-fiscal)).
END.

OUTPUT TO value('c:\temp\bkpfatduplic' + STRING(TIME) + '.txt').
FOR EACH ttFatDuplic:
    EXPORT  ttfatDuplic EXCEPT ttfatDuplic.nota ttfatDuplic.rRowid.
END.
OUTPUT CLOSE.

FOR EACH ttfatDuplic:
    CREATE ems2med.fat-duplic.
            ASSIGN  ems2med.fat-duplic.nr-fatura        = TTFATDUPLIC.NOTA
                    ems2med.fat-duplic.serie            = TTFATDUPLIC.serie                                       
                    ems2med.fat-duplic.nome-ab-cli      = TTFATDUPLIC.nome-ab-cli                    
                    ems2med.fat-duplic.parcela          = TTFATDUPLIC.parcela                        
                    ems2med.fat-duplic.dt-venciment     = TTFATDUPLIC.dt-venciment                   
                    ems2med.fat-duplic.vl-parcela       = TTFATDUPLIC.vl-parcela                     
                    ems2med.fat-duplic.flag-atualiz     = TTFATDUPLIC.flag-atualiz                   
                    ems2med.fat-duplic.nat-operacao     = TTFATDUPLIC.nat-operacao                   
                    ems2med.fat-duplic.dt-desconto      = TTFATDUPLIC.dt-desconto                    
                    ems2med.fat-duplic.cod-vencto       = TTFATDUPLIC.cod-vencto                     
                    ems2med.fat-duplic.ind-fat-nota     = TTFATDUPLIC.ind-fat-nota                   
                    ems2med.fat-duplic.cod-estabel      = TTFATDUPLIC.cod-estabel                    
                    ems2med.fat-duplic.ct-recven        = TTFATDUPLIC.ct-recven                      
                    ems2med.fat-duplic.sc-recven        = TTFATDUPLIC.sc-recven                      
                    ems2med.fat-duplic.vl-desconto      = TTFATDUPLIC.vl-desconto                    
                    ems2med.fat-duplic.vl-comis         = TTFATDUPLIC.vl-comis                       
                    ems2med.fat-duplic.nr-pedido        = TTFATDUPLIC.nr-pedido                      
                    ems2med.fat-duplic.vl-acum-dup      = TTFATDUPLIC.vl-acum-dup                    
                    ems2med.fat-duplic.dt-emissao       = TTFATDUPLIC.dt-emissao                     
                    ems2med.fat-duplic.vl-icms          = TTFATDUPLIC.vl-icms                        
                    ems2med.fat-duplic.vl-iss           = TTFATDUPLIC.vl-iss                         
                    ems2med.fat-duplic.vl-despesa       = TTFATDUPLIC.vl-despesa                     
                    ems2med.fat-duplic.vl-mercad        = TTFATDUPLIC.vl-mercad                      
                    ems2med.fat-duplic.vl-ipi           = TTFATDUPLIC.vl-ipi                         
                    ems2med.fat-duplic.vl-icmsr         = TTFATDUPLIC.vl-icmsr                       
                    ems2med.fat-duplic.vl-finsocial     = TTFATDUPLIC.vl-finsocial                   
                    ems2med.fat-duplic.vl-pis           = TTFATDUPLIC.vl-pis                         
                    ems2med.fat-duplic.vl-irf           = TTFATDUPLIC.vl-irf                         
                    ems2med.fat-duplic.vl-comis-e       = TTFATDUPLIC.vl-comis-e                     
                    ems2med.fat-duplic.vl-parcela-e     = TTFATDUPLIC.vl-parcela-e                   
                    ems2med.fat-duplic.vl-desconto-e    = TTFATDUPLIC.vl-desconto-e                  
                    ems2med.fat-duplic.vl-acumdup-e     = TTFATDUPLIC.vl-acumdup-e                   
                    ems2med.fat-duplic.vl-icms-e        = TTFATDUPLIC.vl-icms-e                      
                    ems2med.fat-duplic.vl-iss-e         = TTFATDUPLIC.vl-iss-e                       
                    ems2med.fat-duplic.vl-despesa-e     = TTFATDUPLIC.vl-despesa-e                   
                    ems2med.fat-duplic.vl-mercad-e      = TTFATDUPLIC.vl-mercad-e                    
                    ems2med.fat-duplic.vl-ipi-e         = TTFATDUPLIC.vl-ipi-e                       
                    ems2med.fat-duplic.vl-icmsr-e       = TTFATDUPLIC.vl-icmsr-e                     
                    ems2med.fat-duplic.vl-finsocial-e   = TTFATDUPLIC.vl-finsocial-e                 
                    ems2med.fat-duplic.vl-pis-e         = TTFATDUPLIC.vl-pis-e                       
                    ems2med.fat-duplic.vl-irf-e         = TTFATDUPLIC.vl-irf-e                       
                    ems2med.fat-duplic.cod-esp          = TTFATDUPLIC.cod-esp                        
                    ems2med.fat-duplic.tp-receita       = TTFATDUPLIC.tp-receita                     
                    ems2med.fat-duplic.tx-pr-emis-fasb  = TTFATDUPLIC.tx-pr-emis-fasb                
                    ems2med.fat-duplic.tx-pr-vcto-fasb  = TTFATDUPLIC.tx-pr-vcto-fasb                
                    ems2med.fat-duplic.tx-cmi-emis      = TTFATDUPLIC.tx-cmi-emis                    
                    ems2med.fat-duplic.tx-anbid-emis-cmi= TTFATDUPLIC.tx-anbid-emis-cmi              
                    ems2med.fat-duplic.mo-negoc         = TTFATDUPLIC.mo-negoc                       
                    ems2med.fat-duplic.vl-moeda-fasb    = TTFATDUPLIC.vl-moeda-fasb                  
                    ems2med.fat-duplic.char-1           = TTFATDUPLIC.char-1                         
                    ems2med.fat-duplic.char-2           = TTFATDUPLIC.char-2                         
                    ems2med.fat-duplic.dec-1            = TTFATDUPLIC.dec-1                          
                    ems2med.fat-duplic.dec-2            = TTFATDUPLIC.dec-2                          
                    ems2med.fat-duplic.int-1            = TTFATDUPLIC.int-1                          
                    ems2med.fat-duplic.int-2            = TTFATDUPLIC.int-2                          
                    ems2med.fat-duplic.log-1            = TTFATDUPLIC.log-1                          
                    ems2med.fat-duplic.log-2            = TTFATDUPLIC.log-2                          
                    ems2med.fat-duplic.data-1           = TTFATDUPLIC.data-1                         
                    ems2med.fat-duplic.data-2           = TTFATDUPLIC.data-2                         
                    ems2med.fat-duplic.check-sum        = TTFATDUPLIC.check-sum                      
                    ems2med.fat-duplic.vl-parcela-me    = TTFATDUPLIC.vl-parcela-me                  
                    ems2med.fat-duplic.vl-desconto-me   = TTFATDUPLIC.vl-desconto-me                 
                    ems2med.fat-duplic.vl-comis-me      = TTFATDUPLIC.vl-comis-me                    
                    ems2med.fat-duplic.vl-acum-dup-me   = TTFATDUPLIC.vl-acum-dup-me                 
                    ems2med.fat-duplic.vl-despesa-me    = TTFATDUPLIC.vl-despesa-me                  
                    ems2med.fat-duplic.vl-mercad-me     = TTFATDUPLIC.vl-mercad-me                   
                    ems2med.fat-duplic.vl-irf-me        = TTFATDUPLIC.vl-irf-me                      
                    ems2med.fat-duplic.vl-moeda-fasb-me = TTFATDUPLIC.vl-moeda-fasb-me               
                    ems2med.fat-duplic.vl-frete         = TTFATDUPLIC.vl-frete                       
                    ems2med.fat-duplic.vl-tax-fre-div   = TTFATDUPLIC.vl-tax-fre-div                 
                    ems2med.fat-duplic.perc-desc-an     = TTFATDUPLIC.perc-desc-an                   
                    ems2med.fat-duplic.log-livre-1      = TTFATDUPLIC.log-livre-1                    
                    ems2med.fat-duplic.log-livre-2      = TTFATDUPLIC.log-livre-2                   
                    ems2med.fat-duplic.log-livre-3      = TTFATDUPLIC.log-livre-3                   
                    ems2med.fat-duplic.log-livre-4      = TTFATDUPLIC.log-livre-4                    
                    ems2med.fat-duplic.log-livre-5      = TTFATDUPLIC.log-livre-5                    
                    ems2med.fat-duplic.log-livre-6      = TTFATDUPLIC.log-livre-6                    
                    ems2med.fat-duplic.cod-livre-1      = TTFATDUPLIC.cod-livre-1                    
                    ems2med.fat-duplic.cod-livre-2      = TTFATDUPLIC.cod-livre-2                    
                    ems2med.fat-duplic.num-livre-1      = TTFATDUPLIC.num-livre-1                    
                    ems2med.fat-duplic.num-livre-2      = TTFATDUPLIC.num-livre-2                    
                    ems2med.fat-duplic.val-livre-1      = TTFATDUPLIC.val-livre-1                    
                    ems2med.fat-duplic.val-livre-2      = TTFATDUPLIC.val-livre-2                    
                    ems2med.fat-duplic.dat-livre-1      = TTFATDUPLIC.dat-livre-1                    
                    ems2med.fat-duplic.dat-livre-2      = TTFATDUPLIC.dat-livre-2                    
                    ems2med.fat-duplic.cod-formula      = TTFATDUPLIC.cod-formula                    
                    ems2med.fat-duplic.val-retenc-pis   = TTFATDUPLIC.val-retenc-pis                 
                    ems2med.fat-duplic.val-retenc-cofins= TTFATDUPLIC.val-retenc-cofins              
                    ems2med.fat-duplic.val-retenc-csll  = TTFATDUPLIC.val-retenc-csll                
                    ems2med.fat-duplic.val-base-contrib-social     = TTFATDUPLIC.val-base-contrib-social       
                    ems2med.fat-duplic.log-impto-retid             = TTFATDUPLIC.log-impto-retid               
                    ems2med.fat-duplic.cod-safra                   = TTFATDUPLIC.cod-safra                     
                    ems2med.fat-duplic.val-tax-ptlidad             = TTFATDUPLIC.val-tax-ptlidad               
                    ems2med.fat-duplic.cod-familia-mater           = TTFATDUPLIC.cod-familia-mater             
                    ems2med.fat-duplic.qti-dias-carenc             = TTFATDUPLIC.qti-dias-carenc               
                    ems2med.fat-duplic.cod-cta-ctbl-recta-vda      = TTFATDUPLIC.cod-cta-ctbl-recta-vda    . 
END.


//END.

PROCEDURE acertaNota:
DEFINE INPUT  PARAMETER rRowid AS ROWID       NO-UNDO.
DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.
FIND EMS2MED.nota-fiscal
    WHERE ROWID(nota-fiscal) = rRowid  NO-ERROR.
IF AVAIL nota-fiscal THEN DO:
   IF nota-fiscal.nr-fatura <> nota-fiscal.nr-nota-fis THEN
      ASSIGN nota-fiscal.nr-fatura = nota-fiscal.nr-nota-fis.
   ASSIGN iCont = 0.
   FOR EACH EMS2MED.fat-duplic EXCLUSIVE-LOCK
        WHERE fat-duplic.cod-estabel = nota-fiscal.cod-estabel
        AND   fat-duplic.serie = nota-fiscal.serie
        AND   fat-duplic.nr-fatura = string(int(nota-fiscal.nr-nota-fis) - 1,'9999999').
        ASSIGN iCont = iCont + 1.
        
        ASSIGN ttFatDuplic.nota = nota-fiscal.nr-nota-fis
               ttFatDuplic.rRowid = ROWID(fat-duplic).
        DELETE fat-duplic.
   END. 
   IF iCont = 0 THEN DO:
      FOR EACH fat-duplic EXCLUSIVE-LOCK
        WHERE fat-duplic.cod-estabel = nota-fiscal.cod-estabel
        AND   fat-duplic.serie = nota-fiscal.serie
        AND   fat-duplic.nr-fatura = nota-fiscal.nr-nota-fis.
        ASSIGN iCont = iCont + 1.
        CREATE ttfatDuplic.
        BUFFER-COPY fat-duplic TO ttFatDuplic.
        ASSIGN ttFatDuplic.nota = nota-fiscal.nr-nota-fis
               ttFatDuplic.rRowid = ROWID(fat-duplic).
        DELETE fat-duplic.
      END.  
   END.
END.
END PROCEDURE.


/* /*IF nota-fiscal.nr-fatura <> nota-fiscal.nr-nota-fis THEN
       ASSIGN nota-fiscal.nr-fatura = nota-fiscal.nr-nota-fis.*/
    FOR EACH fat-duplic EXCLUSIVE-LOCK
        WHERE fat-duplic.cod-estabel = nota-fiscal.cod-estabel
        AND   fat-duplic.serie = nota-fiscal.serie
        AND   fat-duplic.nr-fatura = string(int(nota-fiscal.nr-nota-fis) - 1,'9999999').
        ASSIGN fat-duplic.nr-fatura = nota-fiscal.nr-nota-fis.
        DISP nota-fiscal.nr-nota-fis.


       /* DISP nota-fiscal.nr-nota-fis fat-duplic.nr-fatura.*/
        /*FIND FIRST  tit_acr
            WHERE tit_acr.cod_estab = '501'
            AND   tit_acr.cod_espec_docto = fat-duplic.cod-esp
            AND   tit_acr.cod_ser_docto = fat-duplic.serie
            AND   tit_acr.cod_tit_acr =  string(int(nota-fiscal.nr-nota-fis) - 1,'9999999')
            AND   tit_acr.cod_parcela = fat-duplic.parcela 
            EXCLUSIVE-LOCK NO-ERROR.
            /*DISP nota-fiscal.cod-estabel
                    fat-duplic.serie 
                    string(int(nota-fiscal.nr-nota-fis) - 1,'9999999') 
                    fat-duplic.parcela
                    fat-duplic.cod-esp.*/
                
        IF AVAIL tit_acr THEN DO:
           ASSIGN  iCont = 0.
           FOR EACH  movto_tit_acr OF tit_acr NO-LOCK.
               ASSIGN iCont = iCont + 1.
           END.
           IF iCont  > 1  THEN NEXT.
           EXPORT DELIMITER "|" tit_acr.cod_estab tit_acr.cod_tit_acr 
                  tit_acr.cod_ser_docto tit_acr.cod_parcela "titulo alterado".
           ASSIGN tit_acr.cod_tit_acr = nota-fiscal.nr-nota-fis.
        END.
        ELSE DO:
             EXPORT DELIMITER "|" nota-fiscal.cod-estabel nota-fiscal.nr-nota-fis 
                  nota-fiscal.serie fat-duplic.parcela "titulo n∆o encontrado". 
        END. */                                                                   
*/
