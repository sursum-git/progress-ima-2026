DEFINE VARIABLE dTot AS DECIMAL     NO-UNDO.
FOR EACH ems2med.nota-fiscal
    WHERE ems2med.nota-fiscal.dt-emis-nota >= 11.03.2017
    AND  ems2med.nota-fiscal.dt-emis-nota  <=  11.06.2017
    BY ems2med.nota-fiscal.nr-nota-fis.
    //DISP  ems2med.nota-fiscal.nr-nota-fis ems2med.nota-fiscal.vl-tot-nota.
    ASSIGN dTot = 0.
    FOR EACH medprod.fat-duplic NO-LOCK
        WHERE medprod.fat-duplic.cod-estabel = nota-fiscal.cod-estabel
        AND   medprod.fat-duplic.serie = nota-fiscal.serie
        AND   string(int(medprod.fat-duplic.nr-fatura) + 1, '9999999' ) = nota-fiscal.nr-nota-fis.
        ASSIGN dTot = dTot + fat-duplic.vl-parcela.
    END.
    //DISP dTot.
    MESSAGE "nota fiscal:" ems2med.nota-fiscal.nr-nota-fis SKIP
            "valor:" ems2med.nota-fiscal.vl-tot-nota SKIP
            "tot.duplic:" dTot SKIP
            "confirma c¢pia?" UPDATE lResposta AS LOGICAL
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO.
    IF lResposta THEN DO:
       FOR EACH medprod.fat-duplic NO-LOCK
        WHERE medprod.fat-duplic.cod-estabel = nota-fiscal.cod-estabel
        AND   medprod.fat-duplic.serie = nota-fiscal.serie
        AND   string(int(medprod.fat-duplic.nr-fatura) + 1, '9999999' ) = nota-fiscal.nr-nota-fis.
            CREATE ems2med.fat-duplic.
            ASSIGN  ems2med.fat-duplic.nr-fatura        = ems2med.nota-fiscal.nr-nota-fis
                    ems2med.fat-duplic.serie            = MEDPROD.fat-duplic.serie                                       
                    ems2med.fat-duplic.nome-ab-cli      = MEDPROD.fat-duplic.nome-ab-cli                    
                    ems2med.fat-duplic.parcela          = MEDPROD.fat-duplic.parcela                        
                    ems2med.fat-duplic.dt-venciment     = MEDPROD.fat-duplic.dt-venciment                   
                    ems2med.fat-duplic.vl-parcela       = MEDPROD.fat-duplic.vl-parcela                     
                    ems2med.fat-duplic.flag-atualiz     = MEDPROD.fat-duplic.flag-atualiz                   
                    ems2med.fat-duplic.nat-operacao     = MEDPROD.fat-duplic.nat-operacao                   
                    ems2med.fat-duplic.dt-desconto      = MEDPROD.fat-duplic.dt-desconto                    
                    ems2med.fat-duplic.cod-vencto       = MEDPROD.fat-duplic.cod-vencto                     
                    ems2med.fat-duplic.ind-fat-nota     = MEDPROD.fat-duplic.ind-fat-nota                   
                    ems2med.fat-duplic.cod-estabel      = MEDPROD.fat-duplic.cod-estabel                    
                    ems2med.fat-duplic.ct-recven        = MEDPROD.fat-duplic.ct-recven                      
                    ems2med.fat-duplic.sc-recven        = MEDPROD.fat-duplic.sc-recven                      
                    ems2med.fat-duplic.vl-desconto      = MEDPROD.fat-duplic.vl-desconto                    
                    ems2med.fat-duplic.vl-comis         = MEDPROD.fat-duplic.vl-comis                       
                    ems2med.fat-duplic.nr-pedido        = MEDPROD.fat-duplic.nr-pedido                      
                    ems2med.fat-duplic.vl-acum-dup      = MEDPROD.fat-duplic.vl-acum-dup                    
                    ems2med.fat-duplic.dt-emissao       = MEDPROD.fat-duplic.dt-emissao                     
                    ems2med.fat-duplic.vl-icms          = MEDPROD.fat-duplic.vl-icms                        
                    ems2med.fat-duplic.vl-iss           = MEDPROD.fat-duplic.vl-iss                         
                    ems2med.fat-duplic.vl-despesa       = MEDPROD.fat-duplic.vl-despesa                     
                    ems2med.fat-duplic.vl-mercad        = MEDPROD.fat-duplic.vl-mercad                      
                    ems2med.fat-duplic.vl-ipi           = MEDPROD.fat-duplic.vl-ipi                         
                    ems2med.fat-duplic.vl-icmsr         = MEDPROD.fat-duplic.vl-icmsr                       
                    ems2med.fat-duplic.vl-finsocial     = MEDPROD.fat-duplic.vl-finsocial                   
                    ems2med.fat-duplic.vl-pis           = MEDPROD.fat-duplic.vl-pis                         
                    ems2med.fat-duplic.vl-irf           = MEDPROD.fat-duplic.vl-irf                         
                    ems2med.fat-duplic.vl-comis-e       = MEDPROD.fat-duplic.vl-comis-e                     
                    ems2med.fat-duplic.vl-parcela-e     = MEDPROD.fat-duplic.vl-parcela-e                   
                    ems2med.fat-duplic.vl-desconto-e    = MEDPROD.fat-duplic.vl-desconto-e                  
                    ems2med.fat-duplic.vl-acumdup-e     = MEDPROD.fat-duplic.vl-acumdup-e                   
                    ems2med.fat-duplic.vl-icms-e        = MEDPROD.fat-duplic.vl-icms-e                      
                    ems2med.fat-duplic.vl-iss-e         = MEDPROD.fat-duplic.vl-iss-e                       
                    ems2med.fat-duplic.vl-despesa-e     = MEDPROD.fat-duplic.vl-despesa-e                   
                    ems2med.fat-duplic.vl-mercad-e      = MEDPROD.fat-duplic.vl-mercad-e                    
                    ems2med.fat-duplic.vl-ipi-e         = MEDPROD.fat-duplic.vl-ipi-e                       
                    ems2med.fat-duplic.vl-icmsr-e       = MEDPROD.fat-duplic.vl-icmsr-e                     
                    ems2med.fat-duplic.vl-finsocial-e   = MEDPROD.fat-duplic.vl-finsocial-e                 
                    ems2med.fat-duplic.vl-pis-e         = MEDPROD.fat-duplic.vl-pis-e                       
                    ems2med.fat-duplic.vl-irf-e         = MEDPROD.fat-duplic.vl-irf-e                       
                    ems2med.fat-duplic.cod-esp          = MEDPROD.fat-duplic.cod-esp                        
                    ems2med.fat-duplic.tp-receita       = MEDPROD.fat-duplic.tp-receita                     
                    ems2med.fat-duplic.tx-pr-emis-fasb  = MEDPROD.fat-duplic.tx-pr-emis-fasb                
                    ems2med.fat-duplic.tx-pr-vcto-fasb  = MEDPROD.fat-duplic.tx-pr-vcto-fasb                
                    ems2med.fat-duplic.tx-cmi-emis      = MEDPROD.fat-duplic.tx-cmi-emis                    
                    ems2med.fat-duplic.tx-anbid-emis-cmi= MEDPROD.fat-duplic.tx-anbid-emis-cmi              
                    ems2med.fat-duplic.mo-negoc         = MEDPROD.fat-duplic.mo-negoc                       
                    ems2med.fat-duplic.vl-moeda-fasb    = MEDPROD.fat-duplic.vl-moeda-fasb                  
                    ems2med.fat-duplic.char-1           = MEDPROD.fat-duplic.char-1                         
                    ems2med.fat-duplic.char-2           = MEDPROD.fat-duplic.char-2                         
                    ems2med.fat-duplic.dec-1            = MEDPROD.fat-duplic.dec-1                          
                    ems2med.fat-duplic.dec-2            = MEDPROD.fat-duplic.dec-2                          
                    ems2med.fat-duplic.int-1            = MEDPROD.fat-duplic.int-1                          
                    ems2med.fat-duplic.int-2            = MEDPROD.fat-duplic.int-2                          
                    ems2med.fat-duplic.log-1            = MEDPROD.fat-duplic.log-1                          
                    ems2med.fat-duplic.log-2            = MEDPROD.fat-duplic.log-2                          
                    ems2med.fat-duplic.data-1           = MEDPROD.fat-duplic.data-1                         
                    ems2med.fat-duplic.data-2           = MEDPROD.fat-duplic.data-2                         
                    ems2med.fat-duplic.check-sum        = MEDPROD.fat-duplic.check-sum                      
                    ems2med.fat-duplic.vl-parcela-me    = MEDPROD.fat-duplic.vl-parcela-me                  
                    ems2med.fat-duplic.vl-desconto-me   = MEDPROD.fat-duplic.vl-desconto-me                 
                    ems2med.fat-duplic.vl-comis-me      = MEDPROD.fat-duplic.vl-comis-me                    
                    ems2med.fat-duplic.vl-acum-dup-me   = MEDPROD.fat-duplic.vl-acum-dup-me                 
                    ems2med.fat-duplic.vl-despesa-me    = MEDPROD.fat-duplic.vl-despesa-me                  
                    ems2med.fat-duplic.vl-mercad-me     = MEDPROD.fat-duplic.vl-mercad-me                   
                    ems2med.fat-duplic.vl-irf-me        = MEDPROD.fat-duplic.vl-irf-me                      
                    ems2med.fat-duplic.vl-moeda-fasb-me = MEDPROD.fat-duplic.vl-moeda-fasb-me               
                    ems2med.fat-duplic.vl-frete         = MEDPROD.fat-duplic.vl-frete                       
                    ems2med.fat-duplic.vl-tax-fre-div   = MEDPROD.fat-duplic.vl-tax-fre-div                 
                    ems2med.fat-duplic.perc-desc-an     = MEDPROD.fat-duplic.perc-desc-an                   
                    ems2med.fat-duplic.log-livre-1      = MEDPROD.fat-duplic.log-livre-1                    
                    ems2med.fat-duplic.log-livre-2      = MEDPROD.fat-duplic.log-livre-2                   
                    ems2med.fat-duplic.log-livre-3      = MEDPROD.fat-duplic.log-livre-3                   
                    ems2med.fat-duplic.log-livre-4      = MEDPROD.fat-duplic.log-livre-4                    
                    ems2med.fat-duplic.log-livre-5      = MEDPROD.fat-duplic.log-livre-5                    
                    ems2med.fat-duplic.log-livre-6      = MEDPROD.fat-duplic.log-livre-6                    
                    ems2med.fat-duplic.cod-livre-1      = MEDPROD.fat-duplic.cod-livre-1                    
                    ems2med.fat-duplic.cod-livre-2      = MEDPROD.fat-duplic.cod-livre-2                    
                    ems2med.fat-duplic.num-livre-1      = MEDPROD.fat-duplic.num-livre-1                    
                    ems2med.fat-duplic.num-livre-2      = MEDPROD.fat-duplic.num-livre-2                    
                    ems2med.fat-duplic.val-livre-1      = MEDPROD.fat-duplic.val-livre-1                    
                    ems2med.fat-duplic.val-livre-2      = MEDPROD.fat-duplic.val-livre-2                    
                    ems2med.fat-duplic.dat-livre-1      = MEDPROD.fat-duplic.dat-livre-1                    
                    ems2med.fat-duplic.dat-livre-2      = MEDPROD.fat-duplic.dat-livre-2                    
                    ems2med.fat-duplic.cod-formula      = MEDPROD.fat-duplic.cod-formula                    
                    ems2med.fat-duplic.val-retenc-pis   = MEDPROD.fat-duplic.val-retenc-pis                 
                    ems2med.fat-duplic.val-retenc-cofins= MEDPROD.fat-duplic.val-retenc-cofins              
                    ems2med.fat-duplic.val-retenc-csll  = MEDPROD.fat-duplic.val-retenc-csll                
                    ems2med.fat-duplic.val-base-contrib-social     = MEDPROD.fat-duplic.val-base-contrib-social       
                    ems2med.fat-duplic.log-impto-retid             = MEDPROD.fat-duplic.log-impto-retid               
                    ems2med.fat-duplic.cod-safra                   = MEDPROD.fat-duplic.cod-safra                     
                    ems2med.fat-duplic.val-tax-ptlidad             = MEDPROD.fat-duplic.val-tax-ptlidad               
                    ems2med.fat-duplic.cod-familia-mater           = MEDPROD.fat-duplic.cod-familia-mater             
                    ems2med.fat-duplic.qti-dias-carenc             = MEDPROD.fat-duplic.qti-dias-carenc               
                    ems2med.fat-duplic.cod-cta-ctbl-recta-vda      = MEDPROD.fat-duplic.cod-cta-ctbl-recta-vda    .    
                      
       END.      
    END.                  
END.                            
                          
/*                      
                          
Order Field        Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   20 serie                            char        im
   35 nome-ab-cli                      char        im
   50 parcela                          char        im
   60 nr-fatura                        char        im
   70 dt-venciment                     date        i
   80 vl-parcela                       deci-2      m
   90 flag-atualiz                     logi        im
  170 nat-operacao                     char
  180 dt-desconto                      date
  270 cod-vencto                       inte        m
  280 ind-fat-nota                     inte        im
  290 cod-estabel                      char        im
  300 ct-recven                        char        im
  310 sc-recven                        char        im
  320 vl-desconto                      deci-2      m
  330 vl-comis                         deci-2      m
  340 nr-pedido                        inte        m
  350 vl-acum-dup                      deci-2      m
  360 dt-emissao                       date        i
  370 vl-icms                          deci-2      m
  380 vl-iss                           deci-2      m
  390 vl-despesa                       deci-2      m
  400 vl-mercad                        deci-2      m
  410 vl-ipi                           deci-2      m
  420 vl-icmsr                         deci-2      m
  430 vl-finsocial                     deci-2      m
  440 vl-pis                           deci-2      m
  450 vl-irf                           deci-2      m
  470 vl-comis-e                       deci-2[3]
  480 vl-parcela-e                     deci-2[3]
  490 vl-desconto-e                    deci-2[3]
  500 vl-acumdup-e                     deci-2[3]
  510 vl-icms-e                        deci-2[3]
  520 vl-iss-e                         deci-2[3]
  530 vl-despesa-e                     deci-2[3]
  540 vl-mercad-e                      deci-2[3]
  550 vl-ipi-e                         deci-2[3]
  560 vl-icmsr-e                       deci-2[3]
  570 vl-finsocial-e                   deci-2[3]
  580 vl-pis-e                         deci-2[3]
  590 vl-irf-e                         deci-2[3]
  600 cod-esp                          char        m
  670 tp-receita                       inte
  680 tx-pr-emis-fasb                  deci-8
  690 tx-pr-vcto-fasb                  deci-8
  700 tx-cmi-emis                      deci-8
  710 tx-anbid-emis-cmi                deci-8
  720 mo-negoc                         inte
  730 vl-moeda-fasb                    deci-2
  740 char-1                           char
  750 char-2                           char
  760 dec-1                            deci-8
  770 dec-2                            deci-8
  780 int-1                            inte
  790 int-2                            inte
  800 log-1                            logi
  810 log-2                            logi
  820 data-1                           date
  830 data-2                           date
  840 check-sum                        char
  850 vl-parcela-me                    deci-5      m
  860 vl-desconto-me                   deci-5      m
  870 vl-comis-me                      deci-5      m
  880 vl-acum-dup-me                   deci-5      m
  890 vl-despesa-me                    deci-5      m
  900 vl-mercad-me                     deci-5      m
  910 vl-irf-me                        deci-5      m
  920 vl-moeda-fasb-me                 deci-5
  930 vl-frete                         deci-2
  940 vl-tax-fre-div                   deci-2
  950 perc-desc-an                     deci-5
  960 log-livre-1                      logi
  970 log-livre-2                      logi
  980 log-livre-3                      logi
  990 log-livre-4                      logi
 1000 log-livre-5                      logi
 1010 log-livre-6                      logi
 1020 cod-livre-1                      char
 1030 cod-livre-2                      char
 1040 num-livre-1                      inte
 1050 num-livre-2                      inte
 1060 val-livre-1                      deci-8
 1070 val-livre-2                      deci-8
 1080 dat-livre-1                      date
 1090 dat-livre-2                      date
 1100 cod-formula                      char        im
 1110 val-retenc-pis                   deci-5      m
 1120 val-retenc-cofins                deci-5      m
 1130 val-retenc-csll                  deci-5      m
 1140 val-base-contrib-social          deci-2      m
 1150 log-impto-retid                  logi        m
 1160 cod-safra                        char
 1170 val-tax-ptlidad                  deci-2      m
 1180 cod-familia-mater                char
 1190 qti-dias-carenc                  inte        m
 1200 cod-cta-ctbl-recta-vda           char



*/
