/* Programa: corte-comerc.p
** Lista tabela de Cortes Comerciais
*/

DEF VAR c-tipo AS CHAR FORMAT "x(10)".

def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

OUTPUT TO "c:/lixo/lixo.txt" CONVERT SOURCE "ibm850".

FIND it-nota-fisc WHERE it-nota-fisc.cod-estabel = "3"
                    AND it-nota-fisc.serie       = "CF"
                    AND it-nota-fisc.nr-nota-fis = "0001629"
                    AND it-nota-fisc.nr-seq-fat  = 1010
                  NO-LOCK.
/*
UPDATE it-nota-fisc.qt-devolvida[1]
       it-nota-fisc.qt-devolvida[2].
*/
DISPLAY it-nota-fisc.cod-estabel                     
        it-nota-fisc.serie                           
        it-nota-fisc.nr-nota-fis                     
        it-nota-fisc.nr-seq-fat                      
        it-nota-fisc.it-codigo                       
        it-nota-fisc.peso-liq-fat                    
        it-nota-fisc.peso-bruto                      
        it-nota-fisc.qt-faturada                     
        it-nota-fisc.un-fatur                        
        it-nota-fisc.vl-pretab                       
        it-nota-fisc.vl-preori                       
        it-nota-fisc.vl-preuni                       
        it-nota-fisc.vl-merc-tab                     
        it-nota-fisc.vl-merc-ori                     
        it-nota-fisc.vl-merc-liq                     
        it-nota-fisc.vl-tot-item                     
        it-nota-fisc.per-des-item                    
        it-nota-fisc.nr-ordem                        
        it-nota-fisc.parcela                         
        it-nota-fisc.ind-imp-desc                    
        it-nota-fisc.baixa-estoq                     
        it-nota-fisc.tipo-con-est                    
        it-nota-fisc.cod-depos                       
        it-nota-fisc.nat-operacao                    
        it-nota-fisc.cd-trib-ipi                     
        it-nota-fisc.aliquota-ipi                    
        it-nota-fisc.perc-red-ipi                    
        it-nota-fisc.cd-trib-iss                     
        it-nota-fisc.aliquota-ISS                    
        it-nota-fisc.cd-trib-icm                     
        it-nota-fisc.aliquota-icm                    
        it-nota-fisc.perc-red-icm                    
        it-nota-fisc.atual-estat                     
        it-nota-fisc.vl-despes-it                    
        it-nota-fisc.vl-bicms-it                     
        it-nota-fisc.vl-icms-it                      
        it-nota-fisc.vl-icmsnt-it                    
        it-nota-fisc.vl-icmsou-it                    
        it-nota-fisc.vl-bipi-it                      
        it-nota-fisc.vl-ipi-it                       
        it-nota-fisc.vl-ipint-it                     
        it-nota-fisc.vl-ipiou-it                     
        it-nota-fisc.vl-biss-it                      
        it-nota-fisc.vl-iss-it                       
        it-nota-fisc.vl-issnt-it                     
        it-nota-fisc.vl-issou-it                     
        it-nota-fisc.vl-bsubs-it                     
        it-nota-fisc.vl-icmsub-it                    
        it-nota-fisc.manut-icm-it                    
        it-nota-fisc.manut-ipi-it                    
        it-nota-fisc.nr-pedido                       
        it-nota-fisc.vl-reajuste                     
        it-nota-fisc.vl-cuscontab                    
        it-nota-fisc.tipo-contr                      
        it-nota-fisc.vl-precon                       
        it-nota-fisc.perc-red-iss                    
        it-nota-fisc.ind-icm-ret                     
        it-nota-fisc.ct-cuscon                       
        it-nota-fisc.sc-cuscon                       
        it-nota-fisc.ct-cusven                       
        it-nota-fisc.sc-cusven                       
        it-nota-fisc.ind-imprenda                    
        it-nota-fisc.vl-irf-it                       
        it-nota-fisc.vl-merc-sicm                    
        it-nota-fisc.no-ab-vend                      
        it-nota-fisc.nr-ord-produ                    
        it-nota-fisc.dt-retorno                      
        it-nota-fisc.cd-vendedor                     
        it-nota-fisc.dt-cancela                      
        it-nota-fisc.dt-confirma                     
        it-nota-fisc.dt-emis-nota                    
        it-nota-fisc.codigo-rejei                    
        it-nota-fisc.desc-devol                      
        it-nota-fisc.nr-ficha-cq                     
        it-nota-fisc.per-des-icms                    
        it-nota-fisc.tipo-atend                      
        it-nota-fisc.ind-componen                    
        it-nota-fisc.vl-ir-adic                      
        it-nota-fisc.class-fiscal                    
        it-nota-fisc.ind-sit-nota                    
        it-nota-fisc.cod-servico                     
        it-nota-fisc.vl-finsocial                    
        it-nota-fisc.vl-pis                          
        it-nota-fisc.nr-nota-ant                     
        it-nota-fisc.serie-ant                       
        it-nota-fisc.cod-refer                       
        it-nota-fisc.nr-seq-ped                      
        it-nota-fisc.nr-pedcli                       
        it-nota-fisc.nome-ab-cli                     
        it-nota-fisc.vl-pretab-e                     
        it-nota-fisc.vl-preori-e                     
        it-nota-fisc.vl-preuni-e                     
        it-nota-fisc.vl-merctab-e                    
        it-nota-fisc.vl-mercori-e                    
        it-nota-fisc.vl-mercliq-e                    
        it-nota-fisc.vl-totitem-e                    
        it-nota-fisc.vl-despesit-e                   
        it-nota-fisc.vl-bicmsit-e                    
        it-nota-fisc.vl-icmsit-e                     
        it-nota-fisc.vl-icmsntit-e                   
        it-nota-fisc.vl-icmsouit-e                   
        it-nota-fisc.vl-bipiit-e                     
        it-nota-fisc.vl-ipiit-e                      
        it-nota-fisc.vl-ipintit-e                    
        it-nota-fisc.vl-ipiouit-e                    
        it-nota-fisc.vl-bissit-e                     
        it-nota-fisc.vl-issit-e                      
        it-nota-fisc.vl-issntit-e                    
        it-nota-fisc.vl-issouit-e                    
        it-nota-fisc.vl-bsubsit-e                    
        it-nota-fisc.vl-icmsubit-e                   
        it-nota-fisc.vl-reajuste-e                   
        it-nota-fisc.vl-cuscontab-e                  
        it-nota-fisc.vl-precon-e                     
        it-nota-fisc.vl-irfit-e                      
        it-nota-fisc.vl-mercsimc-e                   
        it-nota-fisc.vl-ipi-dev                      
        it-nota-fisc.vl-sub-emp                      
        it-nota-fisc.vl-comp-acum                    
        it-nota-fisc.it-nota-fisc                    
        it-nota-fisc.fat-retro                       
        it-nota-fisc.vl-icmscomp-it                  
        it-nota-fisc.vl-icmscmp-it-e                 
        it-nota-fisc.cod-est-ven                     
        it-nota-fisc.nr-entrega                      
        it-nota-fisc.cd-sit-desp                     
        it-nota-fisc.cd-emitente                     
        it-nota-fisc.nat-docum                       
        it-nota-fisc.nivel-restituicao               
        it-nota-fisc.nr-docum                        
        it-nota-fisc.pc-restituicao                  
        it-nota-fisc.serie-docum                     
        it-nota-fisc.nr-ord-prod                     
        it-nota-fisc.ind-fat-qtfam                   
        it-nota-fisc.vl-preuni-zfm                   
        it-nota-fisc.vl-merc-liq-zfm                 
        it-nota-fisc.conh-frete                      
        it-nota-fisc.emite-duplic                    
        it-nota-fisc.aliquota-tax                    
        it-nota-fisc.cod-vat                         
        it-nota-fisc.cod-tax                         
        it-nota-fisc.vl-iva-it                       
        it-nota-fisc.vl-biva-it                      
        it-nota-fisc.referencia-ct                   
        it-nota-fisc.cod-localiz                     
        it-nota-fisc.qt-devolvida                    
        it-nota-fisc.vl-frete-it                     
        it-nota-fisc.char-1                          
        it-nota-fisc.char-2                          
        it-nota-fisc.dec-1                           
        it-nota-fisc.dec-2                           
        it-nota-fisc.int-1                           
        it-nota-fisc.int-2                           
        it-nota-fisc.log-1                           
        it-nota-fisc.log-2                           
        it-nota-fisc.data-1                          
        it-nota-fisc.data-2                          
        it-nota-fisc.vl-desconto                     
        it-nota-fisc.nr-remito                       
        it-nota-fisc.check-sum                       
        it-nota-fisc.log-usa-tabela-desconto         
        it-nota-fisc.val-pct-desconto-tab-preco      
        it-nota-fisc.des-pct-desconto-inform         
        it-nota-fisc.val-desconto-inform             
        it-nota-fisc.val-pct-desconto-total          
        it-nota-fisc.val-pct-desconto-periodo        
        it-nota-fisc.val-pct-desconto-prazo          
        it-nota-fisc.val-desconto-total              
        it-nota-fisc.val-desconto                    
        it-nota-fisc.nr-tabpre                       
        it-nota-fisc.vl-pretab-me                    
        it-nota-fisc.vl-preori-me                    
        it-nota-fisc.vl-preuni-me                    
        it-nota-fisc.vl-merc-tab-me                  
        it-nota-fisc.vl-merc-ori-me                  
        it-nota-fisc.vl-merc-liq-me                  
        it-nota-fisc.vl-tot-item-me                  
        it-nota-fisc.vl-despes-it-me                 
        it-nota-fisc.vl-reajuste-me                  
        it-nota-fisc.vl-cuscontab-me                 
        it-nota-fisc.vl-precon-me                    
        it-nota-fisc.vl-merc-sicm-me                 
        it-nota-fisc.vl-iva-it-me                    
        it-nota-fisc.vl-frete-it-me                  
        it-nota-fisc.vl-biva-it-me                   
        it-nota-fisc.vl-desconto-me                  
        it-nota-fisc.ser-remito                      
        it-nota-fisc.nr-seq-it-rmt                   
        it-nota-fisc.nr-embarque
        VIEW-AS FILL-IN WITH SIDE-LABELS 1 COLUMN WIDTH 500.

OUTPUT CLOSE.

run Execute in h-prog(input "wordpad.exe", input "c:\lixo\lixo.txt").
delete procedure h-prog.


