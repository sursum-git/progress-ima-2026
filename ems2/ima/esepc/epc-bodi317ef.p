
/*
controle de altera‡äes:
15/08/2025 - tsp01 - acrescentar o numeo do pedido e o representante na observa‡Æo da nota fiscal
29/08/2025 - tsp02 - dados de importa‡Æo na observa‡Æo
*/



{include/i-epc200.i bodi317ef-upc}


DEF INPUT PARAM p-ind-event  AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-epc.

DEF BUFFER b-nota-fiscal FOR nota-fiscal.

DEF VAR c-pagto-redesp          AS CHARACTER FORMAT "x" INIT " ".
DEF VAR c-resp-redesp           AS CHARACTER FORMAT "x(20)".
DEF VAR c-clibnf                AS CHARACTER FORMAT "x(200)".
DEF VAR c-cliadq                AS CHARACTER FORMAT "x(200)".
DEFINE VARIABLE dadosImp        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hboConsParam    AS HANDLE      NO-UNDO.
DEFINE VARIABLE cListaNat       AS CHARACTER   NO-UNDO.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

IF p-ind-event = "EndEfetivaNota":U THEN DO:

   FIND FIRST tt-epc WHERE
              tt-epc.cod-event = p-ind-event AND
              tt-epc.cod-parameter = "ROWID(nota-fiscal)":U NO-LOCK NO-ERROR.

   IF AVAIL tt-epc THEN DO:

      FIND FIRST nota-fiscal WHERE
           ROWID(nota-fiscal) = TO-ROWID(tt-epc.val-parameter) NO-ERROR.
           
           

      IF AVAIL nota-fiscal //AND nota-fiscal.nr-pedcli <> '' 
      THEN DO:
      
         // retirar o codigo abaixo depois que aplicar o pacote
         // codigo feito em 03/09/25 peloas mudan‡as no SEFEZ em 01/09/25
         IF nota-fiscal.cod-cond-pag = 1 THEN DO:
            FOR EACH fat-duplic WHERE 
                     fat-duplic.cod-estabel = nota-fiscal.cod-estabel AND
                     fat-duplic.serie       = nota-fiscal.serie AND
                     fat-duplic.nr-fatura   = nota-fiscal.nr-fatura SHARE-LOCK. 
                ASSIGN fat-duplic.dt-venciment = fat-duplic.dt-venciment + 1.
            END.
         END.
      
         FOR EACH it-nota-fisc OF nota-fiscal SHARE-LOCK.
             FIND ITEM OF it-nota-fisc NO-LOCK NO-ERROR.

             IF nota-fiscal.cod-estabel <> '502' AND
                NOT it-nota-fisc.it-codigo BEGINS 'DD' THEN DO.
                FIND nar-it-nota WHERE 
                     nar-it-nota.cod-estabel  = it-nota-fisc.cod-estabel AND
                     nar-it-nota.serie        = it-nota-fisc.serie AND
                     nar-it-nota.nr-nota-fis  = it-nota-fisc.nr-nota-fis AND
                     nar-it-nota.nr-sequencia = it-nota-fisc.nr-seq-fat AND
                     nar-it-nota.it-codigo    = it-nota-fisc.it-codigo
                     SHARE-LOCK NO-ERROR.
            
                IF NOT AVAIL nar-it-nota then DO:
                   CREATE nar-it-nota.
                   ASSIGN nar-it-nota.cod-estabel  = it-nota-fisc.cod-estabel
                          nar-it-nota.serie        = it-nota-fisc.serie      
                          nar-it-nota.nr-nota-fis  = it-nota-fisc.nr-nota-fis
                          nar-it-nota.nr-sequencia = it-nota-fisc.nr-seq-fat 
                          nar-it-nota.it-codigo    = it-nota-fisc.it-codigo.
                END.

                // Solicitado por Adriano em 18/01/2024, descricao do item no danfe estava duplicando...
                ASSIGN nar-it-nota.narrativa = ENTRY(1,ITEM.narrativa,CHR(10)).
             END.

             IF it-nota-fisc.cod-refer <> "" THEN DO:
                FIND item WHERE 
                     item.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.
                
                /*
                IF nota-fiscal.nat-operacao = "59301" THEN /* Nota de remessa para industrializa‡Æo */
                   ASSIGN nar-it-nota.narrativa = "".
                */

                /*
                /* Altera a descri‡Æo do item para buscar da primeira linha da narrativa quando o item estiver como narrativa informada e quando nÆo for nota de importa‡Æo */
                IF ITEM.ind-imp-desc = 7               AND 
                   nota-fiscal.nat-operacao <> "31201" AND  /* Nota de importa‡Æo*/
                   nota-fiscal.nat-operacao <> "31203" THEN /* Nota de importa‡Æo*/
                   ASSIGN nar-it-nota.narrativa = ENTRY(1,ITEM.narrativa,CHR(10)). /* Busca uma linha narrativa*/
                */

                /*
                /* Busca a narrativa da Classifica‡Æo Fiscal quando o item 
                   estiver como narrativa informada e quando for nota de 
                   importa‡Æo */
                IF ITEM.ind-imp-desc = 7 AND 
                   nota-fiscal.ind-tip-nota = 8 THEN DO.
                   FIND classif-fisc OF ITEM NO-LOCK NO-ERROR.

                   ASSIGN nar-it-nota.narrativa = classif-fisc.des-descr-detdo. /* Busca uma linha narrativa*/
                END.
                */

                ASSIGN nar-it-nota.narrativa = nar-it-nota.narrativa + " REF: " + it-nota-fisc.cod-refer. /* Adiciona Referencia do item*/
                
                FIND item-ext WHERE 
                     item-ext.it-codigo = ITEM.it-codigo NO-LOCK NO-ERROR. /* Adiciona Gramatura do item*/
                
                IF AVAIL item-ext AND item-ext.gramatura > 0 THEN
                   ASSIGN nar-it-nota.narrativa = nar-it-nota.narrativa + "  G/M: " + STRING(item-ext.gramatura).
                IF ITEM.cod-imagem <> "" THEN /* Adiciona Regra de lavagem do item*/
                   ASSIGN nar-it-nota.narrativa = nar-it-nota.narrativa + "  RL: " + ITEM.cod-imagem. 
             END.
             

             FIND ped-item WHERE
                  ped-item.nr-pedcli = nota-fiscal.nr-pedcli AND
                  ped-item.nome-abrev = nota-fiscal.nome-ab-cli AND
                  ped-item.nr-sequencia = it-nota-fisc.nr-seq-ped
                  NO-LOCK NO-ERROR.

             IF AVAIL ped-item THEN DO.
                IF it-nota-fisc.val-desconto-total > 0 THEN
                   ASSIGN SUBSTR(it-nota-fisc.char-2,1500,10) = STRING(ped-item.val-desconto-total,">>>,>>9.99")
                          it-nota-fisc.val-desconto-total = 0.
                ELSE IF it-nota-fisc.val-desconto-inform > 0 THEN
                   ASSIGN SUBSTR(it-nota-fisc.char-2,1500,10) = STRING(ped-item.val-desconto-inform,">>>,>>9.99")
                          it-nota-fisc.val-desconto-inform = 0.
             END.
             ELSE DO.
                IF it-nota-fisc.val-desconto-total > 0 THEN
                   ASSIGN SUBSTR(it-nota-fisc.char-2,1500,10) = STRING(it-nota-fisc.val-desconto-total,">>>,>>9.99")
                          it-nota-fisc.val-desconto-total = 0.
                ELSE IF it-nota-fisc.val-desconto-inform > 0 THEN
                   ASSIGN SUBSTR(it-nota-fisc.char-2,1500,10) = STRING(it-nota-fisc.val-desconto-inform,">>>,>>9.99")
                          it-nota-fisc.val-desconto-inform = 0.
             END.

             ASSIGN it-nota-fisc.vl-merc-ori = it-nota-fisc.vl-merc-liq                 
                    it-nota-fisc.vl-merc-ori-me = it-nota-fisc.vl-merc-liq
                    it-nota-fisc.vl-merc-tab = it-nota-fisc.vl-merc-liq                
                    it-nota-fisc.vl-merc-tab-me = it-nota-fisc.vl-merc-liq.
        
             ASSIGN it-nota-fisc.vl-preori = it-nota-fisc.vl-preuni                                    
                    it-nota-fisc.vl-pretab = it-nota-fisc.vl-preuni. 
        
             ASSIGN it-nota-fisc.vl-preori-me = it-nota-fisc.vl-preuni 
                    it-nota-fisc.vl-pretab-me = it-nota-fisc.vl-preuni.               
        
             ASSIGN it-nota-fisc.val-pct-desconto-total = 0
                    it-nota-fisc.val-desconto-inform = 0.     

         END.
         
         ASSIGN nota-fiscal.vl-desconto = 0
                nota-fiscal.val-desconto-total = 0
                nota-fiscal.val-pct-desconto-total = 0.
         
         FIND natur-oper WHERE
              natur-oper.nat-oper = nota-fiscal.nat-oper NO-LOCK NO-ERROR.
    
         // Ver se ‚ Triangular, Limpa o Desconto da nota de Remessa
         IF natur-oper.log-oper-triang THEN DO.  
            FIND FIRST b-nota-fiscal WHERE
                       b-nota-fiscal.cod-estabel = nota-fiscal.cod-estabel AND
                       b-nota-fiscal.serie = nota-fiscal.serie AND
                       b-nota-fiscal.nr-nota-fis >= nota-fiscal.nr-nota-fis AND
                       b-nota-fiscal.nome-ab-cli = nota-fiscal.nome-abrev-tri AND
                       b-nota-fiscal.vl-tot-nota = nota-fiscal.vl-tot-nota
                       SHARE-LOCK NO-ERROR. 
            FOR EACH it-nota-fisc OF b-nota-fiscal SHARE-LOCK.
                IF it-nota-fisc.val-desconto-total > 0 THEN
                   ASSIGN SUBSTR(it-nota-fisc.char-2,1500,10) = STRING(it-nota-fisc.val-desconto-total,">>>,>>9.99")
                          it-nota-fisc.val-desconto-total = 0.
                ELSE IF it-nota-fisc.val-desconto-inform > 0 THEN
                   ASSIGN SUBSTR(it-nota-fisc.char-2,1500,10) = STRING(it-nota-fisc.val-desconto-inform,">>>,>>9.99")
                          it-nota-fisc.val-desconto-inform = 0.

                ASSIGN it-nota-fisc.vl-merc-ori     = it-nota-fisc.vl-merc-liq                 
                       it-nota-fisc.vl-merc-ori-me  = it-nota-fisc.vl-merc-liq
                       it-nota-fisc.vl-merc-tab     = it-nota-fisc.vl-merc-liq                
                       it-nota-fisc.vl-merc-tab-me  = it-nota-fisc.vl-merc-liq.

                ASSIGN it-nota-fisc.vl-preori = it-nota-fisc.vl-preuni                                    
                       it-nota-fisc.vl-pretab = it-nota-fisc.vl-preuni. 

                ASSIGN it-nota-fisc.vl-preori-me = it-nota-fisc.vl-preuni 
                       it-nota-fisc.vl-pretab-me = it-nota-fisc.vl-preuni.               

                ASSIGN it-nota-fisc.val-pct-desconto-total = 0
                       it-nota-fisc.val-desconto-inform = 0.     

            END.

            ASSIGN b-nota-fiscal.vl-desconto = 0
                   b-nota-fiscal.val-desconto-total = 0
                   b-nota-fiscal.val-pct-desconto-total = 0.
         END.

         FIND ped-venda WHERE
              ped-venda.nome-abrev = nota-fiscal.nome-ab-cli AND 
              ped-venda.nr-pedcli = nota-fiscal.nr-pedcli
              NO-LOCK NO-ERROR.
    
         IF AVAIL ped-venda THEN DO.
             FIND ped-venda-ext WHERE
                  ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND  /*  daf  */
                  ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.
        
             IF AVAIL ped-venda-ext AND
                ped-venda-ext.tp-frete <> 'Cif Destaque NF' THEN
                ASSIGN nota-fiscal.vl-frete = 0.
    
             IF ped-venda-ext.compl-observ <> '' THEN 
                ASSIGN nota-fiscal.observ-nota = ped-venda-ext.compl-observ + nota-fiscal.observ-nota.

             /*
             ASSIGN nota-fiscal.observ-nota = nota-fiscal.observ-nota + 
                                              " Pedido Interno: "  + STRING(ped-venda.nr-pedido,">>>>>>9") +
                                              " Pedido Cliente: " + ped-venda.nr-pedcli +
                                              " Pedido Repres: "  + ped-venda.nr-pedrep.
             */

             IF nota-fiscal.nome-tr-red <> "" THEN DO:
                FIND transporte WHERE
                     transporte.nome-abrev = nota-fiscal.nome-tr-red NO-LOCK NO-ERROR.
    
                IF AVAIL transporte THEN DO.
                   IF ped-venda-ext.tp-frete = 'Cif Total' OR
                      ped-venda-ext.tp-frete = 'Fob at‚ Redesp' THEN
                      ASSIGN c-pagto-redesp = "0"
                             c-resp-redesp = "EMITENTE".
                       
                   IF ped-venda-ext.tp-frete = 'Cif at‚ Redesp' OR
                      ped-venda-ext.tp-frete = 'Cif Destaque NF' OR
                      (ped-venda-ext.tp-frete = 'Fob Total' AND nota-fiscal.nome-tr-red <> "") THEN
                      ASSIGN c-pagto-redesp = "1"
                             c-resp-redesp = "DESTINATµRIO".
    
                   ASSIGN nota-fiscal.observ-nota = nota-fiscal.observ-nota + 
                                                    "REDESPACHO por conta do " + c-resp-redesp +
                                                    " Nome/RazÆo Social: " + transporte.nome +
                                                    " Endere‡o: "          + transporte.endereco +
                                                    " Munic¡pio: "         + transporte.cidade +
                                                    " UF: "                + transporte.estado.
                END.
             END.
         END.

         FIND emitente WHERE
              emitente.nome-abrev = nota-fiscal.nome-ab-cli NO-LOCK NO-ERROR.

         /*
         Comentado por Toninho em 12/09/2018, em acordo om o Adriano,
         utilizar  o # padrÆo da Totvs nas Mensagens ( cd0405a )

         IF natur-oper.log-oper-triang THEN DO.
            ASSIGN c-cliadq = emitente.nome-emit + " - " +
                              emitente.endereco + " - " +
                              emitente.bairro + " - CEP: " +
                              emitente.cep + " - " +
                              emitente.cidade + " - " +
                              emitente.estado + " - CNPJ: " +
                              emitente.cgc + " - I.E.: " +
                              emitente.ins-estadual.
    
            /* Busca a Nota Beneficiada */
            FIND FIRST b-nota-fiscal WHERE
                       b-nota-fiscal.nr-nota-fis >= nota-fiscal.nr-nota-fis AND
                       b-nota-fiscal.cod-estabel = nota-fiscal.cod-estabel AND
                       b-nota-fiscal.serie = nota-fiscal.serie AND
                       b-nota-fiscal.nome-ab-cli = ped-venda.nome-abrev-tri AND
                       b-nota-fiscal.vl-tot-nota = nota-fiscal.vl-tot-nota
                       NO-ERROR. 
    
            /* Modifica Observa‡Æo da Nota que Adquiriu */
            ASSIGN nota-fiscal.observ-nota = "NF. REF. · NF. " + b-nota-fiscal.nr-nota-fis + " DE " +
                                             STRING(b-nota-fiscal.dt-emis,"99/99/9999") +  ' "  ' +
                                             nota-fiscal.observ-nota.
    
            /* Modifica Observa‡Æo da Nota de Remessa*/
            ASSIGN b-nota-fiscal.observ-nota = "NF. REF. · NF. " + nota-fiscal.nr-nota-fis + " DE " +
                                                STRING(nota-fiscal.dt-emis,"99/99/9999") + ' "  ' +
                                                b-nota-fiscal.observ-nota.

            IF b-nota-fiscal.ind-tp-frete = 2 THEN
               ASSIGN nota-fiscal.observ-nota = nota-fiscal.observ-nota + " // " + 
                                 "FRETE DE RESPONSABILIDADE DA EMPRESA PROPRIETµRIA DA MERCADORIA // ".

            FIND emitente WHERE
                 emitente.nome-abrev = b-nota-fiscal.nome-ab-cli NO-LOCK NO-ERROR.
            ASSIGN c-clibnf = emitente.nome-emit + " - " +
                              emitente.endereco + " - " +
                              emitente.bairro + " - CEP: " +
                              emitente.cep + " - " +
                              emitente.cidade + " - " +
                              emitente.estado + " - CNPJ: " +
                              emitente.cgc + " - I.E.: " +
                              emitente.ins-estadual.
    
            IF INDEX(nota-fiscal.observ-nota,"#CLIBNF") > 0 THEN
               ASSIGN nota-fiscal.observ-nota = REPLACE(nota-fiscal.observ-nota,"#CLIBNF",TRIM(c-clibnf)).
    
            IF INDEX(b-nota-fiscal.observ-nota,"#CLIADQ") > 0 THEN
               ASSIGN b-nota-fiscal.observ-nota = REPLACE(b-nota-fiscal.observ-nota,"#CLIADQ",TRIM(c-cliadq)).
         END.
         */
         
         IF nota-fiscal.nat-operacao = '51221' THEN DO.
            FIND nota-fisc-adc WHERE 
                 nota-fisc-adc.cod-estab = nota-fiscal.cod-estabel AND
                 nota-fisc-adc.cod-serie = nota-fiscal.serie AND
                 nota-fisc-adc.cod-nota-fisc = nota-fiscal.nr-nota-fis AND
                 nota-fisc-adc.idi-tip-dado = 3   
                 NO-LOCK NO-ERROR.
    
            IF AVAIL nota-fisc-adc THEN
               ASSIGN nota-fiscal.observ-nota = nota-fiscal.observ-nota + 
                                               ' Nota Fiscal Complementar de ICMS Referente ao Documento: ' + nota-fisc-adc.cod-docto-referado + 
                                               ' Emitida em ' + STRING(nota-fisc-adc.dat-docto-referado) +
                                               ' Conforme Notifica‡Æo SEI55/2018' .
         END.
    
         RUN esdlg/d01-saida-nfe.p (OUTPUT nota-fiscal.dt-saida).
         
         //tsp01
         IF nota-fiscal.nr-pedcli <> '' THEN DO:
            ASSIGN nota-fiscal.observ-nota = "PEDIDO:" + nota-fiscal.nr-pedcli + " - REPRES:" + nota-fiscal.no-ab-reppri + ' . ' + CHR(13) + nota-fiscal.observ-nota.     
         END.
         
         
       
         //tsp02
         RUN esbo/boConsParam.p PERSIST SET HboConsParam.          
         RUN getListaNaturezaDadosImpNF IN hBoConsParam(OUTPUT cListaNat). 
         IF VALID-HANDLE(hBoConsParam) THEN DO:
            DELETE PROCEDURE hBoConsParam.             
         END. 
         IF  lookup(nota-fiscal.nat-operacao,cListaNat) > 0 THEN
         DO:
           RUN esapi/getDescrNFImportacao.p(ROWID(nota-fiscal), OUTPUT dadosImp).  
           ASSIGN  nota-fiscal.observ-nota = nota-fiscal.observ-nota + CHR(13) +  dadosimp .
         END.           
               
         
      END.
   END.
END.

