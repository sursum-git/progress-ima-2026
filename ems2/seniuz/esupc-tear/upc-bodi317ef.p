{include/i-epc200.i bodi317ef-upc}

DEF INPUT PARAM p-ind-event  AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-epc.
DEF BUFFER b-nota-fiscal FOR nota-fiscal.

DEF VAR c-pagto-redesp AS CHARACTER FORMAT "x" INIT " ".
DEF VAR c-clibnf       AS   CHAR FORMAT "x(200)".
DEF VAR c-cliadq       AS   CHAR FORMAT "x(200)".

IF p-ind-event = "EndEfetivaNota":U THEN DO:
   FIND FIRST tt-epc WHERE
              tt-epc.cod-event = p-ind-event AND
              tt-epc.cod-parameter = "ROWID(nota-fiscal)":U NO-LOCK NO-ERROR.
   IF AVAIL tt-epc THEN DO:
      FIND FIRST nota-fiscal WHERE
           ROWID(nota-fiscal) = TO-ROWID(tt-epc.val-parameter) NO-ERROR.

      FIND natur-oper WHERE
           natur-oper.nat-oper = nota-fiscal.nat-oper NO-LOCK NO-ERROR.

      FIND ped-venda WHERE
           ped-venda.nome-abrev = nota-fiscal.nome-ab-cli AND 
           ped-venda.nr-pedcli = nota-fiscal.nr-pedcli
           NO-LOCK NO-ERROR.

      IF AVAIL ped-venda THEN DO.
         FIND ped-venda-ext WHERE
              ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.
    
         IF AVAIL ped-venda-ext AND
            ped-venda-ext.tp-frete <> 'Cif Destaque NF' THEN
            ASSIGN nota-fiscal.vl-frete = 0.

         IF ped-venda-ext.compl-observ <> '' THEN 
            ASSIGN nota-fiscal.observ-nota = ped-venda-ext.compl-observ + nota-fiscal.observ-nota.

         /* Acrescenta Tipo de Frete na primeira linha da Observa‡Æo da Nota */
         ASSIGN nota-fiscal.observ-nota = "TIPO DE FRETE: " + ped-venda-ext.tp-frete + "  //  " + nota-fiscal.observ-nota.

         ASSIGN nota-fiscal.observ-nota = nota-fiscal.observ-nota + 
                                          " Pedido Interno: "  + STRING(ped-venda.nr-pedido,">>>>>>9") +
                                          " Pedido Cliente: " + ped-venda.nr-pedcli +
                                          " Pedido Repres: "  + ped-venda.nr-pedrep.

         IF nota-fiscal.nome-tr-red <> "" THEN DO:
            FIND transporte WHERE
                 transporte.nome-abrev = nota-fiscal.nome-tr-red NO-LOCK NO-ERROR.

            IF AVAIL transporte THEN DO.
               IF ped-venda-ext.tp-frete = 'Cif Total' OR
                  ped-venda-ext.tp-frete = 'Fob at‚ Redesp' THEN
                  ASSIGN c-pagto-redesp = "0". /* Emitente */

               IF ped-venda-ext.tp-frete = 'Cif at‚ Redesp' OR
                  ped-venda-ext.tp-frete = 'Cif Destaque NF' OR
                  (ped-venda-ext.tp-frete = 'Fob Total' AND nota-fiscal.nome-tr-red <> "") THEN
                  ASSIGN c-pagto-redesp = "1". /* Destinatario */

               ASSIGN nota-fiscal.observ-nota = nota-fiscal.observ-nota + 
                      "REDESPACHO por conta do " + IF c-pagto-redesp = "0" 
                                                   THEN "EMITENTE" 
                                                   ELSE "DESTINATµRIO" +
                      " Nome/RazÆo Social: " + transporte.nome +
                      " Endere‡o: "          + transporte.endereco +
                      " Munic¡pio: "         + transporte.cidade +
                      " UF: "                + transporte.estado +
                      " Telefone: "          + transporte.telefone.
            END.
         END.
      END.
                        
      FIND emitente WHERE
           emitente.nome-abrev = nota-fiscal.nome-ab-cli NO-LOCK NO-ERROR.

      IF AVAIL ped-venda AND natur-oper.log-oper-triang THEN DO.
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
                                          STRING(b-nota-fiscal.dt-emis,"99/99/9999") +  ' " ' +
                                          nota-fiscal.observ-nota.

         /* Modifica Observa‡Æo da Nota Beneficada */
         IF ped-venda-ext.compl-observ <> '' THEN 
            ASSIGN b-nota-fiscal.observ-nota = ped-venda-ext.compl-observ + b-nota-fiscal.observ-nota.

         ASSIGN b-nota-fiscal.observ-nota = "NF. REF. · NF. " + nota-fiscal.nr-nota-fis + " DE " +
                                             STRING(nota-fiscal.dt-emis,"99/99/9999") + ' " ' +
                                             b-nota-fiscal.observ-nota.

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

         IF nota-fiscal.nome-tr-red <> "" THEN DO:
            FIND transporte WHERE
                 transporte.nome-abrev = nota-fiscal.nome-tr-red NO-LOCK NO-ERROR.

            IF AVAIL transporte THEN DO.
               ASSIGN b-nota-fiscal.observ-nota = b-nota-fiscal.observ-nota + 
                      "REDESPACHO por conta do " + IF c-pagto-redesp = "0" 
                                                   THEN "EMITENTE" 
                                                   ELSE "DESTINATµRIO" +
                      " Nome/RazÆo Social: " + transporte.nome +
                      " Endere‡o: "          + transporte.endereco +
                      " Munic¡pio: "         + transporte.cidade +
                      " UF: "                + transporte.estado.
            END.
         END.
      END.

      IF AVAIL emitente AND 
         NOT natur-oper.log-oper-triang AND /* NÆo mostra local de entrega para NF Remessa Triangular, conf. Robson 29/04/17 */
         (emitente.cgc      <> nota-fiscal.cgc      OR
          emitente.endereco <> nota-fiscal.endereco OR
          emitente.bairro   <> nota-fiscal.bairro   OR
          emitente.cidade   <> nota-fiscal.cidade   OR
          emitente.estado   <> nota-fiscal.estado   OR
          emitente.pais     <> nota-fiscal.pais     OR
          emitente.cep      <> nota-fiscal.cep)     THEN DO:
          ASSIGN nota-fiscal.observ-nota = nota-fiscal.observ-nota + 
                                           "  LOCAL DE ENTREGA: "  + nota-fiscal.endereco +
                                           " Bairro/Distrito: "    + nota-fiscal.bairro   +
                                           " Municipio: "          + nota-fiscal.cidade   + 
                                           " UF: "                 + nota-fiscal.estado   +
                                           " Cep: "                + nota-fiscal.cep      + 
                                           " Caixa Postal: "       + nota-fiscal.caixa-postal +
                                           " Pais: "               + nota-fiscal.pais.
      END.

      IF AVAIL nota-fiscal THEN DO:
         RUN esdlg/d01-saida-nfe.p (OUTPUT nota-fiscal.dt-saida,
                                    OUTPUT nota-fiscal.placa,
                                    OUTPUT nota-fiscal.uf-placa).
         FIND CURRENT nota-fiscal NO-LOCK NO-ERROR. /* Libera Registro da Nf */
      END.
   END.
END.

