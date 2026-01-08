/* Programa: upc-re1001m1.p
** Objetivo: Validar se a metragem das etiquetas devolvidas
**           ‚ igual … metragem informada na Nota Fiscal de devolu‡Æo.
** Autor...: Prodb - Toninho  Abril/2006
*/

DEF NEW GLOBAL SHARED VAR gr-row-in090 AS ROWID.
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF VAR c-mensagem AS CHAR FORMAT "x(50)".
DEF VAR de-tot-dev LIKE item-doc-est.quantidade.

FIND docum-est WHERE
     ROWID(docum-est) = gr-row-in090 NO-LOCK NO-ERROR.

IF docum-est.esp-docto = 20 AND
   docum-est.ce-atual = NO /*AND
   SUBSTR(docum-est.char-1,93,50) = "" */ THEN DO.

   FOR EACH item-doc-est OF docum-est NO-LOCK,
       FIRST item WHERE
             item.it-codigo = item-doc-est.it-codigo AND
             item.ge-codigo >= 50 AND
             item.ge-codigo <= 58 NO-LOCK.

       FIND nota-fiscal WHERE
            nota-fiscal.cod-estabel = docum-est.cod-estabel AND
            nota-fiscal.serie = item-doc-est.serie-comp AND
            nota-fiscal.nr-nota-fis = item-doc-est.nro-comp
            NO-LOCK NO-ERROR.
    
       FIND it-nota-fisc OF nota-fiscal WHERE
            it-nota-fisc.nr-seq-fat = item-doc-est.seq-comp AND
            it-nota-fisc.it-codigo = item-doc-est.it-codigo
            NO-LOCK NO-ERROR.

       ASSIGN de-tot-dev = 0.
       FOR EACH ped-item-res WHERE 
                ped-item-res.nome-abrev   = nota-fiscal.nome-ab-cli AND 
                ped-item-res.nr-pedcli    = it-nota-fisc.nr-pedcli AND
                ped-item-res.it-codigo    = it-nota-fisc.it-codigo AND 
                ped-item-res.nr-sequencia = it-nota-fisc.nr-seq-ped
                NO-LOCK.

           FOR EACH ped-item-rom WHERE
                    ped-item-rom.nome-abrev = ped-item-res.nome-abrev AND
                    ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli AND
                    ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia AND
                    ped-item-rom.marca = "DEV" NO-LOCK.
               ASSIGN de-tot-dev = de-tot-dev + ped-item-rom.quantidade.
           END.
       END.
       
       IF de-tot-dev <> item-doc-est.quantidade THEN DO.
          MESSAGE "Quantidade Devolvida difere da soma das Etiquetas Informadas..." SKIP
                  "Sequencia: " + STRING(item-doc-est.sequencia) + "   Item:" + item-doc-est.it-codigo SKIP(1)
                  "Confirma ?"
                  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                  TITLE "Divergˆncia em Quantidades"
                  UPDATE l-choice AS LOGICAL.

          IF l-choice = NO THEN 
             RETURN "ADM-ERROR".

          /*
          ASSIGN c-mensagem = "Quantidade Devolvida difere da soma das Etiquetas Informadas..." + CHR(13) +
                              "Nota Fiscal: " + item-doc-est.nro-docto + " Cliente: " + STRING(item-doc-est.cod-emitente) + CHR(13) + 
                              "Sequencia: " + STRING(item-doc-est.sequencia) + "   Item:" + item-doc-est.it-codigo + CHR(13) +
                              "Qtd Nota: " + STRING(item-doc-est.quantidade) + " Soma das Etiquetas: " + STRING(de-tot-dev).
              
          RUN esapi/esapi002.p (INPUT "controle.acabado@teartextil.com.br", /* e-mail remetente */
                                INPUT "albino.junior@teartextil.com.br" , /* e-mail destinat rio */
                                INPUT "Divergˆncia na Devolu‡Æo de Notas Fiscais" , /* Assunto */
                                INPUT c-mensagem, /* Mensagem */
                                INPUT "", /*arquivo anexo*/
                                INPUT NO). /* Mostra Erros */
          */                      
       END.
       
       FOR EACH ped-item-res WHERE 
                ped-item-res.nome-abrev   = nota-fiscal.nome-ab-cli AND 
                ped-item-res.nr-pedcli    = it-nota-fisc.nr-pedcli AND
                ped-item-res.it-codigo    = it-nota-fisc.it-codigo AND 
                ped-item-res.nr-sequencia = it-nota-fisc.nr-seq-ped
                NO-LOCK.

           FOR EACH ped-item-rom WHERE
                    ped-item-rom.nome-abrev = ped-item-res.nome-abrev AND
                    ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli AND
                    ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia AND
                    ped-item-rom.marca = "DEV" EXCLUSIVE-LOCK.

               FIND ob-etiqueta WHERE
                    ob-etiqueta.cod-estabel  = ped-item-rom.cod-estabel AND
                    ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                    EXCLUSIVE-LOCK NO-ERROR.

               IF AVAIL ob-etiqueta THEN
                  ASSIGN ob-etiqueta.situacao = 3
                         ob-etiqueta.localizacao = ''.
               
               CREATE dev-item-rom.
               BUFFER-COPY ped-item-rom TO dev-item-rom.

               DELETE ped-item-rom.
           END.
       END.
   END.
END.

APPLY 'choose' TO SELF.

