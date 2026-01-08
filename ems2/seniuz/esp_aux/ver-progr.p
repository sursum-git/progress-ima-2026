  DEF VAR de-sld-pron AS DEC.
  DEF VAR de-tot-pron AS DEC.
  DEF VAR c-num-pcp AS CHAR.
  DEF VAR l-ok AS LOG.

  ASSIGN c-num-pcp = ""
         de-sld-pron = 0.
  FOR EACH ob-pcp WHERE
           ob-pcp.situacao = 1 AND
           ob-pcp.it-codigo = '504891' NO-LOCK,
      EACH ob-pcp-ref OF ob-pcp WHERE
           ob-pcp-ref.situacao = 1 AND
           ob-pcp-ref.cod-refer = '0180520' AND
           ob-pcp-ref.qtd-pron > 0 NO-LOCK.

      ASSIGN de-sld-pron = de-sld-pron + ob-pcp-ref.qtd-pron.
      FOR EACH ordem-benefic WHERE
               ordem-benefic.it-codigo = ob-pcp.it-codigo AND
               ordem-benefic.cod-refer = ob-pcp-ref.cod-refer AND
               ordem-benefic.num-prog = ob-pcp.num-prog.

          FOR EACH ob-etiqueta OF ordem-benefic WHERE
                   ob-etiqueta.nr-reporte = 0 NO-LOCK.
              ASSIGN de-sld-pron = de-sld-pron - ob-etiqueta.quantidade.
          END.
      END.

      IF de-sld-pron > 0 THEN 
         ASSIGN c-num-pcp = IF c-num-pcp = ""
                            THEN STRING(ob-pcp.num-progr)
                            ELSE c-num-pcp + ';' + STRING(ob-pcp.num-progr)
                de-tot-pron = de-tot-pron + de-sld-pron
                de-sld-pron = 0.
  END.

  IF NUM-ENTRIES(c-num-pcp,";") = 0 THEN DO.
     MESSAGE 'NÆo Encontrado Programa‡Æo de Produ‡Æo com Saldo para o Item/Referˆncia...' SKIP
             'Saldo Pronto: ' + STRING(de-tot-pron,">>>,>>9.99") 
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN 'ADM-ERROR'.
  END.

  
  IF NUM-ENTRIES(c-num-pcp,";") >= 2 THEN DO.
     RUN esp/essp0100b.p (INPUT-OUTPUT c-num-pcp,
                          INPUT '0180520',
                          OUTPUT l-ok).

     IF l-ok = NO THEN DO.
        MESSAGE 'NÆo Selecionada a Programa‡Æo de Produ‡Æo para o Item/Referˆncia...'
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN 'ADM-ERROR'.
     END.
  END.
  

  IF de-tot-pron = 0 THEN DO.
     MESSAGE 'NÆo existe Quantidade Pronta para o Item/Referˆncia...'
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN 'ADM-ERROR'.  
  END.

  FIND ob-pcp-ref WHERE
       ob-pcp-ref.num-prog = INT(c-num-pcp) AND
       ob-pcp-ref.cod-refer = "0180520"
       NO-LOCK NO-ERROR.

  DISP c-num-pcp
       STRING(ob-pcp-ref.qtd-pron).

