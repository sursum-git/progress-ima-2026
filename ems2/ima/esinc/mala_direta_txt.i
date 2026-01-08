MESSAGE "{1}"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

IF var-tg-partir THEN DO:
   FOR EACH tt-conteudo BREAK BY tt-conteudo.nom-rep-ven.
                             /* "{1}".  {1} = breakby + " BY tt-conteudo." + tt-disp-colunas.coluna.*/
       If FIRST-OF(tt-conteudo.nom-rep-ven) then  DO: 
          ASSIGN salvar-como = caminho + nome-arquivo + "-" + tt-conteudo.nom-rep-ven + ".txt".
          OUTPUT TO VALUE(salvar-como).
          PUT trim(tt-conteudo.nom-rep-ven)
              SKIP(1).
          FOR EACH tt-disp-colunas NO-LOCK BREAK BY tt-disp-colunas.ordem.
              PUT trim(tt-disp-colunas.coluna)
                  "#".
          END.
       END.

       
       {1} /* {2} = var-put + ' ASSIGN coluna = coluna + 1. chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.' + TRIM(tt-disp-colunas.coluna) + '.'.*/
       PUT SKIP.

       If last-of(tt-conteudo.nom-rep-ven) then  DO:
          OUTPUT CLOSE.
       END.
   END.
END.

/*
ELSE DO:
     ASSIGN salvar-como = caminho + nome-arquivo + ".txt".
     OUTPUT TO VALUE(salvar-como).
     FOR EACH tt-disp-colunas NO-LOCK BREAK BY tt-disp-colunas.ordem.
         PUT trim(tt-disp-colunas.coluna)
             "#".
     END.
     PUT SKIP.
   
     FOR EACH tt-conteudo BREAK {1} . /* {1} = breakby + " BY tt-conteudo." + tt-disp-colunas.coluna.*/
         {2} /* {2} = var-put + ' ASSIGN coluna = coluna + 1. chWorkSheet:Range(fn-letra(coluna) + STRING(i-linha)):VALUE = tt-conteudo.' + TRIM(tt-disp-colunas.coluna) + '.'.*/
         PUT SKIP.
     END.
     OUTPUT CLOSE.
END.
*/
