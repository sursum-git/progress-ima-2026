/*
* programa: esbo/boLisaIntegra01.p
* Objetivo: Bo para manipulaá∆o da tabela lisa-integra para processamento da Nota Fiscal de Remessa
* Autor: Tadeu Silva
* Data: 10/2023
*/

{esp/ttChave.i}
{esbo/boLisaIntegra02.i}
{esapi/infNotaEntrada.i}
&SCOPED-DEFINE bomsg hBoMsg
DEFINE VARIABLE nrContainer AS INTEGER     NO-UNDO.
DEFINE VARIABLE TRANSACAO   AS CHARACTER   NO-UNDO INIT 'NotaRemessa'.
DEFINE VARIABLE {&bomsg}    AS HANDLE      NO-UNDO.
DEFINE VARIABLE cErros      AS CHARACTER   NO-UNDO.

DEF DATASET dsNFe FOR infNotaEntrada.

PROCEDURE iniciar:

    RUN esbo/bomsg.p PERSIST SET {&bomsg}.

END PROCEDURE.

PROCEDURE finalizar:

    IF VALID-HANDLE({&bomsg}) THEN
       DELETE PROCEDURE {&boMsg}.

END PROCEDURE.

PROCEDURE setContainer:

    DEFINE INPUT  PARAMETER pContainer AS INTEGER   NO-UNDO.
    ASSIGN nrContainer = pContainer.

END PROCEDURE.

PROCEDURE getNfsRemessa:
    DEFINE OUTPUT PARAMETER TABLE FOR ttRemessas.
    RUN limparTTMsg IN {&bomsg} .
    EMPTY TEMP-TABLE ttRemessas.
    FIND FIRST pp-container WHERE
         pp-container.nr-container = nrContainer
         NO-LOCK NO-ERROR.
    IF AVAIL pp-container THEN DO:
        EMPTY TEMP-TABLE ttRemessas.
        FOR EACH nota-fiscal NO-LOCK
            WHERE nota-fiscal.cod-estabel = pp-container.cod-estabel 
            AND   nota-fiscal.nro-proc-entrada = pp-container.nr-container .
            RUN criarTTRemessa(nota-fiscal.cod-estabel,
                               nota-fiscal.serie,
                               nota-fiscal.nr-nota-fis,
                               nota-fiscal.dt-cancela,
                               ROWID(nota-fiscal)).
        END.
    END.
    ELSE DO:
         RUN setMsg IN {&bomsg}(4,
                               'N∆o foi poss°vel encontrar o Container:' + string(nrContainer),
                               'erro'
                               ).
         RETURN 'nok'.
    END.


END PROCEDURE.

PROCEDURE getNfsRemessaAvulsas:
     DEFINE INPUT  PARAMETER pDataIni AS DATE     NO-UNDO.
     DEFINE OUTPUT PARAMETER TABLE FOR ttRemessas.
     RUN limparTTMsg IN {&bomsg} .
     EMPTY TEMP-TABLE ttRemessas.
     FOR EACH nota-fiscal NO-LOCK
          WHERE nota-fiscal.cod-estabel = '505'
          AND   nota-fiscal.nat-operacao = '59207i'
          AND   nota-fiscal.dt-emis-nota >= pDataIni
          AND   NOT CAN-FIND(pp-container WHERE nota-fiscal.nro-proc-entrada = pp-container.nr-container and nota-fiscal.nro-proc-entrada > 0) .
          RUN criarTTRemessa(nota-fiscal.cod-estabel,
                             nota-fiscal.serie,
                             nota-fiscal.nr-nota-fis,
                             nota-fiscal.dt-cancela,
                             ROWID(nota-fiscal)).
                             

      END.
END PROCEDURE.

PROCEDURE getNFMae:

    DEFINE OUTPUT PARAMETER codEstabel AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER serie      AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER nf         AS CHARACTER   NO-UNDO.
    RUN limparTTMsg IN {&bomsg} .
    FIND pp-container WHERE
         pp-container.nr-container = nrContainer
         NO-LOCK NO-ERROR.
    IF AVAIL pp-container THEN DO:
       FIND processo-imp WHERE
             processo-imp.nr-proc-imp = STRING(pp-container.nr-container)
             NO-LOCK NO-ERROR.
       IF AVAIL processo-imp THEN DO:
          FIND FIRST item-doc-est WHERE
              item-doc-est.num-pedido = processo-imp.num-pedido NO-LOCK NO-ERROR.
          IF AVAIL item-doc-est THEN DO:
             FIND LAST nota-fiscal WHERE
                  nota-fiscal.cod-estabel = pp-container.cod-estabel AND
                  nota-fiscal.serie = item-doc-est.serie-docto       AND
                  nota-fiscal.nr-nota-fis = item-doc-est.nro-docto   AND
                  nota-fiscal.dt-cancela = ?
                  NO-LOCK NO-ERROR.
             IF AVAIL nota-fiscal THEN DO:
                ASSIGN codEstabel   = nota-fiscal.cod-estabel
                       serie        = nota-fiscal.serie
                       nf           = nota-fiscal.nr-nota-fis .

             END.
             ELSE DO:
                RUN setMsg IN {&bomsg}(1,
                                       'NF n∆o encontrada com o numero:' + item-doc-est.nro-docto,
                                       'erro'
                                       ).
                RETURN 'nok'.
             END.

          END.
          ELSE DO:
              RUN setMsg IN {&bomsg}(2,
                                    'N∆o foi poss°vel encontrar o documento de entrada:' + string(processo-imp.num-pedido),
                                    'erro'
                                    ).
              RETURN 'nok'.
          END.
       END.
       ELSE DO:
           RUN setMsg IN {&bomsg}(3,
                                 'N∆o foi poss°vel encontrar o Processo:' +  string(nrcontainer),
                                 'erro'
                                 ).
           RETURN 'nok'.
       END.
    END.
    ELSE DO:
        RUN setMsg IN {&bomsg}(4,
                               'N∆o foi poss°vel encontrar o Container:' + string(nrContainer),
                               'erro'
                               ).
        RETURN 'nok'.



    END.


END PROCEDURE.

PROCEDURE enviarExclusaoNF:
     
      
     DEFINE INPUT  PARAMETER rRowidNF   AS ROWID       NO-UNDO.
     DEFINE VARIABLE h-dataset          AS HANDLE.
     DEFINE VARIABLE id                 AS INTEGER     NO-UNDO.

     ASSIGN h-dataset = DATASET dsNFe:HANDLE.
     EMPTY TEMP-TABLE infNotaEntrada.
     
     RUN criarInfNotaEntrada(rRowidNF,
                             YES, //exclusao
                             OUTPUT id,
                             OUTPUT cErros).
     IF cErros <> '' THEN DO:
        RUN setMsg IN {&bomsg}(999,'Erro LISA:' + CHR(13) + cErros,'erro').
        RETURN 'nok'.
     END.


     FIND FIRST  infNotaEntrada NO-ERROR.

     RUN lisa/excluirNFRemessa.p(h-dataset,
                                  infNotaEntrada.cnota,
                                 OUTPUT cErros).
    IF cErros <> '' THEN DO:
        RUN setMsg IN {&bomsg}(999,'Erro LISA:' + CHR(13) + cErros,'erro').
        RETURN 'nok'.
     END.

    //posteriormente colocar toda aá∆o que est† no painel para aqui

END PROCEDURE.


PROCEDURE getErros:

    DEFINE OUTPUT PARAMETER pErros AS CHAR  NO-UNDO.
    RUN getErros IN {&bomsg}(OUTPUT pErros).
    
    
       

END PROCEDURE.



/*Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cod-trans                        char
   20 chave                            char        i
   30 conteudo                         char
   40 val-livre-1                      char
   50 val-livre-2                      char
   60 val-livre-3                      char
   70 val-livre-4                      char
   80 val-livre-5                      char
   90 ind-situacao                     inte
  100 acao                             char        i

*/


