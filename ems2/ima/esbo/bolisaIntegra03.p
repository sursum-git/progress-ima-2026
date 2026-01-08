/*
* programa: esbo/boLisaIntegra01.p
* Objetivo: Bo para manipulaá∆o da tabela lisa-integra para processamento da Nota Fiscal de Remessa Avulsa
* Autor: Tadeu Silva
* Data: 10/2023
*/

{esp/ttChave.i}
&SCOPED-DEFINE bomsg hBoMsg
DEFINE VARIABLE cNF       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSerie    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEstab    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cErros    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE TRANSACAO AS CHARACTER   NO-UNDO INIT 'NotaRemessa'.
DEFINE VARIABLE {&bomsg} AS HANDLE      NO-UNDO.


PROCEDURE iniciar:

    RUN esbo/bomsg.p PERSIST SET {&bomsg}.

END PROCEDURE.

PROCEDURE finalizar:

    IF VALID-HANDLE({&bomsg}) THEN
       DELETE PROCEDURE {&boMsg}.

END PROCEDURE.

PROCEDURE setNF:

    DEFINE INPUT  PARAMETER pNF AS CHARACTER   NO-UNDO.
    ASSIGN cNF = pNF.

END PROCEDURE.

PROCEDURE setEstab:

    DEFINE INPUT  PARAMETER pEstab AS CHARACTER   NO-UNDO.
    ASSIGN cEstab = pEstab.

END PROCEDURE.

PROCEDURE setSerie:

    DEFINE INPUT  PARAMETER pSerie AS CHARACTER  NO-UNDO.
    ASSIGN cSerie = pSerie.

END PROCEDURE.


PROCEDURE enviarExclusaoNF:

    /* No caso da exclusao n∆o cabe  
    
        FIND LAST lisa-integra NO-LOCK
        WHERE lisa-integra.cod-trans    = TRANSACAO
        AND   lisa-integra.acao         = 'gerar'
        AND   lisa-integra.chave        = cEstab + "|" + cSerie + "|" + cNF
        AND   lisa-integra.ind-situacao = 2.
    IF AVAIL lisa-integra THEN DO:
       FIND CURRENT lisa-integra EXCLUSIVE-LOCK NO-ERROR.
       ASSIGN lisa-integra


    END.
    */
END PROCEDURE.


PROCEDURE getErros:

    DEFINE OUTPUT PARAMETER pErros AS CHAR  NO-UNDO.
    ASSIGN pErros = cErros .
       

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


