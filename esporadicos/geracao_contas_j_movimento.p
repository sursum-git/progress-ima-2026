DEFINE VARIABLE cLinha     AS CHARACTER   NO-UNDO FORMAT 'x(200)'.
DEFINE VARIABLE cRegistro  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cConta     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cContaPai  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCC        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ctipo      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNivel     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDescConta AS CHARACTER   NO-UNDO FORMAT 'x(100)'.
DEFINE VARIABLE cNatureza  AS CHARACTER   NO-UNDO.

DEFINE TEMP-TABLE ttDadosConta 
    FIELD conta AS CHAR
    FIELD nivel AS CHAR
    FIELD conta_pai AS CHAR
    FIELD natureza AS CHAR
    FIELD desc_conta AS CHAR FORMAT 'x(100)'.


DEFINE TEMP-TABLE ttPlano
    FIELD conta AS CHAR
    FIELD descricao AS CHAR FORMAT 'x(100)'.

DEFINE TEMP-TABLE ttMatriz
    FIELD conta AS CHAR
    FIELD contaRef AS CHAR FORMAT 'x(20)'.

DEFINE TEMP-TABLE ttCadContas
    FIELD conta AS CHAR
    FIELD cc    AS CHAR.

DEFINE TEMP-TABLE ttMovContas
    FIELD conta AS CHAR
    FIELD cc   AS CHAR
    FIELD LOG_achou AS LOGICAL
    FIELD contaRef LIKE ttMatriz.contaRef
    FIELD nivel AS CHAR
    FIELD conta_pai AS CHAR
    FIELD natureza AS CHAR
    FIELD desc_conta AS CHAR FORMAT 'x(50)' .


INPUT FROM c:\temp\ecfExportado_j.txt.
REPEAT:
    IMPORT UNFORM cLinha.
    
    IF cLinha = ''  THEN NEXT. 
    ASSIGN cRegistro = ENTRY(2,cLinha,"|").
    IF cRegistro = 'j050' THEN DO:
       ASSIGN cConta    = substr(ENTRY(7,cLinha,"|"),1,7)
              ctipo     = ENTRY(5,cLinha,"|")
              cNivel    = ENTRY(6,cLinha,"|")
              cContaPai = substr(ENTRY(8,cLinha,"|"),1,7)
             cDescConta = ENTRY(9,cLinha,"|")
             cNatureza  = ENTRY(4,cLinha,"|").
    END.
    ELSE
      ASSIGN cTipo = ''.
           
   IF cTipo = 'a'  THEN DO:
      IF cContaPai = cConta THEN  DO:
         ASSIGN cNivel = string(INT(cNivel) - 1).
         FIND FIRST estrut_cta_ctbl
             WHERE estrut_cta_ctbl.cod_cta_ctbl_filho = cConta NO-LOCK NO-ERROR.
         IF AVAIL estrut_cta_ctbl THEN
            ASSIGN cContaPai = estrut_cta_ctbl.cod_cta_ctbl_pai.
         ELSE
            ASSIGN cContaPai = 'Pai n∆o encontrado'.
      END.
         
      FIND FIRST ttDadosConta
          WHERE ttDadosConta.conta = cConta
          AND   ttDadosConta.nivel = cNivel
          AND   ttDadosConta.conta_pai = cContaPai NO-ERROR.
      IF NOT AVAIL ttDadosConta THEN DO:
          CREATE ttDadosConta.
          ASSIGN  ttDadosConta.conta = cConta        
                  ttDadosConta.nivel = cNivel        
                  ttDadosConta.conta_pai = cContaPai 
                  ttDadosConta.DESC_conta = cDescConta
                  ttDadosConta.natureza = cNatureza.
      END.
   END.
END.


INPUT CLOSE.

/*FOR EACH ttDadosConta.
    DISP ttDadosConta WITH 1 COL 1 DOWN WIDTH 550.
END.*/

INPUT FROM c:\temp\plano_conta.txt.
 REPEAT:
     CREATE ttPlano.
     IMPORT DELIMITER "|" ttPlano.
 END.

INPUT CLOSE.
INPUT FROM c:\temp\matriz.txt.
REPEAT:
    CREATE ttMatriz.
    IMPORT DELIMITER "|" ttMatriz.
END.
INPUT CLOSE.

   
INPUT FROM c:\temp\j_ajuste.txt.
REPEAT:
    IMPORT UNFORM cLinha.
    ASSIGN cRegistro = ENTRY(2,cLinha,"|").
    IF cRegistro = 'j050' THEN DO:
        ASSIGN cConta = ENTRY(7,cLinha,"|") 
               cTipo  = ENTRY(5,cLinha,"|").
       IF cTipo = 'A' THEN DO:
          
       END.
    END.
    IF cRegistro = 'j051' THEN DO:
       CREATE ttCadContas.
       ASSIGN ttCadContas.conta = cConta
              ttCadContas.cc    =  ENTRY(3,cLinha,"|").
    END.
END.

INPUT CLOSE.

INPUT FROM  c:\temp\k_ajuste.txt.
REPEAT:
    IMPORT UNFORM cLinha.
    ASSIGN cRegistro = ENTRY(2,cLinha,"|").
   
    IF cRegistro = 'k155' OR cRegistro = 'k355' THEN DO:
        FIND FIRST ttMovContas
            WHERE ttmovContas.conta = ENTRY(3,cLinha,"|")
            AND   ttMovContas.cc    = ENTRY(4,cLinha,"|")
            NO-ERROR.
        IF NOT AVAIL ttMovContas THEN DO:
            CREATE ttMovContas.
            ASSIGN ttMovContas.conta = ENTRY(3,cLinha,"|")
                   ttMovContas.cc    = ENTRY(4,cLinha,"|").
        END.
    END.       
END.

INPUT CLOSE.

FOR EACH ttMovContas.
    FIND FIRST ttCadContas
        WHERE ttCadContas.conta = ttMovContas.conta             
        AND   ttcadContas.cc    = ttMovContas.cc
        NO-ERROR.
    ASSIGN ttMovcontas.LOG_achou = AVAIL ttCadContas.
    IF ttMovcontas.LOG_achou = NO THEN DO:
       FIND FIRST ttDadosConta
           WHERE ttDadosConta.conta = ttMovContas.conta
           NO-ERROR.
       IF AVAIL ttDadosConta THEN DO:
          ASSIGN ttMovContas.nivel = ttDadosConta.nivel
                 ttMovContas.conta_pai = ttDadosConta.conta_pai
                 ttMovContas.DESC_conta = ttDadosConta.DESC_conta
                 ttMovContas.natureza   = ttDadosConta.natureza.
       END.
       ELSE DO:
         ASSIGN ttMovContas.nivel           = 'x'
                 ttMovContas.conta_pai      = 'x'
                 ttMovContas.DESC_conta     = 'x'
                 ttMovContas.natureza       = 'x'.

       END.

       FIND FIRST ttMatriz 
           WHERE ttMatriz.conta = ttMovContas.conta
           NO-ERROR.
       IF AVAIL ttmatriz THEN DO:
          ASSIGN ttMovcontas.contaRef = ttmatriz.contaRef.
       END.
       ELSE
         ASSIGN ttMovcontas.contaRef = 'x'.
    END.
    
END.
OUTPUT TO c:\temp\ttMovCOntas.txt.
FOR EACH ttMovContas
    WHERE ttMovContas.LOG_achou = NO BREAK BY ttMovContas.conta BY ttmovContas.cc .
    IF  first-of(ttMovContas.conta) THEN DO:
        EXPORT DELIMITER "|" "|J050" "01012015" ttMovContas.natureza "A" ttMovcontas.nivel ttMovContas.conta ttMovContas.conta_pai ttMovcontas.DESC_conta + "|".
    END.
    EXPORT DELIMITER "|" "|J051" ttMovContas.cc ttMovContas.contaRef + "|".
    
END.
OUTPUT CLOSE.

OUTPUT TO c:\temp\ttmovContasX.txt.
FOR EACH ttMovContas
    WHERE ttMovcontas.LOG_achou = NO
    AND   ttmovContas.nivel = 'x'.
    EXPORT DELIMITER "|" ttMovContas.
END.

OUTPUT CLOSE.
