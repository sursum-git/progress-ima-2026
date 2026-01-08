DEFINE VARIABLE cDescNatOPeracao LIKE natur-oper.denominacao   NO-UNDO.
DEFINE VARIABLE cCodCFOP         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDescCFOP        AS CHARACTER FORMAT 'x(200)'  NO-UNDO.
DEFINE VARIABLE dtini AS DATE        NO-UNDO FORMAT '99/99/9999'.
DEFINE VARIABLE dtfim AS DATE        NO-UNDO FORMAT '99/99/9999'.
UPDATE dtini dtfim.
OUTPUT TO c:\temp\nota-fiscal-saida.txt.
FOR EACH nota-fiscal NO-LOCK
    WHERE nota-fiscal.dt-emis-nota >= dtini
    AND   nota-fiscal.dt-emis-nota <= dtfim:
    
    RUN descNatOperacao(nota-fiscal.nat-operacao,OUTPUT cDescNatOPeracao, OUTPUT cCodCFOP, OUTPUT cDEscCFOP).
    EXPORT DELIMITER "|" 
        nota-fiscal.dt-emis-nota
        nota-fiscal.serie 
        nota-fiscal.nr-nota-fis
        nota-fiscal.vl-tot-nota 
        nota-fiscal.nat-operacao
        cDescNatOPeracao
        nota-fiscal.idi-sit-nf-eletro
        nota-fiscal.dt-cancel
        nota-fiscal.esp-docto
        cCodCFOP
        cDescCFOP .

END.
OUTPUT CLOSE.

OUTPUT TO c:\temp\nota-fiscal-entrada.txt.
FOR EACH docum-est
    WHERE docum-est.dt-trans >= dtini
    AND   docum-est.dt-trans  <= dtFim:
    RUN descNatOperacao(docum-est.nat-operacao,OUTPUT cDescNatOPeracao,OUTPUT cCodCFOP, OUTPUT cDEscCFOP).
    EXPORT DELIMITER "|" docum-est.dt-trans 
        docum-est.serie
        docum-est.nro-docto
        docum-est.tot-vl
        docum-est.nat-operacao
        docum-est.esp-docto 
        cDescNatOperacao
        docum-est.idi-sit-nf-eletro
        cCodCFOP
        cDescCFOP.

END.
OUTPUT CLOSE.


PROCEDURE descNatOperacao:
   DEFINE INPUT  PARAMETER cNatOperacao LIKE natur-oper.nat-operacao   NO-UNDO.
   DEFINE OUTPUT PARAMETER cDescNatOperacao LIKE natur-oper.denominacao NO-UNDO INIT ''.
   DEFINE OUTPUT PARAMETER cCFOP AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER cDescCFOP AS CHARACTER FORMAT 'x(200)'  NO-UNDO.
   FIND FIRST natur-oper WHERE 
       natur-oper.nat-operacao = cNatOPeracao
        NO-LOCK NO-ERROR.
   IF AVAIL natur-oper THEN DO:
      ASSIGN cDescnatOperacao = natur-oper.denominacao
             cCFOP = natur-oper.cod-cfop.
      FIND FIRST cfop-natur OF natur-oper
          NO-LOCK NO-ERROR.
      ASSIGN cDescCFOP = IF AVAIL cfop-natur THEN cfop-natur.des-cfop ELSE ''.


   END.
      
   



END PROCEDURE.

