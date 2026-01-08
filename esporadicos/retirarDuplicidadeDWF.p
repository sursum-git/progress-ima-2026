DEFINE BUFFER bf-dwf-cta-ctbl-refer FOR dwf-cta-ctbl-refer.
OUTPUT TO c:\temp\bf-dwf-cta-ctbl-refer.txt.
FOR EACH  dwf-cta-ctbl-refer
    WHERE dwf-cta-ctbl-refer.cod-cta-ctbl >= '3'
    AND    dwf-cta-ctbl-refer.cod-empresa = '100':
     FIND FIRST bf-dwf-cta-ctbl-refer 
     WHERE dwf-cta-ctbl-refer.cod-cta-ctbl = bf-dwf-cta-ctbl-refer.cod-cta-ctbl
     AND   dwf-cta-ctbl-refer.cod-ccusto = bf-dwf-cta-ctbl-refer.cod-ccusto 
     AND   ROWID(bf-dwf-cta-ctbl-refer) <> rowid(dwf-cta-ctbl-refer)
     AND   dwf-cta-ctbl-refer.cod-empresa = bf-dwf-cta-ctbl-refer.cod-empresa
     EXCLUSIVE-LOCK NO-ERROR.
     IF AVAIL bf-dwf-cta-ctbl-refer THEN DO:
        EXPORT DELIMITER "|" bf-dwf-cta-ctbl-refer.
        DELETE bf-dwf-cta-ctbl-refer.
     END.
END.
OUTPUT CLOSE.
