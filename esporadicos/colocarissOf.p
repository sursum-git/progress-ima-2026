FOR EACH doc-fiscal
    WHERE nat-operacao = '19905'
    AND dt-docto >= 01.01.2017:
    FOR EACH  it-doc-fisc OF doc-fiscal.
        ASSIGN vl-biss-it = vl-tot-item
               cd-trib-iss = 1
               aliquota-iss = 2 
               vl-iss-it  = vl-biss-it * aliquota-iss.

    END.
END.
