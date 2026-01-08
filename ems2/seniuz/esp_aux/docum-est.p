DEF VAR c-forn LIKE emitente.nome-abrev.
DEF VAR de-quantidade LIKE item-doc-est.quantidade.
DEF VAR de-peso-liq   LIKE item-doc-est.peso-liquido.
DEF VAR de-desp-ace   LIKE despesa-aces.valor.

OUTPUT TO "c:/lixo/lixo.txt".

PUT "Fornecedor;"  
    "Docto.;"
    "Serie;"
    "Data;"
    "Valor NF;"      
    "Quantidade;"    
    "Peso;"          
    "Desp.Nota;"     
    SKIP.

FOR EACH docum-est WHERE docum-est.dt-emissao >= 01/01/2006 
                     AND docum-est.esp-docto   = 21
                   NO-LOCK:
    ASSIGN de-desp-ace = 0.
    FOR EACH despesa-aces WHERE despesa-aces.nro-docto   = docum-est.nro-docto
                            AND despesa-aces.serie-docto = docum-est.serie-docto
                          NO-LOCK:
        ASSIGN de-desp-ace = de-desp-ace + despesa-ace.valor.
    END.
    ASSIGN de-desp-ace = de-desp-ace + docum-est.valor-frete.

    ASSIGN de-quantidade = 0
           de-peso-liq   = 0.
    FOR EACH item-doc-est OF docum-est NO-LOCK.
        ASSIGN de-quantidade = de-quantidade + item-doc-est.quantidade
               de-peso-liq   = de-peso-liq + item-doc-est.peso-liquido.
    END.
    
    FIND emitente OF docum-est NO-LOCK.
    ASSIGN c-forn = emitente.nome-abrev.

    IF docum-est.valor-frete <> 0 or de-desp-ace <> 0 THEN
       put c-forn ";" 
           docum-est.nro-docto ";"
           docum-est.serie-docto ";"
           docum-est.dt-trans ";"
           docum-est.tot-valor ";"    
           de-quantidade ";" 
           de-peso-liq ";"            
           de-desp-ace ";" 
           SKIP.
END.
OUTPUT CLOSE.
