
DEFINE INPUT  PARAMETER p-cod-estabel AS CHARACTER.
DEFINE INPUT  PARAMETER p-it-codigo   AS CHARACTER.
DEFINE OUTPUT PARAMETER p-preco       AS DECIMAL.

FIND FIRST estabelec
    WHERE estabelec.cod-estabel = p-cod-estabel
    NO-LOCK NO-ERROR.

FIND FIRST ITEM
    WHERE ITEM.it-codigo = p-it-codigo
    NO-LOCK NO-ERROR.

FIND FIRST item-uni-estab
    WHERE item-uni-estab.it-codigo   = ITEM.it-codigo AND 
          item-uni-estab.cod-estabel = estabelec.cod-estabel
    NO-LOCK. 

FIND FIRST natureza-despesa
    WHERE natureza-despesa.nat-despesa = item-uni-estab.nat-despesa
    NO-LOCK.

IF AVAIL estabelec AND AVAIL item-estab THEN DO:
    IF estabelec.custo-contab = 1 THEN
        ASSIGN p-preco = item-estab.val-unit-mat-m[1] + item-estab.val-unit-ggf-m[1] + item-estab.val-unit-mob-m[1].
    ELSE
        IF estabelec.custo-contab = 2 THEN
            ASSIGN p-preco = item-estab.val-unit-mat-o[1] + item-estab.val-unit-ggf-o[1] + item-estab.val-unit-mob-o[1].
        ELSE
            IF estabelec.custo-contab = 3 THEN
                ASSIGN p-preco = item-estab.val-unit-mat-p[1] + item-estab.val-unit-ggf-p[1] + item-estab.val-unit-mob-p[1].
END.

IF p-preco = 0 THEN DO:
    ASSIGN p-preco = item.preco-ul-ent.

    IF AVAIL requisicao THEN DO:
        IF AVAIL item-uni-estab THEN    
            ASSIGN p-preco = item-uni-estab.preco-ul-ent.
    END.     
END.

IF p-preco = 0 THEN DO:    /* Pre¯o Reposi¯Êo */
    ASSIGN p-preco = item.preco-repos.

    IF AVAIL requisicao THEN DO:
        IF AVAIL item-uni-estab THEN    
            ASSIGN p-preco = item-uni-estab.preco-repos.
    END.     
END.

IF p-preco = 0 THEN DO:    /* Pre¯o Base */
    ASSIGN p-preco = item.preco-base.

    IF AVAIL requisicao THEN DO:
        IF AVAIL item-uni-estab THEN    
            ASSIGN p-preco = item-uni-estab.preco-base.
    END.     
END.

IF p-preco = 0 THEN     /* Valorizada a 1,00 */
    ASSIGN p-preco = 1.
