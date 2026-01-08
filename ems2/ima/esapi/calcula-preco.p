/****************************************************************************
** Programa: calcula-preco.p
** Autor   : Toninho    
** Data    : 13/01/2005
** Objetivo: Calcular e devolver o ultimo preco m‚dio de um Item
*****************************************************************************/
DEFINE INPUT  PARAMETER p-cod-estabel AS CHARACTER.
DEFINE INPUT  PARAMETER p-it-codigo   AS CHARACTER.
DEFINE OUTPUT PARAMETER p-preco       AS DECIMAL.
 
 
FIND FIRST estabelec WHERE 
           estabelec.cod-estabel = p-cod-estabel
           NO-LOCK NO-ERROR.

FIND FIRST ITEM WHERE 
           ITEM.it-codigo = p-it-codigo
           NO-LOCK NO-ERROR.

FIND FIRST item-estab WHERE 
           item-estab.it-codigo = ITEM.it-codigo AND 
           item-estab.cod-estabel = estabelec.cod-estabel 
           NO-LOCK NO-ERROR. 

IF AVAIL item-estab THEN DO:
   IF estabelec.custo-contab = 1 THEN
      ASSIGN p-preco = item-estab.val-unit-mat-m[1] + item-estab.val-unit-ggf-m[1] + item-estab.val-unit-mob-m[1].
   ELSE IF estabelec.custo-contab = 2 THEN
      ASSIGN p-preco = item-estab.val-unit-mat-o[1] + item-estab.val-unit-ggf-o[1] + item-estab.val-unit-mob-o[1].
   ELSE IF estabelec.custo-contab = 3 THEN
      ASSIGN p-preco = item-estab.val-unit-mat-p[1] + item-estab.val-unit-ggf-p[1] + item-estab.val-unit-mob-p[1].
 
END.

IF p-preco = 0 THEN DO.  
   FIND LAST sl-it-per WHERE
             sl-it-per.periodo <= TODAY AND
             sl-it-per.periodo >= ADD-INTERVAL(TODAY,-5,'years') AND
             sl-it-per.it-codigo = p-it-codigo AND
             sl-it-per.cod-estabel = p-cod-estabel 
             NO-LOCK NO-ERROR.
            
   IF AVAIL sl-it-per THEN
      ASSIGN p-preco = sl-it-per.val-unit-mat-m[1] + sl-it-per.val-unit-ggf-m[1].
END.

IF p-preco = 0 THEN DO.  
   FIND LAST pr-it-per WHERE
             pr-it-per.periodo <= TODAY AND
             pr-it-per.periodo >= ADD-INTERVAL(TODAY,-5,'years') AND
             pr-it-per.cod-estabel = estabelec.cod-estabel AND
             pr-it-per.it-codigo = ITEM.it-codigo NO-LOCK NO-ERROR.

   IF AVAIL pr-it-per THEN
      ASSIGN p-preco = pr-it-per.val-unit-mat-m[1] + sl-it-per.val-unit-ggf-m[1].
END.


IF p-preco = 0 THEN 
   ASSIGN p-preco = item.preco-ul-ent.

IF p-preco = 0 THEN     /* Pre¯o Reposi¯Êo */
   ASSIGN p-preco = item.preco-repos.

IF p-preco = 0 THEN     /* Pre¯o Base */
   ASSIGN p-preco = item.preco-base.

IF p-preco = 0 THEN     /* Valorizada a 1,00 */
    ASSIGN p-preco = 1.
