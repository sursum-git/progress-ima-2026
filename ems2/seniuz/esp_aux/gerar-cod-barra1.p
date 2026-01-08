{esinc/sz-pcl.i}

DEFINE VARIABLE i-col AS INTEGER      NO-UNDO.
DEFINE VARIABLE i-lin AS INTEGER      NO-UNDO.
DEFINE VARIABLE i-num-bar AS INTEGER  NO-UNDO.
DEFINE VARIABLE c-corte AS CHARACTER  NO-UNDO.

FIND ob-etiqueta  where
     ob-etiqueta.num-etiqueta = 789400 NO-LOCK NO-ERROR.

FIND ITEM WHERE
     ITEM.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

ASSIGN c-corte = "SC".


ASSIGN i-num-bar = INT(STRING(ob-etiqueta.num-etiqueta) + fn-calc-digito(INPUT STRING(ob-etiqueta.num-etiqueta,"9999999999"))).
                                                                                                               



 OUTPUT TO c:\temp\t1.prn.   
 /*   OUTPUT TO PRINTER.     */

        /* Parametros 1= Posicao X
                      2= Posicao Y
                      3= Texto
                      4= Fonte
                      5= Tamanho
                      6= Negrito
                      7= Italico
                      8= Fixo
                      9= Sentido (Horizontal/Vertical */

        PUT UNFORMATTED  /* Linha 1  */
        fn-texto(INPUT i-col +  110, INPUT i-lin +  35, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  300, INPUT i-lin +  35, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  540, INPUT i-lin +  35, INPUT TRIM(c-corte),                                INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col +  665, INPUT i-lin +  35, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  860, INPUT i-lin +  35, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1100, INPUT i-lin +  35, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1225, INPUT i-lin +  35, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1420, INPUT i-lin +  35, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1660, INPUT i-lin +  35, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1795, INPUT i-lin +  35, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1980, INPUT i-lin +  35, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 2220, INPUT i-lin +  35, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED    /* Linha 2  */
        fn-texto(INPUT i-col + 110, INPUT i-lin +  190, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 300, INPUT i-lin +  190, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 540, INPUT i-lin +  190, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col +  665, INPUT i-lin + 190, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  860, INPUT i-lin + 190, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1100, INPUT i-lin + 190, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1225, INPUT i-lin + 190, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1420, INPUT i-lin + 190, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1660, INPUT i-lin + 190, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1795, INPUT i-lin + 190, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1980, INPUT i-lin + 190, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 2220, INPUT i-lin + 190, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

        PUT UNFORMATTED    /* Linha 3  */
        fn-texto(INPUT i-col + 110, INPUT i-lin +  340, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 300, INPUT i-lin +  340, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 540, INPUT i-lin +  340, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col +  665, INPUT i-lin + 340, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  860, INPUT i-lin + 340, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1100, INPUT i-lin + 340, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1225, INPUT i-lin + 340, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1420, INPUT i-lin + 340, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1660, INPUT i-lin + 340, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1795, INPUT i-lin + 340, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1980, INPUT i-lin + 340, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 2220, INPUT i-lin + 340, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED    /* Linha 4  */
        fn-texto(INPUT i-col + 110, INPUT i-lin +  490, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 300, INPUT i-lin +  490, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 540, INPUT i-lin +  490, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col +  665, INPUT i-lin + 490, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  860, INPUT i-lin + 490, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1100, INPUT i-lin + 490, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1225, INPUT i-lin + 490, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1420, INPUT i-lin + 490, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1660, INPUT i-lin + 490, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1795, INPUT i-lin + 490, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1980, INPUT i-lin + 490, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 2220, INPUT i-lin + 490, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

        PUT UNFORMATTED    /* Linha 5 */
        fn-texto(INPUT i-col + 110, INPUT i-lin +  642, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 300, INPUT i-lin +  642, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 540, INPUT i-lin +  642, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col +  665, INPUT i-lin + 642, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  860, INPUT i-lin + 642, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1100, INPUT i-lin + 642, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1225, INPUT i-lin + 642, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1420, INPUT i-lin + 642, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1660, INPUT i-lin + 642, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1795, INPUT i-lin + 642, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1980, INPUT i-lin + 642, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 2220, INPUT i-lin + 642, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

        PUT UNFORMATTED  /* Linha 6  */
        fn-texto(INPUT i-col +  110, INPUT i-lin + 790, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  300, INPUT i-lin + 790, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  540, INPUT i-lin + 790, INPUT TRIM(c-corte),                                INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col +  665, INPUT i-lin + 790, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  860, INPUT i-lin + 790, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1100, INPUT i-lin + 790, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1225, INPUT i-lin + 790, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1420, INPUT i-lin + 790, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1660, INPUT i-lin + 790, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1795, INPUT i-lin + 790, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1980, INPUT i-lin + 790, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 2220, INPUT i-lin + 790, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED    /* Linha 7   */
        fn-texto(INPUT i-col + 110, INPUT i-lin +  940, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 300, INPUT i-lin +  940, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 540, INPUT i-lin +  940, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col +  665, INPUT i-lin + 940, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  860, INPUT i-lin + 940, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1100, INPUT i-lin + 940, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1225, INPUT i-lin + 940, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1420, INPUT i-lin + 940, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1660, INPUT i-lin + 940, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1795, INPUT i-lin + 940, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1980, INPUT i-lin + 940, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 2220, INPUT i-lin + 940, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

        PUT UNFORMATTED    /* Linha 8 */
        fn-texto(INPUT i-col +  110, INPUT i-lin + 1090, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  300, INPUT i-lin + 1090, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  540, INPUT i-lin + 1090, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col +  665, INPUT i-lin + 1090, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  860, INPUT i-lin + 1090, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1100, INPUT i-lin + 1090, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1225, INPUT i-lin + 1090, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1420, INPUT i-lin + 1090, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1660, INPUT i-lin + 1090, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1795, INPUT i-lin + 1090, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1980, INPUT i-lin + 1090, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 2220, INPUT i-lin + 1090, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED    /* Linha 9 */
        fn-texto(INPUT i-col +  110, INPUT i-lin + 1242, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  300, INPUT i-lin + 1242, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  540, INPUT i-lin + 1242, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col +  665, INPUT i-lin + 1242, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  860, INPUT i-lin + 1242, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1100, INPUT i-lin + 1242, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1225, INPUT i-lin + 1242, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1420, INPUT i-lin + 1242, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1660, INPUT i-lin + 1242, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1795, INPUT i-lin + 1242, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1980, INPUT i-lin + 1242, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 2220, INPUT i-lin + 1242, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

        PUT UNFORMATTED    /* Linha 10  */
        fn-texto(INPUT i-col +  110, INPUT i-lin + 1395, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  300, INPUT i-lin + 1395, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  540, INPUT i-lin + 1395, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col +  665, INPUT i-lin + 1395, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  860, INPUT i-lin + 1395, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1100, INPUT i-lin + 1395, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1225, INPUT i-lin + 1395, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1420, INPUT i-lin + 1395, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1660, INPUT i-lin + 1395, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1795, INPUT i-lin + 1395, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1980, INPUT i-lin + 1395, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 2220, INPUT i-lin + 1395, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

        /*------------------------------------------------------*/

        PUT UNFORMATTED  /* Linha 11  */
        fn-texto(INPUT i-col +  110, INPUT i-lin + 1537, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  300, INPUT i-lin + 1537, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  540, INPUT i-lin + 1537, INPUT TRIM(c-corte),                                INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col +  665, INPUT i-lin + 1537, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  860, INPUT i-lin + 1537, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1100, INPUT i-lin + 1537, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1225, INPUT i-lin + 1537, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1420, INPUT i-lin + 1537, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1660, INPUT i-lin + 1537, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1795, INPUT i-lin + 1537, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1980, INPUT i-lin + 1537, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 2220, INPUT i-lin + 1537, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED    /* Linha 12 */
        fn-texto(INPUT i-col +  110, INPUT i-lin + 1687, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  300, INPUT i-lin + 1687, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  540, INPUT i-lin + 1687, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col +  665, INPUT i-lin + 1687, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  860, INPUT i-lin + 1687, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1100, INPUT i-lin + 1687, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1225, INPUT i-lin + 1687, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1420, INPUT i-lin + 1687, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1660, INPUT i-lin + 1687, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1795, INPUT i-lin + 1687, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1980, INPUT i-lin + 1687, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 2220, INPUT i-lin + 1687, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

        PUT UNFORMATTED    /* Linha 13 */
        fn-texto(INPUT i-col +  110, INPUT i-lin + 1839, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  300, INPUT i-lin + 1839, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  540, INPUT i-lin + 1839, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col +  665, INPUT i-lin + 1839, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  860, INPUT i-lin + 1839, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1100, INPUT i-lin + 1839, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1225, INPUT i-lin + 1839, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1420, INPUT i-lin + 1839, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1660, INPUT i-lin + 1839, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1795, INPUT i-lin + 1839, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1980, INPUT i-lin + 1839, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 2220, INPUT i-lin + 1839, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED    /* Linha 14 */
        fn-texto(INPUT i-col +  110, INPUT i-lin + 1987, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  300, INPUT i-lin + 1987, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  540, INPUT i-lin + 1987, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col +  665, INPUT i-lin + 1987, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  860, INPUT i-lin + 1987, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1100, INPUT i-lin + 1987, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1225, INPUT i-lin + 1987, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1420, INPUT i-lin + 1987, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1660, INPUT i-lin + 1987, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1795, INPUT i-lin + 1987, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1980, INPUT i-lin + 1987, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 2220, INPUT i-lin + 1987, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

        PUT UNFORMATTED    /* Linha 15 */
        fn-texto(INPUT i-col +  110, INPUT i-lin + 2142, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  300, INPUT i-lin + 2142, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  540, INPUT i-lin + 2142, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col +  665, INPUT i-lin + 2142, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  860, INPUT i-lin + 2142, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1100, INPUT i-lin + 2142, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1225, INPUT i-lin + 2142, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1420, INPUT i-lin + 2142, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1660, INPUT i-lin + 2142, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1795, INPUT i-lin + 2142, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1980, INPUT i-lin + 2142, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 2220, INPUT i-lin + 2142, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

        PUT UNFORMATTED  /* Linha 16 */
        fn-texto(INPUT i-col +  110, INPUT i-lin + 2287, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  300, INPUT i-lin + 2287, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  540, INPUT i-lin + 2287, INPUT TRIM(c-corte),                                INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col +  665, INPUT i-lin + 2287, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  860, INPUT i-lin + 2287, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1100, INPUT i-lin + 2287, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1225, INPUT i-lin + 2287, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1420, INPUT i-lin + 2287, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1660, INPUT i-lin + 2287, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1795, INPUT i-lin + 2287, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1980, INPUT i-lin + 2287, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 2220, INPUT i-lin + 2287, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED    /* Linha 17  */
        fn-texto(INPUT i-col +  110, INPUT i-lin + 2441, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  300, INPUT i-lin + 2441, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  540, INPUT i-lin + 2441, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col +  665, INPUT i-lin + 2441, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  860, INPUT i-lin + 2441, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1100, INPUT i-lin + 2441, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1225, INPUT i-lin + 2441, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1420, INPUT i-lin + 2441, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1660, INPUT i-lin + 2441, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1795, INPUT i-lin + 2441, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1980, INPUT i-lin + 2441, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 2220, INPUT i-lin + 2441, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

        PUT UNFORMATTED    /* Linha 18 */
        fn-texto(INPUT i-col +  110, INPUT i-lin + 2592, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  300, INPUT i-lin + 2592, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  540, INPUT i-lin + 2592, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col +  665, INPUT i-lin + 2592, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  860, INPUT i-lin + 2592, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1100, INPUT i-lin + 2592, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1225, INPUT i-lin + 2592, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1420, INPUT i-lin + 2592, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1660, INPUT i-lin + 2592, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1795, INPUT i-lin + 2592, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1980, INPUT i-lin + 2592, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 2220, INPUT i-lin + 2592, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED    /* Linha 19 */
        fn-texto(INPUT i-col +  110, INPUT i-lin + 2742, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  300, INPUT i-lin + 2742, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  540, INPUT i-lin + 2742, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col +  665, INPUT i-lin + 2742, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  860, INPUT i-lin + 2742, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1100, INPUT i-lin + 2742, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1225, INPUT i-lin + 2742, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1420, INPUT i-lin + 2742, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1660, INPUT i-lin + 2742, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1795, INPUT i-lin + 2742, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1980, INPUT i-lin + 2742, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 2220, INPUT i-lin + 2742, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

        PUT UNFORMATTED    /* Linha 20  */
        fn-texto(INPUT i-col +  110, INPUT i-lin + 2892, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  300, INPUT i-lin + 2892, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  540, INPUT i-lin + 2892, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col +  665, INPUT i-lin + 2892, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  860, INPUT i-lin + 2892, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1100, INPUT i-lin + 2892, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1225, INPUT i-lin + 2892, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1420, INPUT i-lin + 2892, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1660, INPUT i-lin + 2892, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1795, INPUT i-lin + 2892, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1980, INPUT i-lin + 2892, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 2220, INPUT i-lin + 2892, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

    /* Parametros 1= Posi‡Æo do Eixo X
                  2= Posicao do Eixo Y
                  3= Texto
                  4= Sentido
                  5= Altura
                  6= Largura */
        PUT UNFORMATTED  /* Linha 1  */
        fn-code25 (INPUT i-col + 113, input i-lin + 40,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",              
                   INPUT 1.8,
                   INPUT 4.5)
        fn-code25 (INPUT i-col + 673, input i-lin + 40,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",              
                   INPUT 1.8,
                   INPUT 4.5)
       fn-code25 (INPUT i-col + 1233, input i-lin + 40,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",             
                  INPUT 1.8,
                  INPUT 4.5)
       fn-code25 (INPUT i-col + 1798, input i-lin + 40,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",              
                  INPUT 1.8,
                  INPUT 4.5).

        PUT UNFORMATTED  /* Linha 2 */
        fn-code25 (INPUT i-col + 113, input i-lin + 195,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",              
                   INPUT 1.8,
                   INPUT 4.5)
        fn-code25 (INPUT i-col + 673, input i-lin + 195,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",            
                   INPUT 1.8,
                   INPUT 4.5)
       fn-code25 (INPUT i-col + 1233, input i-lin + 195,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",             
                  INPUT 1.8,
                  INPUT 4.5)
       fn-code25 (INPUT i-col + 1798, input i-lin + 195,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",            
                  INPUT 1.8,
                  INPUT 4.5).

        PUT UNFORMATTED  /* Linha 3 */
        fn-code25 (INPUT i-col + 113, input i-lin + 345,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",              
                   INPUT 1.8,
                   INPUT 4.5)
        fn-code25 (INPUT i-col + 673, input i-lin + 345,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",            
                   INPUT 1.8,
                   INPUT 4.5)
       fn-code25 (INPUT i-col + 1233, input i-lin + 345,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",             
                  INPUT 1.8,
                  INPUT 4.5)
       fn-code25 (INPUT i-col + 1798, input i-lin + 345,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",            
                  INPUT 1.8,
                  INPUT 4.5).

        PUT UNFORMATTED  /* Linha 4 */
        fn-code25 (INPUT i-col + 113, input i-lin + 495,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",              
                   INPUT 1.8,
                   INPUT 4.5)
        fn-code25 (INPUT i-col + 673, input i-lin + 495,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",            
                   INPUT 1.8,
                   INPUT 4.5)
       fn-code25 (INPUT i-col + 1233, input i-lin + 495,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",             
                  INPUT 1.8,
                  INPUT 4.5)
       fn-code25 (INPUT i-col + 1798, input i-lin + 495,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",            
                  INPUT 1.8,
                  INPUT 4.5).

        PUT UNFORMATTED  /* Linha 5 */
        fn-code25 (INPUT i-col + 113, input i-lin + 647,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",              
                   INPUT 1.8,
                   INPUT 4.5)
        fn-code25 (INPUT i-col + 673, input i-lin + 647,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",            
                   INPUT 1.8,
                   INPUT 4.5)
       fn-code25 (INPUT i-col + 1233, input i-lin + 647,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",             
                  INPUT 1.8,
                  INPUT 4.5)
       fn-code25 (INPUT i-col + 1798, input i-lin + 647,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",            
                  INPUT 1.8,
                  INPUT 4.5).

        PUT UNFORMATTED   /* Linha 6  */
        fn-code25 (INPUT i-col + 113, input i-lin + 795,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",              
                   INPUT 1.8,
                   INPUT 4.5)
        fn-code25 (INPUT i-col + 673, input i-lin + 795,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",              
                   INPUT 1.8,
                   INPUT 4.5)
       fn-code25 (INPUT i-col + 1233, input i-lin + 795,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",             
                  INPUT 1.8,
                  INPUT 4.5)
       fn-code25 (INPUT i-col + 1798, input i-lin + 795,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",              
                  INPUT 1.8,
                  INPUT 4.5).

        PUT UNFORMATTED  /* Linha 7  */
        fn-code25 (INPUT i-col + 113, input i-lin + 945,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",              
                   INPUT 1.8,
                   INPUT 4.5)
        fn-code25 (INPUT i-col + 673, input i-lin + 945,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",            
                   INPUT 1.8,
                   INPUT 4.5)
       fn-code25 (INPUT i-col + 1233, input i-lin + 945,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",             
                  INPUT 1.8,
                  INPUT 4.5)
       fn-code25 (INPUT i-col + 1798, input i-lin + 945,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",            
                  INPUT 1.8,
                  INPUT 4.5).

        PUT UNFORMATTED  /* Linha 8  */
        fn-code25 (INPUT i-col + 113, input i-lin + 1095,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",              
                   INPUT 1.8,
                   INPUT 4.5)
        fn-code25 (INPUT i-col + 673, input i-lin + 1095,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",            
                   INPUT 1.8,
                   INPUT 4.5)
       fn-code25 (INPUT i-col + 1233, input i-lin + 1095,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",             
                  INPUT 1.8,
                  INPUT 4.5)
       fn-code25 (INPUT i-col + 1798, input i-lin + 1095,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",            
                  INPUT 1.8,
                  INPUT 4.5).

        PUT UNFORMATTED  /* Linha 9  */
        fn-code25 (INPUT i-col + 113, input i-lin + 1247,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",              
                   INPUT 1.8,
                   INPUT 4.5)
        fn-code25 (INPUT i-col + 673, input i-lin + 1247,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",            
                   INPUT 1.8,
                   INPUT 4.5)
       fn-code25 (INPUT i-col + 1233, input i-lin + 1247,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",             
                  INPUT 1.8,
                  INPUT 4.5)
       fn-code25 (INPUT i-col + 1798, input i-lin + 1247,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",            
                  INPUT 1.8,
                  INPUT 4.5).

        PUT UNFORMATTED  /* Linha 10  */
        fn-code25 (INPUT i-col + 113, input i-lin + 1400,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",              
                   INPUT 1.8,
                   INPUT 4.5)
        fn-code25 (INPUT i-col + 673, input i-lin + 1400,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",            
                   INPUT 1.8,
                   INPUT 4.5)
       fn-code25 (INPUT i-col + 1233, input i-lin + 1400,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",             
                  INPUT 1.8,
                  INPUT 4.5)
       fn-code25 (INPUT i-col + 1798, input i-lin + 1400,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",            
                  INPUT 1.8,
                  INPUT 4.5).
        /*----------------------------------------------------*/

        PUT UNFORMATTED  /* Linha 11  */
        fn-code25 (INPUT i-col + 113, input i-lin + 1542,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",              
                   INPUT 1.8,
                   INPUT 4.5)
        fn-code25 (INPUT i-col + 673, input i-lin + 1542,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",              
                   INPUT 1.8,
                   INPUT 4.5)
       fn-code25 (INPUT i-col + 1233, input i-lin + 1542,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",             
                  INPUT 1.8,
                  INPUT 4.5)
       fn-code25 (INPUT i-col + 1798, input i-lin + 1542,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",              
                  INPUT 1.8,
                  INPUT 4.5).

        PUT UNFORMATTED  /* Linha 12 */
        fn-code25 (INPUT i-col + 113, input i-lin + 1692,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",              
                   INPUT 1.8,
                   INPUT 4.5)
        fn-code25 (INPUT i-col + 673, input i-lin + 1692,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",            
                   INPUT 1.8,
                   INPUT 4.5)
       fn-code25 (INPUT i-col + 1233, input i-lin + 1692,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",             
                  INPUT 1.8,
                  INPUT 4.5)
       fn-code25 (INPUT i-col + 1798, input i-lin + 1692,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",            
                  INPUT 1.8,
                  INPUT 4.5).

        PUT UNFORMATTED  /* Linha 13 */
        fn-code25 (INPUT i-col + 113, input i-lin + 1844,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",              
                   INPUT 1.8,
                   INPUT 4.5)
        fn-code25 (INPUT i-col + 673, input i-lin + 1844,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",            
                   INPUT 1.8,
                   INPUT 4.5)
       fn-code25 (INPUT i-col + 1233, input i-lin + 1844,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",             
                  INPUT 1.8,
                  INPUT 4.5)
       fn-code25 (INPUT i-col + 1798, input i-lin + 1844,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",            
                  INPUT 1.8,
                  INPUT 4.5).

        PUT UNFORMATTED  /* Linha 14 */
        fn-code25 (INPUT i-col + 113, input i-lin + 1992,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",              
                   INPUT 1.8,
                   INPUT 4.5)
        fn-code25 (INPUT i-col + 673, input i-lin + 1992,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",            
                   INPUT 1.8,
                   INPUT 4.5)
       fn-code25 (INPUT i-col + 1233, input i-lin + 1992,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",             
                  INPUT 1.8,
                  INPUT 4.5)
       fn-code25 (INPUT i-col + 1798, input i-lin + 1992,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",            
                  INPUT 1.8,
                  INPUT 4.5).

        PUT UNFORMATTED  /* Linha 15 */
        fn-code25 (INPUT i-col + 113, input i-lin + 2147,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",              
                   INPUT 1.8,
                   INPUT 4.5)
        fn-code25 (INPUT i-col + 673, input i-lin + 2147,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",            
                   INPUT 1.8,
                   INPUT 4.5)
       fn-code25 (INPUT i-col + 1233, input i-lin + 2147,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",             
                  INPUT 1.8,
                  INPUT 4.5)
       fn-code25 (INPUT i-col + 1798, input i-lin + 2147,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",            
                  INPUT 1.8,
                  INPUT 4.5).

        PUT UNFORMATTED   /* Linha 16  */
        fn-code25 (INPUT i-col + 113, input i-lin + 2292,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",              
                   INPUT 1.8,
                   INPUT 4.5)
        fn-code25 (INPUT i-col + 673, input i-lin + 2292,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",              
                   INPUT 1.8,
                   INPUT 4.5)
       fn-code25 (INPUT i-col + 1233, input i-lin + 2292,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",             
                  INPUT 1.8,
                  INPUT 4.5)
       fn-code25 (INPUT i-col + 1798, input i-lin + 2292,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",              
                  INPUT 1.8,
                  INPUT 4.5).

        PUT UNFORMATTED  /* Linha 17  */
        fn-code25 (INPUT i-col + 113, input i-lin + 2446,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",              
                   INPUT 1.8,
                   INPUT 4.5)
        fn-code25 (INPUT i-col + 673, input i-lin + 2446,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",            
                   INPUT 1.8,
                   INPUT 4.5)
       fn-code25 (INPUT i-col + 1233, input i-lin + 2446,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",             
                  INPUT 1.8,
                  INPUT 4.5)
       fn-code25 (INPUT i-col + 1798, input i-lin + 2446,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",            
                  INPUT 1.8,
                  INPUT 4.5).

        PUT UNFORMATTED  /* Linha 18  */
        fn-code25 (INPUT i-col + 113, input i-lin + 2596,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",              
                   INPUT 1.8,
                   INPUT 4.5)
        fn-code25 (INPUT i-col + 673, input i-lin + 2596,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",            
                   INPUT 1.8,
                   INPUT 4.5)
       fn-code25 (INPUT i-col + 1233, input i-lin + 2596,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",             
                  INPUT 1.8,
                  INPUT 4.5)
       fn-code25 (INPUT i-col + 1798, input i-lin + 2596,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",            
                  INPUT 1.8,
                  INPUT 4.5).

        PUT UNFORMATTED  /* Linha 19  */
        fn-code25 (INPUT i-col + 113, input i-lin + 2747,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",              
                   INPUT 1.8,
                   INPUT 4.5)
        fn-code25 (INPUT i-col + 673, input i-lin + 2747,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",            
                   INPUT 1.8,
                   INPUT 4.5)
       fn-code25 (INPUT i-col + 1233, input i-lin + 2747,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",             
                  INPUT 1.8,
                  INPUT 4.5)
       fn-code25 (INPUT i-col + 1798, input i-lin + 2747,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",            
                  INPUT 1.8,
                  INPUT 4.5).

        PUT UNFORMATTED  /* Linha 20  */
        fn-code25 (INPUT i-col + 113, input i-lin + 2897,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",              
                   INPUT 1.8,
                   INPUT 4.5)
        fn-code25 (INPUT i-col + 673, input i-lin + 2897,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",            
                   INPUT 1.8,
                   INPUT 4.5)
       fn-code25 (INPUT i-col + 1233, input i-lin + 2897,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",             
                  INPUT 1.8,
                  INPUT 4.5)
       fn-code25 (INPUT i-col + 1798, input i-lin + 2897,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",            
                  INPUT 1.8,
                  INPUT 4.5).



OUTPUT CLOSE.
