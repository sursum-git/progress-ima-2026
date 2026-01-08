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
/* OUTPUT TO PRINTER.  */
 
        /* Parametros 1= Posicao X
                      2= Posicao Y
                      3= Texto
                      4= Fonte
                      5= Tamanho
                      6= Negrito
                      7= Italico
                      8= Fixo
                      9= Sentido (Horizontal/Vertical */

        PUT UNFORMATTED  /* Linha 1   */
            fn-texto(INPUT i-col +  145, INPUT i-lin +  130, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  155, INPUT i-lin +   30, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col +  465, INPUT i-lin +   30, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
    
        PUT UNFORMATTED  /* Linha 1   */
            fn-texto(INPUT i-col +  715, INPUT i-lin +  130, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  725, INPUT i-lin +   30, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1035, INPUT i-lin +   30, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED  /* Linha 1   */
            fn-texto(INPUT i-col + 1275, INPUT i-lin +  130, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1285, INPUT i-lin +   30, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1595, INPUT i-lin +   30, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
        

        PUT UNFORMATTED  /* Linha 1   */
            fn-texto(INPUT i-col + 1835, INPUT i-lin +  130, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1845, INPUT i-lin +   30, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 2155, INPUT i-lin +   30, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED  /* Linha 2   */
            fn-texto(INPUT i-col +  145, INPUT i-lin +  285, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  155, INPUT i-lin +  185, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col +  465, INPUT i-lin +  185, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
    
        PUT UNFORMATTED  /* Linha 2   */
            fn-texto(INPUT i-col +  715, INPUT i-lin +  285, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  725, INPUT i-lin +  185, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1035, INPUT i-lin +  185, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED  /* Linha 2   */
            fn-texto(INPUT i-col + 1275, INPUT i-lin +  285, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1285, INPUT i-lin +  185, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1595, INPUT i-lin +  185, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
        

        PUT UNFORMATTED  /* Linha 2   */
            fn-texto(INPUT i-col + 1835, INPUT i-lin +  285, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1845, INPUT i-lin +  185, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 2155, INPUT i-lin +  185, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED  /* Linha 3   */
            fn-texto(INPUT i-col +  145, INPUT i-lin +  435, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  155, INPUT i-lin +  335, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col +  465, INPUT i-lin +  335, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
    
        PUT UNFORMATTED  /* Linha 3   */
            fn-texto(INPUT i-col +  715, INPUT i-lin +  435, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  725, INPUT i-lin +  335, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1035, INPUT i-lin +  335, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED  /* Linha 3   */
            fn-texto(INPUT i-col + 1275, INPUT i-lin +  435, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1285, INPUT i-lin +  335, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1595, INPUT i-lin +  335, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
        

        PUT UNFORMATTED  /* Linha 3   */
            fn-texto(INPUT i-col + 1835, INPUT i-lin +  435, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1845, INPUT i-lin +  335, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 2155, INPUT i-lin +  335, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

        PUT UNFORMATTED  /* Linha 4   */
            fn-texto(INPUT i-col +  145, INPUT i-lin +  585, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  155, INPUT i-lin +  485, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col +  465, INPUT i-lin +  485, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
    
        PUT UNFORMATTED  /* Linha 4   */
            fn-texto(INPUT i-col +  715, INPUT i-lin +  585, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  725, INPUT i-lin +  485, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1035, INPUT i-lin +  485, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED  /* Linha 4   */
            fn-texto(INPUT i-col + 1275, INPUT i-lin +  585, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1285, INPUT i-lin +  485, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1595, INPUT i-lin +  485, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
        

        PUT UNFORMATTED  /* Linha 4   */
            fn-texto(INPUT i-col + 1835, INPUT i-lin +  585, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1845, INPUT i-lin +  485, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 2155, INPUT i-lin +  485, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

        PUT UNFORMATTED  /* Linha 5   */
            fn-texto(INPUT i-col +  145, INPUT i-lin +  737, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  155, INPUT i-lin +  637, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col +  465, INPUT i-lin +  637, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
    
        PUT UNFORMATTED  /* Linha 5   */
            fn-texto(INPUT i-col +  715, INPUT i-lin +  737, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  725, INPUT i-lin +  637, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1035, INPUT i-lin +  637, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED  /* Linha 5   */
            fn-texto(INPUT i-col + 1275, INPUT i-lin +  737, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1285, INPUT i-lin +  637, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1595, INPUT i-lin +  637, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
        

        PUT UNFORMATTED  /* Linha 5   */
            fn-texto(INPUT i-col + 1835, INPUT i-lin +  737, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1845, INPUT i-lin +  637, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 2155, INPUT i-lin +  637, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

        PUT UNFORMATTED  /* Linha 6   */
            fn-texto(INPUT i-col +  145, INPUT i-lin +  885, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  155, INPUT i-lin +  785, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col +  465, INPUT i-lin +  785, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
    
        PUT UNFORMATTED  /* Linha 6   */
            fn-texto(INPUT i-col +  715, INPUT i-lin +  885, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  725, INPUT i-lin +  785, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1035, INPUT i-lin +  785, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED  /* Linha 6   */
            fn-texto(INPUT i-col + 1275, INPUT i-lin +  885, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1285, INPUT i-lin +  785, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1595, INPUT i-lin +  785, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
        

        PUT UNFORMATTED  /* Linha 6   */
            fn-texto(INPUT i-col + 1835, INPUT i-lin +  885, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1845, INPUT i-lin +  785, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 2155, INPUT i-lin +  785, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

        PUT UNFORMATTED  /* Linha 7   */
            fn-texto(INPUT i-col +  145, INPUT i-lin + 1035, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  155, INPUT i-lin +  935, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col +  465, INPUT i-lin +  935, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
    
        PUT UNFORMATTED  /* Linha 7   */
            fn-texto(INPUT i-col +  715, INPUT i-lin + 1035, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  725, INPUT i-lin +  935, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1035, INPUT i-lin +  935, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED  /* Linha 7   */
            fn-texto(INPUT i-col + 1275, INPUT i-lin + 1035, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1285, INPUT i-lin +  935, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1595, INPUT i-lin +  935, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
        

        PUT UNFORMATTED  /* Linha 7   */
            fn-texto(INPUT i-col + 1835, INPUT i-lin + 1035, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1845, INPUT i-lin +  935, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 2155, INPUT i-lin +  935, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

        PUT UNFORMATTED  /* Linha 8   */
            fn-texto(INPUT i-col +  145, INPUT i-lin + 1185, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  155, INPUT i-lin + 1085, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col +  465, INPUT i-lin + 1085, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
    
        PUT UNFORMATTED  /* Linha 8   */
            fn-texto(INPUT i-col +  715, INPUT i-lin + 1185, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  725, INPUT i-lin + 1085, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1035, INPUT i-lin + 1085, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED  /* Linha 8   */
            fn-texto(INPUT i-col + 1275, INPUT i-lin + 1185, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1285, INPUT i-lin + 1085, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1595, INPUT i-lin + 1085, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
        

        PUT UNFORMATTED  /* Linha 8   */
            fn-texto(INPUT i-col + 1835, INPUT i-lin + 1185, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1845, INPUT i-lin + 1085, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 2155, INPUT i-lin + 1085, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

        PUT UNFORMATTED  /* Linha 9   */
            fn-texto(INPUT i-col +  145, INPUT i-lin + 1337, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  155, INPUT i-lin + 1237, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col +  465, INPUT i-lin + 1237, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
    
        PUT UNFORMATTED  /* Linha 9   */
            fn-texto(INPUT i-col +  715, INPUT i-lin + 1337, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  725, INPUT i-lin + 1237, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1035, INPUT i-lin + 1237, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED  /* Linha 9   */
            fn-texto(INPUT i-col + 1275, INPUT i-lin + 1337, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1285, INPUT i-lin + 1237, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1595, INPUT i-lin + 1237, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
        

        PUT UNFORMATTED  /* Linha 9   */
            fn-texto(INPUT i-col + 1835, INPUT i-lin + 1337, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1845, INPUT i-lin + 1237, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 2155, INPUT i-lin + 1237, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

        PUT UNFORMATTED  /* Linha 10  */
            fn-texto(INPUT i-col +  145, INPUT i-lin + 1490, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  155, INPUT i-lin + 1390, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col +  465, INPUT i-lin + 1390, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
    
        PUT UNFORMATTED  /* Linha 10  */
            fn-texto(INPUT i-col +  715, INPUT i-lin + 1490, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  725, INPUT i-lin + 1390, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1035, INPUT i-lin + 1390, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED  /* Linha 10  */
            fn-texto(INPUT i-col + 1275, INPUT i-lin + 1490, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1285, INPUT i-lin + 1390, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1595, INPUT i-lin + 1390, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
        

        PUT UNFORMATTED  /* Linha 10  */
            fn-texto(INPUT i-col + 1835, INPUT i-lin + 1490, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1845, INPUT i-lin + 1390, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 2155, INPUT i-lin + 1390, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

        PUT UNFORMATTED  /* Linha 11  */
            fn-texto(INPUT i-col +  145, INPUT i-lin + 1632, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  155, INPUT i-lin + 1532, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col +  465, INPUT i-lin + 1532, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
    
        PUT UNFORMATTED  /* Linha 11  */
            fn-texto(INPUT i-col +  715, INPUT i-lin + 1632, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  725, INPUT i-lin + 1532, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1035, INPUT i-lin + 1532, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED  /* Linha 11  */
            fn-texto(INPUT i-col + 1275, INPUT i-lin + 1632, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1285, INPUT i-lin + 1532, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1595, INPUT i-lin + 1532, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
        

        PUT UNFORMATTED  /* Linha 11  */
            fn-texto(INPUT i-col + 1835, INPUT i-lin + 1632, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1845, INPUT i-lin + 1532, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 2155, INPUT i-lin + 1532, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

        PUT UNFORMATTED  /* Linha 12  */
            fn-texto(INPUT i-col +  145, INPUT i-lin + 1782, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  155, INPUT i-lin + 1682, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col +  465, INPUT i-lin + 1682, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
    
        PUT UNFORMATTED  /* Linha 12  */
            fn-texto(INPUT i-col +  715, INPUT i-lin + 1782, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  725, INPUT i-lin + 1682, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1035, INPUT i-lin + 1682, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED  /* Linha 12  */
            fn-texto(INPUT i-col + 1275, INPUT i-lin + 1782, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1285, INPUT i-lin + 1682, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1595, INPUT i-lin + 1682, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
        

        PUT UNFORMATTED  /* Linha 12  */
            fn-texto(INPUT i-col + 1835, INPUT i-lin + 1782, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1845, INPUT i-lin + 1682, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 2155, INPUT i-lin + 1682, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

        PUT UNFORMATTED  /* Linha 13  */
            fn-texto(INPUT i-col +  145, INPUT i-lin + 1934, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  155, INPUT i-lin + 1834, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col +  465, INPUT i-lin + 1834, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
    
        PUT UNFORMATTED  /* Linha 13  */
            fn-texto(INPUT i-col +  715, INPUT i-lin + 1934, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  725, INPUT i-lin + 1834, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1035, INPUT i-lin + 1834, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED  /* Linha 13  */
            fn-texto(INPUT i-col + 1275, INPUT i-lin + 1934, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1285, INPUT i-lin + 1834, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1595, INPUT i-lin + 1834, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
        

        PUT UNFORMATTED  /* Linha 13  */
            fn-texto(INPUT i-col + 1835, INPUT i-lin + 1934, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1845, INPUT i-lin + 1834, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 2155, INPUT i-lin + 1834, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

        PUT UNFORMATTED  /* Linha 14  */
            fn-texto(INPUT i-col +  145, INPUT i-lin + 2082, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  155, INPUT i-lin + 1982, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col +  465, INPUT i-lin + 1982, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
    
        PUT UNFORMATTED  /* Linha 14  */
            fn-texto(INPUT i-col +  715, INPUT i-lin + 2082, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  725, INPUT i-lin + 1982, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1035, INPUT i-lin + 1982, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED  /* Linha 14  */
            fn-texto(INPUT i-col + 1275, INPUT i-lin + 2082, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1285, INPUT i-lin + 1982, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1595, INPUT i-lin + 1982, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
        

        PUT UNFORMATTED  /* Linha 14  */
            fn-texto(INPUT i-col + 1835, INPUT i-lin + 2082, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1845, INPUT i-lin + 1982, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 2155, INPUT i-lin + 1982, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED  /* Linha 15  */
            fn-texto(INPUT i-col +  145, INPUT i-lin + 2237, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  155, INPUT i-lin + 2137, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col +  465, INPUT i-lin + 2137, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
    
        PUT UNFORMATTED  /* Linha 15  */
            fn-texto(INPUT i-col +  715, INPUT i-lin + 2237, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  725, INPUT i-lin + 2137, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1035, INPUT i-lin + 2137, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED  /* Linha 15  */
            fn-texto(INPUT i-col + 1275, INPUT i-lin + 2237, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1285, INPUT i-lin + 2137, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1595, INPUT i-lin + 2137, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
        

        PUT UNFORMATTED  /* Linha 15  */
            fn-texto(INPUT i-col + 1835, INPUT i-lin + 2237, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1845, INPUT i-lin + 2137, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 2155, INPUT i-lin + 2137, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

        PUT UNFORMATTED  /* Linha 16  */
            fn-texto(INPUT i-col +  145, INPUT i-lin + 2382, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  155, INPUT i-lin + 2282, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col +  465, INPUT i-lin + 2282, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
    
        PUT UNFORMATTED  /* Linha 16  */
            fn-texto(INPUT i-col +  715, INPUT i-lin + 2382, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  725, INPUT i-lin + 2282, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1035, INPUT i-lin + 2282, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED  /* Linha 16  */
            fn-texto(INPUT i-col + 1275, INPUT i-lin + 2382, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1285, INPUT i-lin + 2282, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1595, INPUT i-lin + 2282, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
        

        PUT UNFORMATTED  /* Linha 16  */
            fn-texto(INPUT i-col + 1835, INPUT i-lin + 2382, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1845, INPUT i-lin + 2282, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 2155, INPUT i-lin + 2282, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

        PUT UNFORMATTED  /* Linha 17  */
            fn-texto(INPUT i-col +  145, INPUT i-lin + 2536, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  155, INPUT i-lin + 2436, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col +  465, INPUT i-lin + 2436, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
    
        PUT UNFORMATTED  /* Linha 17  */
            fn-texto(INPUT i-col +  715, INPUT i-lin + 2536, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  725, INPUT i-lin + 2436, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1035, INPUT i-lin + 2436, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED  /* Linha 17  */
            fn-texto(INPUT i-col + 1275, INPUT i-lin + 2536, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1285, INPUT i-lin + 2436, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1595, INPUT i-lin + 2436, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
        

        PUT UNFORMATTED  /* Linha 17  */
            fn-texto(INPUT i-col + 1835, INPUT i-lin + 2536, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1845, INPUT i-lin + 2436, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 2155, INPUT i-lin + 2436, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

        PUT UNFORMATTED  /* Linha 18  */
            fn-texto(INPUT i-col +  145, INPUT i-lin + 2687, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  155, INPUT i-lin + 2587, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col +  465, INPUT i-lin + 2587, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
    
        PUT UNFORMATTED  /* Linha 18  */
            fn-texto(INPUT i-col +  715, INPUT i-lin + 2687, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  725, INPUT i-lin + 2587, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1035, INPUT i-lin + 2587, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED  /* Linha 18  */
            fn-texto(INPUT i-col + 1275, INPUT i-lin + 2687, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1285, INPUT i-lin + 2587, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1595, INPUT i-lin + 2587, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
        

        PUT UNFORMATTED  /* Linha 18  */
            fn-texto(INPUT i-col + 1835, INPUT i-lin + 2687, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1845, INPUT i-lin + 2587, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 2155, INPUT i-lin + 2587, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

        PUT UNFORMATTED  /* Linha 19  */
            fn-texto(INPUT i-col +  145, INPUT i-lin + 2837, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  155, INPUT i-lin + 2737, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col +  465, INPUT i-lin + 2737, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
    
        PUT UNFORMATTED  /* Linha 19  */
            fn-texto(INPUT i-col +  715, INPUT i-lin + 2837, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  725, INPUT i-lin + 2737, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1035, INPUT i-lin + 2737, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED  /* Linha 19  */
            fn-texto(INPUT i-col + 1275, INPUT i-lin + 2837, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1285, INPUT i-lin + 2737, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1595, INPUT i-lin + 2737, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").
        

        PUT UNFORMATTED  /* Linha 19  */
            fn-texto(INPUT i-col + 1835, INPUT i-lin + 2837, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1845, INPUT i-lin + 2737, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 2155, INPUT i-lin + 2737, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

        PUT UNFORMATTED  /* Linha 20  */
            fn-texto(INPUT i-col +  145, INPUT i-lin + 2987, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  155, INPUT i-lin + 2887, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col +  465, INPUT i-lin + 2887, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

        PUT UNFORMATTED  /* Linha 20  */
            fn-texto(INPUT i-col +  715, INPUT i-lin + 2987, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col +  725, INPUT i-lin + 2887, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1035, INPUT i-lin + 2887, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED  /* Linha 20  */
            fn-texto(INPUT i-col + 1275, INPUT i-lin + 2987, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1285, INPUT i-lin + 2887, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 1595, INPUT i-lin + 2887, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED  /* Linha 20  */
            fn-texto(INPUT i-col + 1835, INPUT i-lin + 2987, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "V")
            fn-texto(INPUT i-col + 1845, INPUT i-lin + 2887, INPUT SUBSTR(ITEM.desc-item,1,20),                  INPUT 16602, INPUT 7, INPUT 2, INPUT 0, INPUT 1, INPUT "H")
            fn-texto(INPUT i-col + 2155, INPUT i-lin + 2887, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").



/*----------------------------------------------------------------------------------------------------------------------------------------*/

        PUT UNFORMATTED  /* Linha 1   */
            fn-code25 (INPUT i-col +  155, input i-lin + 40,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
    
        PUT UNFORMATTED  /* Linha 1   */
            fn-code25 (INPUT i-col +  725, input i-lin + 40,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).

        PUT UNFORMATTED  /* Linha 1   */
            fn-code25 (INPUT i-col + 1285, input i-lin + 40,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).

        PUT UNFORMATTED  /* Linha 1   */
            fn-code25 (INPUT i-col + 1845, input i-lin + 40,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).

        PUT UNFORMATTED  /* Linha 2   */
            fn-code25 (INPUT i-col +  155, input i-lin + 195,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
    
        PUT UNFORMATTED  /* Linha 2   */
            fn-code25 (INPUT i-col +  725, input i-lin + 195,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).

        PUT UNFORMATTED  /* Linha 2   */
            fn-code25 (INPUT i-col + 1285, input i-lin + 195,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 2  */
            fn-code25 (INPUT i-col + 1845, input i-lin + 195,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 3   */
            fn-code25 (INPUT i-col +  155, input i-lin + 345,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 3   */
            fn-code25 (INPUT i-col +  725, input i-lin + 345,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 3   */
            fn-code25 (INPUT i-col + 1285, input i-lin + 345,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 3  */
            fn-code25 (INPUT i-col + 1845, input i-lin + 345,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 4   */
            fn-code25 (INPUT i-col +  155, input i-lin + 495,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 4   */
            fn-code25 (INPUT i-col +  725, input i-lin + 495,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 4   */
            fn-code25 (INPUT i-col + 1285, input i-lin + 495,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 4  */
            fn-code25 (INPUT i-col + 1845, input i-lin + 495,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 5   */
            fn-code25 (INPUT i-col +  155, input i-lin + 647,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 5   */
            fn-code25 (INPUT i-col +  725, input i-lin + 647,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 5   */
            fn-code25 (INPUT i-col + 1285, input i-lin + 647,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 5  */
            fn-code25 (INPUT i-col + 1845, input i-lin + 647,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 6   */
            fn-code25 (INPUT i-col +  155, input i-lin + 795,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 6   */
            fn-code25 (INPUT i-col +  725, input i-lin + 795,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 6   */
            fn-code25 (INPUT i-col + 1285, input i-lin + 795,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 6  */
            fn-code25 (INPUT i-col + 1845, input i-lin + 795,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 7   */
            fn-code25 (INPUT i-col +  155, input i-lin + 945,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 7   */
            fn-code25 (INPUT i-col +  725, input i-lin + 945,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 7   */
            fn-code25 (INPUT i-col + 1285, input i-lin + 945,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 7  */
            fn-code25 (INPUT i-col + 1845, input i-lin + 945,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 8   */
            fn-code25 (INPUT i-col +  155, input i-lin + 1095,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 8   */
            fn-code25 (INPUT i-col +  725, input i-lin + 1095,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 8   */
            fn-code25 (INPUT i-col + 1285, input i-lin + 1095,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 8  */
            fn-code25 (INPUT i-col + 1845, input i-lin + 1095,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 9   */
            fn-code25 (INPUT i-col +  155, input i-lin + 1247,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 9   */
            fn-code25 (INPUT i-col +  725, input i-lin + 1247,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 9   */
            fn-code25 (INPUT i-col + 1285, input i-lin + 1247,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 9  */
            fn-code25 (INPUT i-col + 1845, input i-lin + 1247,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 10  */
            fn-code25 (INPUT i-col +  155, input i-lin + 1400,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 10  */
            fn-code25 (INPUT i-col +  725, input i-lin + 1400,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 10  */
            fn-code25 (INPUT i-col + 1285, input i-lin + 1400,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 10 */
            fn-code25 (INPUT i-col + 1845, input i-lin + 1400,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 11  */
            fn-code25 (INPUT i-col +  155, input i-lin + 1542,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 11  */
            fn-code25 (INPUT i-col +  725, input i-lin + 1542,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 11  */
            fn-code25 (INPUT i-col + 1285, input i-lin + 1542,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 11 */
            fn-code25 (INPUT i-col + 1845, input i-lin + 1542,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 12  */
            fn-code25 (INPUT i-col +  155, input i-lin + 1692,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 12  */
            fn-code25 (INPUT i-col +  725, input i-lin + 1692,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 12  */
            fn-code25 (INPUT i-col + 1285, input i-lin + 1692,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 12 */
            fn-code25 (INPUT i-col + 1845, input i-lin + 1692,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 13  */
            fn-code25 (INPUT i-col +  155, input i-lin + 1844,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 13  */
            fn-code25 (INPUT i-col +  725, input i-lin + 1844,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 13  */
            fn-code25 (INPUT i-col + 1285, input i-lin + 1844,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 13 */
            fn-code25 (INPUT i-col + 1845, input i-lin + 1844,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 14  */
            fn-code25 (INPUT i-col +  155, input i-lin + 1992,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 14  */
            fn-code25 (INPUT i-col +  725, input i-lin + 1992,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 14  */
            fn-code25 (INPUT i-col + 1285, input i-lin + 1992,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 14 */
            fn-code25 (INPUT i-col + 1845, input i-lin + 1992,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 15  */
            fn-code25 (INPUT i-col +  155, input i-lin + 2147,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 15  */
            fn-code25 (INPUT i-col +  725, input i-lin + 2147,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 15  */
            fn-code25 (INPUT i-col + 1285, input i-lin + 2147,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 15 */
            fn-code25 (INPUT i-col + 1845, input i-lin + 2147,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 16  */
            fn-code25 (INPUT i-col +  155, input i-lin + 2292,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 16  */
            fn-code25 (INPUT i-col +  725, input i-lin + 2292,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 16  */
            fn-code25 (INPUT i-col + 1285, input i-lin + 2292,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 16 */
            fn-code25 (INPUT i-col + 1845, input i-lin + 2292,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 17  */
            fn-code25 (INPUT i-col +  155, input i-lin + 2446,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 17  */
            fn-code25 (INPUT i-col +  725, input i-lin + 2446,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 17  */
            fn-code25 (INPUT i-col + 1285, input i-lin + 2446,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 17 */
            fn-code25 (INPUT i-col + 1845, input i-lin + 2446,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 18  */
            fn-code25 (INPUT i-col +  155, input i-lin + 2597,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 18  */
            fn-code25 (INPUT i-col +  725, input i-lin + 2597,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 18  */
            fn-code25 (INPUT i-col + 1285, input i-lin + 2597,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 18 */
            fn-code25 (INPUT i-col + 1845, input i-lin + 2597,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 19  */
            fn-code25 (INPUT i-col +  155, input i-lin + 2747,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 19  */
            fn-code25 (INPUT i-col +  725, input i-lin + 2747,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 19  */
            fn-code25 (INPUT i-col + 1285, input i-lin + 2747,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 19 */
            fn-code25 (INPUT i-col + 1845, input i-lin + 2747,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 20  */
            fn-code25 (INPUT i-col +  155, input i-lin + 2897,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 20  */
            fn-code25 (INPUT i-col +  725, input i-lin + 2897,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 20  */
            fn-code25 (INPUT i-col + 1285, input i-lin + 2897,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).
        PUT UNFORMATTED  /* Linha 20 */
            fn-code25 (INPUT i-col + 1845, input i-lin + 2897,
                       INPUT STRING(i-num-bar,"9999999999"),
                       INPUT "H",              
                       INPUT 1.8,
                       INPUT 4.4).



OUTPUT CLOSE.
