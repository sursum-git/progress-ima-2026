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
/* OUTPUT TO PRINTER.    */
       /* Parametros   1= Posi‡Æo Inicial X
                    2= Posi‡Æo Inicial Y
                    3= Posicao Final   X
                    4= Posicao Final   Y
                    5= Espessura */
        PUT UNFORMATTED /*  Linha 1 */
                  /*      Xi        Yi        Xf          Yf     */
                  /* Vert.Ini   Horiz.Ini  Vert.Fin   Horiz.Fin  */
        fn-retangulo(INPUT  100, INPUT   0, INPUT  598, INPUT 135, INPUT 3)
        fn-retangulo(INPUT  655, INPUT   0, INPUT 1158, INPUT 135, INPUT 3)
        fn-retangulo(INPUT 1215, INPUT   0, INPUT 1718, INPUT 135, INPUT 3)
        fn-retangulo(INPUT 1785, INPUT   0, INPUT 2283, INPUT 135, INPUT 3).

        PUT UNFORMATTED   /* Linha 2     Yi Variou  150 */
        fn-retangulo(INPUT  100, INPUT 145, INPUT  598, INPUT 280, INPUT 3)
        fn-retangulo(INPUT  655, INPUT 145, INPUT 1158, INPUT 280, INPUT 3)
        fn-retangulo(INPUT 1215, INPUT 145, INPUT 1718, INPUT 280, INPUT 3)
        fn-retangulo(INPUT 1785, INPUT 145, INPUT 2283, INPUT 280, INPUT 3).

        PUT UNFORMATTED  /* Linha 3  Yi Variou 146 */
        fn-retangulo(INPUT  100, INPUT 290, INPUT  598, INPUT 425, INPUT 3)
        fn-retangulo(INPUT  655, INPUT 290, INPUT 1158, INPUT 425, INPUT 3)
        fn-retangulo(INPUT 1215, INPUT 290, INPUT 1718, INPUT 425, INPUT 3)
        fn-retangulo(INPUT 1785, INPUT 290, INPUT 2283, INPUT 425, INPUT 3).

        PUT UNFORMATTED  /* Linha 4  Yi Variou 146 */
        fn-retangulo(INPUT  100, INPUT 435, INPUT  598, INPUT 570, INPUT 3)
        fn-retangulo(INPUT  655, INPUT 435, INPUT 1158, INPUT 570, INPUT 3)
        fn-retangulo(INPUT 1215, INPUT 435, INPUT 1718, INPUT 570, INPUT 3)
        fn-retangulo(INPUT 1785, INPUT 435, INPUT 2283, INPUT 570, INPUT 3).

        PUT UNFORMATTED  /* Linha 5  Yi Variou 146 */
        fn-retangulo(INPUT  100, INPUT 580, INPUT  598, INPUT 715, INPUT 3)
        fn-retangulo(INPUT  655, INPUT 580, INPUT 1158, INPUT 715, INPUT 3)
        fn-retangulo(INPUT 1215, INPUT 580, INPUT 1718, INPUT 715, INPUT 3)
        fn-retangulo(INPUT 1785, INPUT 580, INPUT 2283, INPUT 715, INPUT 3).

/*      PUT UNFORMATTED  /* Linha 6  Yi Variou 146 */
        fn-retangulo(INPUT  100, INPUT 734, INPUT  610, INPUT 874, INPUT 3)
        fn-retangulo(INPUT  655, INPUT 734, INPUT 1170, INPUT 874, INPUT 3)
        fn-retangulo(INPUT 1215, INPUT 734, INPUT 1730, INPUT 874, INPUT 3)
        fn-retangulo(INPUT 1785, INPUT 734, INPUT 2295, INPUT 874, INPUT 3).

        PUT UNFORMATTED  /* Linha 7  Yi Variou 146 */
        fn-retangulo(INPUT  100, INPUT 894, INPUT  610, INPUT 1034, INPUT 3)
        fn-retangulo(INPUT  655, INPUT 894, INPUT 1170, INPUT 1034, INPUT 3)
        fn-retangulo(INPUT 1215, INPUT 894, INPUT 1730, INPUT 1034, INPUT 3)
        fn-retangulo(INPUT 1785, INPUT 894, INPUT 2295, INPUT 1034, INPUT 3).

        PUT UNFORMATTED  /* Linha 8  Yi Variou 146 */
        fn-retangulo(INPUT  100, INPUT 1040, INPUT  610, INPUT 1180, INPUT 3)
        fn-retangulo(INPUT  655, INPUT 1040, INPUT 1170, INPUT 1180, INPUT 3)
        fn-retangulo(INPUT 1215, INPUT 1040, INPUT 1730, INPUT 1180, INPUT 3)
        fn-retangulo(INPUT 1785, INPUT 1040, INPUT 2295, INPUT 1180, INPUT 3).
        
        PUT UNFORMATTED  /* Linha 9  Yi Variou 146 */
        fn-retangulo(INPUT  100, INPUT 1190, INPUT  610, INPUT 1330, INPUT 3)
        fn-retangulo(INPUT  655, INPUT 1190, INPUT 1170, INPUT 1330, INPUT 3)
        fn-retangulo(INPUT 1215, INPUT 1190, INPUT 1730, INPUT 1330, INPUT 3)
        fn-retangulo(INPUT 1785, INPUT 1190, INPUT 2295, INPUT 1300, INPUT 3).
        
        PUT UNFORMATTED  /* Linha 10 Yi Variou 146 */
        fn-retangulo(INPUT  100, INPUT 1340, INPUT  610, INPUT 1480, INPUT 3)
        fn-retangulo(INPUT  655, INPUT 1340, INPUT 1170, INPUT 1480, INPUT 3)
        fn-retangulo(INPUT 1215, INPUT 1340, INPUT 1730, INPUT 1480, INPUT 3)
        fn-retangulo(INPUT 1785, INPUT 1340, INPUT 2295, INPUT 1480, INPUT 3).
        
        PUT UNFORMATTED  /* Linha 11 Yi Variou 146 */
        fn-retangulo(INPUT  100, INPUT 1464, INPUT  610, INPUT 1604, INPUT 3)
        fn-retangulo(INPUT  655, INPUT 1464, INPUT 1170, INPUT 1604, INPUT 3)
        fn-retangulo(INPUT 1215, INPUT 1464, INPUT 1730, INPUT 1604, INPUT 3)
        fn-retangulo(INPUT 1785, INPUT 1464, INPUT 2295, INPUT 1604, INPUT 3).
        
        PUT UNFORMATTED  /* Linha 12 Yi Variou 146 */
        fn-retangulo(INPUT  100, INPUT 1610, INPUT  610, INPUT 1750, INPUT 3)
        fn-retangulo(INPUT  655, INPUT 1610, INPUT 1170, INPUT 1750, INPUT 3)
        fn-retangulo(INPUT 1215, INPUT 1610, INPUT 1730, INPUT 1750, INPUT 3)
        fn-retangulo(INPUT 1785, INPUT 1610, INPUT 2295, INPUT 1750, INPUT 3).
        
        PUT UNFORMATTED  /* Linha 13 Yi Variou 146 */
        fn-retangulo(INPUT  100, INPUT 1756, INPUT  610, INPUT 1896, INPUT 3)
        fn-retangulo(INPUT  655, INPUT 1756, INPUT 1170, INPUT 1896, INPUT 3)
        fn-retangulo(INPUT 1215, INPUT 1756, INPUT 1730, INPUT 1896, INPUT 3)
        fn-retangulo(INPUT 1785, INPUT 1756, INPUT 2295, INPUT 1896, INPUT 3).
        
        PUT UNFORMATTED  /* Linha 14 Yi Variou 146 */
        fn-retangulo(INPUT  100, INPUT 1902, INPUT  610, INPUT 2042, INPUT 3)
        fn-retangulo(INPUT  655, INPUT 1902, INPUT 1170, INPUT 2042, INPUT 3)
        fn-retangulo(INPUT 1215, INPUT 1902, INPUT 1730, INPUT 2042, INPUT 3)
        fn-retangulo(INPUT 1785, INPUT 1902, INPUT 2295, INPUT 2042, INPUT 3).
        
        PUT UNFORMATTED  /* Linha 15 Yi Variou 146 */
        fn-retangulo(INPUT  100, INPUT 2048, INPUT  610, INPUT 2188, INPUT 3)
        fn-retangulo(INPUT  655, INPUT 2048, INPUT 1170, INPUT 2188, INPUT 3)
        fn-retangulo(INPUT 1215, INPUT 2048, INPUT 1730, INPUT 2188, INPUT 3)
        fn-retangulo(INPUT 1785, INPUT 2048, INPUT 2295, INPUT 2188, INPUT 3).
        
        PUT UNFORMATTED  /* Linha 16 Yi Variou 146 */
        fn-retangulo(INPUT  100, INPUT 2194, INPUT  610, INPUT 2334, INPUT 3)
        fn-retangulo(INPUT  655, INPUT 2194, INPUT 1170, INPUT 2334, INPUT 3)
        fn-retangulo(INPUT 1215, INPUT 2194, INPUT 1730, INPUT 2334, INPUT 3)
        fn-retangulo(INPUT 1785, INPUT 2194, INPUT 2295, INPUT 2334, INPUT 3).
        
        PUT UNFORMATTED  /* Linha 17 Yi Variou 146 */
        fn-retangulo(INPUT  100, INPUT 2340, INPUT  610, INPUT 2480, INPUT 3)
        fn-retangulo(INPUT  655, INPUT 2340, INPUT 1170, INPUT 2480, INPUT 3)
        fn-retangulo(INPUT 1215, INPUT 2340, INPUT 1730, INPUT 2480, INPUT 3)
        fn-retangulo(INPUT 1785, INPUT 2340, INPUT 2295, INPUT 2480, INPUT 3).
        
        PUT UNFORMATTED  /* Linha 18 Yi Variou 146 */
        fn-retangulo(INPUT  100, INPUT 2486, INPUT  610, INPUT 2626, INPUT 3)
        fn-retangulo(INPUT  655, INPUT 2486, INPUT 1170, INPUT 2626, INPUT 3)
        fn-retangulo(INPUT 1215, INPUT 2486, INPUT 1730, INPUT 2626, INPUT 3)
        fn-retangulo(INPUT 1785, INPUT 2486, INPUT 2295, INPUT 2626, INPUT 3).
        
        PUT UNFORMATTED  /* Linha 19 Yi Variou 146 */
        fn-retangulo(INPUT  100, INPUT 2632, INPUT  610, INPUT 2772, INPUT 3)
        fn-retangulo(INPUT  655, INPUT 2632, INPUT 1170, INPUT 2772, INPUT 3)
        fn-retangulo(INPUT 1215, INPUT 2632, INPUT 1730, INPUT 2772, INPUT 3)
        fn-retangulo(INPUT 1785, INPUT 2632, INPUT 2295, INPUT 2772, INPUT 3).
        
        PUT UNFORMATTED  /* Linha 20 Yi Variou 146 */
        fn-retangulo(INPUT  100, INPUT 2778, INPUT  610, INPUT 2918, INPUT 3)
        fn-retangulo(INPUT  655, INPUT 2778, INPUT 1170, INPUT 2918, INPUT 3)
        fn-retangulo(INPUT 1215, INPUT 2778, INPUT 1730, INPUT 2918, INPUT 3)
        fn-retangulo(INPUT 1785, INPUT 2778, INPUT 2295, INPUT 2918, INPUT 3).
*/        
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
        fn-texto(INPUT i-col + 110, INPUT i-lin +  35, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 300, INPUT i-lin +  35, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"), INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 540, INPUT i-lin +  35, INPUT TRIM(c-corte),                                INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col +  665, INPUT i-lin +  35, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  860, INPUT i-lin +  35, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1100, INPUT i-lin +  35, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1225, INPUT i-lin +  35, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1420, INPUT i-lin +  35, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1660, INPUT i-lin +  35, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1795, INPUT i-lin +  35, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1980, INPUT i-lin +  35, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 2220, INPUT i-lin +  35, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED    /* Linha 2  Y (Linha) variou 150 */
        fn-texto(INPUT i-col + 110, INPUT i-lin +  180, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 300, INPUT i-lin +  180, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 540, INPUT i-lin +  180, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col +  665, INPUT i-lin + 180, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  860, INPUT i-lin + 180, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1100, INPUT i-lin + 180, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1225, INPUT i-lin + 180, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1420, INPUT i-lin + 180, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1660, INPUT i-lin + 180, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1795, INPUT i-lin + 180, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1980, INPUT i-lin + 180, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 2220, INPUT i-lin + 180, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

        PUT UNFORMATTED    /* Linha 3  Y (Linha) variou 150 */
        fn-texto(INPUT i-col + 110, INPUT i-lin +  325, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 300, INPUT i-lin +  325, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 540, INPUT i-lin +  325, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col +  665, INPUT i-lin + 325, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  860, INPUT i-lin + 325, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1100, INPUT i-lin + 325, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1225, INPUT i-lin + 325, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1420, INPUT i-lin + 325, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1660, INPUT i-lin + 325, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1795, INPUT i-lin + 325, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1980, INPUT i-lin + 325, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 2220, INPUT i-lin + 325, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H").


        PUT UNFORMATTED    /* Linha 4  Y (Linha) variou 150 */
        fn-texto(INPUT i-col + 110, INPUT i-lin +  470, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 300, INPUT i-lin +  470, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 540, INPUT i-lin +  470, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col +  665, INPUT i-lin + 470, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  860, INPUT i-lin + 470, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1100, INPUT i-lin + 470, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1225, INPUT i-lin + 470, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1420, INPUT i-lin + 470, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1660, INPUT i-lin + 470, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1795, INPUT i-lin + 470, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1980, INPUT i-lin + 470, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 2220, INPUT i-lin + 470, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

        PUT UNFORMATTED    /* Linha 5  Y (Linha) variou 150 */
        fn-texto(INPUT i-col + 110, INPUT i-lin +  615, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 300, INPUT i-lin +  615, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 540, INPUT i-lin +  615, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col +  665, INPUT i-lin + 615, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col +  860, INPUT i-lin + 615, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1100, INPUT i-lin + 615, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1225, INPUT i-lin + 615, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1420, INPUT i-lin + 615, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1660, INPUT i-lin + 615, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")

        fn-texto(INPUT i-col + 1795, INPUT i-lin + 615, INPUT ob-etiqueta.it-codigo,                       INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 1980, INPUT i-lin + 615, INPUT STRING(ob-etiqueta.num-etiqueta, "99999999"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
        fn-texto(INPUT i-col + 2220, INPUT i-lin + 615, INPUT TRIM(c-corte),                               INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H").



/*
*/



    /* Parametros 1= Posi‡Æo do Eixo X
                  2= Posicao do Eixo Y
                  3= Texto
                  4= Sentido
                  5= Altura
                  6= Largura */
        PUT UNFORMATTED  /* Linha 1 */
        fn-code25 (INPUT i-col + 150, input i-lin + 40,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",              
                   INPUT 1.8,
                   INPUT 4.0)


        fn-code25 (INPUT i-col + 710, input i-lin + 40,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",              
                   INPUT 1.8,
                   INPUT 4.0)


       fn-code25 (INPUT i-col + 1270, input i-lin + 40,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",             
                  INPUT 1.8,
                  INPUT 4.0)

       fn-code25 (INPUT i-col + 1835, input i-lin + 40,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",              
                  INPUT 1.8,
                  INPUT 4.0).

        PUT UNFORMATTED  /* Linha 2 */
        fn-code25 (INPUT i-col + 150, input i-lin + 185,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",              
                   INPUT 1.8,
                   INPUT 4.0)


        fn-code25 (INPUT i-col + 710, input i-lin + 185,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",            
                   INPUT 1.8,
                   INPUT 4.0)


       fn-code25 (INPUT i-col + 1270, input i-lin + 185,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",             
                  INPUT 1.8,
                  INPUT 4.0)

       fn-code25 (INPUT i-col + 1835, input i-lin + 185,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",            
                  INPUT 1.8,
                  INPUT 4.0).

        PUT UNFORMATTED  /* Linha 3 */
        fn-code25 (INPUT i-col + 150, input i-lin + 330,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",              
                   INPUT 1.8,
                   INPUT 4.0)


        fn-code25 (INPUT i-col + 710, input i-lin + 330,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",            
                   INPUT 1.8,
                   INPUT 4.0)


       fn-code25 (INPUT i-col + 1270, input i-lin + 330,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",             
                  INPUT 1.8,
                  INPUT 4.0)

       fn-code25 (INPUT i-col + 1835, input i-lin + 330,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",            
                  INPUT 1.8,
                  INPUT 4.0).

        PUT UNFORMATTED  /* Linha 4 */
        fn-code25 (INPUT i-col + 150, input i-lin + 475,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",              
                   INPUT 1.8,
                   INPUT 4.0)


        fn-code25 (INPUT i-col + 710, input i-lin + 475,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",            
                   INPUT 1.8,
                   INPUT 4.0)


       fn-code25 (INPUT i-col + 1270, input i-lin + 475,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",             
                  INPUT 1.8,
                  INPUT 4.0)

       fn-code25 (INPUT i-col + 1835, input i-lin + 475,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",            
                  INPUT 1.8,
                  INPUT 4.0).

        PUT UNFORMATTED  /* Linha 5 */
        fn-code25 (INPUT i-col + 150, input i-lin + 620,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",              
                   INPUT 1.8,
                   INPUT 4.0)


        fn-code25 (INPUT i-col + 710, input i-lin + 620,
                   INPUT STRING(i-num-bar,"9999999999"),
                   INPUT "H",            
                   INPUT 1.8,
                   INPUT 4.0)


       fn-code25 (INPUT i-col + 1270, input i-lin + 620,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",             
                  INPUT 1.8,
                  INPUT 4.0)

       fn-code25 (INPUT i-col + 1835, input i-lin + 620,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",            
                  INPUT 1.8,
                  INPUT 4.0).
OUTPUT CLOSE.
