{esinc/sz-pcl.i}

DEFINE VARIABLE i-col AS INTEGER      NO-UNDO.
DEFINE VARIABLE i-lin AS INTEGER      NO-UNDO.
DEFINE VARIABLE i-num-bar AS INTEGER  NO-UNDO.
DEFINE VARIABLE c-corte AS CHARACTER  NO-UNDO.

/* OUTPUT TO c:\temp\t1.prn.    */
 OUTPUT TO PRINTER.   
       /* Parametros   1= Posi‡Æo Inicial X
                    2= Posi‡Æo Inicial Y
                    3= Posicao Final   X
                    4= Posicao Final   Y
                    5= Espessura */
        PUT UNFORMATTED /*  Linha 1 */
                  /*      Xi        Yi        Xf          Yf     */
                  /* Vert.Ini   Horiz.Ini  Vert.Fin   Horiz.Fin  */
        fn-retangulo(INPUT 0 , INPUT   0, INPUT  2310, INPUT 3300, INPUT 3).
        
/*        PUT UNFORMATTED  /* Linha 20 Yi Variou 146 */
        fn-retangulo(INPUT  100, INPUT 2778, INPUT  610, INPUT 2918, INPUT 3)
        fn-retangulo(INPUT  655, INPUT 2778, INPUT 1170, INPUT 2918, INPUT 3)
        fn-retangulo(INPUT 1215, INPUT 2778, INPUT 1730, INPUT 2918, INPUT 3)
        fn-retangulo(INPUT 1785, INPUT 2778, INPUT 2295, INPUT 2918, INPUT 3). */
OUTPUT CLOSE.
