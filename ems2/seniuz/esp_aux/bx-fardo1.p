DEF VAR i-num-fardos AS INT EXTENT 43.
DEF VAR i-ct AS INT.
DEF VAR i-cont AS INT.
DEF VAR de-peso AS DEC.
ASSIGN i-num-fardos[ 1] = 11091164
       i-num-fardos[ 2] = 10091364
       i-num-fardos[ 3] = 10091384
       i-num-fardos[ 4] = 09092172
       i-num-fardos[ 5] = 09092226
       i-num-fardos[ 6] = 09095066
       i-num-fardos[ 7] = 10090614
       i-num-fardos[ 8] = 10090615
       i-num-fardos[ 9] = 10090616
       i-num-fardos[10] = 10090617
       i-num-fardos[11] = 10090632
       i-num-fardos[12] = 10090633
       i-num-fardos[13] = 10090648
       i-num-fardos[14] = 10090649
       i-num-fardos[15] = 10090922
       i-num-fardos[16] = 12090404
       i-num-fardos[17] = 12091827
       i-num-fardos[18] = 12091828
       i-num-fardos[19] = 12091829
       i-num-fardos[20] = 11095206
       i-num-fardos[21] = 11095217
       i-num-fardos[22] = 11091220
       i-num-fardos[23] = 09090642
       i-num-fardos[24] = 09090655
       i-num-fardos[25] = 09090666
       i-num-fardos[26] = 08090177
       i-num-fardos[27] = 09094285
       i-num-fardos[28] = 09094370
       i-num-fardos[29] = 09095769
       i-num-fardos[30] = 09091326
       i-num-fardos[31] = 09091336
       i-num-fardos[32] = 11090313
       i-num-fardos[33] = 05092884
       i-num-fardos[34] = 05094093
       i-num-fardos[35] = 05091865
       i-num-fardos[36] = 05091866
       i-num-fardos[37] = 05091867
       i-num-fardos[38] = 09090510
       i-num-fardos[39] = 09090536
       i-num-fardos[40] = 09090540
       i-num-fardos[41] = 09090541
       i-num-fardos[42] = 01102522
       i-num-fardos[43] = 01100164.

DO i-ct = 1 TO 43:
   FIND mp-fardo WHERE
        mp-fardo.nr-fardo = i-num-fardos[i-ct] SHARE-LOCK NO-ERROR.
   IF AVAIL mp-fardo THEN DO:
      IF mp-fardo.situacao = 3 THEN DO.
         ASSIGN de-peso = de-peso + mp-fardo.peso
                i-cont  = i-cont + 1.
         ASSIGN mp-fardo.situacao = 5.
      END.
   END.
END.
DISP i-cont
     de-peso.
























