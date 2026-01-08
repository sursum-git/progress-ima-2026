DEF VAR i-ct-lin AS INT.
DEF VAR i-ct-col AS INT.
DEF VAR i-sair   AS INT.
DEF VAR i-ct     AS INT.
DEF VAR i-lin AS INT EXTENT 6 INIT [00,590,1190,1795,2395,2990].
DEF VAR i-col AS INT EXTENT 3 INIT [0,810,1620].
DEF VAR c-composicao LIKE composi.descricao EXTENT 3.

{esinc/sz-pcl.i}

fn-grava-macro("n:\especificos\Etiqueta\image\logo-etq10.prn"). 
fn-grava-macro("n:\especificos\Etiqueta\image\rlgp11.prn").
fn-grava-macro("n:\especificos\Etiqueta\image\rlgp12.prn"). 
fn-grava-macro("n:\especificos\Etiqueta\image\rlgp13.prn").
fn-grava-macro("n:\especificos\Etiqueta\image\rlgp14.prn"). 
fn-grava-macro("n:\especificos\Etiqueta\image\rlgp15.prn"). 
fn-grava-macro("n:\especificos\Etiqueta\image\rlgp16.prn"). 
fn-grava-macro("n:\especificos\Etiqueta\image\rlgp17.prn"). 
fn-grava-macro("n:\especificos\Etiqueta\image\rlgp18.prn"). 
fn-grava-macro("n:\especificos\Etiqueta\image\rlgp19.prn"). 
fn-grava-macro("n:\especificos\Etiqueta\image\rlgp20.prn"). 
fn-grava-macro("n:\especificos\Etiqueta\image\rlgp21.prn"). 
fn-grava-macro("n:\especificos\Etiqueta\image\rlgp22.prn"). 
fn-grava-macro("n:\especificos\Etiqueta\image\rlgp23.prn"). 

 OUTPUT TO PRINTER.
/* OUTPUT TO c:\temp\bim.prn.  */


   PUT UNFORMATTED 
       "~033&l3A" /* Pagina Oficio */
       "~033&l164F"
       "~033&l128P"
       "~033&l1L"
       "~033&l1M"
       "~033&l1E".
    
   ASSIGN i-ct-lin = 1
          i-ct-col = 0
          i-sair   = 0.

   FOR EACH ob-etiqueta WHERE
            ob-etiqueta.dt-emissao > 11/11/2008 NO-LOCK. 
       IF ob-etiqueta.dt-emissao = ? THEN NEXT.

       ASSIGN i-ct-col = i-ct-col + 1.
       IF i-ct-col > 3 THEN DO.
          ASSIGN i-ct-col = 1
                 i-ct-lin = i-ct-lin + 1.

          IF i-ct-lin > 6 THEN DO.
             ASSIGN i-ct-lin = 1.
             PAGE.
          END.
       END.
       ASSIGN i-sair = i-sair + 1.
       RUN pi-imp-etiqueta (INPUT i-lin[i-ct-lin], INPUT i-col[i-ct-col]). 
       IF i-sair = 18 THEN LEAVE.
   END.
OUTPUT CLOSE.



/*
 *********** P    R    O   C   E   D   I   M   E   N   T   O   S *************
*/

PROCEDURE pi-imp-etiqueta.


 DEF INPUT PARAMETER i-lin AS INT.
 DEF INPUT PARAMETER i-col AS INT.

 DEF VAR i-num-bar AS INT.
 DEF VAR c-corte   AS CHAR.

 ASSIGN i-num-bar = INT(STRING(ob-etiqueta.num-etiqueta) + fn-calc-digito(INPUT STRING(ob-etiqueta.num-etiqueta,"999999999"))).

 FIND ITEM WHERE
      ITEM.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.


 FIND FIRST item-ext WHERE
           item-ext.it-codigo = ITEM.it-codigo NO-LOCK NO-ERROR.

IF AVAIL item-ext THEN DO.
   FIND FIRST composi WHERE
              composi.cod-composi = item-ext.cod-composi NO-LOCK NO-ERROR.

   ASSIGN c-composicao = "".
   IF AVAIL composi THEN DO.
      DO i-ct = 1 TO NUM-ENTRIES(composi.descricao).
         ASSIGN c-composicao[INT(i-ct / 2)] = c-composicao[INT(i-ct / 2)] + ENTRY(i-ct,composi.descricao).
      END.
   END.
END.

IF c-composicao[2] = "" THEN
   ASSIGN c-composicao[2] = "Zero% da Granpoula da Parafuseta Enfezada".


 PUT UNFORMATTED 
     fn-retangulo(input i-col, input i-lin, input i-col + 760, INPUT i-lin + 535, INPUT 3).

 ASSIGN c-corte = IF LOOKUP(SUBSTR(ob-etiqueta.acondic,1,1),"P,R") > 0 
                  THEN SUBSTR(ob-etiqueta.acondic,1,1) + " " + ENTRY(2,ob-etiqueta.acondic," ")
                  ELSE SUBSTR(ob-etiqueta.acondic,1,1).

 ASSIGN c-corte = "R 100".

 PUT UNFORMATTED 
     fn-texto(INPUT i-col +  15, INPUT i-lin +  30, INPUT "TEAR TEXTIL INDUSTRIA E COMERCIO LTDA.",       INPUT 16602, INPUT  7, INPUT 4, INPUT 0, INPUT 1, INPUT "H")
     fn-texto(INPUT i-col + 230, INPUT i-lin +  60, INPUT "Av. General David Sarnoff, 5005D",             INPUT 16602, INPUT  6, INPUT 0, INPUT 0, INPUT 1, INPUT "H")
     fn-texto(INPUT i-col + 230, INPUT i-lin +  85, INPUT "32210-110 / CONTAGEM - MG   BRASIL",           INPUT 16602, INPUT  6, INPUT 0, INPUT 0, INPUT 1, INPUT "H")
     fn-texto(INPUT i-col + 230, INPUT i-lin + 110, INPUT "CNPJ:03.123.987/0002-00 IE:186020807.0187",    INPUT 16602, INPUT  6, INPUT 0, INPUT 0, INPUT 1, INPUT "H")
     fn-imp-macro(INPUT i-col + 5, INPUT i-lin + 28, INPUT 10)  

     fn-linha(INPUT i-col + 00, INPUT i-lin + 120, INPUT  760, INPUT 3, INPUT "H")  

     fn-texto(INPUT i-col + 135, INPUT i-lin + 160, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT  9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
     fn-texto(INPUT i-col + 290, INPUT i-lin + 160, INPUT ob-etiqueta.cod-refer,                        INPUT 16602, INPUT  9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
     fn-texto(INPUT i-col + 470, INPUT i-lin + 160, INPUT STRING(ob-etiqueta.nr-ob),                    INPUT 16602, INPUT  9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
     fn-texto(INPUT i-col + 650, INPUT i-lin + 160, INPUT TRIM(c-corte),                                INPUT 16602, INPUT  9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
     fn-texto(INPUT i-col + 135, INPUT i-lin + 190, INPUT ITEM.desc-item,                               INPUT 16602, INPUT  6, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
     fn-texto(INPUT i-col + 135, INPUT i-lin + 215, INPUT c-composicao[1],                              INPUT 16602, INPUT  6, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
     fn-texto(INPUT i-col + 135, INPUT i-lin + 240, INPUT c-composicao[2],                              INPUT 16602, INPUT  6, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
     fn-texto(INPUT i-col + 135, INPUT i-lin + 273, INPUT "g/m:",                                       INPUT 16602, INPUT  9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
     fn-texto(INPUT i-col + 230, INPUT i-lin + 273, INPUT STRING((item.peso-liquido * 1000),"999.99"),  INPUT 16602, INPUT  9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
     fn-linha(INPUT i-col + 120, INPUT i-lin + 120, INPUT  415, INPUT 3, INPUT "V")  
     fn-texto(INPUT i-col + 230, INPUT i-lin + 525, INPUT STRING(ob-etiqueta.num-etiqueta,"999999999"), INPUT 16602, INPUT 20, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

 PUT UNFORMATTED
     fn-code25 (INPUT i-col + 190, input i-lin + 283,
                INPUT STRING(i-num-bar,"9999999999"),
                INPUT "H",
                INPUT 3.5,
                INPUT 5.0).

   IF AVAIL item-ext THEN DO. /*Escolha da imagem para Imprimir*/
      CASE item-ext.cod-rlgp: 
        WHEN 1 THEN
            PUT UNFORMATTED
                fn-imp-macro(INPUT i-col + 13, INPUT i-lin + 130, INPUT 11) 
                fn-imp-macro(INPUT i-col + 13, INPUT i-lin + 200, INPUT 21)
                fn-imp-macro(INPUT i-col + 13, INPUT i-lin + 290, INPUT 13)
                fn-imp-macro(INPUT i-col +  8, INPUT i-lin + 370, INPUT 14)
                fn-imp-macro(INPUT i-col + 13, INPUT i-lin + 445, INPUT 23).
        WHEN 2 THEN                         
            PUT UNFORMATTED
                fn-imp-macro(INPUT i-col + 13, INPUT i-lin + 130, INPUT 11) 
                fn-imp-macro(INPUT i-col + 13, INPUT i-lin + 200, INPUT 21)
                fn-imp-macro(INPUT i-col + 13, INPUT i-lin + 290, INPUT 13)
                fn-imp-macro(INPUT i-col +  8, INPUT i-lin + 370, INPUT 14)
                fn-imp-macro(INPUT i-col + 13, INPUT i-lin + 445, INPUT 23).
        WHEN 3 THEN 
            PUT UNFORMATTED
                fn-imp-macro(INPUT i-col + 13, INPUT i-lin + 130, INPUT 17) 
                fn-imp-macro(INPUT i-col + 13, INPUT i-lin + 200, INPUT 21)
                fn-imp-macro(INPUT i-col + 13, INPUT i-lin + 290, INPUT 16)
                fn-imp-macro(INPUT i-col +  8, INPUT i-lin + 370, INPUT 14)
                fn-imp-macro(INPUT i-col + 13, INPUT i-lin + 445, INPUT 23).
        WHEN 4 THEN 
            PUT UNFORMATTED
                fn-imp-macro(INPUT i-col + 13, INPUT i-lin + 130, INPUT 22) 
                fn-imp-macro(INPUT i-col + 13, INPUT i-lin + 200, INPUT 21)
                fn-imp-macro(INPUT i-col + 13, INPUT i-lin + 290, INPUT 13)
                fn-imp-macro(INPUT i-col + 13, INPUT i-lin + 370, INPUT 20)
                fn-imp-macro(INPUT i-col + 13, INPUT i-lin + 445, INPUT 23).
     END CASE.
  END.

END PROCEDURE.
