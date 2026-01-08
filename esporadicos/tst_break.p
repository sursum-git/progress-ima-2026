DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.
DEFINE VARIABLE icont1 AS INTEGER     NO-UNDO.
/*FOR each class_item_ref fields(it_codigo cod_refer) BREAK BY class_item_ref.it_codigo by class_item_ref.cod_refer.
    IF FIRST-OF(it_codigo) AND first-of(cod_refer) THEN
       ASSIGN iCont = iCont + 1.

END.*/
OUTPUT TO c:\temp\BREAK1ou2.txt.
FOR EACH ped-venda
    WHERE ped-venda.dt-implant > 01.01.2021
    BREAK BY dt-implant BY cod-emitente.
    IF 
        FIRST-OF(dt-implant) 
       OR  FIRST-OF(cod-emitente)
         THEN DO:
         ASSIGN iCont = iCont + 1.
         DISP dt-implant cod-emitente.
    END.
       

    ASSIGN iCont1 = iCont1 + 1.

END.


DISP iCont iCont1.


OUTPUT CLOSE.
