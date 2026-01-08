{esapi/extrairTextoEmPartes.i}

 DEFINE INPUT  PARAMETER pTexto   AS LONGCHAR  NO-UNDO.
 DEFINE INPUT  PARAMETER pTamanho AS INTEGER     NO-UNDO.
 DEFINE OUTPUT PARAMETER TABLE FOR ttTexto .
 DEFINE VARIABLE cParte AS CHARACTER   NO-UNDO.

 DEFINE VARIABLE iTamanho AS INTEGER     NO-UNDO.
 DEFINE VARIABLE qtParte  AS INTEGER     NO-UNDO.
 DEFINE VARIABLE icont    AS INTEGER     NO-UNDO.
 DEFINE VARIABLE inicio   AS INTEGER     NO-UNDO.
 DEFINE VARIABLE fim      AS INTEGER     NO-UNDO.

 ASSIGN iTamanho = length(pTexto).
 IF iTamanho > pTamanho THEN
    ASSIGN qtParte  = iTamanho / pTamanho . 
 ELSE 
   ASSIGN qtParte = 1.

/*MESSAGE 'oi' iTamanho SKIP
         pTamanho SKIP
         qtParte
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
REPEAT iCont = 1 TO qtParte:

    ASSIGN inicio =  1 + pTamanho * (iCont - 1)
           fim    = iCont * pTamanho .
    /*MESSAGE 'inicio:' inicio SKIP
            'fim:' fim
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
    CREATE ttTexto.
    ASSIGN ttTexto.ordem = iCont
           ttTexto.texto  = SUBSTR(pTexto,inicio,fim).

END.


