/****************************************************************************
** Programa : TWAD098 - trigger de Write para a tabela item  - TONINHO
** Data     : Maio 2017
** Objetivo : trigger de Write para a tabela item
** Empresa  : IMA 
** Vers∆o   : 2.04.001
** Alterado : 06/2020 - tadeu - inclusao do tipo do item design e da forma de 
exibiá∆o da referencia para itens do grupo de estoque 60
** Fluxo    : 
*****************************************************************************/

DEFINE PARAMETER BUFFER b-item-new FOR item.
DEFINE PARAMETER BUFFER b-item-old FOR item.  

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEFINE VARIABLE h-registro-old  AS HANDLE     NO-UNDO.
DEFINE VARIABLE h-registro-new  AS HANDLE     NO-UNDO.
DEFINE VARIABLE h-campo-old     AS HANDLE     NO-UNDO.
DEFINE VARIABLE h-campo-new     AS HANDLE     NO-UNDO.
DEFINE VARIABLE c-campos        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i-cont          AS INTEGER    NO-UNDO.
DEFINE VARIABLE hBoItemExt      AS HANDLE      NO-UNDO.
DEFINE VARIABLE iTipo           AS INTEGER     NO-UNDO.
DEFINE VARIABLE iForma          AS INTEGER     NO-UNDO.
IF AVAIL b-item-new THEN DO:

    IF b-item-new.ge-codigo = 60 THEN DO:
       RUN esbo/boItemExt.p PERSISTENT SET hboItemExt.
       RUN setItem IN hBoItemExt(b-item-new.it-codigo).
       IF SUBSTR(b-item-new.it-codigo,3,1) = '5' THEN DO: //liso
          ASSIGN iTipo  = 2
                 iForma = 2.
       END.
       ELSE DO:
         ASSIGN iTipo  = 1
                iForma = 1.
       END.
          
       RUN setTipoDesign    IN hBoItemExt(iTipo).
       RUN setFormaExibicao IN hBoItemExt(iForma).

    END.
    
   BUFFER-COMPARE b-item-new TO b-item-old SAVE RESULT IN c-campos NO-ERROR.
   IF c-campos <> "" THEN DO:
      ASSIGN h-registro-new = BUFFER b-item-new:HANDLE
             h-registro-old = BUFFER b-item-old:HANDLE.
       
      DO i-cont = 1 TO NUM-ENTRIES(c-campos).
         CREATE im-ocorrencias.
         ASSIGN im-ocorrencias.oc-codigo = b-item-new.it-codigo
                im-ocorrencias.usuario   = c-seg-usuario
                im-ocorrencias.dt-trans  = TODAY
                im-ocorrencias.hr-trans  = TIME.

         ASSIGN h-campo-old = h-registro-old:BUFFER-FIELD(ENTRY(i-cont,c-campos))
                h-campo-new = h-registro-new:BUFFER-FIELD(ENTRY(i-cont,c-campos)).

         ASSIGN im-ocorrencias.narrativa = "Alterado o Campo: " + ENTRY(i-cont,c-campos) + CHR(10) + 
                                           "De: " + h-campo-old:STRING-VALUE + CHR(10) +
                                           "Para: " + h-campo-new:STRING-VALUE.
      END.
   END.
END.

/*
TRIGGER PROCEDURE FOR WRITE OF testbf OLD BUFFER oldtestbf.

DEFINE VARIABLE h-registro-old AS HANDLE     NO-UNDO.
DEFINE VARIABLE h-registro-new AS HANDLE     NO-UNDO.
DEFINE VARIABLE h-campo-old AS HANDLE     NO-UNDO.
DEFINE VARIABLE h-campo-new AS HANDLE     NO-UNDO.
DEFINE VARIABLE c-campos   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i-cont AS INTEGER    NO-UNDO.

BUFFER-COMPARE testbf TO oldtestbf SAVE RESULT IN c-campos NO-ERROR.
IF c-campos <> "" THEN DO:
    ASSIGN
        h-registro-old = BUFFER  testbf:HANDLE
        h-registro-new = BUFFER  oldtestbf:HANDLE.
   
    DO i-cont = 1 TO NUM-ENTRIES(c-campos):
        CREATE Monitor.
        ASSIGN
            Monitor.rOldRecid = RECID(oldtestbf)
            Monitor.rNewRecId = RECID(testbf)
            Monitor.cUserId   = USERID("DICTDB")
            Monitor.cDate      = TODAY
            Monitor.cTime     = STRING(TIME,"HH:MM:SS")
            Monitor.cTableName =  h-registro-old:NAME
            Monitor.cFieldName =  ENTRY(i-cont, c-campos)
            h-campo-old = h-registro-old:BUFFER-FIELD(ENTRY(i-cont, c-campos))
            Monitor.cOldStringValue = h-campo-old:STRING-VALUE
            h-campo-new = h-registro-new:BUFFER-FIELD(ENTRY(i-cont, c-campos))
            Monitor.cNewStringValue = h-campo-new:STRING-VALUE.
    END.
END.

*/
