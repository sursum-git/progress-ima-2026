DEFINE VARIABLE dataIni         AS DATE        NO-UNDO.
DEFINE VARIABLE dataFim         AS DATE        NO-UNDO.
DEFINE VARIABLE iDiaIniSemana   AS INTEGER     NO-UNDO.
{esbo\esboger999.i}
PROCEDURE definirDataIni:
    DEFINE INPUT  PARAMETER pData  AS DATE        NO-UNDO.
    ASSIGN dataIni = pData.
END PROCEDURE.

PROCEDURE definirDataFim:
    DEFINE INPUT  PARAMETER pData  AS DATE        NO-UNDO.
    ASSIGN dataFim = pData.
END PROCEDURE.

PROCEDURE limparDados:
EMPTY TEMP-TABLE ttSemana.

END.
PROCEDURE definirInicioSemana:
    DEFINE INPUT  PARAMETER pInicio AS INTEGER     NO-UNDO.
    ASSIGN  iDiaIniSemana = pInicio.
END PROCEDURE.

PROCEDURE calcularSemanas:
    DEFINE VARIABLE iDiaSemana AS INTEGER     NO-UNDO.
    DEFINE VARIABLE nrSemana   AS INTEGER     NO-UNDO.
    DEFINE VARIABLE qtDias     AS INTEGER     NO-UNDO.
    DEFINE VARIABLE i          AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iOrdem     AS INTEGER     NO-UNDO.
    DEFINE VARIABLE dataAtu    AS DATE        NO-UNDO.
     

    ASSIGN iDiaSemana   = WEEKDAY(dataIni)
           qtDias       = dataFim - DataIni + 1.
    ASSIGN dataAtu      = dataIni.
    
    OUTPUT TO c:\temp\LOG_semana.txt.
    PUT  "===============================" SKIP
         'Data Inicial:' dataIni   SKIP
         'Data Final:'   dataFim   SKIP
         'Data Atual:'   dataAtu   SKIP
         'Qt.Dias:'      qtDias    SKIP
         "==============================" SKIP.

    REPEAT i = 1 TO qtDias:
        ASSIGN iDiaSemana = WEEKDAY(dataAtu).
        PUT  "==================================================================================================" SKIP
             'repeat i:' i ' | dataAtu:' dataAtu ' |dia da Semana:' iDiaSemana  SKIP.
        IF i = 1 THEN DO:
           CREATE ttSemana.
           ASSIGN ttSemana.diaInicial = dataIni.
           PUT 'COND1:entrei na condi‡Æo de ser o primeiro registro do repeat e criei a ttSemana' SKIP.

           IF iDiaSemana = iDiaIniSemana - 1 THEN DO:
              PUT 'COND1.1:o primeiro dia ‚ igual ao ultimo dia da semana  e por isto a data final est  sendo preenchida com dia inicial' SKIP.
              ASSIGN ttSemana.diafinal = dataIni.
           END.
              
        END.
        ELSE DO:
           PUT 'COND2:entrei na condi‡Æo de NAO ser o primeiro registro do repeat e criei a ttSemana' SKIP.
           IF iDiaSemana = iDiaIniSemana THEN DO:
              PUT 'COND2.1:entrei na condi‡Æo de dia da semana = primeiro dia da semana | dataAtu:' dataAtu ' iDiaSemana:' iDiaSemana 'iDiaIniSemana:' iDiaIniSemana   SKIP
                  'criei novo registro na ttSemana' SKIP.
              CREATE ttSemana.
              ASSIGN ttSemana.diaInicial = dataAtu.
           END.
           IF iDiaSemana = iDiaIniSemana - 1 THEN DO:
              PUT 'COND2.2:entrei na condi‡Æo de dia da semana = primeiro dia da semana - 1  | dataAtu:' dataAtu ' DiaSemana:' iDiaSemana 'iDiaIniSemana:' iDiaIniSemana   SKIP
                  'data final preenchida na ttSemana corrente' SKIP.
               ASSIGN ttSemana.diaFinal = dataAtu.
           END.
           IF i = qtDias THEN DO:
               PUT "COND2.3: Ultimo dia - vou preencher a data final da ultima semana ".
              ASSIGN ttSemana.diaFinal = dataAtu.
           END.
        END.
        ASSIGN dataAtu = dataAtu  + 1.
    END.
    OUTPUT CLOSE.
    ASSIGN i = 1.
    FOR EACH ttSemana:
        ASSIGN ttSemana.ordem = i
               i = i + 1.
        

    END.
END PROCEDURE.

PROCEDURE retornarRegistros:
    DEFINE OUTPUT PARAM TABLE FOR ttSemana.
END.



