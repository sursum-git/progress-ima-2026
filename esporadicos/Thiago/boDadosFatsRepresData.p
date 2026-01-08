DEFINE VARIABLE dDataIni    AS DATE        NO-UNDO.
DEFINE VARIABLE dDataFIm    AS DATE        NO-UNDO.
DEFINE VARIABLE iCodRepIni  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCodRepFim  AS INTEGER     NO-UNDO.
DEFINE VARIABLE cItemIni    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cItemFim    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cUnidMedIni AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cUnidMedFim AS CHARACTER   NO-UNDO.

DEFINE TEMP-TABLE ttFatsRepresData LIKE fats_repres_clientes_prod_data.

PROCEDURE iniciarBos:

END PROCEDURE.

PROCEDURE finalizarBos:

END PROCEDURE.

PROCEDURE setData:
    DEFINE INPUT PARAMETER pDataIni AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER pDataFim AS DATE NO-UNDO.
    ASSIGN dDataIni = pDataIni
           dDataFim = pDataFim.
END PROCEDURE.

PROCEDURE setCodRep:
    DEFINE INPUT PARAMETER pCodRepIni AS INTEGER       NO-UNDO.
    DEFINE INPUT PARAMETER pCodRepFim AS INTEGER       NO-UNDO.
    ASSIGN iCodrepIni = pCodrepIni
           iCodRepFim = pCodRepFim.
END PROCEDURE.

PROCEDURE setCodItem:
    DEFINE INPUT PARAMETER pCodItemIni AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pCodItemFim AS CHARACTER NO-UNDO.
    ASSIGN cItemIni = pCodItemIni
           cItemFim = pCodItemFim.
END PROCEDURE.

PROCEDURE setUnidMedida:
    DEFINE INPUT PARAMETER pUnidMedIni AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pUnidMedFim AS CHARACTER NO-UNDO.
    ASSIGN cUnidMedIni = pUnidMedIni
           cUnidMedFim = pUnidMedFim.
END PROCEDURE.

PROCEDURE executar:

    FOR EACH fats_repres_clientes_prod_data
        WHERE fats_repres_clientes_prod_data.data      >= dDataIni AND
              fats_repres_clientes_prod_data.data      <= dDataFim AND
              fats_repres_clientes_prod_data.cod_rep   >= iCodRepIni AND
              fats_repres_clientes_prod_data.cod_rep   <= iCodRepFim AND
              fats_repres_clientes_prod_data.it_codigo >= cItemIni AND
              fats_repres_clientes_prod_data.it_codigo <= cItemFim AND
              fats_repres_clientes_prod_data.um        >= cUnidMedIni AND
              fats_repres_clientes_prod_data.um        <= cUnidMedFim.
        
        CREATE ttFatsRepresData. 
        BUFFER-COPY fats_repres_clientes_prod_data TO ttFatsRepresData.
    END.
END PROCEDURE.

PROCEDURE getDadosFatsRepres:
    DEFINE OUTPUT PARAMETER TABLE FOR ttFatsRepresData.
END PROCEDURE.

