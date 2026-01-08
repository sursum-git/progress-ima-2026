DEFINE TEMP-TABLE ttItemRef LIKE itens_pedido_lisa 
       FIELD qtPlanilha AS DECIMAL.
DEFINE TEMP-TABLE ttPlanLisa NO-UNDO
    FIELD itCodigo      AS CHAR
    FIELD descricao     AS CHAR
    FIELD quantidade    AS DECIMAL
    FIELD nfRetorno     AS CHAR FORMAT 'x(20)'
    FIELD pedidoLisa    AS CHAR
    FIELD nfRemessa     AS CHAR FORMAT 'X(20)'
    FIELD codRefer      AS CHAR
    FIELD nrPedido      AS CHAR FORMAT 'x(20)'
    FIELD qtEncontrada  AS DECIMAL
    FIELD linhas        AS INT
    INDEX ind-pri IS PRIMARY itCodigo codRefer.
{esp/util.i}
DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.
INPUT FROM c:\temp\retornos.csv.
ASSIGN iCont = 1.
REPEAT:
    CREATE ttPlanLisa.
    ASSIGN iCont = iCont + 1.
    IMPORT DELIMITER ";" tt NO-ERROR.
    ASSIGN ttPlanLisa.linha = iCont.
END.

FOR EACH tt
    WHERE ttPlanLisa.itCodigo <> '':
    ASSIGN ttPlanLisa.itCodigo = REPLACE(ttPlanLisa.itCodigo,'ima','').
    //DISP tt WITH 1 COL 1 DOWN.
    ASSIGN iCont = iCont + 1.
END.

//DISP iCont.

// sem retonro
/*FOR EACH tt
    WHERE ttPlanLisa.itCodigo <> ''
    AND  ttPlanLisa.nfRetorno = '':
    DISP tt WITH 1 COL WIDTH 550 1 DOWN.

END.*/

FOR EACH itens_pedido_lisa NO-LOCK.
    CREATE ttItemRef.
    BUFFER-COPY itens_pedido_lisa TO ttItemREf.
END.

//partindo da planilha lisa
FOR EACH tt  WHERE ttPlanLisa.Itcodigo <> '' AND ttPlanLisa.nfRetorno <> '':
    FIND ttItemRef 
        WHERE ttItemRef.it_codigo = ttPlanLisa.itCodigo
        AND   ttItemRef.cod_Refer = ttPlanLisa.codRefer
        AND   int(ttItemRef.nf_retorno) = int(ttPlanLisa.nfRetorno)
        AND    INT(ttItemREf.nf_origem) = INT(ttPlanLisa.nfRemessa)
        NO-ERROR.
    IF AVAIL ttItemRef THEN DO:
        ASSIGN ttPlanLisa.qtEncontrada          = ttItemRef.qt_faturada
               ttItemRef.qtPlanilha     = ttPlanLisa.quantidade.
    END.
END.


/*FOR EACH tt WHERE ttPlanLisa.Itcodigo <> ''  AND ttPlanLisa.nfRetorno <> ''
    AND ttPlanLisa.quantidade <> ttPlanLisa.qtEncontrada :
    DISP tt WITH 1 COL WIDTH 550 1 DOWN.
END.*/

{esp/exportarTabelacsv3.i tt " " " " "ttPlanilhaLisa" }
{esp/exportarTabelacsv3.i ttItemRef " " " " "ttItemPedidoLisa" }
