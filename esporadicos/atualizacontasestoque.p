FOR EACH docum-est
    WHERE 
    cod-estabel = '5' 
    AND     serie-docto = ''
    AND     cod-emitente = 27241
    AND     nat-operacao = '19905'
    AND     nro-docto = '0001249':
    /*AND   (nro-docto = '0001249' OR nro-docto = '0000077'
           OR nro-docto = '0007354'OR nro-docto = '0082014'
           OR nro-docto = '0007421' OR nro-docto = '1282014'
           OR nro-docto = '0000558' OR nro-docto = '0010819'
           OR nro-docto = '0001749') :*/
    FOR EACH item-doc-est OF docum-est:
       /* IF ct-codigo = '41402000' THEN DO:
           ASSIGN  ct-codigo = '41402031'
                   sc-codigo = '200001'.
*/
           DISP  item-doc-est EXCEPT char-1 char-2 WITH 1 COL WIDTH 550. /*nro-docto it-codigo ct-codigo sc-codigo. */
        END.
           
     
END.
