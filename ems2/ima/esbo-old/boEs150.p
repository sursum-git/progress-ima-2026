/*
Nome do programa: Es150.p
Autor:  Thiago Vieira
Objetivo: Retornar o c¢digo da transportadora a partir de parƒmentros permitindo a rela‡Æo transportadora x estabelecimento.
Data: 06/2023
*/
DEFINE TEMP-TABLE ttParam
    FIELD codEstab   AS INT
    FIELD cidade     AS CHAR
    FIELD ibge       AS INT
    FIELD codTpFrete AS CHAR.

DEFINE VARIABLE iTransp AS INTEGER     NO-UNDO INIT 50.
DEFINE VARIABLE id      AS INTEGER     NO-UNDO.
{esp/util.i}
{esp/setProp.i ttParam}

PROCEDURE iniciar:

CREATE ttParam.
    
END PROCEDURE.

PROCEDURE finalizar:
    DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

PROCEDURE exec:
    DEFINE VARIABLE cParam AS CHARACTER   NO-UNDO.
    ASSIGN iTransp = 50
           id      = 0 .
    FIND FIRST ttParam NO-ERROR.                                                                      

    IF ttParam.codTpFrete = 'Cif at‚ Redesp' THEN DO:
        RUN getvlParametro('cod_transp_redesp_' + string(ttParam.codEstab),OUTPUT cParam).
        IF cParam = '' THEN
           ASSIGN iTransp = 762.
        ELSE
           ASSIGN iTransp = INT(cParam).
    END.
    ELSE DO:
        FIND LAST transp_estab                                                                         
             WHERE transp_estab.cod_estab = ttParam.codEstab                                           
             AND                                                                                       
             ( (transp_estab.cidade_dest = ttParam.cidade AND ttParam.cidade <> '') OR                 
               (transp_estab.cod_ibge    = ttParam.ibge AND ttParam.ibge <> 0)                         
             )                                                                                         
             AND transp_estab.LOG_inativo = 0 AND dt_ini <= TODAY AND dt_fim >= TODAY NO-LOCK NO-ERROR.
         IF AVAIL transp_estab THEN DO:                                                                
             ASSIGN itransp = transp_estab.cod_transp                                                  
                    id      = transp_estab.transp_estab_id .                                           
         END.                                                                                          
    END.                                                                                           
       
       
       
       
       
       
     
    

    

    //RUN exportarTTParam.
END PROCEDURE.


PROCEDURE getTransportadora:
    DEFINE OUTPUT PARAMETER pTransp AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER pId     AS INTEGER     NO-UNDO.
    ASSIGN pTransp = iTransp
           pid     = id.
END PROCEDURE.

PROCEDURE exportarTTParam:

    OUTPUT TO c:\temp\ttParamBoES150.txt.
    FOR EACH ttParam.
        DISP ttparam WITH 1 COL WIDTH 550.
    END.

    OUTPUT CLOSE.


END PROCEDURE.
