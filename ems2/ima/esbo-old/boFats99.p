/* Programa: esbo/boFats99.p
** Objetivo: Controlar as datas que sofreram altera‡äes no faturamento e que precisam 
** ser atualizadas;
** Autor...: Tadeu Silva Parreiras
**data: 05/2024
**
*/
{esp/util.i}
{utp/ut-glob.i}

DEFINE TEMP-TABLE ttResult NO-UNDO LIKE fats_99.
    
              
DEFINE BUFFER bf FOR fats_99 .          
DEFINE VARIABLE dt              AS DATE        NO-UNDO.
DEFINE VARIABLE id              AS INT64       NO-UNDO.
DEFINE VARIABLE progOrigem      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE tipoRegistro    AS CHARACTER   NO-UNDO.


PROCEDURE iniciar:

END PROCEDURE.

PROCEDURE finalizar:

   DELETE PROCEDURE THIS-PROCEDURE.

END PROCEDURE.

PROCEDURE setData:

    DEFINE INPUT  PARAMETER pData AS DATE        NO-UNDO.

    ASSIGN dt = pdata.


END PROCEDURE.

PROCEDURE setProgOrigem:

    DEFINE INPUT  PARAMETER pProg AS CHARACTER   NO-UNDO.
    ASSIGN progOrigem = pProg.

END PROCEDURE.

PROCEDURE setTipoRegistro:

    DEFINE INPUT  PARAMETER pTipo AS CHARACTER   NO-UNDO.
    ASSIGN tipoRegistro = pTipo.

END PROCEDURE.



PROCEDURE inserir:

    FIND bf EXCLUSIVE-LOCK
        WHERE bf.data           = dt
        AND   bf.num_situacao   = 0
        NO-ERROR.
    IF AVAIL bf THEN DO:
       ASSIGN bf.cod_usuario    = c-seg-usuario
              bf.dt_hr_registro = NOW. 
       RELEASE bf.
    END.
    ELSE DO:
       CREATE bf.
       ASSIGN bf.fat_99_id              = next-value(seq_fat_99)
              bf.data                   = dt
              bf.dt_hr_registro         = NOW 
              bf.cod_usuario            = c-seg-usuario 
              bf.cod_tipo_registro      = tipoRegistro
              bf.cod_programa_origem    = IF progOrigem = '' THEN PROGRAM-NAME(1) ELSE progOrigem
              .
    END.                                            

END PROCEDURE.

PROCEDURE setId:

    DEFINE INPUT  PARAMETER pId AS INT64     NO-UNDO.

    ASSIGN id = pId.

END PROCEDURE.

PROCEDURE setSituacao:

    DEFINE INPUT  PARAMETER pSituacao       AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pTransacaoId    AS INT64       NO-UNDO.

    FIND FIRST bf 
        WHERE bf.fat_99_id = id NO-ERROR.
    IF AVAIL bf THEN DO:
       ASSIGN bf.num_situacao = pSituacao
              bf.transacao_id = pTransacaoId
              .
       RELEASE bf.
    END.          
END PROCEDURE.


PROCEDURE getDatasPendentes:

    DEFINE INPUT  PARAMETER pTiposRegistro AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttResult.
    
    EMPTY TEMP-TABLE ttResult.
    FOR EACH bf NO-LOCK
        WHERE bf.num_situacao       = 0
        AND   lookup(bf.cod_tipo_registro, pTiposRegistro) > 0.
        CREATE ttResult.
        BUFFER-COPY bf TO ttResult.
    END.


END PROCEDURE.

/*PROCEDURE atuIDTransacao:

    DEFINE INPUT  PARAMETER  pID AS INT64          NO-UNDO.
    DEFINE INPUT  PARAMETER  pIDTrans AS INT64     NO-UNDO.

    
    FIND fats_99 EXCLUSIVE-LOCK
         WHERE fats_99.fat_99_id = pID NO-ERROR.
    IF AVAIL fats_99 THEN DO:
       ASSIGN fats_99.num_situacao = 2 //concluido
              fats_99.transacao_id = pIDTrans .
    END.                                  
    




END PROCEDURE.*/


/*
Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 fat_99_id                        int6
   20 data                             date
   30 dt_hr_registro                   datetm
   40 cod_usuario                      char
   50 num_situacao                     inte
*/
