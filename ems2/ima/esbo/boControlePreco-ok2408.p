/*
 programa:boControlePreco.p
 Objetivo: Manter a tabela controle-preco

*/            
{esbo/boMsg.i}
{utp/ut-glob.i}
DEFINE VARIABLE hBoMsgCP   AS HANDLE      NO-UNDO.
DEFINE VARIABLE hAcompExt  AS HANDLE      NO-UNDO.
DEFINE VARIABLE lIniciarBO AS LOGICAL     NO-UNDO.
DEFINE VARIABLE iTbPreco   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iTpPreco   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iNrContainer AS INTEGER     NO-UNDO.
DEFINE VARIABLE iNivel     AS INTEGER     NO-UNDO.
DEFINE VARIABLE dtIni      AS DATE        NO-UNDO INIT TODAY.
DEFINE VARIABLE dtFim      AS DATE        NO-UNDO INIT ?.
DEFINE VARIABLE cItem      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRef       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE idPrecoAtual AS INTEGER     NO-UNDO.

DEFINE BUFFER bf-controle_preco FOR controle_preco.

PROCEDURE iniciarBos:
    IF lIniciarBo = NO THEN DO:
       RUN esbo/boMsg.p PERSISTENT SET hBomsgCP.
       ASSIGN lIniciarBo = YES.
    END.                       
END PROCEDURE.


PROCEDURE finalizarBos:
    IF VALID-HANDLE(hBoMsgCP) THEN
       DELETE PROCEDURE hBoMsgCP.
END PROCEDURE.


PROCEDURE setTbPreco:
    DEFINE INPUT  PARAMETER pTbPreco AS INTEGER     NO-UNDO.
    ASSIGN iTbPreco = pTbPreco.
END PROCEDURE.


PROCEDURE setTpPreco:
    DEFINE INPUT  PARAMETER pTpPreco AS INTEGER     NO-UNDO. //1 - Pronta Entrega 2- Programa‡Æo de Importados 3- Promocional
    ASSIGN iTpPreco = pTpPreco .
END PROCEDURE.

PROCEDURE setNrContainer:
    DEFINE INPUT  PARAMETER pNrContainer AS INTEGER     NO-UNDO.
    ASSIGN iNrContainer = pNrContainer.

END PROCEDURE.

PROCEDURE setNivel:
    DEFINE INPUT  PARAMETER pNivel AS INTEGER     NO-UNDO. //1-item 2-referencia
    ASSIGN iNivel = pNivel .


END PROCEDURE.

PROCEDURE setDtInicio:
    DEFINE INPUT  PARAMETER pDtInicio AS DATE        NO-UNDO.
    ASSIGN dtIni = pDtInicio.
END PROCEDURE.

PROCEDURE setDtFinal:
    DEFINE INPUT  PARAMETER pDtFinal AS DATE        NO-UNDO.
    ASSIGN dtFim = pDtFinal.
END PROCEDURE.

PROCEDURE setItem:
    DEFINE INPUT  PARAMETER pItem AS CHARACTER   NO-UNDO.
    ASSIGN cItem = pItem.

END PROCEDURE.

PROCEDURE setCodRefer:
    DEFINE INPUT  PARAMETER pCodRefer AS CHARACTER   NO-UNDO.
    ASSIGN cRef = pcodRefer.

END PROCEDURE.


PROCEDURE inserirPreco:
    DEFINE INPUT  PARAMETER dVlReal   LIKE controle_preco.vl_real   NO-UNDO.
    DEFINE INPUT  PARAMETER dVlDolar  LIKE controle_preco.vl_dolar  NO-UNDO.
    
    //RUN getIdPrecoAtual(OUTPUT idPrecoAtual).
    RUN vencerPreco(dtIni).   
    CREATE controle_preco.
    ASSIGN controle_preco.cod_controle_preco = next-value(seq_controle_preco)
           controle_preco.tp_preco           = iTpPreco
           controle_preco.dt_inicial         = dtIni 
           controle_preco.dt_final           = dtFim
           controle_preco.it_codigo          = cItem
           controle_preco.cod_refer          = cRef 
           controle_preco.tb_preco           = iTbPreco
           controle_preco.vl_real            = dvlReal
           controle_preco.vl_dolar           = dvlDolar
           controle_preco.dt_hr_criacao      = NOW
           controle_preco.cod_usuario_criacao = c-seg-usuario
           controle_preco.LOG_vencido        = NO
           controle_preco.num_nivel          = iNivel .
    
     

END PROCEDURE.


PROCEDURE getPrecoAtual:
    DEFINE OUTPUT PARAMETER pId         AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER vlReal      AS DECIMAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER vlDolar     AS DECIMAL     NO-UNDO.
    /*MESSAGE 
        iTbPreco        SKIP
        iTpPreco        SKIP 
        iNrContainer    SKIP
        cItem           SKIP
        cRef            SKIP
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
      FIND LAST bf-controle_preco 
         WHERE bf-controle_preco.tb_preco      = iTbPreco
         AND   bf-controle_preco.num_nivel     = 2
         AND   bf-controle_preco.tp_preco      = iTpPreco 
         AND   bf-controle_preco.nr_container  = iNrContainer
         AND   bf-controle_preco.it_codigo     = cItem
         AND   bf-controle_preco.cod_refer     = cRef
         AND   bf-controle_preco.LOG_vencido   = NO 
         AND   bf-controle_preco.dt_inicial   <= TODAY
         AND   bf-controle_preco.dt_final     >= TODAY
         NO-LOCK NO-ERROR.
      IF NOT AVAIL bf-controle_preco THEN
         FIND LAST bf-controle_preco 
             WHERE bf-controle_preco.tb_preco      = iTbPreco
             AND   bf-controle_preco.num_nivel     = 1
             AND   bf-controle_preco.tp_preco      = iTpPreco 
             AND   bf-controle_preco.nr_container  = iNrContainer
             AND   bf-controle_preco.it_codigo     = cItem
             //AND   bf-controle_preco.cod_refer     = cRef
             AND   bf-controle_preco.LOG_vencido   = NO
             AND   bf-controle_preco.dt_inicial   <= TODAY
             AND   bf-controle_preco.dt_final     >= TODAY
             NO-LOCK NO-ERROR.

      IF AVAIL bf-controle_preco THEN DO:
         ASSIGN pId     = bf-controle_preco.cod_controle_preco
                vlReal  = bf-controle_preco.vl_real
                vlDolar = bf-controle_preco.vl_dolar .
      END.
      /*MESSAGE 
            pId     
            vlReal  
            vlDolar 

          VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

END PROCEDURE.


PROCEDURE getIdPrecoAtualNivel: 
    DEFINE OUTPUT PARAMETER pIdPrecoAtual AS INTEGER     NO-UNDO.
    FIND LAST bf-controle_preco 
         WHERE bf-controle_preco.tb_preco      = iTbPreco
         AND   bf-controle_preco.num_nivel     = iNivel 
         AND   bf-controle_preco.tp_preco      = iTpPreco 
         AND   bf-controle_preco.nr_container  = iNrContainer
         AND   bf-controle_preco.it_codigo     = cItem
         AND   bf-controle_preco.cod_refer     = cRef
         AND   bf-controle_preco.LOG_vencido   = NO 
    NO-LOCK NO-ERROR.
    IF AVAIL bf-controle_preco THEN
        ASSIGN 
               pIdPrecoAtual = idPrecoAtual.
END PROCEDURE.

PROCEDURE getPrecoAtualNivel: 
    DEFINE INPUT  PARAMETER pIdPrecoAtual    AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER vlReal           AS DECIMAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER vlDolar          AS DECIMAL     NO-UNDO.

    FIND  bf-controle_preco 
         WHERE  bf-controle_preco.cod_controle_preco = pIdPrecoAtual 
     NO-LOCK NO-ERROR.
     IF AVAIL bf-controle_preco THEN
        ASSIGN vlReal  = bf-controle_preco.vl_real
               vlDolar = bf-controle_preco.vl_dolar .
END PROCEDURE.


PROCEDURE vencerPreco:
    DEFINE INPUT  PARAMETER pDtIni AS DATE        NO-UNDO.
    FOR EACH  controle_preco
        WHERE controle_preco.tb_preco_id  = iTbPreco
        AND   controle_preco.num_nivel    = iNivel 
        AND   controle_preco.tp_preco     = iTpPreco
        AND   controle_preco.nr_container = iNrContainer
        AND   controle_preco.it_codigo    = cItem
        AND   controle_preco.cod_refer    = cRef
        AND   controle_preco.log_vencido  = NO
        EXCLUSIVE-LOCK .
        ASSIGN controle_preco.dt_final               = IF TODAY = pDtIni THEN pDtIni ELSE pDtIni - 1
               controle_preco.LOG_vencido            = TODAY = pDtIni 
               controle_preco.dt_hr_alteracao        = NOW
               controle_preco.cod_usuario_alteracao  = c-seg-usuario .
    END.
END PROCEDURE.


PROCEDURE getPrecoPorID:
    DEFINE INPUT  PARAMETER pID    AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER dPrecoReal  AS DECIMAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER dPrecoDolar AS DECIMAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER iTipoPreco  AS INTEGER     NO-UNDO.

    FIND controle_preco
        WHERE controle_preco.cod_controle_preco = pID
        NO-LOCK NO-ERROR.
    IF AVAIL controle_preco THEN
        ASSIGN dPrecoReal   = controle_preco.vl_real
               dPrecoDolar  = controle_preco.vl_dolar
               iTipoPreco   = controle_preco.tp_preco.   // 1-Pe, 2-PI, 3-Outlet


END PROCEDURE.


/*Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cod_controle_preco               inte        i
   20 tp_preco                         inte        i
   30 dt_inicial                       date        i
   40 dt_final                         date        i
   50 it_codigo                        char        i
   60 cod_refer                        char        i
   70 nr_container                     inte        i
   80 vl_dolar                         deci-2
   90 vl_real                          deci-2
  100 cod_usuario_criacao              char
  110 cod_usuario_alteracao            char
  140 dt_hr_criacao                    datetm
  150 dt_hr_alteracao                  datetm
  160 log_vencido                      logi
  170 tb_preco_id                      inte        i
  180 num_nivel                        inte        i

*/


