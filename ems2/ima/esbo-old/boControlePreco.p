/*
 programa:boControlePreco.p
 Objetivo: Manter a tabela controle-preco
 Autor: Tadeu Silva
 Alteraá‰es:
 05/2021 - Tadeu - Acrescentar procedure para retornar dados do agrupamento do preáo -  tsp01
 03/2023 - tadeu - Acrescentar procedure verifRestrOutlet e coloca-la para verificar restriá∆o ao preco outlet quando a procedure Ç chamada pelo portal - tsp02
           tadeu - acrescimo do procedimento setloginPortal para que seja possivel fazer a restriá∆o do outlet apenas quando a BO for utilizada pelo portal
 05/2023 - tadeu - acrescimo de procedimento setPedRubiX para informar se o pedido Ç rubix ou n∆o - tsp03          
 */            
{esbo/boMsg.i}
{utp/ut-glob.i}
DEFINE VARIABLE hBoMsgCP   AS HANDLE      NO-UNDO.
DEFINE VARIABLE hAcompExt  AS HANDLE      NO-UNDO.
DEFINE VARIABLE lIniciarBO AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lPedRubiX  AS LOGICAL     NO-UNDO.
DEFINE VARIABLE iTbPreco   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iTpPreco   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iNrContainer AS INTEGER     NO-UNDO.
DEFINE VARIABLE iNivel     AS INTEGER     NO-UNDO.
DEFINE VARIABLE dtIni      AS DATE        NO-UNDO INIT TODAY.
DEFINE VARIABLE dtFim      AS DATE        NO-UNDO INIT ?.
DEFINE VARIABLE dtRefer    AS DATE        NO-UNDO.
DEFINE VARIABLE cItem      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRef       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE idPrecoAtual AS INTEGER   NO-UNDO.
DEFINE VARIABLE lVencido   AS LOGICAL     NO-UNDO.

DEFINE VARIABLE lPortal        AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cLoginPortal   AS CHARACTER   NO-UNDO.

//tsp01
DEFINE VARIABLE lDivideComis        AS LOGICAL     NO-UNDO.
DEFINE VARIABLE dPercComisVend      AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dPercComisRepres    AS DECIMAL     NO-UNDO.
{esp/util.i}
DEFINE TEMP-TABLE ttDados
    FIELD itCodigo   AS CHAR
    FIELD codRef     AS CHAR.


DEFINE BUFFER bf FOR controle_preco.
PROCEDURE iniciarBos:
    IF lIniciarBo = NO THEN DO:
       RUN esbo/boMsg.p PERSISTENT SET hBomsgCP.
       ASSIGN lIniciarBo = YES.
    END.                       
END PROCEDURE.


PROCEDURE setLoginPortal:
    DEFINE INPUT  PARAMETER pLogin AS CHARACTER   NO-UNDO.
    ASSIGN cLoginPortal = pLogin.

    ASSIGN lPortal = cLoginPortal <> ''.

END PROCEDURE.

PROCEDURE setPedRubiX:

    DEFINE INPUT  PARAMETER pRubiX AS LOGICAL     NO-UNDO.
    ASSIGN lPedRubiX = pRubiX.

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
    DEFINE INPUT  PARAMETER pTpPreco AS INTEGER     NO-UNDO. //1 - Pronta Entrega 2- Programaá∆o de Importados 3- Promocional
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

PROCEDURE setVencido:

    DEFINE INPUT  PARAMETER pVencido AS LOGICAL     NO-UNDO.
    ASSIGN lVencido = pVencido.

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
           controle_preco.LOG_vencido        = lVencido
           controle_preco.num_nivel          = iNivel .
    IF lVencido THEN
       ASSIGN controle_preco.dt_hr_alteracao       = NOW
              controle_preco.cod_usuario_alteracao = c-seg-usuario
              .
     

END PROCEDURE.

PROCEDURE setDtRefer:
    DEFINE INPUT  PARAMETER pData AS DATE        NO-UNDO.
    ASSIGN dtRefer = pdata.

END PROCEDURE.

PROCEDURE getPrecoAtual:
    DEFINE OUTPUT PARAMETER pId         AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER vlReal      AS DECIMAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER vlDolar     AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE lRestrOutlet        AS LOGICAL     NO-UNDO.
    /*MESSAGE 
        iTbPreco        SKIP
        iTpPreco        SKIP 
        iNrContainer    SKIP
        cItem           SKIP
        cRef            SKIP
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

    IF lPortal THEN
       RUN verifRestrOutlet(OUTPUT lRestrOutlet).




    IF dtRefer = ? THEN
       ASSIGN dtRefer = TODAY.

      FIND LAST bf 
         WHERE bf.tb_preco      = iTbPreco
         AND   bf.num_nivel     = 2
         AND   bf.tp_preco      = iTpPreco 
         AND   bf.nr_container  = iNrContainer
         AND   bf.it_codigo     = cItem
         AND   bf.cod_refer     = cRef
         AND   ( bf.LOG_vencido   = NO OR( bf.LOG_vencido = YES  AND DATE(bf.dt_hr_alt) >= dtRefer))
         AND   bf.dt_inicial   <= dtRefer
         AND   bf.dt_final     >= dtRefer
         NO-LOCK NO-ERROR.
      IF NOT AVAIL bf THEN
         FIND LAST bf 
             WHERE bf.tb_preco      = iTbPreco
             AND   bf.num_nivel     = 1
             AND   bf.tp_preco      = iTpPreco 
             AND   bf.nr_container  = iNrContainer
             AND   bf.it_codigo     = cItem
             //AND   bf.cod_refer     = cRef
             AND   ( bf.LOG_vencido   = NO OR( bf.LOG_vencido = YES  AND DATE(bf.dt_hr_alt) >= dtRefer))
             AND   bf.dt_inicial   <= dtRefer
             AND   bf.dt_final     >= dtRefer
             NO-LOCK NO-ERROR.

      IF AVAIL bf THEN DO:
         //tsp02
         IF ( iTpPreco = 3 //outlet
            AND lRestrOutlet = NO )
             OR ( iTpPreco = 3  AND lPedRubiX ) //tsp03
             OR iTpPreco <> 3  THEN DO:
            ASSIGN pId     = bf.cod_controle_preco
                   vlReal  = bf.vl_real
                   vlDolar = bf.vl_dolar .
            //tsp01
            IF bf.campanha_id <> 0 THEN DO:
            FIND campanhas
                WHERE campanhas.campanha_id = bf.campanha_id
                NO-LOCK NO-ERROR.
            IF AVAIL campanhas THEN DO:
               ASSIGN lDivideComis         = campanhas.log_dividir_comis
                      dPercComisVend       = campanhas.perc_vend
                      dPercComisRepres     = campanhas.perc_repres.
            END.
            ELSE DO:
                ASSIGN lDivideComis        = NO
                      dPercComisVend       = 0
                      dPercComisRepres     = 0. 

            END.

            END.
            ELSE  DO:
             ASSIGN lDivideComis         = NO
                    dPercComisVend       = 0
                    dPercComisRepres     = 0. 

            END.
         END.
      END.
      /*MESSAGE 
            pId     
            vlReal  
            vlDolar 

          VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

      /*RUN gerarLog(
         'c:\temp\log_preco_' + STRING(TIME) + '.txt' ,  
         cItem    ,  
         cRef     ,  
         iNrContainer   ,  
         dtRefer  ,  
         pId      ,  
         vlReal ,
         iTbPreco,
         iTpPreco
        ). */




END PROCEDURE.

PROCEDURE getVarsAgrup:

    DEFINE OUTPUT PARAMETER pDivideComis     AS LOGICAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER pPercComisVend   AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER pPercComisRepres AS INTEGER     NO-UNDO.

    ASSIGN pDivideComis     = lDivideComis
           pPercComisVend   = dPercComisVend
           pPercComisRepres = dPercComisRepres .

END PROCEDURE.

PROCEDURE getIdPrecoAtualNivel: 
    DEFINE OUTPUT PARAMETER pIdPrecoAtual AS INTEGER     NO-UNDO.
    FIND LAST bf 
         WHERE bf.tb_preco      = iTbPreco
         AND   bf.num_nivel     = iNivel 
         AND   bf.tp_preco      = iTpPreco 
         AND   bf.nr_container  = iNrContainer
         AND   bf.it_codigo     = cItem
         AND   bf.cod_refer     = cRef
         AND   bf.LOG_vencido   = NO 
    NO-LOCK NO-ERROR.
    IF AVAIL bf THEN
        ASSIGN 
               pIdPrecoAtual = idPrecoAtual.
END PROCEDURE.

PROCEDURE getPrecoAtualNivel: 
    DEFINE INPUT  PARAMETER pIdPrecoAtual    AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER vlReal           AS DECIMAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER vlDolar          AS DECIMAL     NO-UNDO.

    FIND  bf 
         WHERE  bf.cod_controle_preco = pIdPrecoAtual 
     NO-LOCK NO-ERROR.
     IF AVAIL bf THEN
        ASSIGN vlReal  = bf.vl_real
               vlDolar = bf.vl_dolar .
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

PROCEDURE getQtMaxPrecoPorId:

    DEFINE INPUT  PARAMETER pID    AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER dQt    AS DECIMAL     NO-UNDO.

    FIND controle_preco
        WHERE controle_preco.cod_controle_preco = pID
        NO-LOCK NO-ERROR.
    IF AVAIL controle_preco THEN
       ASSIGN dQt = controle_preco.qtd_max_venda . 




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

PROCEDURE vencerPrecosOutletSemEstoque: 

    DEFINE BUFFER bf2 FOR controle_preco.
    DEFINE var lTemEstoq as logical.

    FOR EACH bf2 
    WHERE num_nivel = 2 AND tp_preco = 3 AND LOG_vencido = NO.
        RUN setNivel(2).
        RUN setTbPreco(1).
        RUN setNrContainer(0).
        RUN setTpPreco(3).
        FIND FIRST saldo-estoq WHERE
            saldo-estoq.cod-estabel = '5'
            AND  saldo-estoq.it-codigo = bf2.it_codigo 
            AND saldo-estoq.cod-refer = bf2.cod_refer 
            AND saldo-estoq.lote = saldo-estoq.cod-refer
            AND saldo-estoq.qtidade-atu <= 0
            AND saldo-estoq.cod-depos = 'arm' NO-LOCK NO-ERROR.
    
    
        FIND ttDados WHERE
             ttDados.itCodigo = bf2.it_codigo AND
             ttDados.codRef = bf2.cod_refer NO-ERROR.
    
        IF NOT AVAIL ttDados THEN DO.
           CREATE ttDados.
           ASSIGN ttDados.itCodigo = bf2.it_codigo.
           ASSIGN ttDados.codRef   = bf2.cod_refer.
        END.
    
        IF AVAIL saldo-estoq THEN DO:
           RUN setItem(bf2.it_codigo).
           RUN setCodRefer(bf2.cod_refer).
           RUN vencerPreco(TODAY).
          /* ASSIGN controle_preco.LOG_vencido = YES
                  controle_preco.dt_hr_alt = NOW
                  controle_preco.cod_usuario_alt = c-seg-usuario .*/
        END.
    
    END.

    
    //DEFINE BUFFER bf3 FOR controle_preco.
    
    FOR EACH bf2 
        WHERE num_nivel = 1 AND tp_preco = 3 AND LOG_vencido = NO.
    RUN setNivel(1).
    RUN setTbPreco(1).
    RUN setNrContainer(0).
    RUN setTpPreco(3).
	
    ASSIGN lTemEstoq = false.
    FOR EACH saldo-estoq WHERE
        saldo-estoq.cod-estabel = '5'
        AND saldo-estoq.it-codigo = bf2.it_codigo
        AND saldo-estoq.lote = saldo-estoq.cod-refer
        AND saldo-estoq.qtidade-atu > 0
        AND saldo-estoq.cod-depos = 'arm' NO-LOCK.
		Find ttDados 
		WHERE ttDados.itCodigo = bf2.it_codigo 
		AND   ttDados.codRef   = saldo-estoq.cod-refer NO-LOCK NO-ERROR.	
	    if AVAIL ttDados then next.
		assign lTemEstoq = true.
		leave.     

    END.
	if lTemEstoq = false then do:
	       RUN setItem(bf2.it_codigo).
           RUN setCodRefer('').
           RUN vencerPreco(TODAY).
       
	end.

END.
END PROCEDURE.

PROCEDURE verifRestrOutlet: //tsp02

    DEFINE OUTPUT PARAMETER lRestrito AS LOGICAL     NO-UNDO.

    FIND tbs_preco
        WHERE tbs_preco.tb_preco_id = iTbPreco
        NO-LOCK NO-ERROR.
    IF AVAIL tbs_preco AND tbs_preco.grupo_outlet <> '' THEN DO:
        //passar esta parte do c¢digo para uma BO especifica de seguranca portal
        FIND user-web
            WHERE user-web.usuario = cLoginPortal
            NO-LOCK NO-ERROR.
        IF AVAIL user-web THEN DO:
           FIND usuarios_grupos
               WHERE usuarios_grupos.cod_grupo      = int(tbs_preco.grupo_outlet)
               AND   usuarios_grupos.login_usuario  = user-web.login
               NO-LOCK NO-ERROR.
           ASSIGN lRestrito = NOT AVAIL usuarios_grupos .
        END.
        ELSE DO:
           ASSIGN lRestrito = YES.
        END.
    END.
    ELSE DO:
        ASSIGN lRestrito = NO.
    END.


END PROCEDURE.

PROCEDURE gerarLog:

    DEFINE INPUT  PARAMETER pArquivo        AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pItCodigo       AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pCodRefer       AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pContainer      AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pDtRefer        AS DATE        NO-UNDO.
    DEFINE INPUT  PARAMETER pIdPreco        AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pVlPrecoReal    AS DECIMAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pTbPreco        AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pTpPreco        AS INTEGER     NO-UNDO.

    DEFINE VARIABLE cMsg AS CHARACTER   NO-UNDO.

    
    RUN incrValor(INPUT-OUTPUT cMsg,"tabela preco:" + string(pTbPreco)       ,"-" ).
    RUN incrValor(INPUT-OUTPUT cMsg,"data refer.:"  + STRING(pDtRefer)       ,'-' ).
    RUN incrValor(INPUT-OUTPUT cMsg,"item:"         + pItCodigo              ,'-' ).
    RUN incrValor(INPUT-OUTPUT cMsg,"refer:"        + pCodRefer              ,'-' ).
    RUN incrValor(INPUT-OUTPUT cMsg,"container:"    + string(pContainer)     ,'-' ).
    RUN incrValor(INPUT-OUTPUT cMsg,"Tipo Preáo:"   + string(pTpPreco)       ,'-' ).
    RUN incrValor(INPUT-OUTPUT cMsg, 'Preco:'       + string(pVlPrecoReal)   ,'-' ).
    RUN incrValor(INPUT-OUTPUT cMsg, 'ID Preco:'    + string(pIdPreco)       ,'-' ).
    
    RUN gravarTextoEmArquivo(pArquivo,cMsg).



END PROCEDURE.
