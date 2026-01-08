
/* Definicao global do nome da transacao ---                */
&global-define ProgramName bcx0005a
/************************************************************/

/* Definicao da temp-table de integracao ---                */
&global-define TempTable tt-contagem-bc
{bcp/bc9002.i " "}
DEFINE TEMP-TABLE ttWork NO-UNDO LIKE {&TempTable}.
DEFINE TEMP-TABLE tttemp NO-UNDO LIKE {&TempTable}.
CREATE ttWork.
/************************************************************/

/* Propriedades globais para frames ---                     */              
&global-define FrameSize    50 By 15
/************************************************************/

/***************************************** Frames Inicio ******************************************/
/* Definicao da Frame01 ---                                 */
&global-define Frame01Name   Frame01

&global-define Frame01Defs   'Contagem 1/2'             At Row 01 Col 10          ~
                             'ESTAB:'                   At Row 04 Col 10          ~
                             ttWork.cod-estabel         At Row 04 Col 17 No-Label ~
                             'DEPOS:'                   At Row 06 Col 10          ~
                             ttWork.cod-depos           At Row 06 Col 17 No-Label ~
                             'DOCA:'                   At Row 08 Col 10          ~
                             ttWork.cod-local           At Row 08 Col 17 No-Label ~
                             'DT INV:'                  At Row 10 Col 10          ~
                             ttWork.dat-contagem        At Row 10 Col 18 NO-LABEL ~
                             'CONTAGEM:'                At Row 12 Col 10          ~
                             ttwork.num-contagem        At Row 12 Col 20 NO-LABEL
&global-define Frame01Repeat  No

/************************************************************/

/* Definicao da Frame02 ---                                 */
&global-define Frame02Name   Frame02
&global-define Frame02Defs   'Contagem 2/2'         At Row 01 Col 10          ~
                             'ESTAB:'               At Row 04 Col 10          ~
                             ttWork.cod-estabel     At Row 04 Col 17 No-Label ~
                             'DEPOS:'               At Row 06 Col 10          ~
                             ttWork.cod-depos       At Row 06 Col 17 No-Label ~
                             'DOCA:'                At Row 08 Col 10          ~
                             ttWork.cod-local       At Row 08 Col 17 No-Label ~
                             'DT INV:'              At Row 10 Col 10          ~
                             ttWork.dat-contagem    At Row 10 Col 18 NO-LABEL ~
                             'CONTAGEM:'            At Row 12 Col 10          ~
                             ttwork.num-contagem    At Row 12 Col 20 NO-LABEL ~
                             'NUMERO ETIQUETA:'     At Row 13 Col 10          ~
                             ttWork.cod-livre-1     At Row 14 Col 10 FORMAT 'x(14)' NO-LABEL ~
                             '<Ctrl-Alt-I>-Ler Coletor  <F6>Ler Arquivo' At Row 14 Col 26 


&global-define Frame02Repeat Yes
/************************************************************/

/* Definicao dos campos a serem recebidos ---               */
&global-define Update01Fields   ttWork.cod-estabel ~
                                ttWork.cod-depos ~
                                ttWork.cod-local ~
                                ttWork.dat-contagem ~
                                ttwork.num-contagem
&global-define Update02Fields   ttWork.cod-livre-1
/************************************************************/

/* Definicao das trigger de interacao com a tela ---        */ 
&global-define TriggerBeforeFrame01 Run InicializaCamposFrame01. 
&global-define TriggerBeforeFrame02 Run InicializaCamposFrame02. 
&global-define TriggerAfterFrame01  Run GravaCamposFrame01. 
&global-define TriggerAfterFrame02  Run GravaCamposFrame02. ~
                                    Run GravaTransacao.
/************************************************************/

/* Defini‡Æo das variaveis do coletor */
DEF VAR c-id          AS CHAR FORMAT "x(20)".
DEF VAR c-arq-coletor AS CHAR FORMAT "x(30)".
DEF VAR c-arq-saida   AS CHAR FORMAT "x(30)".
DEF VAR OKpressed AS LOGICAL INITIAL TRUE.

/************************************************************/
DEF VAR c-localiz AS CHAR.
DEF VAR i-tp-embal AS INT.
DEF VAR c-msg AS CHAR NO-UNDO.
DEF STREAM s-coletor.
DEF STREAM e-coletor.
DEF STREAM s-bat.

/* Definicao das trigger de usuario ---                     */ 
&global-define UserTriggers On Leave of ttWork.cod-livre-1 in Frame Frame02 ~
                            Do: /*~ 
                                Define Variable vCodUnico       As Integer Format '9999999999999999' No-undo. ~
                                Define Variable vCodUnicoChar   As Character                         No-undo. ~
                                ~
                                Assign vCodUnico = Int(ttWork.cod-item:Screen-value in Frame Frame02) No-error. ~
                                ~
                                Assign vCodUnicoChar = String(vCodUnico,'9999999999999999'). ~
                                ~
                                Find first bc-ext-saldo-estoq no-lock ~
                                where bc-ext-saldo-estoq.cod-unico   = vCodUnicoChar No-error. ~
                                If   avail bc-ext-saldo-estoq Then Do: ~
                                     Assign ttWork.cod-item        :Screen-value in Frame Frame02 = bc-ext-saldo-estoq.it-codigo ~
                                            ttWork.cod-depos:Screen-value in Frame Frame02 = bc-ext-saldo-estoq.cod-depos ~
                                            ttWork.cod-local:Screen-value in Frame Frame02 = bc-ext-saldo-estoq.cod-localiz ~
                                            ttWork.cod-lote :Screen-value in Frame Frame02 = bc-ext-saldo-estoq.lote. ~
                                End.*/ ~
                             End.

/************************************************************/

/*****************************************   Frames Fim ******************************************/

/**************************************************************************************************
** SECAO DO CODIGO PRINCIPAL DO PROGRAMA                                                         **
** Esta secao contem includes com codigos de execucao das interfaces.                      .     **
** Nao eï necessario efetuar alteracoes nesta sessao.                                            **
***************************************************************************************************/
PROCEDURE FindWindowA EXTERNAL "USER32.DLL":
   DEFINE INPUT  PARAMETER intClassName AS LONG.
   DEFINE INPUT  PARAMETER chrCaption   AS CHARACTER.
   DEFINE RETURN PARAMETER intHandle    AS LONG.
END PROCEDURE.

ON 'CTRL-ALT-I':U ANYWHERE DO:
   IF SELF:NAME <> "cod-livre-1" THEN NEXT.

   RUN pi-ler-coletor.

   {&TriggerBeforeFrame01}
   {&TriggerBeforeFrame02}

   APPLY 'end-error' TO SELF.
END.
ON 'F6' ANYWHERE DO:
   /*IF SELF:NAME <> "c-item" THEN NEXT.*/
        
   ASSIGN c-arq-coletor = ''.
   SYSTEM-DIALOG GET-FILE c-arq-coletor
       TITLE      "Informe Arquivo Texto para Importar..."
       FILTERS    "Arquivos Coletor (*.txt)" "*.txt"
       MUST-EXIST
       USE-FILENAME
       UPDATE OKpressed.

   IF OKpressed = NO OR c-arq-coletor = '' THEN
      NEXT.

   RUN pi-ler-arquivo.

   APPLY 'end-error' TO SELF.
END.


{bcp/bc9100.i} /* Gerador da interface caracter do coleta de dados */
{bcp/bc9101.i} /* Procedure de atualizacao da transacao            */

/**************************************************************************************************/

/************************************* Codigo do Usuario Inicio ************************************
** Este local ‚ destinado ao codigo do usuario.                                                   **
** Para efeitos de escalabilidade entre versoes de produto recomenda-se que o acesso as tabelas   **
** do ERP seja feita atraves de um proxy, caso contrario poderao haver retrabalhos na migracao    **
****************************************************************************************************/

/*************************************************************************************************** 
** Esta procedure esta inicializando o campo estabelecimento da tela Frame01 com o valor 1        **
** Os outros campos nao estao sendo inicializados pois os mesmos devem apresentar valor           **
** caso seja retornado a proxima tela para esta.                                                  **
** Esta procedure eï executada pelo pre-processador {&TriggerBeforeFrame01}.                      **
****************************************************************************************************/
Procedure InicializaCamposFrame01:
ASSIGN
    ttWork.cod-estabel  = '1' 
    ttWork.cod-depos    = '' 
    ttWork.cod-local    = ''
    ttWork.dat-contagem = TODAY
    ttwork.num-contagem = 1
    c-localiz = ''.
End Procedure.

/*************************************************************************************************** 
** Esta procedure esta inicializando os campos da tela Frame02 com valores em branco              **
** Esta procedure eï executada pelo pre-processador {&TriggerBeforeFrame02}.                      **
****************************************************************************************************/
Procedure InicializaCamposFrame02:

    Assign 
        ttWork.cod-estabel:Screen-value In Frame frame02  = ttwork.cod-estabel:Screen-value In Frame frame01
        ttWork.cod-depos:Screen-value In Frame frame02    = ttwork.cod-depos:Screen-value In Frame frame01
        ttWork.cod-local:Screen-value In Frame frame02    = ttwork.cod-local:Screen-value In Frame frame01
        ttWork.cod-livre-1  = ''.

    DISP TTWORK.DAT-CONTAGEM 
         TTWORK.NUM-CONTAGEM 
         WITH FRAME FRAME02.

End Procedure.

/*************************************************************************************************** 
** Esta procedure esta armazenando na temp-table {&Temp-Table} os valores recebidos por ttWork    **
** na tela Frame 01.                                                                              **
** Esta procedure eï executada pelo pre-processador {&TriggerAfterFrame01}.                       **
****************************************************************************************************/
Procedure GravaCamposFrame01:
    /* Validacoes Frame 01 Inicio --- */

    IF  ttWork.cod-estabel = '' Then do:
        {bcp/bc9105.i "0" "Codigo do Estabelecimento deve ser informado"}
        Return Error.
    End.

    If  ttWork.cod-depos = '' Then do:
        {bcp/bc9105.i "1" "Codigo do Deposito deve ser informado"}
        Return Error.
    End.
    /*
    If  ttWork.cod-local = '' Then do:
        {bcp/bc9105.i "2" "Codigo do Local deve ser informado"}
        Return Error.
    End.
    */
    If  ttWork.dat-contagem = ? Then do:
        {bcp/bc9105.i "3" "Data de Fabricacao deve ser informada"}
        Return Error.
    End.

    If  ttWork.num-contagem > 3 
    OR  ttWork.num-contagem < 1 
    Then do:
        {bcp/bc9105.i "3" "N£mero da contagem deve ser 1 ou 2 ou 3"}
        Return Error.
    End.


    /* Validacoes Frame 01 Fim    --- */

    Assign  
        {&TempTable}.cod-estabel    = input Frame Frame01 ttWork.cod-estabel
        {&temptable}.cod-depos      = input Frame Frame01 ttWork.cod-depos
        {&TempTable}.cod-local      = input Frame Frame01 ttWork.cod-local
        {&TempTable}.dat-contagem   = input Frame Frame01 ttWork.dat-contagem
        {&TempTable}.num-contagem   = input Frame Frame01 ttWork.num-contagem.

End Procedure.

/*************************************************************************************************** 
** Esta procedure esta armazenando na temp-table {&Temp-Table} os valores recebidos por ttWork    **
** na tela Frame 02.                                                                              **
** Esta procedure eï executada pelo pre-processador {&TriggerAfterFrame02}.                       **
****************************************************************************************************/
PROCEDURE GravaCamposFrame02:
    /* A hora da entrada da etiqueta ser  feita no campo cod-livre-2 */

    /* Validacoes Frame 02 Inicio --- */
    ASSIGN vLogErro = NO.

    IF ttWork.cod-livre-1 = '' THEN DO:
       {bcp/bc9105.i "4" "O N£mero da Etiqueta dever  ser informado"}
       RETURN ERROR.
    END.

    IF NOT CAN-FIND(bc-etiqueta WHERE
                    bc-etiqueta.progressivo = ttwork.cod-livre-1) THEN DO:
       MESSAGE "N£mero da Etiqueta (" + STRING(ttwork.cod-livre-1) + ") Inv lido"
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       /*{bcp/bc9105.i "5" c-msg}*/
       RETURN ERROR.
    END.

    IF NOT CAN-FIND(bc-etiqueta WHERE
                    bc-etiqueta.progressivo = ttwork.cod-livre-1 AND
                    bc-etiqueta.cod-estabel = ttwork.cod-estabel 
                    USE-INDEX ch-progressivo) THEN DO:
       MESSAGE "A Etiqueta (" + STRING(ttwork.cod-livre-1) + ") nao esta relacionada com o estabelecimento informado"
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       
       /*{bcp/bc9105.i "7" c-msg}*/
       RETURN ERROR.
    END.

    FIND bc-etiqueta
         WHERE bc-etiqueta.progressivo = TTWORK.COD-LIVRE-1
         NO-LOCK NO-ERROR.

    IF bc-etiqueta.cod-estado <> 2 AND
       bc-etiqueta.cod-estado <> 6 THEN DO:
       MESSAGE "O Estado da etiqueta (" + STRING(bc-etiqueta.progressivo) + ") devera ser 2 - Impressa"
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.

       /*{bcp/bc9105.i "6" c-msg}*/
       Return Error.
    END.
    
    FIND bc-trans-etiq WHERE 
         bc-trans-etiq.nr-etiq  = bc-etiqueta.nr-etiq AND
         bc-trans-etiq.cd-trans = 'ima0005a' NO-LOCK NO-ERROR.

    IF AMBIGUOUS bc-trans-etiq THEN DO.
       FOR EACH bc-trans-etiq WHERE
                bc-trans-etiq.cd-trans = 'ima0005a' AND
                bc-TRANS-etiq.nr-etiq  = bc-etiqueta.nr-etiq NO-LOCK:
        
           FOR EACH bc-trans WHERE
                    bc-trans.cd-trans = bc-trans-etiq.cd-trans AND
                    bc-trans.nr-trans = bc-trans-etiq.nr-trans NO-LOCK.
                
               IF NOT bc-trans-ETIQ.cd-trans = 'ima0005a' THEN NEXT.
               IF NOT bc-trans.cd-trans = bc-trans-etiq.cd-trans THEN NEXT.
    
               CREATE tttemp.
               RAW-TRANSFER bc-trans-etiq.conteudo-trans TO tttemp.
           END.
       END.
    END.
    ELSE DO.
        FOR EACH bc-trans WHERE
                 bc-trans.cd-trans = bc-trans-etiq.cd-trans AND
                 bc-trans.nr-trans = bc-trans-etiq.nr-trans NO-LOCK.

            IF NOT bc-trans-ETIQ.cd-trans = 'ima0005a' THEN NEXT.
            IF NOT bc-trans.cd-trans = bc-trans-etiq.cd-trans THEN NEXT.

            CREATE tttemp.
            RAW-TRANSFER bc-trans-etiq.conteudo-trans TO tttemp.
        END.
    END.

    IF (TTWORK.NUM-CONTAGEM = 2 AND
        CAN-FIND(FIRST tttemp WHERE
                       tttemp.num-contagem = 1 AND 
                       tttemp.dat-contagem = ttwork.dat-contagem AND
                       tttemp.cod-livre-1  = ttwork.cod-livre-1) = NO) OR 
       (TTWORK.NUM-CONTAGEM = 3 AND
        CAN-FIND(FIRST tttemp WHERE
                       tttemp.num-contagem = 2 AND
                       tttemp.dat-contagem = ttwork.dat-contagem AND
                       tttemp.cod-livre-1  = ttwork.cod-livre-1) = NO) THEN DO:
        
        ASSIGN c-msg = "NÆo foi executada a contagem anterior de n£mero (" + STRING(ttwork.num-contagem - 1) + ")". 
        {bcp/bc9105.i "0" " " " " " + c-msg "}
        RETURN ERROR.
    END.

    IF CAN-FIND(FIRST tttemp WHERE
                      tttemp.num-contagem = ttwork.num-contagem AND
                      tttemp.dat-contagem = ttwork.dat-contagem AND
                      tttemp.cod-livre-1  = ttwork.cod-livre-1) THEN DO:
       ASSIGN c-msg = "Ja existe contagem para a Etiqueta (" + STRING(ttwork.cod-livre-1) + ") na data informada".

       {bcp/bc9105.i "8" " " " " " + c-msg "}
       RETURN ERROR.
    END.

    EMPTY TEMP-TABLE tttemp.

    /* Validacoes Frame 02 Fim    --- */
    ASSIGN  
        {&TempTable}.cod-livre-1 = INPUT FRAME Frame02 ttWork.cod-livre-1
        {&TempTable}.cod-livre-2 = STRING(TIME,'hh:mm:ss').

END PROCEDURE.

/*************************************************************************************************** 
** Esta procedure esta gerando a transacao no Data Collection atraves da chamada a procedure      **
** _GenerateDCTransaction.                                                                        **
** Esta procedure eï executada pelo pre-processador {&TriggerAfterFrame02}.                       **
****************************************************************************************************/

Procedure GravaTransacao:
    Def Var vNomeUsuario            As Character                No-undo.
    
    Find bc-etiqueta No-lock
        Where bc-etiqueta.progressivo = {&temptable}.cod-livre-1 No-error.

    If  avail bc-etiqueta Then Do:

        IF c-localiz = '' THEN
           ASSIGN c-localiz = {&TempTable}.cod-local.

        FIND ob-etiqueta WHERE
             ob-etiqueta.progressivo = bc-etiqueta.progressivo
             NO-LOCK NO-ERROR.
        IF NOT AVAIL ob-etiqueta THEN DO.
           RUN pi-cria-etiqueta.
           RUN esapi/imp-etq-estoque.p (INPUT ob-etiqueta.num-etiqueta,
                                        INPUT NO).
        END.

        run _RetornaUsuario (Output vNomeUsuario).

        assign {&TempTable}.data-transacao  = Today
               {&TempTable}.cod-usuario     = vNomeUsuario
               {&TempTable}.cod-item        = bc-etiqueta.it-codigo
               {&TempTable}.QTd-APONTADA    = bc-etiqueta.qt-item
               {&TempTable}.cod-refer       = bc-etiqueta.referencia
               {&TempTable}.cod-lote        = bc-etiqueta.lote
               {&TempTable}.cod-local       = "".
    
        Assign vTransDetail = 'IMA0005A'  + 
                            " ETQ: "        + {&temptable}.cod-livre-1          +
                            " ITEM: "       + String({&TempTable}.cod-item)     +
                            " QTD: "        + String({&TempTable}.QTd-APONTADA) +
                            " DT INV: "     + String({&TempTable}.dat-contagem) +
                            " NR INV: "     + String({&TempTable}.num-contagem).

        Raw-transfer {&TempTable} To vConteudoRaw.
        
        Run _GenerateDCTransaction (Input 'IMA0005A',              /* Codigo da Transacao                   */
                                    Input vConteudoRaw,            /* Conteudo da temp-table {&TempTable}   */
                                    Input vTransDetail,            /* Cabecalho de detalhes da transacao    */
                                    Input vNomeUsuario).           /* Usuario responsavel pela transacao    */
        
    End.
End Procedure.
/*************************************  Codigo do Usuario Fim   **********************************/

PROCEDURE pi-ler-coletor.
   ASSIGN  c-id           = "" 
           c-arq-coletor  = "" 
           c-arq-saida    = "".
   DEF VAR c-arq-bat     AS CHAR.
   DEF VAR i-handle      AS INTEGER NO-UNDO.

   ASSIGN c-arq-coletor = SESSION:TEMP-DIRECTORY + "INVENT.TXT"
          c-arq-saida = 'm:\ems206\coletor\inv' +
                        STRING(DAY(TODAY),"99") + 
                        STRING(MONTH(TODAY),"99") +
                        SUBSTR(STRING(TIME,"HH:MM"),1,2) +
                        SUBSTR(STRING(TIME,"HH:MM"),4,2) + '.TXT'.

   /* Baixa os dados do Coletor */
   IF SESSION:SET-WAIT-STATE("general":U) THEN.

   OS-DELETE SILENT VALUE(c-arq-coletor).
   IF SEARCH("C:\IMPROTEC\P220\P220.EXE") <> ? THEN DO.
      ASSIGN c-arq-bat = SESSION:TEMP-DIRECTORY + "p220.bat".
      OUTPUT STREAM s-bat TO VALUE(c-arq-bat).
         PUT STREAM s-bat
             "c:" SKIP
             "cd " SESSION:TEMP-DIRECTORY SKIP
             "C:\IMPROTEC\P220\P220.EXE R 1 1 "
             c-arq-coletor FORMAT "x(40)" SKIP.
      OUTPUT STREAM s-bat CLOSE.

      IF SEARCH(c-arq-bat) <> ? THEN DO.
         OS-COMMAND SILENT VALUE(c-arq-bat).
         PAUSE 5 NO-MESSAGE.
         REPEAT. 
            RUN FindWindowA (0, "IMODEM", OUTPUT i-handle).
            IF i-handle = 0 THEN LEAVE.
         END. 
         OS-DELETE SILENT VALUE(c-arq-bat). 
      END.
   END.
   IF SESSION:SET-WAIT-STATE("") THEN.
    
   IF SEARCH(c-arq-coletor) = ? THEN DO.
      MESSAGE "Arquivo nÆo foi Baixado do Coletor..." VIEW-AS ALERT-BOX.
      RETURN ERROR.
   END.

   OUTPUT STREAM s-coletor TO VALUE(c-arq-saida).
   INPUT STREAM e-coletor FROM VALUE(c-arq-coletor) NO-ECHO.
   REPEAT.                 
       SET STREAM e-coletor c-id.

       IF c-id = '' THEN NEXT.

       ASSIGN c-localiz = SUBSTR(c-id,1,6)
              c-id = SUBSTR(c-id,7,11).

       FIND bc-etiqueta WHERE
            bc-etiqueta.progressivo = c-id NO-LOCK NO-ERROR.
       IF NOT AVAIL bc-etiqueta THEN DO.
          PUT STREAM s-coletor
              c-id
              "N£mero da Etiqueta invalido"
              SKIP.
          NEXT.
       END.

       PUT STREAM s-coletor
           bc-etiqueta.it-codigo ";" 
           bc-etiqueta.referencia ";"
           bc-etiqueta.qt-item ";" 
           bc-etiqueta.progressivo ";" 
           SKIP.

       ASSIGN ttWork.cod-livre-1 = c-id.

       DISP ttWork.cod-livre-1 WITH FRAME {&Frame02Name}.

       {&TriggerAfterFrame01}
       {&TriggerAfterFrame02}
   END.
   OUTPUT STREAM s-coletor CLOSE.
END PROCEDURE.


/************************************** Ler Arquivo **************************************/
/* Anderson Fagner 17/12/09 */

PROCEDURE pi-ler-arquivo.
   ASSIGN  c-id           = ""
           /*c-arq-coletor  = ""*/
           c-arq-saida    = "".
   

   ASSIGN c-arq-saida = 'm:\ems206\coletor\inv' +
                        STRING(DAY(TODAY),"99") + 
                        STRING(MONTH(TODAY),"99") +
                        SUBSTR(STRING(TIME,"HH:MM"),1,2) +
                        SUBSTR(STRING(TIME,"HH:MM"),4,2) + '.TXT'.

   /* Baixa os dados do Coletor */
   OUTPUT STREAM s-coletor TO VALUE(c-arq-saida).
   INPUT STREAM e-coletor FROM VALUE(c-arq-coletor) NO-ECHO.
   REPEAT.                 
       SET STREAM e-coletor c-id.

       IF c-id = '' THEN NEXT.

       ASSIGN c-localiz = SUBSTR(c-id,1,6)
              c-id = SUBSTR(c-id,7,11).

       FIND bc-etiqueta WHERE
            bc-etiqueta.progressivo = c-id NO-LOCK NO-ERROR.
       IF NOT AVAIL bc-etiqueta THEN DO.
          PUT STREAM s-coletor
              c-id
              "N£mero da Etiqueta invalido"
              SKIP.
          NEXT.
       END.

       PUT STREAM s-coletor
           bc-etiqueta.it-codigo ";" 
           bc-etiqueta.referencia ";"
           bc-etiqueta.qt-item ";" 
           bc-etiqueta.progressivo ";" 
           SKIP.

       ASSIGN ttWork.cod-livre-1 = c-id.

       DISP ttWork.cod-livre-1 WITH FRAME {&Frame02Name}.

       {&TriggerAfterFrame01}
       {&TriggerAfterFrame02}

   END.
   OUTPUT STREAM s-coletor CLOSE.
END PROCEDURE.

PROCEDURE pi-cria-etiqueta.

    FIND ITEM WHERE
         ITEM.it-codigo = bc-etiqueta.it-codigo NO-LOCK NO-ERROR.

    ASSIGN i-tp-embal = 1.
    IF ITEM.un = 'kg' THEN 
       ASSIGN i-tp-embal = 5.

    FIND corte-comerc WHERE
         corte-comerc.compr-min <= bc-etiqueta.qt-item AND
         corte-comerc.compr-max >= bc-etiqueta.qt-item AND
         corte-comerc.tp-embalag = i-tp-embal NO-LOCK NO-ERROR.

    CREATE ob-etiqueta.
    ASSIGN ob-etiqueta.cod-estabel     = bc-etiqueta.cod-estabel
           ob-etiqueta.dt-emissao      = TODAY
           ob-etiqueta.hr-emissao      = STRING(TIME,"HH:MM")
           ob-etiqueta.acondic         = ""
           ob-etiqueta.it-codigo       = bc-etiqueta.it-codigo
           ob-etiqueta.cod-refer       = bc-etiqueta.referencia
           ob-etiqueta.nr-lote         = IF bc-etiqueta.lote = '888'
                                         THEN 'RD' ELSE 'RP'
           ob-etiqueta.cod-qualid      = IF bc-etiqueta.lote = '888'
                                         THEN 'D' ELSE 'B'
           ob-etiqueta.corte-comerc    = IF AVAIL corte-comerc
                                         THEN corte-comerc.codigo
                                         ELSE ''
           ob-etiqueta.quantidade      = bc-etiqueta.qt-item
           ob-etiqueta.localizacao     = c-localiz
           ob-etiqueta.situacao        = 3
           ob-etiqueta.num-etiqueta    =  IF bc-etiqueta.cod-estabel = '1' 
                                          THEN NEXT-VALUE(seq-etq-estoq-ima)
                                          ELSE NEXT-VALUE(seq-etq-estoq-med)
           ob-etiqueta.progressivo     = bc-etiqueta.progressivo.
END PROCEDURE.
