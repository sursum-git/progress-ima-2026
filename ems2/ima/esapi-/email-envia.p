/********************************************************************
* EMPRESA.: IMA TECIDOS DA MODA LTDA                                *
* SISTEMA.: DATASUL EMS 2.04.F.58    				                *
* PROGRESS: 10.1B PORTUGUÒS                                         *
*-------------------------------------------------------------------*
* PROGRAMA.....: email.envia.p                                      *
* DESENVOLVEDOR: Marcio Teixeira    	                            *
* DATA DESENV..: 25/05/2010	- 12:00h                                *
* DESCRIÇÃO....: 					                                *
*                                                                   *
*---------------------[ A L T E R A € Ç O ]-------------------------*
* DESENVOLVEDOR.:						                            *
* DATA ALTERA€ÇO:           						                *
* DESCRI€ÇO.....:							                        *
*                                                                   *
********************************************************************/

/*******************[ DBNAME.TABLE UTILIZADAS ]**********************
param-global
*/
DEFINE NEW GLOBAL SHARED VARIABLE c-seg-usuario AS CHAR NO-UNDO. 

/*****************[ DEFINE VARIAVEIS UTILIZADAS ]*******************/
DEFINE INPUT  PARAMETER v-remetente     AS CHARACTER NO-UNDO. /* Remetente                  */
DEFINE INPUT  PARAMETER v-destinatario  AS CHARACTER NO-UNDO. /* Destinatario               */
DEFINE INPUT  PARAMETER v-ccopia        AS CHARACTER NO-UNDO. /* Com copia                  */
DEFINE INPUT  PARAMETER v-ccoculta      AS CHARACTER NO-UNDO. /* Com copia oculta           */        
DEFINE INPUT  PARAMETER v-assunto       AS CHARACTER NO-UNDO. /* Assunto                    */                 
DEFINE INPUT  PARAMETER v-msg-txt       AS CHARACTER NO-UNDO. /* Mensagem em txt            */         
DEFINE INPUT  PARAMETER v-msg-arq       AS CHARACTER NO-UNDO. /* Mensagen em arquivo html   */
DEFINE INPUT  PARAMETER v-anexo         AS CHARACTER NO-UNDO. /* Anexo                      */
DEFINE INPUT  PARAMETER v-anexo-embed   AS CHARACTER NO-UNDO. /* Anexo embutido             */
DEFINE OUTPUT PARAMETER v-confirmar     AS LOG       NO-UNDO. /* Confirma‡Æo de email       */

DEFINE VARIABLE v-host                  AS CHARACTER NO-UNDO. /* Endere‡o de autentica‡Æo   */
DEFINE VARIABLE v-usr-autent            AS CHARACTER NO-UNDO. /* Usuario de autentica‡Æo    */
DEFINE VARIABLE v-senha                 AS CHARACTER NO-UNDO. /* Senha de autentica‡Æo      */
DEFINE VARIABLE v-porta                 AS CHARACTER NO-UNDO. /* Porta de saida             */
DEFINE VARIABLE v-blat                  AS CHARACTER NO-UNDO. /* Nome do programa de envio  */
DEFINE VARIABLE v-postie                AS CHARACTER NO-UNDO. /* Nome do programa de envio  */
                                        
DEFINE VARIABLE v-enviar                AS CHARACTER NO-UNDO. /* Enviar o e-mail */
DEFINE VARIABLE v-qt-destinatario       AS INTEGER   NO-UNDO INITIAL 1.
DEFINE VARIABLE v-i-destinatario        AS INTEGER   NO-UNDO.

DO v-i-destinatario = 1 TO LENGTH(v-destinatario): 
    IF (SUBSTRING(v-destinatario,v-i-destinatario,1) = ",") THEN
        ASSIGN v-qt-destinatario = v-qt-destinatario + 1.
END.

DO v-i-destinatario = 1 TO v-qt-destinatario:
    RUN esapi\email-valida.p (INPUT ENTRY(v-i-destinatario,v-destinatario,","), OUTPUT v-confirmar).
END.

IF v-confirmar THEN DO:
    /******************[ INICIALIZA€ÇO DAS VARIAVEIS ]******************/
    FIND FIRST param-global NO-LOCK NO-ERROR.
    
    ASSIGN v-porta      = STRING(param-global.porta-mail)
           v-host       = param-global.serv-mail         
           v-usr-autent = TRIM(SUBSTRING(param-global.char-2,200,50)) /*v-remetente*/ 
           v-senha      = TRIM(SUBSTRING(param-global.char-2,251,15))
           v-blat       = "T:\fnd\interfac\mail\blat.exe"
           v-postie     = "T:\fnd\interfac\mail\postie\postie.exe".
    
    /**********************[ Inicio do Programa ]**********************/
    ASSIGN v-enviar = v-blat
                      + " -html"
                      + " -server " + v-host
                      + " -port "   + v-porta
                      + " -f "      + v-remetente
                      + " -u "      + v-usr-autent
                      + " -pw "     + v-senha
                      + " -to "     + v-destinatario.

    IF v-ccopia      <> "" THEN ASSIGN v-enviar = v-enviar + " -cc "      + v-ccopia.
    IF v-ccoculta    <> "" THEN ASSIGN v-enviar = v-enviar + " -bcc "     + v-ccoculta.
    IF v-anexo       <> "" THEN ASSIGN v-enviar = v-enviar + " -attach "  + v-anexo.
    IF v-msg-arq     <> "" THEN ASSIGN v-enviar = v-enviar + " -bodyf "   + '"' + v-msg-arq     + '"'.
    IF v-assunto     <> "" THEN ASSIGN v-enviar = v-enviar + " -subject " + '"' + v-assunto     + '"'.
    IF v-anexo-embed <> "" THEN ASSIGN v-enviar = v-enviar + " -embed "   + '"' + v-anexo-embed + '"'.
/*MESSAGE v-enviar
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    OS-COMMAND SILENT VALUE(v-enviar).
END.

/**********************[ Fim do Programa ]**********************/

/*************************[  ]***************************
IF SEARCH(v-blat) <> ? THEN
ASSIGN envio = blat + " -to " + destino + " -cc " + ccopia + " -bcc " + coculta + " -f " + origem + " -subject " + assunto + " -body  " + mensagem + " -server " + hoste + " -u " + usuario + " -pw " + senha + " -html".
ASSIGN envio = blat + " -to " + destino + " -cc " + ccopia + " -bcc " + coculta + " -f " + origem + " -subject " + assunto + " -bodyf " + anexo    + " -server " + hoste + " -u " + usuario + " -pw " + senha + " -html".
    RUN esp-aux\valida-email.p (INPUT emitente.e-mail, OUTPUT email-validado).
    IF email-validado THEN DO:
END.

MESSAGE v-enviar VIEW-AS ALERT-BOX INFO BUTTONS OK.

MESSAGE SEARCH(v-blat) VIEW-AS ALERT-BOX INFO BUTTONS OK.
   RUN utp/ut-msgs.p(INPUT "show", INPUT 4, INPUT "teste").

MESSAGE "Quantidade e-mails" SKIP
        "Enviados: " qt-emails-enviados SKIP
        "Errados.: " qt-emails-errados 
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

OS-COMMAND SILENT VALUE(v-enviar).

*************************[  ]***************************/

/* RUN esapi\email-valida.p (INPUT v-destinatario, OUTPUT v-confirmar). */

/*  ASSIGN v-enviar = v-blat + " -html -u " + v-usr-autent + " -pw " + v-senha + " -server " + v-host + " -to " + v-destinatario. /* + " -html". */
    ASSIGN v-enviar = v-enviar + " -f " + (IF v-remetente <> "" THEN v-remetente ELSE "imatextil@imatextil.com.br").
    IF v-ccopia   <> "" THEN ASSIGN v-enviar = v-enviar + " -cc " + v-ccopia.
    IF v-ccoculta <> "" THEN ASSIGN v-enviar = v-enviar + " -bcc " + v-ccoculta.
    IF v-msg-arq  <> "" THEN
       ASSIGN v-enviar = v-enviar + " -bodyf " + '"' + v-msg-arq + '"'.
    ELSE
       ASSIGN v-enviar = v-enviar + " -body " + IF v-msg-txt <> "" THEN '"' + v-msg-txt + '"' ELSE '" "'.
    
    IF v-assunto     <> "" THEN ASSIGN v-enviar = v-enviar + " -subject " + '"' + v-assunto     + '"'.
    IF v-anexo-embed <> "" THEN ASSIGN v-enviar = v-enviar + " -embed "   + '"' + v-anexo-embed + '"'.
    IF v-anexo       <> "" THEN ASSIGN v-enviar = v-enviar + " -attach "  + v-anexo. 
*/   
/*       ASSIGN v-assunto = '"' + v-assunto + '"'
              v-enviar  = v-enviar + " -subject " + v-assunto. */

/*       ASSIGN v-anexo-embed = '"' + v-anexo-embed + '"'
              v-enviar      = v-enviar + " -embed " + v-anexo-embed. */

/*    OS-COMMAND SILENT VALUE(v-enviar). */

