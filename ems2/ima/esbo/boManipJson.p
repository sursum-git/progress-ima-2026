/*
programa: boManipJson
objetivo: Manipular conteudo de json gerados para adequar os mesmos
para envio ou recebimento. 
*/

{esbo/boManipJson.i}
{esp/util.i}
    
DEFINE TEMP-TABLE ttImprTag
    FIELD tag AS CHAR
    FIELD id  AS INT 
    INDEX primario AS PRIMARY tag id .



DEFINE VARIABLE cTipo    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cArqJson AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lcJson   AS LONGCHAR    NO-UNDO.


//variaveis de controle necess†rias para extrair dados corretamente para tabela tempor†ria
DEFINE VARIABLE qtPosSemAspas           AS INTEGER     NO-UNDO.
DEFINE VARIABLE qtPosSemVirgula         AS INTEGER     NO-UNDO.
DEFINE VARIABLE qtPosEspaco             AS INTEGER     NO-UNDO.
DEFINE VARIABLE qtPosSemAspasVirg       AS INTEGER     NO-UNDO.
DEFINE VARIABLE qtPosSemVirgAspas       AS INTEGER     NO-UNDO.
DEFINE VARIABLE qtPosSemColcheteAbert   AS INTEGER     NO-UNDO.
DEFINE VARIABLE qtPosSemColcheteFech    AS INTEGER     NO-UNDO.
DEFINE VARIABLE qtPosSemChaveAbert      AS INTEGER     NO-UNDO.
DEFINE VARIABLE qtPosSemChaveFech       AS INTEGER     NO-UNDO.
DEFINE VARIABLE qtPosSem2Pontos         AS INTEGER     NO-UNDO.
DEFINE VARIABLE qtPosColcheteChave      AS INTEGER     NO-UNDO.
DEFINE VARIABLE qtPos2PontosColchete    AS INTEGER     NO-UNDO.
DEFINE VARIABLE qtPosRegAtu             AS INTEGER     NO-UNDO.
DEFINE VARIABLE logTextoAberto          AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lVoltarNivelChave       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lVoltarNivelColchete    AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cLetraAnterior          AS CHARACTER  EXTENT 2 NO-UNDO.
DEFINE VARIABLE letraATu                AS CHARACTER  FORMAT 'x(1)' NO-UNDO.
DEFINE VARIABLE letraPos                AS CHARACTER  FORMAT 'x(1)' NO-UNDO.
DEFINE VARIABLE cLetraAtuLog            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE parteCodigo             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lcJsonTT                AS LONGCHAR    NO-UNDO.
DEFINE VARIABLE cListaChavesSemColchete AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iNivel                  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iProxLetraValida        AS INTEGER     NO-UNDO.
DEFINE VARIABLE iTamJson                AS INTEGER     NO-UNDO.
DEFINE VARIABLE iContAtu                AS INTEGER     NO-UNDO.
DEFINE VARIABLE iQtSaldoColchete        AS INTEGER     NO-UNDO.


DEFINE VARIABLE cTagPai                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTag                    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cValor                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAcum                   AS LONGCHAR    NO-UNDO.


FUNCTION getProxLetraValida RETURNS CHAR(iPos AS INT).
    //busca a letra da posiá∆o passada. Caso seja algum caracter abaixo ou igual  a chr(32) busca o proximo. Quando achar um acima de chr(32) grava na variavel iProxLetraValida
    DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.

    IF iProxLetraValida > iPos THEN
       ASSIGN iPos = iProxLetraValida.
    REPEAT iCont = iPos TO iTamJson:
        IF SUBSTR(lcJson,iCont,1) > CHR(32) THEN DO:
           ASSIGN iProxLetraValida = iCont.
           RETURN string(SUBSTR(lcJson,iCont,1)).
        END.                            
    END.

END FUNCTION.

PROCEDURE setArqJson:

    DEFINE INPUT  PARAMETER pArquivo AS CHARACTER   NO-UNDO.
    ASSIGN cArqJson = pArquivo
           cTipo    = 'arq'.
    IF SEARCH(pArquivo) = ? THEN
       MESSAGE 'arquivo n∆o encontrado'
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

END PROCEDURE.


PROCEDURE setLcJson:

    DEFINE INPUT  PARAMETER pLc AS LONGCHAR   NO-UNDO.
    ASSIGN lcJson = pLc
           cTipo  = 'var'.

END PROCEDURE.


PROCEDURE exec:
   
    DEFINE VARIABLE iCont               AS INTEGER     NO-UNDO.
    DEFINE VARIABLE idPriTagPai         AS INTEGER     NO-UNDO.



    RUN _zerarQtsVar.

    IF cTipo = 'arq' THEN DO:
       COPY-LOB FROM FILE cArqJson TO lcJson  CONVERT SOURCE CODEPAGE "UTF-8" .
    END.

    ASSIGN iTamJson = LENGTH(lcJson).
    OUTPUT TO c:\temp\logjson.txt.
    REPEAT iCont = 1 TO iTamJson:
        ASSIGN iContAtu = iCont.
        ASSIGN parteCodigo = ''.
        ASSIGN letraAtu     = substr(lcJson,iCont,1)
               letraPos     = getProxLetraValida(iCont + 1).
               qtPosRegAtu  = qtPosRegAtu + 1 .
        IF logTextoAberto AND letraAtu <> '"' THEN DO:
           ASSIGN cAcum = cAcum + letraAtu
                 parteCodigo = 'texto aberto e diferente de aspas' .
           RUN   _incrQtsVar('').
        END.
        ELSE DO:
            CASE letraAtu:
                WHEN CHR(123) THEN DO: // {
                    ASSIGN parteCodigo = 'texto fechado, chave de abertura'.
                    IF cLetraAnterior[1] = ':' THEN DO:
                       ASSIGN iNivel   = iNivel  + 1
                              lVoltarNivelChave = YES.
                       ASSIGN parteCodigo = parteCodigo + "->letra anterior :(dois pontos)"  .
                       ASSIGN cTagPai  = cTag
                              cTag     = '' .
                       /*RUN _inserirTTJsonConv(iNivel,
                                         cTagPai,
                                         cTag,
                                         cValor
                                          ). */
                       RUN criarRegTagEstrut. 
                    END.
                    ELSE DO:
                        ASSIGN parteCodigo = parteCodigo + "->letra anterior DIFERENTE DE :(dois pontos)"  .
                        /*RUN _inserirTTJsonConv(iNivel,
                                         cTagPai,
                                         cTag,
                                         cValor
                                          ). */
                        IF cLetraAnterior[1] <> '[' THEN DO:
                           ASSIGN parteCodigo = parteCodigo + "->letra anterior DIFERENTE DE [(colchete de abertura)"  .

                           IF cLetraAnterior[1] = '' AND cLetraAnterior[2] = '' THEN DO: //primeira chave de abertura
                              ASSIGN parteCodigo = parteCodigo + "->primeira chave de abertura"  .
                              ASSIGN qtPosRegAtu   = 2.      
                              RUN criarRegTagEstrut.  
                              ASSIGN qtPosRegAtu   = 0.
                           END.
                           ELSE DO:
                              ASSIGN parteCodigo = parteCodigo + "->NAO ê a primeira chave de abertura"  .
                              ASSIGN qtPosRegAtu   = 0
                                  cTagPai       = cTag
                                  cTag          = ''
                                  cAcum         = ''
                                  cValor        = ''.  
                               RUN criarRegTagEstrut.
                           END.
                           
                        END.
                        ELSE DO:
                            ASSIGN parteCodigo = parteCodigo + "->letra anterior IGUAL A [(colchete de abertura)"  .
                            RUN criarRegTagEstrut.
                            //ASSIGN qtPosSemColcheteAbert = 99999.
                        END.                                     
                    END.    
                    
                    
                END. 
                WHEN CHR(125) THEN DO: // }
                    ASSIGN parteCodigo = 'texto fechado - chave de fechamento'.
                    RUN _vencerTagEstrutura('[').
                    RUN   _incrQtsVar('chave_fechada').
                    ASSIGN qtPosRegAtu = 0.
                    IF cLetraAnterior[1] <> CHR(125)  AND cLetraAnterior[1] <> ']'   THEN   DO:
                       ASSIGN parteCodigo = parteCodigo + "-> letra anterior DIFETENTE DE chave e colchete de fechamento".
                       ASSIGN cValor = cAcum.
                       
                       RUN _inserirTTJsonConv(iNivel,
                                            cTagPai,
                                            cTag,
                                            cValor
                                            ).
                    END. 

                    ASSIGN 
                           cTag        = ''
                           cValor      = ''
                           cAcum       = ''.
                    IF lVoltarNivelChave THEN
                       ASSIGN iNivel = iNivel - 1.
                    ASSIGN lVoltarNivelChave = NO.

                    IF cLetraAnterior[1] = ']'  AND cLetraAnterior[2] = CHR(125)   THEN   DO:
                        ASSIGN parteCodigo = parteCodigo + "-> letra anterior 1 = ] and letra anterior 2 = } ".
                        ASSIGN qtPosRegAtu = 2.
                        RUN criarRegTagEstrut.
                        ASSIGN qtPosRegAtu = 0.
                        /*RUN _inserirTTJsonConv(iNivel,
                                            cTagPai,
                                            '',
                                            ''
                                            ).
                        RUN _vencerTagEstrutura(CHR(125)).*/

                    END.
                    ELSE DO:
                        RUN criarRegTagEstrut.
                    END.

                    /*IF cLetraAnterior[1] = ']' OR cLetraAnterior[1] = CHR(125) THEN DO:
                       ASSIGN qtPosSemColcheteFech = 99999999.
                       RUN _inserirTTJsonConv(iNivel,
                                             '',
                                             '',
                                             ''
                                            ).

                    END.*/
                    

                    
                    
                END.                          
                WHEN ':' THEN DO:
                    ASSIGN parteCodigo = "texto fechado - dois pontos".
                    IF letraPos = '[' OR letraPos = CHR(123) THEN DO:
                       ASSIGN parteCodigo = parteCodigo + "-> letra posterior Ç colchete ou chave".
                       ASSIGN cTagPai = cAcum.
                       RUN _inserirTTJsonConv(iNivel,
                                            cTagPai,
                                            '',
                                            ''
                                            ).
                       

                    END.
                    ELSE DO:
                        ASSIGN parteCodigo = parteCodigo + "-> letra posterior NAO Ç colchete ou chave".
                        ASSIGN cTag             = cAcum.
                    END.   
                    ASSIGN cAcum            = ''.
                    RUN   _incrQtsVar('2_pontos').
                           
                END.
                WHEN '[' THEN DO:
                     ASSIGN iQtSaldoColchete = (iQtSaldoColchete +  1).
                     IF iQtSaldoColchete > 1 THEN
                        ASSIGN iQtSaldoColchete = iQtSaldoColchete * 10 .
                     /*MESSAGE 'saldo colchete:' + string(iqtSaldoColchete)
                         VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
                     ASSIGN parteCodigo = "texto fechado - colchete de abertura".
                     ASSIGN
                            //cTagPai = cTag
                            cTag    = ''
                            cAcum   = ''
                            cValor  = ''.
                     //RUN   _incrQtsVar('col_aberto').
                     /*IF cLetraAnterior[1] = ':' THEN DO:
                        RUN _inserirTTJsonConv(iNivel,
                                            cTagPai,
                                            '',
                                            ''
                                            ).

                     END.*/
                     IF cLetraAnterior[1] = ':' THEN
                        ASSIGN iNivel               = iNivel  + 1
                               lVoltarNivelColchete = YES.

                     RUN criarRegTagEstrut.

                     //ASSIGN qtPosRegAtu = 1 .
                     
                     
                    /*
                    IF cLetraAnterior[1] <> ':' THEN DO: // se verdadeiro Ç um jsonarray, sen∆o Ç um array simples dentro de um json
                       RUN   _incrQtsVar('col_aberto').
                       ASSIGN qtPosRegAtu = 0.
                    END.                               
                    ELSE DO:
                        ASSIGN cTagPai = cTag
                               cTag    = ''
                               cAcum   = ''
                               cValor  = ''.
                    END.
                    */
                END.
                WHEN ']' THEN DO:
                    ASSIGN iQtSaldoColchete = iQtSaldoColchete - 1 .
                    ASSIGN parteCodigo = "texto fechado - colchete de fechamento".
                    IF cletraAnterior[1] = CHR(125) THEN DO:
                        ASSIGN parteCodigo = parteCodigo + "->letra anterior igual a chave de fechamento".
                        //RUN   _incrQtsVar('col_fechado').
                       /*MESSAGE ctagpai
                           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
                       RUN _inserirTTJsonConv(iNivel,
                                            cTagPai,
                                            cTag,
                                            cValor
                                            ).
                       RUN _vencerTagEstrutura(CHR(125)).
                       ASSIGN cTag  = ''
                              cAcum = ''.
                      IF  iQtSaldoColchete = 0 THEN DO:
                          RUN criarRegTagEstrut.
                          ASSIGN parteCodigo = parteCodigo + "-> saldo colchete zero".
                      END.
                          

                       //logica para retornar a tag pai ao nivel anterior
                      /* MESSAGE 'tag pai:' cTagPai
                           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
                       FIND FIRST ttJsonConv
                           WHERE trim(ttJsonConv.tagpai) = trim(cTagPai) NO-ERROR.
                       IF AVAIL ttJsonConv THEN DO:
                           ASSIGN idPriTagPai = ttJsonConv.id.
                           /*MESSAGE ttJsonConv.tagpai
                               VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
                           FIND ttJsonConv
                               WHERE ttJsonConv.id = idPriTagPai - 1.
                           IF AVAIL ttJsonConv THEN DO:
                              ASSIGN parteCodigo = parteCodigo + "->achei tag pai:" + ttJsonConv.tagpai.
                              /*MESSAGE 'achei tag avo:' ttJsonConv.tagpai SKIP
                                      cTag SKIP
                                      iContAtu SKIP
                              VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
                              ASSIGN cTagPai = ttJsonConv.tagPai.
                           END.
                       END.
                       ELSE DO:
                           MESSAGE 'NAO ACHEI TAG com:' cTagPai
                               VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                       END.
                    END. 
                    ELSE DO:
                        ASSIGN cValor = cAcum
                               cAcum  = ''.
                        RUN _inserirTTJsonConv(iNivel,
                                            cTagPai,
                                            cTag,
                                            cValor
                                            ).  
                        RUN criarRegTagEstrut.
                    END.
                    IF lVoltarNivelColchete THEN DO:
                       ASSIGN parteCodigo = parteCodigo + "->voltar nivel colchete".
                       ASSIGN iNivel = iNivel - 1 .
                       ASSIGN lVoltarNivelColchete = NO.
                    END.
                       
                     
                END.

                WHEN '"' THEN DO:
                    ASSIGN parteCodigo = 'texto fechado - aspas '.
                    ASSIGN logTextoAberto = NOT logTextoAberto .
                    RUN   _incrQtsVar('aspas').
                    RUN _vencerTagEstrutura(CHR(123)).
                    RUN _vencerTagEstrutura('[').
                    

                END.
                WHEN ' ' OR WHEN CHR(13) OR WHEN CHR(10) THEN DO:
                    ASSIGN parteCodigo = 'texto fechado - espaáo, salto de linha ou inicio de linha '.
                    RUN   _incrQtsVar('espaco').
                END.
                WHEN ','   THEN DO:
                    RUN   _incrQtsVar('virgula'). 
                    ASSIGN parteCodigo = 'texto fechado - virgula'.
                    RUN _vencerTagEstrutura(CHR(123)).
                    RUN _vencerTagEstrutura('[').
                    RUN _vencerTagEstrutura(']').
                    IF cLetraAnterior[1] = ']' AND iQtSaldoColchete > 0 THEN DO:
                       ASSIGN parteCodigo = '->letra anterior ](colchete fechado) e saldo colchete= ' + STRING(iQtSaldoColchete).
                       RUN   _incrQtsVar('col_fechado').
                       RUN criarRegTagEstrut.
                    END.
                    IF cLetraAnterior[1] = ']' THEN
                       ASSIGN iQtSaldoColchete = 0.
                    
                    IF (cLetraAnterior[1] = ']' AND cLetraAnterior[2] = CHR(125)) OR cLetraAnterior[1] = CHR(125) THEN DO:
                       ASSIGN parteCodigo = parteCodigo + '-> (letra anterior = ] e letra anterior 2 igual a chave de fechamento ) ou letra anterior igual a chave de fechamento'.
                       RUN _inserirTTJsonConv(iNivel,
                                          cTagPai,
                                          '',
                                          ''
                                          ).
                    END.
                    ELSE DO:
                        ASSIGN parteCodigo = parteCodigo + '-> DIFERTENTE DE ( (letra anterior = ] e letra anterior 2 igual a chave de fechamento ) ou letra anterior igual a chave de fechamento)'.
                        ASSIGN   cValor       = cAcum.
                        RUN _inserirTTJsonConv(iNivel,
                                          cTagPai,
                                          cTag,
                                          cValor
                                          ). 
                    END.   
                    ASSIGN qtPosRegAtu = 0
                           cTag        = ''
                           cAcum       = ''
                           cValor      = ''.
                    
                    ASSIGN qtPosSemVirgula = 9999999 .
                    
                END.
                OTHERWISE  DO:
                    ASSIGN parteCodigo = "texto fechado - OUTROS CARACTERES".
                    RUN _vencerTagEstrutura(CHR(123)).
                    RUN _vencerTagEstrutura('[').
                    ASSIGN cAcum = cAcum + letraAtu.
                    RUN   _incrQtsVar('').
    
                END.
            END CASE.
        END.
        
        CASE letraAtu:
            WHEN CHR(10) THEN
                ASSIGN cLetraAtuLog = 'quebra_linha'.
            WHEN CHR(13) THEN
                ASSIGN cLetraAtuLog = 'reinicio_Linha'.
            OTHERWISE
               ASSIGN cLetraAtuLog = letraAtu.
        END CASE.
        EXPORT DELIMITER "|"  ASC(letraAtu) cletraAtuLog cLetraAnterior cTag cTagPai cValor qtPosRegAtu qtPosSemVirgula 
            qtPosSemColcheteAbert qtPosSemColcheteFech qtPosSemChaveAbert qtPosSemChaveFech iNivel string(cAcum) FORMAT 'x(100)'  logtextoAberto parteCodigo letraPos 
            iContATu iQtSaldoColchete
            
            .
        IF letraAtu > CHR(32)   THEN DO:
           ASSIGN cLetraAnterior[2]  = cLetraAnterior[1]
                  cLetraAnterior[1]  = letraAtu .
        END.   
    END.   

    OUTPUT CLOSE.
    RUN _retirarColchetesChaves.
END PROCEDURE.

PROCEDURE _incrQtsVar:

    DEFINE INPUT  PARAMETER pVar AS CHARACTER   NO-UNDO.
    ASSIGN qtPosSemAspas           = qtPosSemAspas            + 1
           qtPosSemColcheteAbert   = qtPosSemColcheteAbert    + 1
           qtPosSemChaveAbert      = qtPosSemChaveAbert       + 1 
           qtPosSemChaveFech       = qtPosSemChaveFech        + 1
           qtPosSemColcheteFech    = qtPosSemColcheteFech     + 1
           qtPosSemVirgula         = qtPosSemVirgula          + 1
           qtPosSem2Pontos         = qtPosSem2Pontos          + 1 
           qtPosSemAspasVirg       = qtPosSemAspasVirg        + 1  
           qtPosSemVirgAspas       = qtPosSemVirgAspas        + 1  
           qtPosEspaco             = 0 . 

    CASE pVar:
        WHEN 'aspas' THEN
            ASSIGN  qtPosSemAspas        = 0.
        WHEN 'aspas_virgula' THEN
            ASSIGN  qtPosSemAspasVirg    = 0.
        WHEN 'virgula_aspas' THEN
            ASSIGN  qtPosSemVirgAspas    = 0.
        WHEN 'col_aberto' THEN
            ASSIGN qtPosSemColcheteAbert = 0.
        WHEN 'col_fechado' THEN
            ASSIGN qtPosSemColcheteFech  = 0.
        WHEN 'virgula' THEN
            ASSIGN qtPosSemVirgula       = 0.
        WHEN 'chave_aberta' THEN
            ASSIGN qtPosSemChaveAbert    = 0.
        WHEN 'chave_fechada' THEN
            ASSIGN qtPosSemChaveFech     = 0.
        WHEN '2_pontos' THEN
            ASSIGN qtPosSem2Pontos       = 0.
        WHEN 'espaco'   THEN
            ASSIGN qtPosEspaco = qtPosEspaco + 1 .
    END CASE.
                                               
END PROCEDURE.


PROCEDURE _vencerTagEstrutura:

    DEFINE INPUT  PARAMETER pTag AS CHARACTER   NO-UNDO.

    IF cLetraAnterior[1] = pTag THEN  DO:

       CASE pTag:
           WHEN CHR(123) THEN
               ASSIGN qtPosSemChaveAbert   = 9999999.
           WHEN '[' THEN
              ASSIGN qtPosSemColcheteAbert = 9999999.  
            WHEN CHR(125) THEN
               ASSIGN qtPosSemChaveFech    = 9999999.
            WHEN ']' THEN
              ASSIGN qtPosSemColcheteFech  = 9999999.  

       END CASE.
    END.        

END PROCEDURE.

PROCEDURE _zerarQtsVar:

    ASSIGN qtPosSemAspas           = 0
           qtPosSemColcheteAbert   = 0
           qtPosSemChaveAbert      = 0
           qtPosSemChaveFech       = 0
           qtPosSemColcheteFech    = 0
           qtPosSemVirgula         = 0
           qtPosEspaco             = 0
           qtPosSem2Pontos         = 0
           qtPosSemAspasVirg       = 0
           qtPosSemVirgAspas       = 0
           logTextoAberto          = FALSE.

    
END PROCEDURE.



PROCEDURE _inserirTTJsonConv:

    DEFINE INPUT  PARAMETER pNivel  AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pTagPai AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pTag    AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pValor  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE idNovo AS INTEGER     NO-UNDO.
    /*DEFINE INPUT  PARAMETER pColchAbert AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pColchFech  AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pChaveAbert AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pChaveFech  AS LOGICAL     NO-UNDO.*/
    DEFINE BUFFER bf FOR ttJsonConv.

    FIND LAST bf USE-INDEX indid NO-ERROR.
    IF AVAIL bf THEN
       ASSIGN idNovo = bf.id .
    
    ASSIGN idNovo = idNovo +  1.

    CREATE ttJsonConv.
    ASSIGN ttJsonConv.id                = idNovo
           ttJsonConv.nivel             = pNivel
           ttJsonConv.tagPai            = pTagPai
           ttJsonConv.tag               = pTag
           ttJsonConv.valor             = pValor 
           ttJsonConv.logColchAbert     = qtPosSemColcheteAbert < qtPosRegAtu
           ttJsonConv.logColchFech      = qtPosSemColcheteFech  < qtPosRegAtu
           ttJsonConv.logChaveAbert     = qtPosSemChaveAbert    < qtPosRegAtu
           ttJsonConv.logChaveFech      = qtPosSemChaveFech     < qtPosRegAtu
           ttJsonConv.logVirgula        = qtPosSemVirgula       < qtPosRegAtu 
           ttJsonConv.letraCriacao      = letraAtu
           ttJsonConv.letraAnterior     = cLetraAnterior
           ttJsonConv.tamReg            = qtPosRegAtu
           ttJsonConv.tamVirgula        = qtPosSemVirgula
           ttJsonConv.tamColcheteAb     = qtPosSemColcheteAbert
           ttJsonConv.tamColcheteFe     = qtPosSemColcheteFech
           ttJsonConv.tamChaveAb        = qtPosSemChaveAbert
           ttJsonConv.tamChaveFe        = qtPosSemChaveFech
           ttJsonConv.posicao           = iContAtu.
        .

        /*IF iContAtu = 389 THEN
           MESSAGE ttJsonConv.logColchAbert SKIP
                   ttJsonConv.id
               VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
END PROCEDURE.


PROCEDURE getTTJsonConv:

    DEFINE OUTPUT PARAMETER  TABLE FOR  ttJsonConv .

END PROCEDURE.



PROCEDURE converterTTEmJson:

   
    ASSIGN lcJsonTT = ''.
    FOR EACH ttJsonConv:
        /*MESSAGE  ttJsonConv.id SKIP
             string(lcJsonTT)
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
        IF ttJsonConv.logColchAbert THEN
            RUN incrValor(INPUT-OUTPUT lcJsonTT,'[','').
        IF ttJsonConv.logChaveAbert THEN
           RUN incrValor(INPUT-OUTPUT lcJsonTT,CHR(123),  chr(10) + CHR(9)).
        IF ttJsonConv.tagPai <> '' AND ttJsonCOnv.tag = '' THEN DO:
            /*MESSAGE  'tag.pai:' ttJsonConv.tagPai
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
           FIND FIRST ttImprTag
               WHERE  trim(ttImprTag.tag) = trim(ttJsonConv.tagPai)
               NO-ERROR.
           IF NOT AVAIL ttImprTag THEN DO:
              RUN incrValor(INPUT-OUTPUT lcJsonTT, chr(13) + '"' + trim(ttJsonConv.tagpai) + '":','').
              RUN _inserirTtImprTag(INPUT ttJsonConv.id, ttJsonConv.tagPai).
           END.
        END.   
        IF ttJsonConv.tag <> '' THEN
           RUN incrValor(INPUT-OUTPUT lcJsonTT, CHR(9) +  '"' +  trim(ttJsonConv.tag) + '":"'  + ttJsonConv.valor + '"' + IF ttJsonConv.logVirgula THEN "," ELSE ""   , CHR(13)).
        
        IF ttJsonConv.tag = '' AND ttJsonConv.valor <> '' AND ttJsonConv.tagPai <> '' THEN DO:
           RUN incrValor(INPUT-OUTPUT lcJsonTT, CHR(9) +   ttJsonConv.valor    , '').

        END.
        IF ttJsonConv.logColchFech THEN
           RUN incrValor(INPUT-OUTPUT lcJsonTT,']', chr(9)).

        IF ttJsonConv.logChaveFech THEN
           RUN incrValor(INPUT-OUTPUT lcJsonTT,CHR(125), chr(9) + CHR(13)).
        
        IF ttJsonConv.logVirgula AND ttJsonConv.tag = '' AND ttJsonConv.tagPai = ''  THEN
           RUN incrValor(INPUT-OUTPUT lcJsonTT,',', chr(13)).

        IF ttJsonConv.logVirgula  AND ttJsonConv.tagPai <> ''  AND ttJsonConv.tag = '' THEN DO:
           FIND FIRST ttImprTag
               WHERE  trim(ttImprTag.tag) = trim(ttJsonConv.tagPai) NO-ERROR.
           IF AVAIL ttImprTag THEN
              RUN incrValor(INPUT-OUTPUT lcJsonTT,',', chr(13)).  
        END.

           




    END.

END PROCEDURE.

/*
PROCEDURE converterTTEmJson:

   
    ASSIGN lcJsonTT = ''.
    FOR EACH ttJsonConv
        BREAK BY ttJsonConv.tagpai:
        IF FIRST-OF(ttJsonConv.tagPai) THEN DO:
           IF ttJsonConv.logColchAbert THEN
              RUN incrValor(INPUT-OUTPUT lcJsonTT,'[', chr(10)  + CHR(13)).
           IF ttJsonConv.logChaveAbert THEN
              RUN incrValor(INPUT-OUTPUT lcJsonTT,CHR(123), chr(10)  + CHR(13)).
           IF ttJsonConv.tagPai <> '' THEN
              RUN incrValor(INPUT-OUTPUT lcJsonTT, '"' + ttJsonConv.tagpai + '"', chr(10)  + CHR(13)).
        END.
        IF ttJsonConv.logColchAbert THEN
           RUN incrValor(INPUT-OUTPUT lcJsonTT,'[', chr(10)  + CHR(13)).
        IF ttJsonConv.logChaveAbert THEN
           RUN incrValor(INPUT-OUTPUT lcJsonTT,CHR(123), chr(10)  + CHR(13)).

        IF ttJsonConv.tag <> '' THEN
           RUN incrValor(INPUT-OUTPUT lcJsonTT, '"' +  ttJsonConv.tag + '":"'  + ttJsonConv.valor + '"' + IF ttJsonConv.logVirgula THEN "," ELSE ""   , chr(10)  + CHR(13)).

        IF ttJsonConv.logColchAbert THEN
           RUN incrValor(INPUT-OUTPUT lcJsonTT,CHR(125), chr(10)  + CHR(13)).
        IF ttJsonConv.logChaveAbert THEN
           RUN incrValor(INPUT-OUTPUT lcJsonTT,']', chr(10)  + CHR(13)).


    END.

END PROCEDURE.
*/

PROCEDURE retJsonTT:

    DEFINE OUTPUT PARAMETER lcJson AS LONGCHAR   NO-UNDO.
    ASSIGN lcJson = lcJsonTT .

END PROCEDURE.

PROCEDURE retJsonObject:




END PROCEDURE.


PROCEDURE convJson2Arq:

    DEFINE INPUT  PARAMETER pArq AS CHARACTER   NO-UNDO.

    COPY-LOB lcJsonTT TO FILE pArq.

END PROCEDURE.

PROCEDURE setListaChavesSemColchete:

    DEFINE INPUT  PARAMETER pLista AS CHARACTER   NO-UNDO.
    ASSIGN cListaChavesSemColchete = pLista.

END PROCEDURE.


PROCEDURE _retirarColchetesChaves:

    FOR EACH ttJsonConv
        WHERE lookup(ttJsonConv.tagPai,cListaChavesSemColchete) > 0 :
        ASSIGN ttJsonConv.logColchAbert  = NO
               ttJsonConv.logColchFech   = NO .
    END.


END PROCEDURE.



PROCEDURE _inserirTtImprTag:

        DEFINE INPUT  PARAMETER pId     AS INTEGER     NO-UNDO.
        DEFINE INPUT  PARAMETER pTag    AS CHARACTER   NO-UNDO.

       CREATE ttImprTag.
       ASSIGN ttImprTag.id  = pId
              ttImprTag.tag = pTag .
 
       

END PROCEDURE.



PROCEDURE criarRegTagEstrut:

     CASE letraATu:
         WHEN CHR(123) THEN
            RUN   _incrQtsVar('chave_aberta').    
         WHEN CHR(125) THEN
            RUN   _incrQtsVar('chave_fechada').
         WHEN '[' THEN
            RUN   _incrQtsVar('col_aberto').    
         WHEN ']' THEN
            RUN   _incrQtsVar('col_fechado').    

     END CASE.
     /*IF iContAtu = 389 THEN
        MESSAGE qtPosSemColcheteAbert  SKIP
         qtPosRegAtu
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
     RUN _inserirTTJsonConv(iNivel,cTagPai,'','').
     RUN _vencerTagEstrutura(letraAtu).
     ASSIGN qtPosRegAtu = 0 .


END PROCEDURE.

