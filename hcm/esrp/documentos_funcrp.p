
/* PROGRAMA: documentos_funcrp.p                                             **
** DATA    : 31/07/2008                                                     **
** AUTOR   : Anderson Fagner Dias Silva                                     **
** OBJETIVO: Rela‡Æo Demitidos                                              **
******************************************************************************/


/* include de controle de versÆo */
{include/i-prgvrs.i pesoembarque 2.04.00.001}
{utp/ut-glob.i}
/* defini‡Æo das temp-tables para recebimento de parƒmetros */

define temp-table tt-param no-undo
    field destino                   as integer
    field arquivo                   as char format "x(35)"
    field usuario                   as char format "x(12)"
    field data-exec                 as date
    field hora-exec                 as integer
    field classifica                as integer
    field desc-classifica           as char format "x(40)"
    field modelo-rtf                as char format "x(35)"
    field l-habilitaRtf             as LOG
    field i-imprime                 as integer
    field log-ped-venda-observacoes as logical
    field rs-ordenar                AS INT
    field fi-data-fim               AS DATE
    FIELD fi-est-ini                AS INT
    FIELD fi-est-fim                AS INT.



DEFINE TEMP-TABLE tt-func NO-UNDO
    FIELD cdn_funcionario       LIKE  funcionario.cdn_funcionario        
    FIELD cod_cart_trab         LIKE  funcionario.cod_cart_trab          
    FIELD nom_pessoa_fisic      LIKE  funcionario.nom_pessoa_fisic       
    FIELD dat_nascimento        LIKE  funcionario.dat_nascimento         
    FIELD cod_pis               LIKE  funcionario.cod_pis                
    FIELD cod_id_feder          LIKE  funcionario.cod_id_feder           
    FIELD cod_id_estad_fisic    LIKE  rh_pessoa_fisic.cod_id_estad_fisic 
    FIELD dat_admis_func        LIKE  funcionario.dat_admis_func
    FIELD serie                 LIKE  funcionario.cod_ser_cart_trab
    .

/*
DEFINE NEW SHARED temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    FIELD it-codigo        LIKE ITEM.it-codigo
    FIELD desc-item         LIKE ITEM.desc-item
    index id ordem.
      */
/*def var raw-param        as raw no-undo.*/

DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita    AS  RAW.



/*DEF VAR  tot-qt-geral  AS DEC FORMAT ">>>,>>>,>>9,99".*/

DEF VAR tot-admit     AS INT.
DEF VAR tot-demit     AS INT.
DEF VAR tot-admit-mes AS INT.
DEF VAR tot-demit-mes AS INT.
DEF VAR tot-selecao   AS INT.
DEF VAR tot-admit-sel AS INT.
DEF VAR tot-demit-sel AS INT.



/* recebimento de parƒmetros */

def input parameter raw-param as raw no-undo.
 

def input parameter TABLE for tt-raw-digita.
DEF NEW GLOBAL SHARED TEMP-TABLE tt-digita-2 NO-UNDO
FIELD codigo AS INT LABEL "C¢digo"
FIELD nome-abrev AS CHAR FORMAT "x(13)" LABEL "Nome Abreviado"
FIELD rz-social  AS CHAR FORMAT "x(20)" LABEL "RazÆo Social".

    
create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
DEF VAR cod-rescis-ini AS INT.
DEF VAR cod-rescis-fim AS INT.    
    
def var h-acomp as handle no-undo.

/* defini‡Æo de frames do relat¢rio */




/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
/* LOCALIZACAO DO NOME DA EMPRESA PARA COMPOR O CABECALHO PADRAO */
FIND empresa NO-LOCK WHERE empresa.ep-codigo = i-ep-codigo-usuario NO-ERROR.
IF NOT AVAIL empresa THEN
   RETURN "ADM-ERROR":U.

/* VALORIZACAO DAS OUTRAS VARIAVEIS QUE COMPOEM O CABECALHO PADRAO */

ASSIGN c-programa     = "documentos_funcrp":U
       c-versao       = "2.04":U
       c-revisao      = ".00.001":U
       c-empresa      = empresa.razao-social
       c-sistema      = "EMS":U.

ASSIGN c-titulo-relat = "Rela‡Æo Demitidos":U.              

view  frame f-cabec.
view  frame f-rodape.


FORM HEADER  "Matric"               AT 01  
             "Nome"                 AT 10  
             "Nasc"                 AT 40  
             "CTPS"                 AT 52  
             "Serie"                AT 63  
             "PIS"                  AT 73  
             "CPF"                  AT 86  
             "RG"                   AT 102 
             "Adim"                 AT 123 
             SKIP 
             "------------------------------------------------------------------------------------------------------------------------------------" AT 1
             WITH FRAME f-cabec.


run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).


IF tt-param.rs-ordenar = 1 THEN DO:
    FOR EACH funcionario WHERE
             funcionario.cdn_estab >= tt-param.fi-est-ini AND 
             funcionario.cdn_estab <= tt-param.fi-est-fim AND 
             funcionario.dat_desligto_func = ? AND
             funcionario.dat_admis_func <= tt-param.fi-data-fim 
             NO-LOCK BREAK BY funcionario.cdn_funcionario
                           BY funcionario.nom_pessoa_fisic   
                           BY funcionario.dat_nascimento.
    
        FIND rh_pessoa_fisic WHERE 
             rh_pessoa_fisic.cod_id_feder  = funcionario.cod_id_feder  
             NO-LOCK NO-ERROR.
        FIND tt-func WHERE tt-func.cod_id_feder = funcionario.cod_id_feder NO-ERROR.
        
            IF NOT AVAIL tt-func THEN DO:
           CREATE tt-func.
           ASSIGN tt-func.cdn_funcionario      = funcionario.cdn_funcionario        
                  tt-func.cod_cart_trab        = funcionario.cod_cart_trab          
                  tt-func.nom_pessoa_fisic     = funcionario.nom_pessoa_fisic       
                  tt-func.dat_nascimento       = funcionario.dat_nascimento         
                  tt-func.cod_pis              = funcionario.cod_pis                
                  tt-func.cod_id_feder         = funcionario.cod_id_feder           
                  tt-func.cod_id_estad_fisic   = rh_pessoa_fisic.cod_id_estad_fisic
                  tt-func.dat_admis_func       = funcionario.dat_admis_func
                  tt-func.serie                = funcionario.cod_ser_cart_trab
               .
        END.
    END.  
END.     
ELSE DO:
    IF tt-param.rs-ordenar = 2 THEN DO:
        FOR EACH funcionario WHERE
                 funcionario.cdn_estab >= tt-param.fi-est-ini AND 
                 funcionario.cdn_estab <= tt-param.fi-est-fim AND 
                 funcionario.dat_desligto_func = ? AND
                 funcionario.dat_admis_func <= tt-param.fi-data-fim 
                 NO-LOCK BREAK BY funcionario.nom_pessoa_fisic   
                               BY funcionario.cdn_funcionario
                               BY funcionario.dat_nascimento.
        
            FIND rh_pessoa_fisic WHERE 
                 rh_pessoa_fisic.cod_id_feder  = funcionario.cod_id_feder  
                 NO-LOCK NO-ERROR.
            FIND tt-func WHERE tt-func.cod_id_feder = funcionario.cod_id_feder NO-ERROR.

            IF NOT AVAIL tt-func THEN DO:
               CREATE tt-func.
               ASSIGN tt-func.cdn_funcionario      = funcionario.cdn_funcionario        
                      tt-func.cod_cart_trab        = funcionario.cod_cart_trab          
                      tt-func.nom_pessoa_fisic     = funcionario.nom_pessoa_fisic       
                      tt-func.dat_nascimento       = funcionario.dat_nascimento         
                      tt-func.cod_pis              = funcionario.cod_pis                
                      tt-func.cod_id_feder         = funcionario.cod_id_feder           
                      tt-func.cod_id_estad_fisic   = rh_pessoa_fisic.cod_id_estad_fisic
                      tt-func.dat_admis_func       = funcionario.dat_admis_func
                      tt-func.serie                = funcionario.cod_ser_cart_trab
                      .
            END.
        END.
    END.
    ELSE DO:
         FOR EACH funcionario WHERE
                  funcionario.cdn_estab >= tt-param.fi-est-ini AND 
                  funcionario.cdn_estab <= tt-param.fi-est-fim AND 
                  funcionario.dat_desligto_func = ? AND
                  funcionario.dat_admis_func <= tt-param.fi-data-fim 
                  NO-LOCK BREAK BY funcionario.dat_nascimento
                                BY funcionario.nom_pessoa_fisic   
                                BY funcionario.cdn_funcionario.
        
             FIND rh_pessoa_fisic WHERE 
                  rh_pessoa_fisic.cod_id_feder  = funcionario.cod_id_feder  
                  NO-LOCK NO-ERROR.
             FIND tt-func WHERE tt-func.cod_id_feder = funcionario.cod_id_feder NO-ERROR.

            IF NOT AVAIL tt-func THEN DO:
               CREATE tt-func.
               ASSIGN tt-func.cdn_funcionario      = funcionario.cdn_funcionario        
                      tt-func.cod_cart_trab        = funcionario.cod_cart_trab          
                      tt-func.nom_pessoa_fisic     = funcionario.nom_pessoa_fisic       
                      tt-func.dat_nascimento       = funcionario.dat_nascimento         
                      tt-func.cod_pis              = funcionario.cod_pis                
                      tt-func.cod_id_feder         = funcionario.cod_id_feder           
                      tt-func.cod_id_estad_fisic   = rh_pessoa_fisic.cod_id_estad_fisic
                      tt-func.dat_admis_func       = funcionario.dat_admis_func
                      tt-func.serie                = funcionario.cod_ser_cart_trab
                      . 
            END.
        END.
    END.    
END.

FOR EACH tt-func NO-LOCK.

    PUT tt-func.cdn_funcionario     AT 01  FORMAT ">>>>>>>9"
        tt-func.nom_pessoa_fisic    AT 10  FORMAT "x(29)" 
        tt-func.dat_nascimento      AT 40  FORMAT "99/99/9999"
        tt-func.cod_cart_trab       AT 52  FORMAT "x(10)"
        tt-func.serie               AT 63  FORMAT "x(8)"
        tt-func.cod_pis             AT 73  
        tt-func.cod_id_feder        AT 86  FORMAT "999.999.999-99"
        tt-func.cod_id_estad_fisic  AT 102 FORMAT "x(19)"
        tt-func.dat_admis_func      AT 123 FORMAT "99/99/9999" 
        
        SKIP .
END.


/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

