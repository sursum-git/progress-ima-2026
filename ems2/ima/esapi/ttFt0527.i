DEFINE TEMP-TABLE tt-param NO-UNDO
    FIELD destino          as integer
    FIELD arquivo          as char format "x(35)":U
    FIELD usuario          as char format "x(12)":U
    FIELD data-exec        as date
    FIELD hora-exec        as integer
    FIELD classifica       as integer
    FIELD identific        as char
    FIELD ini-cod-estabel  as char
    FIELD fim-cod-estabel  as char
    FIELD ini-serie        as char
    FIELD fim-serie        as char
    FIELD ini-cdd-embarq   as dec format ">>>>>>>>>>>>>>>9":U
    FIELD fim-cdd-embarq   as dec format ">>>>>>>>>>>>>>>9":U
    FIELD ini-nr-nota-fis  as char
    FIELD fim-nr-nota-fis  as char
    FIELD rs-imprime       as integer
    FIELD banco            as integer
    FIELD cod-febraban     as integer
    FIELD cod-portador     as integer
    FIELD prox-bloq        as char
    FIELD c-instrucao      as char extent 5
    FIELD imprime-bloq     as LOGICAL.

def temp-table tt-param-aux
    field destino                AS INTEGER
    field destino-bloq           AS INTEGER
    field arquivo                AS CHARACTER
    field arquivo-bloq           AS CHARACTER
    field usuario                AS CHARACTER
    field data-exec              AS DATE
    field hora-exec              AS INTEGER
    field parametro              AS LOGICAL
    field formato                AS INTEGER
    field cod-layout             AS CHARACTER
    field des-layout             AS CHARACTER
    field log-impr-dados         AS LOGICAL  
    field v_num_tip_aces_usuar   AS INTEGER
//&IF "{&mguni_version}" >= "2.071" &THEN
    field ep-codigo            LIKE mgcad.empresa.ep-codigo
//&ELSE
//    field ep-codigo              AS INTEGER
//&ENDIF
    field c-cod-estabel        LIKE movdis.nota-fiscal.cod-estabel
    field c-serie              LIKE movdis.nota-fiscal.serie
    field c-nr-nota-fis-ini    LIKE movdis.nota-fiscal.nr-nota-fis
    field c-nr-nota-fis-fim    LIKE movdis.nota-fiscal.nr-nota-fis
    field de-cdd-embarq-ini    LIKE movdis.nota-fiscal.cdd-embarq
    field de-cdd-embarq-fim    LIKE movdis.nota-fiscal.cdd-embarq
    field da-dt-saida          LIKE movdis.nota-fiscal.dt-saida
    field c-hr-saida             AS CHARACTER FORMAT "xx:xx:xx":U INITIAL "000000"
    field banco                  AS INTEGER
    field cod-febraban           AS INTEGER      
    field cod-portador           AS INTEGER      
    field prox-bloq              AS CHARACTER         
    field c-instrucao            AS CHARACTER EXTENT 5
    field imprime-bloq           AS LOGICAL
    field imprime-bloq-danfe     AS LOGICAL
    field rs-imprime             AS INTEGER
    FIELD impressora-so          AS CHARACTER
    FIELD impressora-so-bloq     AS CHARACTER
    FIELD nr-copias              AS INTEGER
    FIELD l-gera-danfe-xml       AS LOGICAL
    FIELD c-dir-hist-xml         AS CHARACTER.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9":U
    field exemplo          as character format "x(30)":U
    index id ordem.
    
    
DEF VAR raw-param AS RAW NO-UNDO.

DEF TEMP-TABLE tt-raw-digita
   FIELD raw-digita AS RAW.
