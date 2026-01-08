{cdp/cdcfgman.i}

define temp-table tt-dados no-undo 
    field c-estab-ini              as char
    field c-estab-fim              as char
    field i-linha-ini              as integer
    field i-linha-fim              as integer
    field i-ordem-ini              as integer
    field i-ordem-fim              as integer
    field d-data-ini               as date
    field d-data-fim               as date
    field requis-por-ordem         as log init no  /* Cria uma sumariza‡Æo para cada ordem */
    field estado                   as int          /* 1- Inclui, 2- Elimina */
    field cod-versao-integracao    as INT
    field c-camp-ini               as char
    field c-camp-fim               as char
    &if defined(bf_man_206b) &then
    FIELD c-unid-negoc-ini         AS CHARACTER
    FIELD c-unid-negoc-fim         AS CHARACTER
    &ENDIF
    .

define temp-table tt-ord-prod-2
   field nr-ord-produ like ord-prod.nr-ord-produ
   field it-codigo    like ord-prod.it-codigo
   field cod-estabel  like ord-prod.cod-estabel
   field nr-linha     like ord-prod.nr-linha
   field qt-ordem     like ord-prod.qt-ordem
   index id is primary unique nr-ord-produ.

define temp-table tt-req-sum
    field nr-req-sum like req-sum.nr-req-sum
    field it-codigo  like req-sum.it-codigo
  &IF DEFINED (bf_man_sfc_lc) &THEN    
    field cod-refer  like req-sum.cod-refer
  &ENDIF
  &IF DEFINED (bf_man_per_ppm) &THEN
    field per-ppm    like req-sum.per-ppm
  &ENDIF  
    field r-rowid    as rowid
    index id is primary unique nr-req-sum 
                               it-codigo
                             &IF DEFINED (bf_man_sfc_lc) &THEN
                               cod-refer
                             &ENDIF
                             &IF DEFINED (bf_man_per_ppm) &THEN
                               per-ppm
                             &ENDIF
                              .
