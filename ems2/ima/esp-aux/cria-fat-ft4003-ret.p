DEF TEMP-TABLE tt-nota-fisc LIKE nota-fiscal.
DEF TEMP-TABLE tt-it-nota-fisc LIKE it-nota-fisc.


DEF TEMP-TABLE tt-notas-geradas NO-UNDO
    FIELD rw-nota-fiscal AS   ROWID
    FIELD nr-nota        LIKE nota-fiscal.nr-nota-fis
    FIELD seq-wt-docto   LIKE wt-docto.seq-wt-docto.

DEF BUFFER b-estabelec FOR estabelec.

DEF VAR c-natur-oper AS CHAR.
DEF VAR i-nr-seq AS INT.
DEF VAR c-nr-nota-fis AS CHAR.


// Coloque aqui o documento da Importa‡Æo
ASSIGN c-nr-nota-fis = '0141350'.


FIND estabelec WHERE
     estabelec.cod-estab = '5' NO-LOCK NO-ERROR.

FIND b-estabelec WHERE
     b-estabelec.cod-estab = '504' NO-LOCK NO-ERROR.

FIND emitente WHERE
     emitente.cod-emit = b-estabelec.cod-emit NO-LOCK NO-ERROR.

ASSIGN c-natur-oper = '51207M'.

FIND docum-est WHERE
     docum-est.dt-emis >= 03.01.2021 AND
     docum-est.nro-docto = c-nr-nota-fis NO-LOCK NO-ERROR.

CREATE tt-nota-fisc.
ASSIGN tt-nota-fisc.cod-estabel = estabelec.cod-estabel  
       tt-nota-fisc.serie = estabelec.serie
       tt-nota-fisc.nat-oper = c-natur-oper 
       tt-nota-fisc.nome-ab-cli = emitente.nome-abrev
       tt-nota-fisc.dt-emis-nota = docum-est.dt-emis. 

ASSIGN i-nr-seq = 0.

FOR EACH item-doc-est OF docum-est NO-LOCK.
    FIND ITEM WHERE
         ITEM.it-codigo = item-doc-est.it-codigo NO-LOCK NO-ERROR.

    ASSIGN i-nr-seq = i-nr-seq + 10.
    
    CREATE tt-it-nota-fisc.
    ASSIGN tt-it-nota-fisc.nr-seq-fat = i-nr-seq
           tt-it-nota-fisc.cod-estabel = tt-nota-fisc.cod-estabel  
           tt-it-nota-fisc.serie = tt-nota-fisc.serie
           tt-it-nota-fisc.nat-oper = tt-nota-fisc.nat-oper
           tt-it-nota-fisc.dt-emis-nota = tt-nota-fisc.dt-emis-nota
           tt-it-nota-fisc.it-codigo = item-doc-est.it-codigo
           tt-it-nota-fisc.cod-refer = item-doc-est.cod-refer
           tt-it-nota-fisc.cod-depos = 'ARM'
           tt-it-nota-fisc.un-fatur[1] = item.un
           tt-it-nota-fisc.un-fatur[2] = item.un
           tt-it-nota-fisc.qt-faturada[1] = item-doc-est.quantidade
           tt-it-nota-fisc.qt-faturada[2] = item-doc-est.quantidade
           tt-it-nota-fisc.vl-preuni = ROUND(item-doc-est.preco-unit[1],2).
END.

FOR EACH tt-nota-fisc.
    DISP tt-nota-fisc.cod-estabel
         tt-nota-fisc.serie      
         tt-nota-fisc.nome-ab-cli
         tt-nota-fisc.dt-emis-nota
         tt-nota-fisc.nat-operacao.
END.


RUN esapi/cria-nota-ft4003.p (INPUT TABLE tt-nota-fisc,
                              INPUT TABLE tt-it-nota-fisc,
                              OUTPUT TABLE tt-notas-geradas). 

FIND FIRST tt-notas-geradas NO-LOCK NO-ERROR.

IF AVAIL tt-notas-geradas THEN DO.
   MESSAGE tt-notas-geradas.nr-nota
       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
   FIND nota-fiscal WHERE
        nota-fiscal.cod-estabel = '5' AND
        nota-fiscal.serie = '3' AND
        nota-fiscal.nr-nota-fis = tt-notas-geradas.nr-nota
        NO-LOCK NO-ERROR.

   FOR EACH it-nota-fisc OF nota-fiscal SHARE-LOCK.
  
       ASSIGN it-nota-fisc.qt-faturada[2] = it-nota-fisc.qt-faturada[1]
              it-nota-fisc.un[2] = it-nota-fisc.un[1].
   END.

       
END.


