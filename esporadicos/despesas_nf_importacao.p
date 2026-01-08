DEFINE STREAM s1.
DEFINE STREAM s2.


OUTPUT STREAM s1 TO c:\temp\item-doc-est-cex.txt.
OUTPUT STREAM s2 TO c:\temp\item-doc-est.txt.
PUT  STREAM s1 "serie|docto|emitente|nat.Operacao|sequencia|emitente desp|cod.desp|val.desp|moeda|cotacao|~
    char-1|char-2|dec-1|dec-2|int-1|int-2|log-1|log-2|data-1|data-2|checksum|embarque|estab.|seq.orig| ~
    dat.cotacao | sit.desp" SKIP.
PUT STREAM s2 "item|refer|%.pis|v.pis|%.cofins|v.cofins" SKIP.
FOR EACH docum-est
    WHERE docum-est.nro-docto = '0076982'
    AND docum-est.serie = '3'.
    FOR EACH  item-doc-est OF docum-est.
        EXPORT STREAM s2 DELIMITER "|" item-doc-est.it-codigo item-doc-est.cod-refer item-doc-est.val-aliq-pis item-doc-est.valor-pis 
            item-doc-est.val-aliq-cofins item-doc-est.val-cofins .
    END.
    FOR EACH item-doc-est-cex OF docum-est:
       EXPORT STREAM s1 DELIMITER "|" item-doc-est-cex .
    END.

END.
OUTPUT STREAM s1 CLOSE.
OUTPUT STREAM s2 CLOSE.

/*
Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 serie-docto                      char        im
   20 nro-docto                        char        im
   30 cod-emitente                     inte        im
   40 nat-operacao                     char        im
   50 sequencia                        inte        im
   55 cod-emitente-desp                inte        im
   60 cod-desp                         inte        im
   70 val-desp                         deci-9      m
   80 mo-codigo                        inte        m
   90 cotacao                          deci-8      m
  300 char-1                           char
  310 char-2                           char
  320 dec-1                            deci-8
  330 dec-2                            deci-8
  340 int-1                            inte
  350 int-2                            inte
  360 log-1                            logi
  370 log-2                            logi
  380 data-1                           date
  390 data-2                           date
  400 check-sum                        char
  410 embarque                         char        im
  420 cod-estabel                      char        im
  430 num-seq-orig                     inte
  440 dat-cotac                        date
  450 idi-sit-despes                   inte

Field Name                       Format
-------------------------------- -----------------------------
serie-docto                      X(5)
nro-docto                        X(16)
cod-emitente                     >>>>>>>>9
nat-operacao                     x(06)
sequencia                        >>9
cod-emitente-desp                >>>>>>>>9
cod-desp                         >>,>>9
val-desp                         >>>>>,>>>,>>9.99999
mo-codigo                        >9
cotacao                          >>>,>>9.99999999
char-1                           x(100)
char-2                           x(100)
dec-1                            ->>>>>>>>>>>9.99999999
dec-2                            ->>>>>>>>>>>9.99999999
int-1                            ->>>>>>>>>9
int-2                            ->>>>>>>>>9
log-1                            Sim/NÆo
log-2                            Sim/NÆo
data-1                           99/99/9999
data-2                           99/99/9999
check-sum                        X(20)
embarque                         X(16)
cod-estabel                      x(5)
num-seq-orig                     >>>>9
dat-cotac                        99/99/9999
idi-sit-despes                   >9
*/
