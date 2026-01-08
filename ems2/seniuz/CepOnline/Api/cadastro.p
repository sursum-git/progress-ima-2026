/* Programa: cep_emitente_num.p
**           Verifica emitente sem n£mero no endere‡o
*/

DEF VARIABLE h-acomp        AS HANDLE NO-UNDO.
DEF VARIABLE c-comando      AS CHAR NO-UNDO.

DEF VARIABLE i-ct-arq       AS INT NO-UNDO.
DEF VARIABLE i-ct-emit      AS INT NO-UNDO.

DEF VARIABLE c-end-novo LIKE emitente.endereco.

DEF VARIABLE i-ind          AS INT NO-UNDO.
DEF VARIABLE i-cont         AS INT NO-UNDO.

DEFINE VARIABLE mat-qtd     AS INTEGER INITIAL 31 NO-UNDO.
DEFINE VARIABLE mat-errado  AS CHARACTER EXTENT 31 INITIAL [" 1",
                                                            " 2",
                                                            " 3",
                                                            " 4",
                                                            " 5",
                                                            " 6",
                                                            " 7",
                                                            " 8", 
                                                            " 9",
                                                            " 10",
                                                            " 11",
                                                            " 12",
                                                            " 13",
                                                            " 14",
                                                            " 15",      
                                                            " 16",
                                                            " 17",
                                                            " 18",
                                                            " 19",
                                                            " 20",
                                                            " 21",
                                                            " 22",
                                                            " 23",
                                                            " 24",
                                                            " 25",
                                                            " 26",
                                                            " 27",
                                                            " 28",
                                                            " 29",
                                                            " 30",
                                                            " 31"].
DEFINE VARIABLE mat-certo  AS CHARACTER EXTENT 50 INITIAL  [" PRIMEIRO ",  
                                                            " DOIS ",  
                                                            " TRES ",  
                                                            " QUATRO ",  
                                                            " CINCO ",  
                                                            " SEIS ",  
                                                            " SETE ",  
                                                            " OITO ",  
                                                            " NOVE ",  
                                                            " DEZ ", 
                                                            " ONZE ", 
                                                            " DOZE ", 
                                                            " TREZE ", 
                                                            " QUARTOZE ", 
                                                            " QUINZE ", 
                                                            " DEZESSEIS ", 
                                                            " DEZESSETE ", 
                                                            " DEZOITO ", 
                                                            " DEZNOVE ", 
                                                            " DEZ ", 
                                                            " VINTE E UM ", 
                                                            " VINTE E DOIS ", 
                                                            " VINTE E TRES ", 
                                                            " VINTE E QUATRO ", 
                                                            " VINTE E CINCO ", 
                                                            " VINTE E SEIS ", 
                                                            " VINTE E SETE ", 
                                                            " VINTE E OITO ", 
                                                            " VINTE E NOVE ",
                                                            " TRINTA ",
                                                            " TRINTA E UM "].

FUNCTION f-numerico RETURN INT (INPUT c-aux AS CHAR).
   DO i-cont = 7 TO LENGTH(c-aux):
      IF SUBSTR(c-aux,i-cont,1) >= "0" AND
         SUBSTR(c-aux,i-cont,1) <= "9" THEN
         LEAVE.
   END.
   RETURN i-cont.
END.

FUNCTION f-caracter RETURN INT (INPUT c-aux AS CHAR).
   DO i-ind = i-cont + 2 TO LENGTH(c-aux):
      IF SUBSTR(c-aux,i-ind,1) < "0" OR
         SUBSTR(c-aux,i-ind,1) > "9" THEN
         LEAVE.
   END.
   RETURN i-ind.
END.


run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Pesquisando_CEPs_por_Emitente *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

OUTPUT TO c:\temp\emit_invalido_num.csv CONVERT SOURCE "ibm850".
PUT "emitente.identif.;
    c-end-novo;            
    emitente.endereco;    
    emitente.cod-emitente;
    emitente.bairro;      
    emitente.cidade;      
    emitente.estado;      
    emitente.cep"         
    SKIP.

FOR EACH emitente NO-LOCK.
    RUN pi-acompanhar in h-acomp (input "Emitente: " + STRING(emitente.cod-emitente)). 

    IF INDEX(emitente.endereco,",") = 0 THEN DO.
       ASSIGN c-end-novo = UPPER(emitente.endereco).
       ASSIGN c-end-novo = REPLACE(c-end-novo," SN "," S/N ").
       ASSIGN c-end-novo = REPLACE(c-end-novo," S N "," S/N ").
       ASSIGN c-end-novo = REPLACE(c-end-novo,":"," ").
       ASSIGN c-end-novo = REPLACE(c-end-novo,";"," ").
       ASSIGN c-end-novo = REPLACE(c-end-novo," STA "," SANTA ").
       ASSIGN c-end-novo = REPLACE(c-end-novo,"  "," ").
       ASSIGN c-end-novo = REPLACE(c-end-novo,"CORONEL ","CEL. ").         
       ASSIGN c-end-novo = REPLACE(c-end-novo,"BRIGADEIRO ","BRIG. ").     
       ASSIGN c-end-novo = REPLACE(c-end-novo,"ACADEMICO ","ACAD. ").      
       ASSIGN c-end-novo = REPLACE(c-end-novo,"CONSELHEIRO ","CONS. ").    
       ASSIGN c-end-novo = REPLACE(c-end-novo,"MONSENHOR ","MONS. ").      
       ASSIGN c-end-novo = REPLACE(c-end-novo,"SENADOR ","SEN. ").         
       ASSIGN c-end-novo = REPLACE(c-end-novo,"DEPUTADO ","DEP. ").        
       ASSIGN c-end-novo = REPLACE(c-end-novo,"ALMIRANTE ","ALM. ").       
       ASSIGN c-end-novo = REPLACE(c-end-novo,"GENERAL ","GAL. ").         
       ASSIGN c-end-novo = REPLACE(c-end-novo,"TENENTE ","TEN. ").         
       ASSIGN c-end-novo = REPLACE(c-end-novo,"SARGENTO ","SARG. ").       
       ASSIGN c-end-novo = REPLACE(c-end-novo,"CAPITAO ","CAP. ").         
       ASSIGN c-end-novo = REPLACE(c-end-novo,"MARECHAL ","MAL. ").        
       ASSIGN c-end-novo = REPLACE(c-end-novo,"PREFEITO ","PREF. ").       
       ASSIGN c-end-novo = REPLACE(c-end-novo,"GOVERNADOR ","GOV. ").      
       ASSIGN c-end-novo = REPLACE(c-end-novo,"SENHORA ","SRA. ").         
       ASSIGN c-end-novo = REPLACE(c-end-novo,"PROFESSOR ","PROF. ").      
       ASSIGN c-end-novo = REPLACE(c-end-novo,"PRESIDENTE ","PRES. ").     
       ASSIGN c-end-novo = REPLACE(c-end-novo,"DESEMBARGADOR ","DESEMB. ").
       ASSIGN c-end-novo = REPLACE(c-end-novo,"COMENDADOR ","COMEND. ").   
       ASSIGN c-end-novo = REPLACE(c-end-novo,"DOUTOR ","DR. ").           
       ASSIGN c-end-novo = REPLACE(c-end-novo,"ENGENHEIRO ","ENG. ").     
       ASSIGN c-end-novo = REPLACE(c-end-novo,"VEREADOR ","VER. ").
       ASSIGN c-end-novo = TRIM(c-end-novo).
        
       ASSIGN c-end-novo = TRIM(SUBSTR(c-end-novo,1,f-numerico(c-end-novo) - 1)) + ", " + 
                           SUBSTR(c-end-novo,f-numerico(c-end-novo)).
        
       IF SUBSTR(c-end-novo,f-caracter(c-end-novo)) <> "" THEN
          ASSIGN c-end-novo = TRIM(SUBSTR(c-end-novo,1,f-caracter(c-end-novo) - 1)) + ", " + 
                              SUBSTR(c-end-novo,f-caracter(c-end-novo)).
/* A */        
       IF c-end-novo BEGINS "AV " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"AV ","AV. ").
       
       IF c-end-novo BEGINS "AV " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"AV ","AV. ").
       
       IF c-end-novo BEGINS "AV-" THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"AV-","AV. ").
       
       IF c-end-novo BEGINS "AV:" THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"AV:","AV. ").
       
       IF c-end-novo BEGINS "AVN " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"AVN ","AV. ").
       
       IF c-end-novo BEGINS "AVN " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"AVN ","AV. ").
       
       IF c-end-novo BEGINS "AV, " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"AV, ","AV. ").

       IF c-end-novo BEGINS "AVE " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"AVE ","AV. ").

       IF c-end-novo BEGINS "AAV " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"AAV ","AV. ").

       IF c-end-novo BEGINS "AVENIDA " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"AVENIDA ","AV. ").

       IF c-end-novo BEGINS "AVENIDA," THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"AVENIDA,","AV. ").

       IF c-end-novo BEGINS "AVENIDA:" THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"AVENIDA:","AV. ").
       
       IF c-end-novo BEGINS "ABILIO " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ABILIO ","AV. ABILIO ").
       
       IF c-end-novo BEGINS "ALAMEDA " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ALAMEDA ","ALAM. ").

       IF c-end-novo BEGINS "ALAMEDA," THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ALAMEDA,","ALAM. ").

       IF c-end-novo BEGINS "AL " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"AL ","ALAM. ").

       IF c-end-novo BEGINS "ALA " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ALA ","ALAM. ").

       IF c-end-novo BEGINS "ALA, " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ALA, ","ALAM. ").

       IF c-end-novo BEGINS "ALM " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ALM ","ALAM. ").

       IF c-end-novo BEGINS "ALAM " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ALAM ","ALAM. ").

       IF c-end-novo BEGINS "ALAM " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ALAM ","ALAM. ").

/* B */        
       IF c-end-novo BEGINS "BR, " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"BR, ","ROD. BR-").
       
       IF c-end-novo BEGINS "BR " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"BR ","ROD. BR-").
       
/* E */        
       IF c-end-novo BEGINS "EST BR, " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"EST BR, ","ROD. BR-").

       IF c-end-novo BEGINS "EST " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"EST ","ESTRADA ").

       IF c-end-novo BEGINS "ESTR " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ESTR ","ESTRADA ").

/* f */        
       IF c-end-novo BEGINS "FAZ " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"FAZ ","FAZENDA ").

/* G */        
       IF c-end-novo BEGINS " GET " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo," GET "," GETULIO ").

/* N */        
       IF c-end-novo BEGINS " NR, " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo," NR, ",",").
       
/* P */        
       IF c-end-novo BEGINS "PC " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"PC ","PRACA ").

       IF c-end-novo BEGINS "PC," THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"PC,","PRACA ").

       IF c-end-novo BEGINS "PR," THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"PR,","PRACA ").

       IF c-end-novo BEGINS "P€ " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"P€ ","PRACA ").

       IF c-end-novo BEGINS "PCA " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"PCA ","PRACA ").

       IF c-end-novo BEGINS "PRA " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"PRA ","PRACA ").

       IF c-end-novo BEGINS "P€A " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"P€A ","PRACA ").

       IF c-end-novo BEGINS "PCA, " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"PCA, ","PRACA ").

       IF c-end-novo BEGINS "PRA€A " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"PRA€A ","PRACA ").

       IF c-end-novo BEGINS "PRAQCA " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"PRAQCA ","PRACA ").

       IF c-end-novo BEGINS "PRC " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"PRC ","PRACA ").

       IF c-end-novo BEGINS "PR€ " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"PR€ ","PRACA ").

/* R */        
       IF c-end-novo BEGINS "R " THEN 
          ASSIGN c-end-novo = REPLACE(c-end-novo,"R ","RUA ").

       IF c-end-novo BEGINS "R. " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"R. ","RUA ").

       IF c-end-novo BEGINS "R, " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"R, ","RUA ").

       IF c-end-novo BEGINS "RUA, " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"RUA, ","RUA ").

       IF c-end-novo BEGINS "RUA: " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"RUA: ","RUA ").

       IF c-end-novo BEGINS "RUAS " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"RUAS ","RUA ").
       
       IF c-end-novo BEGINS "RPRES " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"RPRES ","RUA PRES ").
       
       IF c-end-novo BEGINS "RTUPINAMBAS " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"RTUPINAMBAS ","RUA TUPINAMBAS ").
       
       IF c-end-novo BEGINS "RODOVIA " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"RODOVIA ","ROD. ").

       IF c-end-novo BEGINS "RODDOVIA " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"RODDOVIA ","ROD. ").

       IF c-end-novo BEGINS "ROD " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ROD ","ROD. ").

       IF c-end-novo BEGINS "ROD," THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ROD,","ROD. ").
       
       IF c-end-novo BEGINS "ROD. BR," THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ROD. BR,","ROD. BR-").

       IF c-end-novo BEGINS "ROD. PR," THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ROD. PR,","ROD. PR-").

       IF c-end-novo BEGINS "ROD. MG," THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ROD. MG,","ROD. MG-").
       
       IF c-end-novo BEGINS "ROD. BR- " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ROD. BR- ","ROD. BR-").

       IF c-end-novo BEGINS "ROD. PR- " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ROD. PR- ","ROD. PR-").

       IF c-end-novo BEGINS "ROD. MG- " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ROD. MG- ","ROD. MG-").
       
       IF c-end-novo BEGINS "ROD. PR-," THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ROD. PR-,","ROD. PR-").
       
       IF c-end-novo BEGINS "ROD. PE," THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ROD. PE,","ROD. PE-").
       
       IF c-end-novo BEGINS "ROD. MS," THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ROD. MS,","ROD. MS-").
       
       IF c-end-novo BEGINS "ROD. MS," THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ROD. MT,","ROD. MT-").
       
       IF c-end-novo BEGINS "ROD. PRT," THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ROD. PRT,","ROD. PRT-").
       
       IF c-end-novo BEGINS "ROD. RJ," THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ROD. RJ,","ROD. RJ-").
       
       IF c-end-novo BEGINS "ROD. RN," THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ROD. RN,","ROD. RN-").
       
       IF c-end-novo BEGINS "ROD. RS," THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ROD. RS,","ROD. RS-").
       
       IF c-end-novo BEGINS "ROD. SC," THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ROD. SC,","ROD. SC-").
       
       IF c-end-novo BEGINS "ROD. SP," THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ROD. SP,","ROD. SP-").
       
       IF c-end-novo BEGINS "RUA DRUA " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"RUA DRUA ","RUA DR ").
       
       IF c-end-novo BEGINS "RUA VERUA " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"RUA VERUA ","RUA VER. ").
       
       IF c-end-novo BEGINS "ROD. BA," THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ROD. BA,","ROD. BA-").
       
       IF c-end-novo BEGINS "ROD. BR-," THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ROD. BR-,","ROD. BR-").
       
       IF c-end-novo BEGINS "ROD. PE- " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ROD. PE- ","ROD. PE-").
       
       IF c-end-novo BEGINS "ROD. PRT- " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"ROD. PRT- ","ROD. PRT-").
       
/* T */        
       IF c-end-novo BEGINS "TRAVESSA " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"TRAVESSA ","TRAV. ").

       IF c-end-novo BEGINS "TRA DA " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"TRA DA ","PRACA DA ").

/* V */        
       IF c-end-novo BEGINS "VIL " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"VIL ","VILA ").

       IF c-end-novo BEGINS "VL " THEN
          ASSIGN c-end-novo = REPLACE(c-end-novo,"VL ","VILA ").

/* MATCHES */
       
       ASSIGN c-end-novo = TRIM(c-end-novo).   

       DEFINE VARIABLE i-pos AS INTEGER     NO-UNDO.
       DO i-pos = 1 TO mat-qtd:
          IF TRIM(SUBSTR(c-end-novo,1,f-numerico(c-end-novo) - 1)) MATCHES ("*" + mat-errado[i-pos] + " *") OR 
             TRIM(SUBSTR(c-end-novo,1,f-numerico(c-end-novo) - 1)) MATCHES ("*" + mat-errado[i-pos] + ",*") THEN
             ASSIGN c-end-novo = REPLACE(c-end-novo,mat-errado[i-pos],mat-certo[i-pos]).
       END.
       
       PUT IF emitente.identif = 2
              THEN "FORN;"
              ELSE "CLIENTE;"
           c-end-novo ";"
           emitente.endereco ";" 
           emitente.cod-emitente ";"
           emitente.bairro ";"
           emitente.cidade ";"
           emitente.estado ";"
           emitente.cep ";"
           SKIP.
    END.
END.
OUTPUT CLOSE.
