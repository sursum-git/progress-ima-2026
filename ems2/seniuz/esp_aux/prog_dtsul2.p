/* Programa: prog_dtsul2.p
** Objetivo: Listar a estrutura do Menu do EMS.
*/

DEF VAR c-rotina LIKE rot_dtsul.des_rot_dtsul.
DEF VAR c-subrot LIKE sub_rot_dtsul.des_sub_rot_dtsul.

def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.
    
OUTPUT TO c:\temp\menu.csv CONVERT SOURCE "ibm850".
PUT "APL;" "APLICATIVO;" "SIS;" "SISTEMA;" "MOD;" "MODULO;" "PROCEDIMENTO;" 
    "PROGRAMA;" "ROT;" "ROTINA;" "SUB-ROT;" "SUB-ROTINA" SKIP.

FOR EACH modul_rot_proced NO-LOCK:
    FIND rot_dtsul WHERE rot_dtsul.num_rot_dtsul = modul_rot_proced.num_rot_dtsul NO-LOCK NO-ERROR.
    FIND sub_rot_dtsul WHERE sub_rot_dtsul.num_sub_rot_dtsul = modul_rot_proced.num_sub_rot_dtsul NO-LOCK NO-ERROR.
    FIND modul_dtsul WHERE modul_dtsul.cod_modul_dtsul = modul_rot_proced.cod_modul_dtsul NO-LOCK.
    FIND sist_dtsul WHERE sist_dtsul.cod_sist_dtsul = modul_dtsul.cod_sist_dtsul NO-LOCK.
    FIND aplicat_dtsul WHERE aplicat_dtsul.cod_aplicat_dtsul = sist_dtsul.cod_aplicat_dtsul NO-LOCK.
    
    IF AVAIL rot_dtsul THEN
       ASSIGN c-rotina = rot_dtsul.des_rot_dtsul.
    ELSE
       ASSIGN c-rotina = "".

    IF AVAIL sub_rot_dtsul THEN
       ASSIGN c-subrot = sub_rot_dtsul.des_sub_rot_dtsul.
    ELSE
       ASSIGN c-subrot = "".

    put sist_dtsul.cod_aplicat_dtsul ";"       
        aplicat_dtsul.des_aplicat_dtsul ";"    
        
        modul_dtsul.cod_sist_dtsul ";"         
        sist_dtsul.des_sist_dtsul ";"          
        
        modul_rot_proced.cod_modul_dtsul ";"
        modul_dtsul.des_modul_dtsul ";" 

        modul_rot_proced.cod_proced ";"        
        modul_rot_proced.cod_prog_dtsul ";"    
        
        modul_rot_proced.num_rot_dtsul ";"
        c-rotina ";"

        modul_rot_proced.num_sub_rot_dtsul ";"
        c-subrot ";"
        
        SKIP.
    IF modul_rot_proced.num_sub_rot_dtsul <> 0 THEN DO:
       FOR EACH sub_rot_dtsul_proced WHERE sub_rot_dtsul_proced.num_sub_rot_dtsul = modul_rot_proced.num_sub_rot_dtsul
                                     NO-LOCK:
           IF sub_rot_dtsul_proced.num_separador_menu = 0 THEN
              put ";;;;;;;;;;;"
                  sub_rot_dtsul_proced.cod_proced ";"
                  sub_rot_dtsul_proced.cod_prog_dtsul ";"
                  SKIP.
       END.
    END.
END.

OUTPUT CLOSE.

run Execute in h-prog(input "excel.exe",
 			          input "c:\temp\menu.csv").
delete procedure h-prog.

