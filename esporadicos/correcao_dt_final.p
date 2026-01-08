DEFINE BUFFER bf FOR controle_preco.
DEFINE VARIABLE dtNova AS DATE        NO-UNDO.
OUTPUT TO c:\temp\correcao_dt_final_1.txt.    
FOR EACH controle_preco
    WHERE controle_preco.dt_inicial > controle_preco.dt_final.
    FIND FIRST bf NO-LOCK
        WHERE  controle_preco.tp_preco      = bf.tp_preco
        AND    controle_preco.it_codigo     = bf.it_codigo
        AND    controle_preco.cod_refer     = bf.cod_Refer
        AND    controle_preco.nr_container  = bf.nr_container
        AND    controle_preco.tb_preco_id   = bf.tb_preco_id
        AND    controle_preco.num_nivel     = 2
        AND    bf.dt_inicial > controle_preco.dt_inicial NO-ERROR.
   IF NOT AVAIL bf  THEN DO:
      /*MESSAGE "tipo preco:" controle_preco.tp_preco     SKIP
              "item:" controle_preco.it_codigo    SKIP
              "nr.container:" controle_preco.nr_container SKIP
              "tabela:" controle_preco.tb_preco_id  SKIP
              "dt.inicial:" controle_preco.dt_inicial   SKIP
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
      FIND FIRST bf NO-LOCK
        WHERE  bf.tp_preco      = controle_preco.tp_preco     
        AND    bf.it_codigo     = controle_preco.it_codigo     
        AND    bf.nr_container  = controle_preco.nr_container  
        AND    bf.tb_preco_id   = controle_preco.tb_preco_id  
        AND    bf.num_nivel     = 1
        AND    bf.dt_inicial > controle_preco.dt_inicial NO-ERROR.
        

   END.
      

    ASSIGN dtNova = IF AVAIL  bf THEN bf.dt_inicial - 1 ELSE 01.01.2997.
    DISP
        controle_preco.it_codigo
        controle_preco.cod_refer
        controle_preco.tb_preco_id
        controle_preco.tp_preco
        controle_preco.cod_controle_preco
        IF AVAIL bf THEN  bf.cod_controle_preco ELSE 0
        controle_preco.dt_inicial
        controle_preco.dt_final
        dtNova
        WITH WIDTH 550
        .
    //ASSIGN controle_preco.dt_final = dtNova . 


END.

/*Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cod_controle_preco               inte        i
   20 tp_preco                         inte        i
   30 dt_inicial                       date        i
   40 dt_final                         date        i
   50 it_codigo                        char        i
   60 cod_refer                        char        i
   70 nr_container                     inte        i
   80 vl_dolar                         deci-10
   90 vl_real                          deci-10
  100 cod_usuario_criacao              char
  110 cod_usuario_alteracao            char
  140 dt_hr_criacao                    datetm
  150 dt_hr_alteracao                  datetm
  160 log_vencido                      logi
  170 tb_preco_id                      inte        i
  180 num_nivel                        inte        i
  190 campanha_id                      inte        i
  200 qtd_max_venda                    deci-2
*/
