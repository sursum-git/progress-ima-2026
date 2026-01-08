def VAR  p_des_titulo
    as character
    format "x(40)":U
    no-undo.
def VAR p_nom_path
    as character
    format "x(50)":U
    no-undo.
def VAR p_log_cancdo
    as logical
    format "Sim/NÆo"
    no-undo.
def var v_cdn_lista_item
    as Integer
    format ">>>,>>9":U
    no-undo.
def var v_mmp_browse
    as MemPtr
    no-undo.
def var v_mmp_mostra_nom
    as MemPtr
    no-undo.
def var v_mmp_title_pointer
    as MemPtr
    no-undo.


/***************************** Mostra diret¢rios ****************************/
PROCEDURE SHBrowseForFolder EXTERNAL "shell32.dll":
    DEF INPUT  PARAM lpbi                              AS LONG.
    DEF RETURN PARAM lpItemIDList                      AS LONG.
END PROCEDURE. /* SHBrowseForFolder */

/******************************** Busca Path ********************************/
PROCEDURE SHGetPathFromIDList EXTERNAL "shell32.dll":
    DEF INPUT  PARAM v_cdn_lista                       AS LONG.
    DEF OUTPUT PARAM pszPath                           AS CHARACTER.
END PROCEDURE. /* SHGetPathFromIDList */


/************************** Variable Definition End *************************/

set-size(v_mmp_browse)        = 32.
set-size(v_mmp_mostra_nom)    = 260.
set-size(v_mmp_title_pointer) = length(p_des_titulo) + 1.

put-string(v_mmp_title_pointer,1) = p_des_titulo.

put-long(v_mmp_browse, 1) = 0.
put-long(v_mmp_browse, 5) = 0.
put-long(v_mmp_browse, 9) = get-pointer-value(v_mmp_mostra_nom).
put-long(v_mmp_browse,13) = get-pointer-value(v_mmp_title_pointer).
put-long(v_mmp_browse,17) = 1.
put-long(v_mmp_browse,21) = 0.
put-long(v_mmp_browse,25) = 0.
put-long(v_mmp_browse,29) = 0.


run SHBrowseForFolder( input  get-pointer-value(v_mmp_browse), 
                       output v_cdn_lista_item).


/* parse the result: */
if v_cdn_lista_item = 0 then do:
   p_log_cancdo   = yes.
   p_nom_path = "".
end.
else do:
   assign p_log_cancdo = No
          p_nom_path = fill(" ", 260). 

   run SHGetPathFromIDList(v_cdn_lista_item, output p_nom_path).
   MESSAGE  trim(p_nom_path)
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
end.   

/* free memory: */
set-size(v_mmp_browse) = 0.
set-size(v_mmp_mostra_nom) = 0.
set-size(v_mmp_title_pointer) = 0.


  
