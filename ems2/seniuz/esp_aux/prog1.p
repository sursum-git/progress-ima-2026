
CREATE tab-ocor.
ASSIGN tab-ocor.descricao = "Integrando_Emitente" 
       tab-ocor.i-campo[1] = 11101.  /* Codigo do Emitente */


/*    
FOR EACH tab-ocor WHERE
     tab-ocor.descricao = "Integrando_Emitente" AND
     tab-ocor.i-campo[1] = 11101.
     
DELETE tab-ocor.
      
*/
