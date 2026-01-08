
/*
**
*/

//
// ttMaqEpEstAux - Used to generate Strongly Typed DataSet xsd file
//


namespace StrongTypesNS
{
    using System;
    using System.Data;


    public class ttMaqEpEstAuxDS
    {

        static void Main (string[] args)
        {
            DataSet ds = new DataSet();
            DataRelation drel;
            DataColumn[] parentCols = null, childCols = null;
            DataColumn[] keyCols = null;

            ds.DataSetName = "ttMaqEpEstAux" + "DataSet";
            ds.Namespace = "ttMaqEpEstAux" + "NS";

            
	    DataTable ttMaqEpEstAux = ds.Tables.Add("ttMaqEpEstAux");
	    ttMaqEpEstAux.Columns.Add("cd-maquina", typeof(int));
	    ttMaqEpEstAux.Columns.Add("ep-codigo", typeof(string));
	    ttMaqEpEstAux.Columns.Add("cod-estabel", typeof(string));
	    ttMaqEpEstAux.Columns.Add("tp-conexao", typeof(int));


            ds.WriteXmlSchema("ttMaqEpEstAux.xsd");

        }


    }
}
