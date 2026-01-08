
/*
**
*/

//
// ttGrupoCliente - Used to generate Strongly Typed DataSet xsd file
//


namespace StrongTypesNS
{
    using System;
    using System.Data;


    public class ttGrupoClienteDS
    {

        static void Main (string[] args)
        {
            DataSet ds = new DataSet();
            DataRelation drel;
            DataColumn[] parentCols = null, childCols = null;
            DataColumn[] keyCols = null;

            ds.DataSetName = "ttGrupoCliente" + "DataSet";
            ds.Namespace = "ttGrupoCliente" + "NS";

            
	    DataTable ttGrupoCliente = ds.Tables.Add("ttGrupoCliente");
	    ttGrupoCliente.Columns.Add("codgrcli", typeof(string));


            ds.WriteXmlSchema("ttGrupoCliente.xsd");

        }


    }
}
