
/*
**
*/

//
// ttBoErro - Used to generate Strongly Typed DataSet xsd file
//


namespace StrongTypesNS
{
    using System;
    using System.Data;


    public class ttBoErroDS
    {

        static void Main (string[] args)
        {
            DataSet ds = new DataSet();
            DataRelation drel;
            DataColumn[] parentCols = null, childCols = null;
            DataColumn[] keyCols = null;

            ds.DataSetName = "ttBoErro" + "DataSet";
            ds.Namespace = "ttBoErro" + "NS";

            
	    DataTable ttBoErro = ds.Tables.Add("ttBoErro");
	    ttBoErro.Columns.Add("i-sequen", typeof(int));
	    ttBoErro.Columns.Add("cd-erro", typeof(int));
	    ttBoErro.Columns.Add("mensagem", typeof(string));
	    ttBoErro.Columns.Add("parametros", typeof(string));
	    ttBoErro.Columns.Add("errortype", typeof(string));
	    ttBoErro.Columns.Add("errorhelp", typeof(string));
	    ttBoErro.Columns.Add("errorsubtype", typeof(string));


            ds.WriteXmlSchema("ttBoErro.xsd");

        }


    }
}
