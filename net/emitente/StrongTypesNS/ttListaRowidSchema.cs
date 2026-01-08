
/*
**
*/

//
// ttListaRowid - Used to generate Strongly Typed DataSet xsd file
//


namespace StrongTypesNS
{
    using System;
    using System.Data;


    public class ttListaRowidDS
    {

        static void Main (string[] args)
        {
            DataSet ds = new DataSet();
            DataRelation drel;
            DataColumn[] parentCols = null, childCols = null;
            DataColumn[] keyCols = null;

            ds.DataSetName = "ttListaRowid" + "DataSet";
            ds.Namespace = "ttListaRowid" + "NS";

            
	    DataTable ttListaRowid = ds.Tables.Add("ttListaRowid");
	    ttListaRowid.Columns.Add("uid", typeof(string));


            ds.WriteXmlSchema("ttListaRowid.xsd");

        }


    }
}
