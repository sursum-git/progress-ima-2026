
/*
**
**    Created by PROGRESS ProxyGen (Versão do Progress 11.7) Tue May 24 20:21:32 BRT 2022
**
*/

//
// Emitente
//




    using System;
    using Progress.Open4GL;
    using Progress.Open4GL.Exceptions;
    using Progress.Open4GL.Proxy;
    using Progress.Open4GL.DynamicAPI;
    using Progress.Common.EhnLog;
    using System.Collections.Specialized;
    using System.Configuration;

    /// <summary>
    /// 
    /// 
    /// 
    /// </summary>
    public class Emitente : AppObject
    {
        private static int proxyGenVersion = 1;
        private const  int PROXY_VER = 5;

    // Create a MetaData object for each temp-table parm used in any and all methods.
    // Create a Schema object for each method call that has temp-table parms which
    // points to one or more temp-tables used in that method call.



        static Emitente()
        {

        }


    //---- Constructors
    public Emitente(Connection connectObj) : this(connectObj, false)
    {       
    }
    
    // If useWebConfigFile = true, we are creating AppObject to use with Silverlight proxy
    public Emitente(Connection connectObj, bool useWebConfigFile)
    {
        try
        {
            if (RunTimeProperties.DynamicApiVersion != PROXY_VER)
                throw new Open4GLException(WrongProxyVer, null);

            if ((connectObj.Url == null) || (connectObj.Url.Equals("")))
                connectObj.Url = "Emitente";

            if (useWebConfigFile == true)
                connectObj.GetWebConfigFileInfo("Emitente");

            initAppObject("Emitente",
                          connectObj,
                          RunTimeProperties.tracer,
                          null, // requestID
                          proxyGenVersion);

        }
        catch (System.Exception e)
        {
            throw e;
        }
    }
   
    public Emitente(string urlString,
                        string userId,
                        string password,
                        string appServerInfo)
    {
        Connection connectObj;

        try
        {
            if (RunTimeProperties.DynamicApiVersion != PROXY_VER)
                throw new Open4GLException(WrongProxyVer, null);

            connectObj = new Connection(urlString,
                                        userId,
                                        password,
                                        appServerInfo);

            initAppObject("Emitente",
                          connectObj,
                          RunTimeProperties.tracer,
                          null, // requestID
                          proxyGenVersion);

            /* release the connection since the connection object */
            /* is being destroyed.  the user can't do this        */
            connectObj.ReleaseConnection();

        }
        catch (System.Exception e)
        {
            throw e;
        }
    }


    public Emitente(string userId,
                        string password,
                        string appServerInfo)
    {
        Connection connectObj;

        try
        {
            if (RunTimeProperties.DynamicApiVersion != PROXY_VER)
                throw new Open4GLException(WrongProxyVer, null);

            connectObj = new Connection("Emitente",
                                        userId,
                                        password,
                                        appServerInfo);

            initAppObject("Emitente",
                          connectObj,
                          RunTimeProperties.tracer,
                          null, // requestID
                          proxyGenVersion);

            /* release the connection since the connection object */
            /* is being destroyed.  the user can't do this        */
            connectObj.ReleaseConnection();
        }
        catch (System.Exception e)
        {
            throw e;
        }
    }

    public Emitente()
    {
        Connection connectObj;

        try
        {
            if (RunTimeProperties.DynamicApiVersion != PROXY_VER)
                throw new Open4GLException(WrongProxyVer, null);

            connectObj = new Connection("Emitente",
                                        null,
                                        null,
                                        null);

            initAppObject("Emitente",
                          connectObj,
                          RunTimeProperties.tracer,
                          null, // requestID
                          proxyGenVersion);

            /* release the connection since the connection object */
            /* is being destroyed.  the user can't do this        */
            connectObj.ReleaseConnection();
        }
        catch (System.Exception e)
        {
            throw e;
        }
    }

        	/// <summary>
	/// 
	/// </summary> 
	public boad098 CreatePO_boad098()
	{
		return new boad098(this);
	}

	/// <summary>
	/// 
	/// </summary> 
	public boad098a CreatePO_boad098a()
	{
		return new boad098a(this);
	}

	/// <summary>
	/// 
	/// </summary> 
	public boad098Af CreatePO_boad098Af()
	{
		return new boad098Af(this);
	}

	/// <summary>
	/// 
	/// </summary> 
	public boad098b CreatePO_boad098b()
	{
		return new boad098b(this);
	}

	/// <summary>
	/// 
	/// </summary> 
	public boad098c CreatePO_boad098c()
	{
		return new boad098c(this);
	}

	/// <summary>
	/// 
	/// </summary> 
	public boad098d CreatePO_boad098d()
	{
		return new boad098d(this);
	}

	/// <summary>
	/// 
	/// </summary> 
	public boad098f CreatePO_boad098f()
	{
		return new boad098f(this);
	}

	/// <summary>
	/// 
	/// </summary> 
	public boad098na CreatePO_boad098na()
	{
		return new boad098na(this);
	}

	/// <summary>
	/// 
	/// </summary> 
	public boad098na2 CreatePO_boad098na2()
	{
		return new boad098na2(this);
	}

	/// <summary>
	/// 
	/// </summary> 
	public boad098o CreatePO_boad098o()
	{
		return new boad098o(this);
	}

	/// <summary>
	/// 
	/// </summary> 
	public boad098q01 CreatePO_boad098q01()
	{
		return new boad098q01(this);
	}

	/// <summary>
	/// 
	/// </summary> 
	public boad098q02 CreatePO_boad098q02()
	{
		return new boad098q02(this);
	}



    }


