package br.com.totvs.tds.server.manager;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "br.com.totvs.tds.server.manager.messages"; //$NON-NLS-1$
	public static String ServerManagerImpl_Cause;
	public static String ServerManagerImpl_Connecting_server_credentials_saved;
	public static String ServerManagerImpl_Connection;
	public static String ServerManagerImpl_Connection_refused_server;
	public static String ServerManagerImpl_Reconnections;
	public static String ServerManagerImpl_Server_connect;
	public static String ServerManagerImpl_Server_manager;
	public static String ServerManagerImpl_Server_manager_version_warning;
	public static String ServerManagerImpl_Startubg_local_server;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
