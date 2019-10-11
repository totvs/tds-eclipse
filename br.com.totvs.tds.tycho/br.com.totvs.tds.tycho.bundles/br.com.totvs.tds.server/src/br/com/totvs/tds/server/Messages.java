package br.com.totvs.tds.server;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "br.com.totvs.tds.server.messages"; //$NON-NLS-1$
	public static String ServerActivator_Failed_load_server_file;
	public static String ServerActivator_Import_server_list;
	public static String ServerActivator_List_registered_servers_loaded;
	public static String ServerActivator_Reading_registered_servers;
	public static String ServerActivator_Saving_server_list;
	public static String ServerActivator_Server_list_imported;
	public static String ServerActivator_Server_list_saved;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
