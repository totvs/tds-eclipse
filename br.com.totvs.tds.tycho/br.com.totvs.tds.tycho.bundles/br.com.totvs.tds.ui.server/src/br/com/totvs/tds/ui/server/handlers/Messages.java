package br.com.totvs.tds.ui.server.handlers;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "br.com.totvs.tds.ui.server.handlers.messages"; //$NON-NLS-1$
	public static String BuildPatchHandler_Build_patch;
	public static String BuildPatchHandler_No_active_servers_found;
	public static String EditItemHandler_Server;
	public static String NewItemHandler_Server;
	public static String StopServerHandler_LocalServer;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
