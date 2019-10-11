package br.com.totvs.tds.server.launcher;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "br.com.totvs.tds.server.launcher.messages"; //$NON-NLS-1$
	public static String AppLauncher_Application_not_terminated_correctly;
	public static String AppLauncher_Application_terminated;
	public static String AppLauncher_Process_terminated_exit_code;
	public static String AppLauncher_Requested_server_termination;
	public static String AppLauncher_Server_ready;
	public static String AppLauncher_Starting_server;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
