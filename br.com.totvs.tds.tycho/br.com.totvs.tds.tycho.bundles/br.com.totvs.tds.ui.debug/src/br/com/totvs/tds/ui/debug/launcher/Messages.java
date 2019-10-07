package br.com.totvs.tds.ui.debug.launcher;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "br.com.totvs.tds.ui.debug.launcher.messages"; //$NON-NLS-1$
	public static String DebugLaunchDelegate_COMMAND_LINE_execution;
	public static String DebugLaunchDelegate_Coverage;
	public static String DebugLaunchDelegate_Debug;
	public static String DebugLaunchDelegate_Executor_parameters_invalid;
	public static String DebugLaunchDelegate_Main_function_required;
	public static String DebugLaunchDelegate_No_environment_selected;
	public static String DebugLaunchDelegate_No_server_selected;
	public static String DebugLaunchDelegate_Process_canceled;
	public static String DebugLaunchDelegate_Run;
	public static String DebugLaunchDelegate_Server_not_connected;
	public static String DebugLaunchDelegate_SmartClient_execution_canceled;
	public static String DebugLaunchDelegate_SmartClient_inaccessible_nonexistent;
	public static String DebugLaunchDelegate_Starting_external_process;
	public static String DebugLaunchDelegate_Starting_SmartClient;
	public static String DebugLaunchDelegate_Validating_configuration;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
