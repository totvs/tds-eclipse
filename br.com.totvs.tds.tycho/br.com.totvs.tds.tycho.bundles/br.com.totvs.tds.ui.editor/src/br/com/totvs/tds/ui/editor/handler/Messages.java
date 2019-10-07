package br.com.totvs.tds.ui.editor.handler;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "br.com.totvs.tds.ui.editor.handler.messages"; //$NON-NLS-1$
	public static String CompileHandler_Autosave_warning;
	public static String CompileHandler_Compilation;
	public static String CompileHandler_File;
	public static String CompileHandler_Ignore_resource;
	public static String CompileHandler_Project;
	public static String CompileHandler_TDS_Compilation;
	public static String EditorHandler_Compilation;
	public static String EditorHandler_No_environment_selected;
	public static String EditorHandler_Permission;
	public static String EditorHandler_Server_not_connected;
	public static String EditorHandler_Server_not_selected;
	public static String EditorHandler_User_not_allowed_compile;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
