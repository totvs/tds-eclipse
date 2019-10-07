package br.com.totvs.tds.ui.sdk.job;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "br.com.totvs.tds.ui.sdk.job.messages"; //$NON-NLS-1$
	public static String SearchIncludeFoldersJob_15;
	public static String SearchIncludeFoldersJob_Folders_were_found;
	public static String SearchIncludeFoldersJob_No_definition_files_found;
	public static String SearchIncludeFoldersJob_Partial_search;
	public static String SearchIncludeFoldersJob_Search_completed;
	public static String SearchIncludeFoldersJob_Search_folders_definition_files;
	public static String SearchIncludeFoldersJob_Searching;
	public static String SearchIncludeFoldersJob_Target_resource_not_folder;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
