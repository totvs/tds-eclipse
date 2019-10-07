package br.com.totvs.tds.ui.sdk.widget;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "br.com.totvs.tds.ui.sdk.widget.messages"; //$NON-NLS-1$
	public static String IncludeDataModel_No_definition_files;
	public static String IncludeDataModel_No_folder;
	public static String IncludeDataModel_Not_found;
	public static String IncludeListLabelProvider_Global;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
