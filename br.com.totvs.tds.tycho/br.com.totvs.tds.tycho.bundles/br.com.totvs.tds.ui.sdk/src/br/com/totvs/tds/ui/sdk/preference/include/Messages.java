package br.com.totvs.tds.ui.sdk.preference.include;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "br.com.totvs.tds.ui.sdk.preference.include.messages"; //$NON-NLS-1$
	public static String IncludePreferencePage_Definition_file_search_folders;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
