package br.com.totvs.tds.ui.sdk;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "br.com.totvs.tds.ui.sdk.messages"; //$NON-NLS-1$
	public static String SdkUIUtils_Resource_set_to_be_ignored;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
