package br.com.totvs.tds.ui.sdk.builder;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "br.com.totvs.tds.ui.sdk.builder.messages"; //$NON-NLS-1$
	public static String ToggleTotvsNatureHandler_Internal;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
