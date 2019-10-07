package br.com.totvs.tds.ui.console;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "br.com.totvs.tds.ui.console.messages"; //$NON-NLS-1$
	public static String HttpLink_Navigator;
	public static String LsCaptureLog_12;
	public static String LsCaptureLog_EMPTY_STRING;
	public static String LsCaptureLog_LS_PREFIX;
	public static String LsCaptureLog_TLS;
	public static String UrlMatchListener_Link;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
