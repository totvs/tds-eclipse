package br.com.totvs.tds.lsp.server.model;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "br.com.totvs.tds.lsp.server.model.messages"; //$NON-NLS-1$
	public static String LSConnectionProvider_Could_not_adjust_privileges;
	public static String LSConnectionProvider_LS_insufficient_privileges;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
