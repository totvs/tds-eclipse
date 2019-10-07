package br.com.totvs.tds.lsp.server;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "br.com.totvs.tds.lsp.server.messages"; //$NON-NLS-1$
	public static String LsServiceImpl_EMPTY_STRING;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
