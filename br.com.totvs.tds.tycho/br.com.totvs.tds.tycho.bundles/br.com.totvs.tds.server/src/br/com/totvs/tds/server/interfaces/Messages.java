package br.com.totvs.tds.server.interfaces;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "br.com.totvs.tds.server.interfaces.messages"; //$NON-NLS-1$
	public static String IServerConstants_2;
	public static String IServerConstants_3;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
