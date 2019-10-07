package br.com.totvs.tds.ui.debug.helper;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "br.com.totvs.tds.ui.debug.helper.messages"; //$NON-NLS-1$
	public static String PersistableSourceLocator_Debugger;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
