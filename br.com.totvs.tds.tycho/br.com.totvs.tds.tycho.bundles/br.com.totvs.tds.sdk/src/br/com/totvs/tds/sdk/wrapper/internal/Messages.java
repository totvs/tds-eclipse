package br.com.totvs.tds.sdk.wrapper.internal;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "br.com.totvs.tds.ui.sdk.wrapper.internal.messages"; //$NON-NLS-1$
	public static String ContainerWrapper_Internal;
	public static String FileWrapper_Internal;
	public static String ProjectWrapper_Internal;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
