package br.com.totvs.tds.ui.server.internal.dialog;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "br.com.totvs.tds.ui.server.internal.dialog.messages"; //$NON-NLS-1$
	public static String LocalServerDialog_Add_exclusion_pattern;
	public static String LocalServerDialog_Already_exist_exclusion_pattern;
	public static String LocalServerDialog_Exclusion_pattern;
	public static String LocalServerDialog_Invalid_exclusion_pattern;
	public static String LocalServerDialog_Invalid_regexp;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
