package br.com.totvs.tds.ui.sdk.dialog;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "br.com.totvs.tds.ui.sdk.dialog.messages"; //$NON-NLS-1$
	public static String ExclusionPatternDialog_Add_exclusion_pattern;
	public static String ExclusionPatternDialog_Exclusion_Pattern;
	public static String ExclusionPatternDialog_Existing_exclusion_pattern;
	public static String ExclusionPatternDialog_Invalid_regular_expression;
	public static String ExclusionPatternDialog_Please_enter_valid_exclusion_pattern;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
