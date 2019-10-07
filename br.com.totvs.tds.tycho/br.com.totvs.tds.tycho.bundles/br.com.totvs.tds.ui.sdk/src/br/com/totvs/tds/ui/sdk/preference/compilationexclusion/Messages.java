package br.com.totvs.tds.ui.sdk.preference.compilationexclusion;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "br.com.totvs.tds.ui.sdk.preference.compilationexclusion.messages"; //$NON-NLS-1$
	public static String CompileExclusionPreferencePage_Add;
	public static String CompileExclusionPreferencePage_Add_file_exclusion_patterns;
	public static String CompileExclusionPreferencePage_Exclusion_pattern;
	public static String CompileExclusionPreferencePage_Patter_exclusion;
	public static String CompileExclusionPreferencePage_Remove;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
