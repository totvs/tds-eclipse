package br.com.totvs.tds.ui.server.wizards;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "br.com.totvs.tds.ui.server.wizards.messages"; //$NON-NLS-1$
	public static String BuildPatchWizard_Compiling_sources_patch_generation;
	public static String BuildPatchWizard_Creating_patch;
	public static String BuildPatchWizard_Error_while_processing_resources;
	public static String BuildPatchWizard_Interrupt_process;
	public static String BuildPatchWizard_Operation_canceled;
	public static String BuildPatchWizard_Patch;
	public static String BuildPatchWizard_Patch_generation_canceled_compilation_errors;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
