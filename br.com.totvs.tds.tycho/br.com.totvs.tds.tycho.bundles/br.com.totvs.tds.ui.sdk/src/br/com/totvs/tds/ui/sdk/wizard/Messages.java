package br.com.totvs.tds.ui.sdk.wizard;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "br.com.totvs.tds.ui.sdk.wizard.messages"; //$NON-NLS-1$
	public static String ProjectWizard_Creating_project;
	public static String ProjectWizard_Project;
	public static String ProjectWizard_Protheus_project_wizard;
	public static String ProjectWizardPage_Name;
	public static String ProjectWizardPage_New_Project;
	public static String ProjectWizardPage_Project_name_invalid;
	public static String ProjectWizardPage_Project_name_required;
	public static String ProjectWizardPage_Totvs_project_wizard;
	public static String ProjectWizardPage_Wizard_creates_TOTVS_project;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
