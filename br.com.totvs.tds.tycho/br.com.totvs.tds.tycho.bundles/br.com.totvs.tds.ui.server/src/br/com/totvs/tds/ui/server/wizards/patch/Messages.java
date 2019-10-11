package br.com.totvs.tds.ui.server.wizards.patch;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "br.com.totvs.tds.ui.server.wizards.patch.messages"; //$NON-NLS-1$
	public static String PatchComparisonPage_Generation_by_comparison;
	public static String PatchComparisonPage_Generation_by_comparison_title;
	public static String PatchComparisonPage_Master_rpo;
	public static String PatchComparisonPage_Master_rpo_error;
	public static String PatchComparisonPage_Wizard_assist_you_generating_patch_by_comparison;
	public static String PatchSelectServerPage_Attribute_requited;
	public static String PatchSelectServerPage_Build_patch;
	public static String PatchSelectServerPage_Build_patch_title;
	public static String PatchSelectServerPage_Environemnt_not_selected;
	public static String PatchSelectServerPage_Environment;
	public static String PatchSelectServerPage_Environments;
	public static String PatchSelectServerPage_File_already_exists;
	public static String PatchSelectServerPage_File_path_invalid_nonexistent;
	public static String PatchSelectServerPage_File_will_overwritten;
	public static String PatchSelectServerPage_File_without_extension;
	public static String PatchSelectServerPage_Leave_blank_default_name;
	public static String PatchSelectServerPage_Local;
	public static String PatchSelectServerPage_More_one_selected_environment_files_overwritten;
	public static String PatchSelectServerPage_No_active_servers;
	public static String PatchSelectServerPage_Override_file;
	public static String PatchSelectServerPage_Patch_generation;
	public static String PatchSelectServerPage_Process;
	public static String PatchSelectServerPage_Remote;
	public static String PatchSelectServerPage_SaveTo;
	public static String PatchSelectServerPage_Select_generation_process;
	public static String PatchSelectServerPage_Selected_process_not_allow_more_one_environment;
	public static String PatchSelectServerPage_Server;
	public static String PatchSelectServerPage_Server_not_connected;
	public static String PatchSelectServerPage_This_wizard_assist_you_build_patch;
	public static String PatchWorkareaPage_Check_files_rpo;
	public static String PatchWorkareaPage_Clear_selection;
	public static String PatchWorkareaPage_Clear_selection_hint;
	public static String PatchWorkareaPage_Compile_selected_resources;
	public static String PatchWorkareaPage_Compile_selected_resources_hint;
	public static String PatchWorkareaPage_Currently_selected_resource_contains_error;
	public static String PatchWorkareaPage_Currently_selected_resource_contains_error_use_compile;
	public static String PatchWorkareaPage_Generating_patch_will_take_codes_in_RPO;
	public static String PatchWorkareaPage_Generation_from_workarea;
	public static String PatchWorkareaPage_Select_one_or_more_project;
	public static String PatchWorkareaPage_Selected_resources;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
