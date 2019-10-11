package br.com.totvs.tds.server.jobs;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "br.com.totvs.tds.server.jobs.messages"; //$NON-NLS-1$
	public static String BuildPatchAttributes_Process_type_not_reported;
	public static String BuildPatchAttributes_Server_environment_not_reported;
	public static String BuildPatchAttributes_Uninformed_master_rpo;
	public static String BuildPatchAttributes_Update_package_name_not_entered;
	public static String BuildPatchJob_19;
	public static String BuildPatchJob_By_process;
	public static String BuildPatchJob_Copy_from_to;
	public static String BuildPatchJob_Following_sources_not_found;
	public static String BuildPatchJob_Invalid_generation_attributes;
	public static String BuildPatchJob_Loading_RPO;
	public static String BuildPatchJob_Package_cannot_be_generated;
	public static String BuildPatchJob_Server_returned_error;
	public static String BuildPatchJob_Starting_update_package_generation;
	public static String BuildPatchJob_Unable_save_generated_package;
	public static String BuildPatchJob_Update_package_generation;
	public static String BuildPatchJob_Update_package_generation_succeeded;
	public static String BuildPatchJob_Validating_existence_resource;
	public static String BuildPatchJob_Validating_parameters;
	public static String BuildPatchProcessType_By_comparasion;
	public static String BuildPatchProcessType_From_workarea;
	public static String BuildPatchProcessType_FromRpo;
	public static String BuildPatchProcessType_Undefined;
	public static String CompileJob_Compilation;
	public static String CompileJob_Project;
	public static String CompileJob_Resource_compilation;
	public static String LoadRpoMapJob_Load_rpo;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
