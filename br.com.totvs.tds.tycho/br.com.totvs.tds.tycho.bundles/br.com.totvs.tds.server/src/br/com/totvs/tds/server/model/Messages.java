package br.com.totvs.tds.server.model;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "br.com.totvs.tds.server.model.messages"; //$NON-NLS-1$
	public static String AppServerInfo_File_required_SmartClient;
	public static String AuthorizationKey_Authorization_file_not_found_or_invalid;
	public static String AuthorizationKey_Invalid_authorization;
	public static String AuthorizationKey_Unable_retrieve_data;
	public static String GroupInfo_Already_exist_name;
	public static String ItemInfo_Name_already_exists;
	public static String RootInfo_Servers;
	public static String RPOTypeElement_Function;
	public static String RPOTypeElement_Resource;
	public static String RPOTypeElement_Rpo_object;
	public static String RPOTypeElement_Source;
	public static String RPOTypeElement_Unknow;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
