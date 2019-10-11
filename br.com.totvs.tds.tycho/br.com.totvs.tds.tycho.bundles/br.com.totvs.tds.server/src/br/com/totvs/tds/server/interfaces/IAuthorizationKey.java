package br.com.totvs.tds.server.interfaces;

public interface IAuthorizationKey {

	String SECURE_NODE = "tds.compile.key"; //$NON-NLS-1$
	String AUTHORIZATION_FILE = "authotization.file"; //$NON-NLS-1$
	String ID_FILE = "id.file"; //$NON-NLS-1$
	String GENERATED_AT = "generated.at"; //$NON-NLS-1$
	String VALID_UNTIL = "valid.until"; //$NON-NLS-1$
	String AUTHORIZATION_CODE = "authotization.code"; //$NON-NLS-1$
	String OVERRIDE_PERMISSION = "override.permission"; //$NON-NLS-1$

	String getMachineId();

	void setAuthorizationFile(String autfile);

	boolean isValid();

	String getErrorMessage();

	String getIdFile();

	String getValidUntil();

	String getAuthorizationCode();

	boolean isOverridePermission();

	String getAuthorizationFile();

	void reset();

	boolean apply();

	String getGeneratedAt();

}
