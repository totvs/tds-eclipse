package br.com.totvs.tds.server.interfaces;

public interface IAuthorizationKey {

	String SECURE_NODE = "tds.compile.key";
	String AUTHORIZATION_FILE = "authotization.file";
	String ID_FILE = "id.file";
	String GENERATED_AT = "generated.at";
	String VALID_UNTIL = "valid.until";
	String AUTHORIZATION_CODE = "authotization.code";
	String OVERRIDE_PERMISSION = "override.permission";

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
