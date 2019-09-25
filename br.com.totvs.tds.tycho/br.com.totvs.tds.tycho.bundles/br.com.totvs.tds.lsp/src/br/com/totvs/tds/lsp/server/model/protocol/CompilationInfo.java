package br.com.totvs.tds.lsp.server.model.protocol;

public class CompilationInfo {
	private String connectionToken;
	private String authorizationToken;
	private String environment;
	private String[] includeUris;
	private String[] fileUris;
	private CompileOptions compileOptions;

	/**
	 * @return the connectionToken
	 */
	public String getConnectionToken() {
		return connectionToken;
	}

	/**
	 * @param connectionToken the connectionToken to set
	 */
	public void setConnectionToken(String connectionToken) {
		this.connectionToken = connectionToken;
	}

	/**
	 * @return the authorizationToken
	 */
	public String getAuthorizationToken() {
		return authorizationToken;
	}

	/**
	 * @param authorizationToken the authorizationToken to set
	 */
	public void setAuthorizationToken(String authorizationToken) {
		this.authorizationToken = authorizationToken;
	}

	/**
	 * @return the environment
	 */
	public String getEnvironment() {
		return environment;
	}

	/**
	 * @param environment the environment to set
	 */
	public void setEnvironment(String environment) {
		this.environment = environment;
	}

	/**
	 * @return the includeUris
	 */
	public String[] getIncludeUris() {
		return includeUris;
	}

	/**
	 * @param includeUris the includeUris to set
	 */
	public void setIncludeUris(String[] includeUris) {
		this.includeUris = includeUris;
	}

	/**
	 * @return the fileUris
	 */
	public String[] getFileUris() {
		return fileUris;
	}

	/**
	 * @param fileUris the fileUris to set
	 */
	public void setFileUris(String[] fileUris) {
		this.fileUris = fileUris;
	}

	/**
	 * @return the compileOptions
	 */
	public CompileOptions getCompileOptions() {
		return compileOptions;
	}

	/**
	 * @param compileOptions the compileOptions to set
	 */
	public void setCompileOptions(CompileOptions compileOptions) {
		this.compileOptions = compileOptions;
	}

}