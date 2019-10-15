/**
 *
 */
package br.com.totvs.tds.lsp.server.model.protocol;

/**
 * @author acandido
 *
 */
public class PatchApplyInfo {

	private String connectionToken;
	private String authenticateToken;
	private String environment;
	private String[] patchUris;
	private boolean isLocal;
	private boolean validatePatch;
	private boolean applyOldProgram;

	/**
	 * @return the connectionToken
	 */
	public String getConnectionToken() {
		return connectionToken;
	}

	/**
	 * @param connectionToken the connectionToken to set
	 */
	public void setConnectionToken(final String connectionToken) {
		this.connectionToken = connectionToken;
	}

	/**
	 * @return the authenticateToken
	 */
	public String getAuthenticateToken() {
		return authenticateToken;
	}

	/**
	 * @param authenticateToken the authenticateToken to set
	 */
	public void setAuthenticateToken(final String authenticateToken) {
		this.authenticateToken = authenticateToken;
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
	public void setEnvironment(final String environment) {
		this.environment = environment;
	}

	/**
	 * @return the isLocal
	 */
	public boolean isLocal() {
		return isLocal;
	}

	/**
	 * @param isLocal the isLocal to set
	 */
	public void setLocal(final boolean isLocal) {
		this.isLocal = isLocal;
	}

	/**
	 * @return the validatePatch
	 */
	public boolean isValidatePatch() {
		return validatePatch;
	}

	/**
	 * @param validatePatch the validatePatch to set
	 */
	public void setValidatePatch(final boolean validatePatch) {
		this.validatePatch = validatePatch;
	}

	/**
	 * @return the applyOldProgram
	 */
	public boolean isApplyOldProgram() {
		return applyOldProgram;
	}

	/**
	 * @param applyOldProgram the applyOldProgram to set
	 */
	public void setApplyOldProgram(final boolean applyOldProgram) {
		this.applyOldProgram = applyOldProgram;
	}

	/**
	 * @return the patchUris
	 */
	public String[] getPatchUris() {
		return patchUris;
	}

	/**
	 * @param patchUris the patchUris to set
	 */
	public void setPatchUris(final String[] patchUris) {
		this.patchUris = patchUris;
	}

}
