/**
 *
 */
package br.com.totvs.tds.lsp.server.model.protocol;

/**
 * @author acandido
 *
 */
public class DefragRpoInfo {

	private String connectionToken;
	private String environment;
	private boolean packPatchInfo = true;

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
	 * @return the packPatchInfo
	 */
	public boolean isPackPatchInfo() {
		return packPatchInfo;
	}

	/**
	 * @param packPatchInfo the packPatchInfo to set
	 */
	public void setPackPatchInfo(final boolean packPatchInfo) {
		this.packPatchInfo = packPatchInfo;
	}

}
