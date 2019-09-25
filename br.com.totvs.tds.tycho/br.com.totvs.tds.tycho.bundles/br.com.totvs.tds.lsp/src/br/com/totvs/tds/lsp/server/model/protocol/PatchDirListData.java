/**
 * 
 */
package br.com.totvs.tds.lsp.server.model.protocol;

/**
 * @author acandido
 *
 */
public class PatchDirListData {

	private String connectionToken;
	private String environment;
	private String folder;
	private boolean includeDir;

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
	 * @return the folder
	 */
	public String getFolder() {
		return folder;
	}

	/**
	 * @param folder the folder to set
	 */
	public void setFolder(String folder) {
		this.folder = folder;
	}

	/**
	 * @return the includeDir
	 */
	public boolean isIncludeDir() {
		return includeDir;
	}

	/**
	 * @param includeDir the includeDir to set
	 */
	public void setIncludeDir(boolean includeDir) {
		this.includeDir = includeDir;
	}
}