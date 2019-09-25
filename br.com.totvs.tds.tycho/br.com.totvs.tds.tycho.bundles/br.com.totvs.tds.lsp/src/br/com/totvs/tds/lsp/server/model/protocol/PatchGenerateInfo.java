/**
 * 
 */
package br.com.totvs.tds.lsp.server.model.protocol;

/**
 * @author acandido
 *
 */
public class PatchGenerateInfo {

	private String connectionToken;
	private String authorizationToken;
	private String environment;
	private String patchMaster;
	private String patchDest;
	private boolean isLocal;
	private int patchType;
	private String name;
	private String[] patchFiles;
	private String label;

	/*
	 * 
	 */
	public PatchGenerateInfo() {
		
	}

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
	 * @return the patchMaster
	 */
	public String getPatchMaster() {
		return patchMaster;
	}

	/**
	 * @param patchMaster the patchMaster to set
	 */
	public void setPatchMaster(String patchMaster) {
		this.patchMaster = patchMaster;
	}

	/**
	 * @return the patchDest
	 */
	public String getPatchDest() {
		return patchDest;
	}

	/**
	 * @param patchDest the patchDest to set
	 */
	public void setPatchDest(String patchDest) {
		this.patchDest = patchDest;
	}

	/**
	 * @return the isLocal
	 */
	public boolean isLocal() {
		return isLocal;
	}

	/**
	 * @param local the isLocal to set
	 */
	public void setLocal(boolean local) {
		this.isLocal = local;
	}

	/**
	 * @return the patchType
	 */
	public int getPatchType() {
		return patchType;
	}

	/**
	 * @param patchType the patchType to set
	 */
	public void setPatchType(int patchType) {
		this.patchType = patchType;
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * @param name the name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @return the patchFiles
	 */
	public String[] getPatchFiles() {
		return patchFiles;
	}

	/**
	 * @param patchFiles the patchFiles to set
	 */
	public void setPatchFiles(String[] patchFiles) {
		this.patchFiles = patchFiles;
	}

	public String getLabel() {
		return label;
	}

	public void setLabel(String label) {
		this.label = label;
	}

}
