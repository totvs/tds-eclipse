package br.com.totvs.tds.lsp.server.model.node;

public class ValidKeyNode {

	/**
	 * @return the authorizationToken
	 */
	public String getAuthorizationToken() {
		return authorizationToken;
	}

	/**
	 * @param authorizationToken the authorizationToken to set
	 */
	public void setAuthorizationToken(final String authorizationToken) {
		this.authorizationToken = authorizationToken;
	}

	/**
	 * @return the machineId
	 */
	public String getMachineId() {
		return machineId;
	}

	/**
	 * @param machineId the machineId to set
	 */
	public void setMachineId(final String machineId) {
		this.machineId = machineId;
	}

	/**
	 * @return the issued
	 */
	public String getIssued() {
		return issued;
	}

	/**
	 * @param issued the issued to set
	 */
	public void setIssued(final String issued) {
		this.issued = issued;
	}

	/**
	 * @return the expiry
	 */
	public String getExpiry() {
		return expiry;
	}

	/**
	 * @param expiry the expiry to set
	 */
	public void setExpiry(final String expiry) {
		this.expiry = expiry;
	}

	/**
	 * @return the userId
	 */
	public String getUserId() {
		return userId;
	}

	/**
	 * @param userId the userId to set
	 */
	public void setUserId(final String userId) {
		this.userId = userId;
	}

	/**
	 * @return the buildType
	 */
	public int getBuildType() {
		return buildType;
	}

	/**
	 * @param buildType the buildType to set
	 */
	public void setBuildType(final int buildType) {
		this.buildType = buildType;
	}

	private String authorizationToken;
	private String machineId;
	private String issued;
	private String expiry;
	private String userId;
	private int buildType;

}
