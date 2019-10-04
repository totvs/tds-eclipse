package br.com.totvs.tds.lsp.server.model.protocol;

public class KeyInfo {
	private String id;
	private String issued;
	private String expiry;
	private String canOverride;
	private String token;

	/**
	 * @return the id
	 */
	public String getId() {
		return id;
	}

	/**
	 * @param id the id to set
	 */
	public void setId(final String id) {
		this.id = id;
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
	 * @return the canOverride
	 */
	public String getCanOverride() {
		return canOverride;
	}

	/**
	 * @param canOverride the canOverride to set
	 */
	public void setCanOverride(final String canOverride) {
		this.canOverride = canOverride;
	}

	/**
	 * @return the token
	 */
	public String getToken() {
		return token;
	}

	/**
	 * @param token the token to set
	 */
	public void setToken(final String token) {
		this.token = token;
	}

}