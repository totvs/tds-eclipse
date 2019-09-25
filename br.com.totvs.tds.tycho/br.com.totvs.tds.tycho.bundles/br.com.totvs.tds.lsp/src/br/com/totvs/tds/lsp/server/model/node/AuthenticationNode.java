package br.com.totvs.tds.lsp.server.model.node;

public class AuthenticationNode {

	private String connectionToken;
	private String id;
	private int osType;
	
	/**
	 * @return the connectionToken
	 */
	public String getConnectionToken() {
		return connectionToken;
	}
	/**
	 * @return the id
	 */
	public String getId() {
		return id;
	}
	/**
	 * @return the osType
	 */
	public int getOsType() {
		return osType;
	}

}
