/**
 * 
 */
package br.com.totvs.tds.lsp.server.model.protocol;

/**
 * @author acandido
 *
 */
public class SlaveInfo {

	private String connectionToken;

	/*
	 * 
	 */
	public SlaveInfo() {
		
	}

	/**
	 * @return the token
	 */
	public String getConnectionToken() {
		return connectionToken;
	}

	/**
	 * @param token the token to set
	 */
	public void setConnectionToken(String token) {
		this.connectionToken = token;
	}
}
