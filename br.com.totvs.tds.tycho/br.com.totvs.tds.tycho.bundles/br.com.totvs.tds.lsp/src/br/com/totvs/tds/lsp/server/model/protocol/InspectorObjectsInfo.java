/**
 * 
 */
package br.com.totvs.tds.lsp.server.model.protocol;

/**
 * @author acandido
 *
 */
public class InspectorObjectsInfo {

	private String connectionToken;
	private String environment;
	private boolean includeTres;
	
	/*
	 * 
	 */
	public InspectorObjectsInfo() {
		
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

	public String getEnvironment() {
		return environment;
	}

	public void setEnvironment(String environment) {
		this.environment = environment;
	}

	public boolean isIncludeTres() {
		return includeTres;
	}

	public void setIncludeTres(boolean includeTres) {
		this.includeTres = includeTres;
	}
}
