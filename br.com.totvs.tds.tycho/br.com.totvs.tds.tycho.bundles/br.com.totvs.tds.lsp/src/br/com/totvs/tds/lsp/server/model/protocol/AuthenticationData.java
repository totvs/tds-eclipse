package br.com.totvs.tds.lsp.server.model.protocol;

public class AuthenticationData {

	private AuthenticationInfo authenticationInfo;

	public AuthenticationData(AuthenticationInfo authenticationInfo) {
		this.authenticationInfo = authenticationInfo;
	}

	/**
	 * @return the authenticationInfo
	 */
	public AuthenticationInfo getAuthenticationInfo() {
		return authenticationInfo;
	}

}
