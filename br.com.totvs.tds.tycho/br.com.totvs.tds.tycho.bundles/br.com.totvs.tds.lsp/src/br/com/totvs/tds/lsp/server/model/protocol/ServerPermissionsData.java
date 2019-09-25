package br.com.totvs.tds.lsp.server.model.protocol;

public class ServerPermissionsData {

	private ServerPermissionsInfo serverPermissionsInfo;

	/**
	 * @param serverPermissionsInfo
	 */
	public ServerPermissionsData(ServerPermissionsInfo serverPermissionsInfo) {
		this.serverPermissionsInfo = serverPermissionsInfo;
	}

	/**
	 * @return the serverPermissionsInfo
	 */
	public ServerPermissionsInfo getServerPermissionsInfo() {
		return serverPermissionsInfo;
	}

}
