package br.com.totvs.tds.lsp.server.model.protocol;

public class DisconnectInfo {
	private String connectionToken;
	private String serverName;

	/**
	 * @return the connectionToken
	 */
	public String getConnectionToken() {
		return connectionToken;
	}

	/**
	 * @return the serverName
	 */
	public String getServerName() {
		return serverName;
	}

	/**
	 * @param connectionToken the connectionToken to set
	 */
	public void setConnectionToken(String connectionToken) {
		this.connectionToken = connectionToken;
	}

	/**
	 * @param serverName the serverName to set
	 */
	public void setServerName(String serverName) {
		this.serverName = serverName;
	}
}

//languageClient.sendRequest('$totvsserver/disconnect', {
//	disconnectInfo: {
//		connectionToken: connectedServerItem.token,
//		serverName: connectedServerItem.label
//	}
//}).then((disconnectInfo: DisconnectReturnInfo) => {
//	if (disconnectInfo !== undefined && disconnectInfo.code === undefined) {
//		connectedServerItem = undefined;
//		Utils.clearConnectedServerConfig();
//		if (treeDataProvider !== undefined) {
//			treeDataProvider.refresh();
//		}
//	}
//}, (err) => {
//	Utils.clearConnectedServerConfig();
//	if (treeDataProvider !== undefined) {
//		treeDataProvider.refresh();
//	}
//	handleError(err);
//});
