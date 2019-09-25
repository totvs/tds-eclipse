package br.com.totvs.tds.lsp.server.model.protocol;

public class ValidationInfo {

	private int port;
	private String server;

	/**
	 * 
	 */
	public ValidationInfo() {
	}

	/**
	 * @return the port
	 */
	public int getPort() {
		return port;
	}

	/**
	 * @return the server
	 */
	public String getServer() {
		return server;
	}

	/**
	 * @param port the port to set
	 */
	public void setPort(int port) {
		this.port = port;
	}

	/**
	 * @param server the server to set
	 */
	public void setServer(String server) {
		this.server = server;
	}

	/*
	 * 	languageClient.sendRequest('$totvsserver/validation', {
			validationInfo: {
				server: serverItem.address,
				port: serverItem.port
			}
		}).then((validInfoNode: NodeInfo) => {
			//retornou uma versao valida no servidor.
			const updated = Utils.updateBuildVersion(serverItem.id, validInfoNode.buildVersion);
			serverItem.buildVersion = validInfoNode.buildVersion;
			if (updated) {
				//continua a autenticacao.
				inputConnectionParameters(context, serverItem);
			} else {
				vscode.window.showErrorMessage(localize("tds.webview.serversView.cloudNotConn", "Cloud not connect to server"));
			}
			return;
		}, (err) => {
			vscode.window.showErrorMessage(err.message);
		});

	 */

}
