/**
 * 
 */
package br.com.totvs.tds.lsp.server.model.protocol;

/**
 * @author acandido
 *
 */
public class AuthenticationInfo {

	private boolean autoReconnect;
	private String buildVersion;
	private int connType;
	private String environment;
	private String identification;
	private String password;
	private int port;
	private String server;
	private String user;

	/*
	 * 
	 */
	public AuthenticationInfo() {
		
	}

	/**
	 * @return the buildVersion
	 */
	public String getBuildVersion() {
		return buildVersion;
	}

	/**
	 * @return the connType
	 */
	public int getConnType() {
		return connType;
	}

	/**
	 * @return the environment
	 */
	public String getEnvironment() {
		return environment;
	}

	/**
	 * @return the identification
	 */
	public String getIdentification() {
		return identification;
	}

	/**
	 * @return the password
	 */
	public String getPassword() {
		return password;
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
	 * @return the user
	 */
	public String getUser() {
		return user;
	}

	/**
	 * @return the autoReconnect
	 */
	public boolean isAutoReconnect() {
		return autoReconnect;
	}

	/**
	 * @param autoReconnect the autoReconnect to set
	 */
	public void setAutoReconnect(boolean autoReconnect) {
		this.autoReconnect = autoReconnect;
	}

	/**
	 * @param buildVersion the buildVersion to set
	 */
	public void setBuildVersion(String buildVersion) {
		this.buildVersion = buildVersion;
	}

	/**
	 * @param connType the connType to set
	 */
	public void setConnType(int connType) {
		this.connType = connType;
	}

	/**
	 * @param environment the environment to set
	 */
	public void setEnvironment(String environment) {
		this.environment = environment;
	}

	/**
	 * @param identification the identification to set
	 */
	public void setIdentification(String identification) {
		this.identification = identification;
	}

	/**
	 * @param password the password to set
	 */
	public void setPassword(String password) {
		this.password = password;
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

	/**
	 * @param user the user to set
	 */
	public void setUser(String user) {
		this.user = user;
	}
	
}

//languageClient.sendRequest('$totvsserver/authentication', {
//authenticationInfo: {
//	connType: 1,
//	identification: serverItem.id,
//	server: serverItem.address,
//	port: serverItem.port,
//	buildVersion: serverItem.buildVersion,
//	environment: environment,
//	user: user,
//	password: password,
//	autoReconnect: true
//}
//}).then((authenticationNode: AuthenticationNode) => {
//let token: string = authenticationNode.connectionToken;
//if (token) {
//	//vscode.window.showInformationMessage('Server ' + serverItem.label + ' connected!');
//	Utils.saveSelectServer(serverItem.id, token, serverItem.label, environment, user);
//	Utils.saveConnectionToken(serverItem.id, token, environment);
//	if (treeDataProvider !== undefined) {
//		connectedServerItem = serverItem;
//		connectedServerItem.currentEnvironment = environment;
//		connectedServerItem.token = token;
//		treeDataProvider.refresh();
//	}
//	return true;
//} else {
//	vscode.window.showErrorMessage(localize("tds.webview.serversView.errorConnServer", 'Error connecting server'));
//	return false;
//}
//}, err => {
//vscode.window.showErrorMessage(err);
//});
//}
//
//export class AuthenticationNode {
//// These properties come directly from the language server.
//id: any;
//osType: number;
//connectionToken: string;
//}
//}

