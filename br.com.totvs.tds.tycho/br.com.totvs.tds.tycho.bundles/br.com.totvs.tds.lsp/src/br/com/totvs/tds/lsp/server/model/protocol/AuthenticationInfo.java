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
	private int serverType;
	private int bSecure;

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
	public void setAutoReconnect(final boolean autoReconnect) {
		this.autoReconnect = autoReconnect;
	}

	/**
	 * @param buildVersion the buildVersion to set
	 */
	public void setBuildVersion(final String buildVersion) {
		this.buildVersion = buildVersion;
	}

	/**
	 * @param connType the connType to set
	 */
	public void setConnType(final int connType) {
		this.connType = connType;
	}

	/**
	 * @param environment the environment to set
	 */
	public void setEnvironment(final String environment) {
		this.environment = environment;
	}

	/**
	 * @param identification the identification to set
	 */
	public void setIdentification(final String identification) {
		this.identification = identification;
	}

	/**
	 * @param password the password to set
	 */
	public void setPassword(final String password) {
		this.password = password;
	}

	/**
	 * @param port the port to set
	 */
	public void setPort(final int port) {
		this.port = port;
	}

	/**
	 * @param server the server to set
	 */
	public void setServer(final String server) {
		this.server = server;
	}

	/**
	 * @param user the user to set
	 */
	public void setUser(final String user) {
		this.user = user;
	}

	/**
	 * @return the serverType
	 */
	public int getServerType() {
		return serverType;
	}

	/**
	 * @param serverType the serverType to set
	 */
	public void setServerType(final int serverType) {
		this.serverType = serverType;
	}

	/**
	 * @return the bSecure
	 */
	public int getbSecure() {
		return bSecure;
	}

	/**
	 * @param bSecure the bSecure to set
	 */
	public void setbSecure(final int bSecure) {
		this.bSecure = bSecure;
	}

}
