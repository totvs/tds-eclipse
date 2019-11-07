package br.com.totvs.tds.lsp.server.model.protocol;

public class AppKillUserInfo {
	private String connectionToken;
	private String computerName;
	private String serverName;
	private String userName;
	private long threadId;
	private String message;

	/**
	 * @return the computerName
	 */
	public String getComputerName() {
		return computerName;
	}

	/**
	 * @param computerName the computerName to set
	 */
	public void setComputerName(final String computerName) {
		this.computerName = computerName;
	}

	/**
	 * @return the serverName
	 */
	public String getServerName() {
		return serverName;
	}

	/**
	 * @param serverName the serverName to set
	 */
	public void setServerName(final String serverName) {
		this.serverName = serverName;
	}

	/**
	 * @return the userName
	 */
	public String getUserName() {
		return userName;
	}

	/**
	 * @param userName the userName to set
	 */
	public void setUserName(final String userName) {
		this.userName = userName;
	}

	/**
	 * @return the threadId
	 */
	public long getThreadId() {
		return threadId;
	}

	/**
	 * @param threadId the threadId to set
	 */
	public void setThreadId(final long threadId) {
		this.threadId = threadId;
	}

	/**
	 * @return the message
	 */
	public String getMessage() {
		return message;
	}

	/**
	 * @param message the message to set
	 */
	public void setMessage(final String message) {
		this.message = message;
	}

	/**
	 * @return the connectionToken
	 */
	public String getConnectionToken() {
		return connectionToken;
	}

	/**
	 * @param connectionToken the connectionToken to set
	 */
	public void setConnectionToken(final String connectionToken) {
		this.connectionToken = connectionToken;
	}

}
