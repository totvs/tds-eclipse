package br.com.totvs.tds.lsp.server.model.node;

public class ServerPermissionsNode {
	private Object id;
	private ServerPermissions serverPermissions;
	private String message;

	/**
	 * @return the id
	 */
	public Object getId() {
		return id;
	}

	/**
	 * @param id the id to set
	 */
	public void setId(Object id) {
		this.id = id;
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
	public void setMessage(String message) {
		this.message = message;
	}

	/**
	 * @return the serverPermissions
	 */
	public ServerPermissions getServerPermissions() {
		return serverPermissions;
	}

	/**
	 * @param serverPermissions the serverPermissions to set
	 */
	public void setServerPermissions(ServerPermissions serverPermissions) {
		this.serverPermissions = serverPermissions;
	}
}
