package br.com.totvs.tds.lsp.server.model.protocol;

public class ValidationInfo {

	private int port;
	private String server;
	private int bSecure;

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
