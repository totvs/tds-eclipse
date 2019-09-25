package br.com.totvs.tds.lsp.server.model.node;

public class SlaveNode {
	private Object id;
	private SlaveDataNode[] slaves;
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
	 * @return the slaves
	 */
	public SlaveDataNode[] getSlaves() {
		return slaves;
	}

	/**
	 * @param slaves the slaves to set
	 */
	public void setSlaves(SlaveDataNode[] slaves) {
		this.slaves = slaves;
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

}
