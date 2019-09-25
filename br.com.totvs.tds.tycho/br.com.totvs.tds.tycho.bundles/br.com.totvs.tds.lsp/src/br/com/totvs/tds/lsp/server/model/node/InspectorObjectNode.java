package br.com.totvs.tds.lsp.server.model.node;

import java.util.List;

public class InspectorObjectNode {
	private Object id;
	private List<String> objects;
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
	public void setId(final Object id) {
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
	public void setMessage(final String message) {
		this.message = message;
	}

	public List<String> getObjects() {
		return objects;
	}

	public void setObjects(final List<String> objects) {
		this.objects = objects;
	}

	@SuppressWarnings("unchecked")
	public void setObjects(final Object objects) {
		this.objects = (List<String>) objects;
	}

}
