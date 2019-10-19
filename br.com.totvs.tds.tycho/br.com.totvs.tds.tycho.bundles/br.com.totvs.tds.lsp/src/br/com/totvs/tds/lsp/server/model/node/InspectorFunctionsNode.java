package br.com.totvs.tds.lsp.server.model.node;

public class InspectorFunctionsNode {
	private String message;
	private String[] functions;

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
	 * @return the functions
	 */
	public String[] getFunctions() {
		return functions;
	}

	/**
	 * @param functions the functions to set
	 */
	public void setFunctions(final String[] functions) {
		this.functions = functions;
	}

}
