package br.com.totvs.tds.ui.server.exception;

/**
 * Exception to be thrown when a field in a UI has an invalid entry.
 * 
 * @author daniel.yampolschi
 * 
 */
public class InvalidFieldException extends Exception {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	private String errorMessage;

	public InvalidFieldException(final String errorMessage) {
		this.errorMessage = errorMessage;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Throwable#getLocalizedMessage()
	 */
	@Override
	public String getLocalizedMessage() {
		if (errorMessage != null) {
			return errorMessage;
		}
		return super.getMessage();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Throwable#getMessage()
	 */
	@Override
	public String getMessage() {
		if (errorMessage != null) {
			return errorMessage;
		}
		return super.getMessage();
	}

}
