package br.com.totvs.tds.server.model;

import br.com.totvs.tds.server.interfaces.IRpoFunction;

public class RpoFunction extends RpoObject implements IRpoFunction {

	private RpoSource program = null;
	private String lineNumber;

	@Override
	public RPOTypeElement getType() {

		return RPOTypeElement.FUNCTION;
	}

	/**
	 * @return the program
	 */
	@Override
	public RpoSource getProgram() {
		return program;
	}

	/**
	 * @param program the program to set
	 */
	@Override
	public void setProgram(final RpoSource program) {
		this.program = program;
	}

	/**
	 * @return the lineNumber
	 */
	@Override
	public String getLineNumber() {
		return lineNumber;
	}

	/**
	 * @param lineNumber the lineNumber to set
	 */
	@Override
	public void setLineNumber(final String lineNumber) {
		this.lineNumber = lineNumber;
	}

}
