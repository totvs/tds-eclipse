package br.com.totvs.tds.server.model;

public final class MSRpoFunction {

	private int lineNumber = 0;

	private String name = ""; //$NON-NLS-1$

	private MSRpoObject program = new MSRpoObject();

	private boolean visible = true;

	/**
	 * @return the lineNumber
	 */
	public int getLineNumber() {
		return lineNumber;
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * @return the program
	 */
	public MSRpoObject getProgram() {
		return program;
	}

	/**
	 * @return the visible
	 */
	public boolean isVisible() {
		return visible;
	}

	/**
	 * @param lineNumber
	 *            the lineNumber to set
	 */
	public void setLineNumber(int lineNumber) {
		this.lineNumber = lineNumber;
	}

	/**
	 * @param name
	 *            the name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @param program
	 *            the program to set
	 */
	public void setProgram(MSRpoObject program) {
		this.program = program;
	}

	/**
	 * @param visible
	 *            the visible to set
	 */
	public void setVisible(boolean visible) {
		this.visible = visible;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "MSRpoFunction [name=" + name + ", lineNumber=" + lineNumber + ", program=" + program + ", visible=" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				+ visible + "]"; //$NON-NLS-1$
	}

}
