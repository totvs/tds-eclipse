package br.com.totvs.tds.server.model;

public enum RPOTypeElement {
	FUNCTION("0", Messages.RPOTypeElement_Function), //$NON-NLS-1$
	OBJECT("1", Messages.RPOTypeElement_Rpo_object), //$NON-NLS-1$
	PROGRAM("2", Messages.RPOTypeElement_Source), //$NON-NLS-1$
	RESOURCE("3", Messages.RPOTypeElement_Resource), UNKNOWN("4", Messages.RPOTypeElement_Unknow); //$NON-NLS-1$ //$NON-NLS-3$

	final private String rpoCode;
	final private String title;

	private RPOTypeElement(String rpoCode, String title) {
		this.rpoCode = rpoCode;
		this.title = title;
	}

	/**
	 * @return the rpoCode
	 */
	public String getRpoCode() {

		return rpoCode;
	}

	public String getTitle() {

		return this.title;
	}
}
