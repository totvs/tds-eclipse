package br.com.totvs.tds.server.model;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public final class MSRpoObject {

	// private String dateAsString = ""; //$NON-NLS-1$
	private Date date;

	// XXX necessario???
	private List<MSRpoFunction> functionList = new ArrayList<MSRpoFunction>();

	private final SimpleDateFormat myDF = new SimpleDateFormat("dd/MM/yy HH:mm:ss");

	private String name = ""; //$NON-NLS-1$

	private RPOTypeElement type = null; // $NON-NLS-1$

	private boolean visible = true;

	// public void addFunction(final String functionName, final int lineNumber)
	// {
	// MSRpoFunction function = new MSRpoFunction();
	// function.setLineNumber(lineNumber);
	// function.setName(functionName);
	// function.setProgram(this);
	// functionList.add(function);
	// }

	public void addFunction(final MSRpoFunction function) {
		functionList.add(function);
	}

	// /**
	// * @return the functionList
	// */
	// public List<MSRpoFunction> getFunctionList() {
	// return functionList;
	// }
	//
	// /**
	// * @param functionList
	// * the functionList to set
	// */
	// public void setFunctionList(List<MSRpoFunction> functionList) {
	// this.functionList = functionList;
	// }

	/**
	 * @return the date
	 */
	public Date getDate() {
		return date;
	}

	/**
	 * @return the date
	 */
	public String getDateAsString() {
		return myDF.format(date);
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * @return the type
	 */
	public RPOTypeElement getType() {
		return type;
	}

	// /**
	// * @return the dateAsString
	// */
	// public String getDateAsString() {
	// return dateAsString;
	// }
	//
	// /**
	// * @param dateAsString
	// * the dateAsString to set
	// */
	// public void setDateAsString(String dateAsString) {
	// this.dateAsString = dateAsString;
	// }

	/**
	 * @return the visible
	 */
	public boolean isVisible() {
		return visible;
	}

	/**
	 * @param date
	 *            the date to set
	 */
	public void setDate(Date date) {
		this.date = date;
	}

	/**
	 * @param name
	 *            the name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @param type
	 *            the type to set
	 */
	public void setType(String type) {

		for (RPOTypeElement t : RPOTypeElement.values()) {
			if (t.getRpoCode().equalsIgnoreCase(type)) {
				this.type = t;
				return;
			}
		}
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
		return "MSRpoObject [name=" + name + ", type=" + type + ", date=" + getDateAsString() + ", visible=" + visible
				+ "]";
	}

}
