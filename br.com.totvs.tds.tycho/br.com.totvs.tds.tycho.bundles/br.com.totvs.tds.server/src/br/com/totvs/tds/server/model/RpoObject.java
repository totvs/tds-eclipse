package br.com.totvs.tds.server.model;

import java.text.ParseException;
import java.util.Date;

import br.com.totvs.tds.server.interfaces.IRpoElement;

public class RpoObject implements IRpoElement {

	private String name = ""; //$NON-NLS-1$
	private Date date = null; // $NON-NLS-1$
	private boolean visible = true;

	/**
	 * @return the name
	 */
	@Override
	public String getName() {
		return name;
	}

	/**
	 * @param name the name to set
	 */
	@Override
	public void setName(final String name) {
		this.name = name.trim();
	}

	/**
	 * @return the date
	 */
	@Override
	public Date getDate() {
		return date;
	}

	/**
	 * @param date the date to set
	 */
	@Override
	public void setDate(final Date date) {
		this.date = date;
	}

	/**
	 * @return the visible
	 */
	@Override
	public boolean isVisible() {
		return visible;
	}

	/**
	 * @param visible the visible to set
	 */
	@Override
	public void setVisible(final boolean visible) {
		this.visible = visible;
	}

	/**
	 *
	 */
	@Override
	public String toString() {

		return this.getName();
	}

	@Override
	public boolean equals(final Object obj) {

		return toString().equalsIgnoreCase(obj.toString());
	}

	@Override
	public RpoTypeElement getType() {

		return RpoTypeElement.UNKNOWN;
	}

	@Override
	public void setDate(final String date) {
		try {
			this.date = SDF.parse(date.trim());
		} catch (final ParseException e) {
			e.printStackTrace();
		}
	}

}