package br.com.totvs.tds.server.interfaces;

import java.text.SimpleDateFormat;
import java.util.Date;

import br.com.totvs.tds.server.model.RPOTypeElement;

public interface IRpoElement {

	static final SimpleDateFormat SDF = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss"); //$NON-NLS-1$
	static final SimpleDateFormat SDF_COMPARE = new SimpleDateFormat("yyyyMMddHHmmss"); //$NON-NLS-1$

	public void setName(String name);

	public String getName();

	public RPOTypeElement getType();

	public boolean isVisible();

	Date getDate();

	void setDate(Date date);

	void setDate(String date);

	void setVisible(boolean visible);

	public void setType(RPOTypeElement type);

}