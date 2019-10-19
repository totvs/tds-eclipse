package br.com.totvs.tds.server.interfaces;

import br.com.totvs.tds.server.model.RpoSource;

public interface IRpoFunction extends IRpoElement {

	RpoSource getProgram();

	void setProgram(RpoSource program);

	String getLineNumber();

	void setLineNumber(String lineNumber);

}