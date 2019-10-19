package br.com.totvs.tds.server.interfaces;

import java.util.List;

public interface IRpoSource extends IRpoElement {

	List<IRpoFunction> getFunctionList();

	IRpoFunction addFunction(String functionName, String lineNumber);

}