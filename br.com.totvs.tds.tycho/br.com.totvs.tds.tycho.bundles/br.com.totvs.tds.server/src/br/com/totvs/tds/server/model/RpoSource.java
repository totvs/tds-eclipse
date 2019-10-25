package br.com.totvs.tds.server.model;

import java.util.ArrayList;
import java.util.List;

import br.com.totvs.tds.server.interfaces.IRpoFunction;
import br.com.totvs.tds.server.interfaces.IRpoSource;

public class RpoSource extends RpoObject implements IRpoSource {

	private List<IRpoFunction> functionList = new ArrayList<IRpoFunction>();

	@Override
	public IRpoFunction addFunction(final String functionName, final String lineNumber) {
		final IRpoFunction function = new RpoFunction();

		function.setLineNumber(lineNumber);
		function.setName(functionName);
		function.setProgram(this);
		functionList.add(function);

		return function;
	}

	public void addFunction(final RpoFunction function) {
		functionList.add(function);
	}

	@Override
	public List<IRpoFunction> getFunctionList() {
		return functionList;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.osgi.messageservice.entity.IRpoElement#getType()
	 */
	@Override
	public RpoTypeElement getType() {
		return RpoTypeElement.PROGRAM;
	}

}
