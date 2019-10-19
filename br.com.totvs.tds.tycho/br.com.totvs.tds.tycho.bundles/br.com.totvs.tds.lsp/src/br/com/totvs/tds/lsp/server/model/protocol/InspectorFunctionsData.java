package br.com.totvs.tds.lsp.server.model.protocol;

/**
 * @author acandido
 *
 */
public class InspectorFunctionsData {
	private InspectorFunctionsInfo inspectorFunctionsInfo;

	public InspectorFunctionsData(final InspectorFunctionsInfo inspectorFunctionsInfo) {
		this.setInspectorFunctionsInfo(inspectorFunctionsInfo);
		// TODO Auto-generated constructor stub
	}

	/**
	 * @return the inspectorFunctionsInfo
	 */
	public InspectorFunctionsInfo getInspectorFunctionsInfo() {
		return inspectorFunctionsInfo;
	}

	/**
	 * @param inspectorFunctionsInfo the inspectorFunctionsInfo to set
	 */
	public void setInspectorFunctionsInfo(final InspectorFunctionsInfo inspectorFunctionsInfo) {
		this.inspectorFunctionsInfo = inspectorFunctionsInfo;
	}

}
