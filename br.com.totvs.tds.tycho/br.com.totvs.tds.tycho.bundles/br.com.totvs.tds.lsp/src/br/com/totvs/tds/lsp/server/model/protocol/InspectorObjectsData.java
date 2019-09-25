/**
 * 
 */
package br.com.totvs.tds.lsp.server.model.protocol;

/**
 * @author acandido
 *
 */
public class InspectorObjectsData {

	private InspectorObjectsInfo inspectorObjectsInfo;
	
	/*
	 * 
	 */
	public InspectorObjectsData(InspectorObjectsInfo inspectorObjectsInfo) {
		this.inspectorObjectsInfo = inspectorObjectsInfo;

	}

	public InspectorObjectsInfo getInspectorObjectsInfo() {
		return inspectorObjectsInfo;
	}

}
