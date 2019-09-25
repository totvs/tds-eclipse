package br.com.totvs.tds.ui.listener;

import org.eclipse.swt.events.SelectionEvent;

public class WorkspaceSelectionEvent {

	private String selectionPath;
	private Object firstResult;
	private Object[] result;
	private int returnCode;
	private SelectionEvent selectionEvent;

	/**
	 * The constructor.
	 * 
	 * @param selectionPath
	 *            - The selection path.
	 * @param firstResult
	 *            - The first result of the selection.
	 * @param result
	 *            - The result array of the selection.
	 * @param returnCode
	 *            - The return code
	 * @param selectionEvent
	 *            - The selection event itself.
	 */
	public WorkspaceSelectionEvent(final String selectionPath, final Object firstResult, final Object[] result,
	        final int returnCode, final SelectionEvent selectionEvent) {
		this.selectionPath = selectionPath;
		this.firstResult = firstResult;
		this.result = result;
		this.returnCode = returnCode;
		this.selectionEvent = selectionEvent;
	}

	public final String getSelectionPath() {
		return selectionPath;
	}

	public final Object getFirstResult() {
		return firstResult;
	}

	public final Object[] getResult() {
		return result;
	}

	public final int getReturnCode() {
		return returnCode;
	}

	public final SelectionEvent getSelectionEvent() {
		return selectionEvent;
	}

}
