package br.com.totvs.tds.ui.server.commands;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.server.interfaces.IServerInfo;
import br.com.totvs.tds.ui.server.handlers.ServerHandler;

/**
 * Aciona a Edição de item selecionado.
 *
 * @author acandido
 */
public final class EditItemCommand extends ServerHandler {

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.
	 * ExecutionEvent)
	 */
	@Override
	public Object execute(final ExecutionEvent event) {
		// recupera o elemento
		IItemInfo element = getSelection();

		// recupera o descritor do novo item
		String itemDescId = event.getParameter("br.com.totvs.tds.ui.server.itemDescriptor"); //$NON-NLS-1$
//		IConfigurationElement ce = ServerHelper.getConfigurationElement(itemDescId);
//		if (ce == null) {
//			ce = ServerHelper.getConfigurationElement(element);
//		}

//		if (element != null) {
//			openEditor(element, ce);
//		}

		return null;
	}

	@Override
	public boolean isEnabled() {
		boolean ret = super.isEnabled();

		if (getSelection().getParent() == null) {
			return false;
		}

//		IConfigurationElement ce = ServerHelper.getConfigurationElement(getSelection());
//		String editor = ce.getAttribute("editorId"); //$NON-NLS-1$
//		ret = (editor != null) && (!editor.isEmpty());

		return ret;
	}

	/**
	 * Abre o editor associado ao item.
	 *
	 * @param itemInfo    Item a ser editado.
	 * @param editorId    ID do editor do item
	 * @param editorInput Nome da classe do editorInput
	 */
	private void openEditor(final IItemInfo itemInfo, IConfigurationElement ce) {
		try {
			IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
			IWorkbenchPage page = window.getActivePage();

			IEditorInput editorInput = null;

			Object input = ce.createExecutableExtension("editorInput"); //$NON-NLS-1$
//			IServerEditorInput editorInputObj = (IServerEditorInput) input;
//			editorInputObj.setItemInfo(itemInfo);

//			editorInput = editorInputObj;

			// determina o editor e editorInput a ser utilizado
			String editorId = ce.getAttribute("editorId"); //$NON-NLS-1$

			page.openEditor(editorInput, editorId);

		} catch (Exception e1) {
			e1.printStackTrace();
			((IServerInfo) itemInfo).setConnected(false);
		}
	}
}
