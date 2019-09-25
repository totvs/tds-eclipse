/**
 *
 */
package br.com.totvs.tds.ui.server.commands;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Display;

import br.com.totvs.tds.ui.server.handlers.ServerHandler;
import br.com.totvs.tds.ui.server.vo.ServerImporExportAttributesVO;
import br.com.totvs.tds.ui.server.wizards.ExportServersWizard;

/**
 * @author acandido
 */
public class ExportServerCommand extends ServerHandler {

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.
	 * ExecutionEvent)
	 */
	@Override
	public Object execute(ExecutionEvent event) {
		ServerImporExportAttributesVO impAttributes = new ServerImporExportAttributesVO();
		impAttributes.selection = getSelection();
		impAttributes.setTotvsServers(null);
		ExportServersWizard wizard = new ExportServersWizard(impAttributes);
		WizardDialog dialog = new WizardDialog(Display.getCurrent().getActiveShell(), wizard);
		dialog.setBlockOnOpen(true);
		dialog.open();

		return null;
	}
}
