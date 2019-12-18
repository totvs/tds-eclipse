package br.com.totvs.tds.ui.server.wizards;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.wizard.Wizard;

import br.com.totvs.tds.ui.server.ServerUIActivator;
import br.com.totvs.tds.ui.server.nl.Messages;
import br.com.totvs.tds.ui.server.vo.ServerImporExportAttributesVO;

/**
 * Assistente de exportação de servidores.
 *
 * @author acandido
 */
public class ExportServersWizard extends Wizard {

	private final ServerImporExportAttributesVO expAttributes;

	private SelectExportServersPage select;

	public ExportServersWizard(ServerImporExportAttributesVO attributes) {
		setWindowTitle(Messages.ExportServersWizard_export_server_wizard_title);
		expAttributes = attributes;
	}

	// @Override
	@Override
	public void addPages() {
		select = new SelectExportServersPage(expAttributes);
		addPage(select);
	}

	@Override
	public boolean canFinish() {
		return expAttributes.validAttributes();
	}

	protected void doFinish(final IProgressMonitor monitor) throws Throwable {
		// HashMap<String, IItemInfo> serversMap = expAttributes.getItemsSelected();
		// serversMap = getOnlyTopItems(serversMap);
		monitor.beginTask(Messages.ExportServersWizard_exporing_server, 10);
		String targetFile = expAttributes.getTargetFile();
		java.io.File file = new java.io.File(targetFile);
		if (file.exists()) {
			file.delete();
		}
		monitor.subTask(Messages.ExportServersWizard_file_created);
		monitor.worked(1);

//		XMLServerRoot xmlServerRoot = expAttributes.getXMLServerRoot();
//		ExportTool.exportServers(xmlServerRoot, file, monitor);
		monitor.subTask(Messages.ExportServersWizard_exportation_ended);
		monitor.worked(1);

		monitor.done();
		ServerUIActivator.logStatus(IStatus.WARNING, Messages.ExportServersWizard_server_exported_file_warning,
				file.getAbsoluteFile());
	}

	// @Override
	@Override
	public boolean performFinish() {
		IRunnableWithProgress op = new IRunnableWithProgress() {
			@Override
			public void run(IProgressMonitor monitor) {
				try {
					doFinish(monitor);
				} catch (Throwable e) {
					ServerUIActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
				} finally {
					monitor.done();
				}
			}
		};
		try {
			getContainer().run(true, false, op);
		} catch (InterruptedException e) {
			ServerUIActivator.logStatus(IStatus.CANCEL, e.getMessage(), e);
			return false;
		} catch (InvocationTargetException e) {
			ServerUIActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
			return false;
		}

		return true;
	}

}
