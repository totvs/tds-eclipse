package br.com.totvs.tds.ui.monitor.handlers;

import java.io.File;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;

import br.com.totvs.tds.server.IServerInfo;
import br.com.totvs.tds.server.dba.connector.IDbaServerConnector;
import br.com.totvs.tds.server.dba.jobs.DBAccessAtividadesJob;
import br.com.totvs.tds.server.factory.ServerConnectorFactory;
import br.com.totvs.tds.server.ui.Messages;
import br.com.totvs.tds.server.ui.internal.monitors.views.ItemMonitor.MonitorType;
import br.com.totvs.tds.server.ui.monitor.IItemMonitor;
import br.com.totvs.tds.ui.dialog.TDSFileDialog;

public class ActivitiesConnections extends ServerMonitorHandler {

	private boolean isEnabled = false;

	@Override
	public Object execute(final ExecutionEvent event) throws ExecutionException {
		DateFormat dateFormat = new SimpleDateFormat("ddMMyyyy"); //$NON-NLS-1$
		Date date = new Date();
		String fileName = "DBSnapShot_" + dateFormat.format(date) + ".snp"; //$NON-NLS-1$ //$NON-NLS-2$
		// Get Server
		final IItemMonitor item = getSelection();

		TDSFileDialog dialog = new TDSFileDialog(new Shell(), SWT.SAVE);
		dialog.setFilterNames(new String[] { "Snapshots (*.snp)" }); //$NON-NLS-1$
		dialog.setFilterExtensions(new String[] { "*.snp" }); //$NON-NLS-1$
		dialog.setFileName(fileName);
		dialog.setRememberKey("activitiesConnections");
		String pathFile = dialog.open();

		File snapshotFile = new File(pathFile);
		// Verificar se arquivo existe
		if (snapshotFile != null && snapshotFile.exists()) {
			if (DialogYesNoMessage(Messages.ActivitiesConnections_0, Messages.ActivitiesConnections_1)) {
				deleteFile(pathFile);
			} else {
				return null;
			}
		}
		IDbaServerConnector connector = null;
		try {
			connector = (IDbaServerConnector) ServerConnectorFactory.getConnector(item.getServerInfo());
		} catch (Exception e) {
			e.printStackTrace();
		}

		DBAccessAtividadesJob jobAtividades = new DBAccessAtividadesJob(Messages.ActivitiesConnections_2, pathFile,
				fileName, connector);
		jobAtividades.schedule();

		return null;
	}

	@Override
	public boolean isEnabled() {
		IItemMonitor select = getSelection();
		isEnabled = false;
		if (select != null) {
			IServerInfo serverInfo = select.getServerInfo();
			boolean isServer = select.getMonitorType().equals(MonitorType.SERVER);
			isEnabled = serverInfo.getServerType().contains("dba") && isServer; //$NON-NLS-1$
			// System.out.println("Activities enabled: " + isEnabled);
		}
		return isEnabled;
	}

	private boolean DialogYesNoMessage(final String text, final String message) {

		boolean retMessage = false;

		// Pergunta se quer detalhamento completo.
		MessageDialog messageDialog = new MessageDialog(new Shell(), text, null, message, MessageDialog.WARNING,
				new String[] { Messages.ActivitiesConnections_4, Messages.ActivitiesConnections_5 }, 1);

		final int buttonID = messageDialog.open();
		switch (buttonID) {
		case 0:
			retMessage = true;
			break;
		case 1:
			retMessage = false;
			break;
		}

		return retMessage;
	}

	private void deleteFile(final String pathFile) {
		for (int j = 0; j < 25; j++) {
			if (new File(pathFile).delete()) {
				break;
			}
		}

		if (new File(pathFile).exists()) {
			System.out.println("Nao foi possivel excluir o arquivo " + pathFile); //$NON-NLS-1$
		}
	}
}
