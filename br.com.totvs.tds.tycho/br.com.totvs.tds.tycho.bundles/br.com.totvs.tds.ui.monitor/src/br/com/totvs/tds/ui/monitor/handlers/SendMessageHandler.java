package br.com.totvs.tds.ui.monitor.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;

import br.com.totvs.tds.ui.monitor.dialogs.SendMessageDialog;
import br.com.totvs.tds.ui.monitor.jobs.SendMessageJob;
import br.com.totvs.tds.ui.monitor.model.IItemMonitor;
import br.com.totvs.tds.ui.monitor.model.IServerMonitor;
import br.com.totvs.tds.ui.monitor.model.IUserMonitor;

public class SendMessageHandler extends ServerMonitorHandler {

	@Override
	public Object execute(final ExecutionEvent event) throws ExecutionException {
		final IItemMonitor[] select = getSelections();

		final SendMessageDialog dialog = new SendMessageDialog(new Shell(), select);
		if (dialog.open() == Window.OK) {
			final Job job = new SendMessageJob(dialog.getRecipients(), dialog.getMessageText());
			job.schedule();
		}

		return null;
	}

	@Override
	public boolean isEnabled() {
		IServerMonitor server = null;
		final IItemMonitor selection = getSelection();

		if (selection instanceof IServerMonitor) {
			server = (IServerMonitor) selection;
		} else if (selection instanceof IUserMonitor) {
			final IUserMonitor user = (IUserMonitor) selection;
			server = user.getParent();
		}

		return (selection != null) && server.isConnected() && server.isSendMessageEnabled();

	}

}
