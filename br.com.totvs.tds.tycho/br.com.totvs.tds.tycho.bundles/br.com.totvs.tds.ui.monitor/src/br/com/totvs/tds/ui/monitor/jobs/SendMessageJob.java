package br.com.totvs.tds.ui.monitor.jobs;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import br.com.totvs.tds.ui.monitor.model.IItemMonitor;
import br.com.totvs.tds.ui.monitor.model.IServerMonitor;
import br.com.totvs.tds.ui.monitor.model.IUserMonitor;

public class SendMessageJob extends Job {

	private List<IUserMonitor> recipients;
	private String messageText;

	public SendMessageJob(final List<IUserMonitor> recipients, final String messageText) {
		super("Envio de Mensagem");

		this.recipients = recipients;
		this.messageText = messageText;
	}

	@Override
	protected IStatus run(final IProgressMonitor monitor) {
		monitor.beginTask("Envio de mensagem", recipients.size());

		final Map<IServerMonitor, List<IUserMonitor>> recipientMap =
				recipients.stream().collect(Collectors.groupingBy(IUserMonitor::getParent));

		recipientMap..
;

		recipientMap.forEach((ISeverMonitor severMonitor, IusetsMonitor usetsMonito) -;

		if (itemMonitor.getMonitorType().equals(MonitorType.SERVER)) {
			for (final IItemMonitor item : itemMonitor.getChildren()) {
				final Long threadID = Long.valueOf(item.getThreadId());
				monitorConnector.sendMessageUser(item.getUser(), item.getMachine(), threadID, item.getUserServer(),
						messageToUser);
			}
		} else {
			final Long threadID = Long.valueOf(itemMonitor.getThreadId());
			monitorConnector.sendMessageUser(itemMonitor.getUser(), itemMonitor.getMachine(), threadID,
					itemMonitor.getUserServer(), messageToUser);
		}

		return Status.CANCEL_STATUS;
	}

	private void sendMessageUser(final String userName, final String computerName, final Long threadID,
			final String serverName, final String message) throws Exception {
		final final

thread = new MsHandleT(threadID);
		getClientManager().sendMessageToUser(userName, computerName, thread, serverName, message);
	}

	// XXX Monitor???
	public void sendMessageToUser(final String userName, final String computerName, final MsHandleT threadID,
			final String serverName, final String message) throws Exception {
		final MsDbgMessage mntMessageMsg = new MsDbgMessage(connId);
		mntMessageMsg.computerName = computerName;
		mntMessageMsg.serverName = serverName;
		mntMessageMsg.userName = userName;
		mntMessageMsg.threadId = threadID;
		mntMessageMsg.message = message;
		//
		sendMessage(mntMessageMsg);
	}

}
