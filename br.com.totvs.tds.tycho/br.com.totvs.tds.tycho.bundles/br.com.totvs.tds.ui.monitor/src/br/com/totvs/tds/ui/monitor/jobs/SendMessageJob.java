package br.com.totvs.tds.ui.monitor.jobs;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

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

		monitor.setTaskName("Verificando destinatários");
		final Map<IServerMonitor, List<IUserMonitor>> recipientMap = recipients.stream()
				.collect(Collectors.groupingBy(IUserMonitor::getParent));

		monitor.setTaskName("Enviando mensagem");
		for (final Entry<IServerMonitor, List<IUserMonitor>> element : recipientMap.entrySet()) {

			for (final IUserMonitor user : element.getValue()) {
				monitor.worked(1);
				final IStatus status = user.getParent().sendMessageUser(user.getUsername(), user.getComputerName(),
						user.getThreadId(), user.getServer(), messageText);

				if (!status.isOK()) {
					return status;
				}
			}
		}

		return Status.OK_STATUS;

	}
}
