package br.com.totvs.tds.ui;

import java.net.URI;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Collections;
import java.util.Date;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.mylyn.commons.notifications.core.AbstractNotification;
import org.eclipse.mylyn.commons.notifications.ui.AbstractUiNotification;
import org.eclipse.mylyn.commons.notifications.ui.NotificationsUi;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.console.MessageConsoleStream;

import br.com.totvs.tds.ui.console.ConsoleWrapper;
import br.com.totvs.tds.ui.console.MainConsole;
import br.com.totvs.tds.ui.nl.Messages;

@SuppressWarnings("restriction")
public class TDSMessageHandler {

	private static ConsoleWrapper consoleWrapper;

	private TDSMessageHandler() {

	}

	private static final DateFormat SDF = new SimpleDateFormat("HH:mm:ss"); //$NON-NLS-1$
	public static final Object[] _EMPTY_ARGS = new Object[0];

	private static class TDSNotification extends AbstractUiNotification {

		private final String label;
		private final IStatus status;
		private URI link;

		public TDSNotification(final String label, final IStatus status) {
			super(String.format("%s", severityToString(status.getSeverity()))); //$NON-NLS-1$
			final String[] parts = label.split("@"); //$NON-NLS-1$

			this.label = parts[0];

			if (parts.length == 2) {
				this.link = URI.create(parts[1]);
			}

			this.status = status;
		}

		@Override
		public String getLabel() {
			return label;
		}

		@Override
		public String getDescription() {
			return status.getMessage();
		}

		@Override
		public Date getDate() {
			return new Date();
		}

		@Override
		public <T> T getAdapter(final Class<T> adapter) {
			if (adapter == IStatus.class) {
				return adapter.cast(status);
			} else if (adapter == URI.class) {
				return adapter.cast(link);
			}

			return null;
		}

		@Override
		public Image getNotificationImage() {
			return null;
		}

		@Override
		public Image getNotificationKindImage() {
			switch (status.getSeverity()) {
			case IStatus.ERROR:
				return PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJS_ERROR_TSK);
			case IStatus.WARNING:
				return PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJS_WARN_TSK);
			case IStatus.INFO:
				return PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJS_INFO_TSK);
			case IStatus.CANCEL:
				// return
				// PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJS_INFO_TSK);
				return null;
			default:
				// return
				// PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJS_INFO_TSK);
				return null;
			}
		}

		@Override
		public void open() {
			final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();

			if (link != null) {
				System.out.println(link);
			} else {
				switch (status.getSeverity()) {
				case IStatus.ERROR:
					MessageDialog.openError(shell, label, status.getMessage());
					break;
				case IStatus.WARNING:
					MessageDialog.openWarning(shell, label, status.getMessage());
					break;
				case IStatus.INFO:
					MessageDialog.openInformation(shell, label, status.getMessage());
					break;
				case IStatus.CANCEL:
					MessageDialog.openInformation(shell, label, status.getMessage());
					break;
				default:
					MessageDialog.open(MessageDialog.NONE, shell, label, status.getMessage(), SWT.None);
				}
			}
		}

	}

	private static void logMessage(final AbstractNotification notification) {
		final ConsoleWrapper console = findConsole(Messages.TDSMessageHandler_tds);
		final IStatus status = notification.getAdapter(IStatus.class);

		final StringBuilder log = new StringBuilder();
		log.append('[');
		log.append(SDF.format(notification.getDate()));
		log.append('-');
		log.append(String.format("%-4.4s", severityToString(status.getSeverity()))); //$NON-NLS-1$
		log.append(']');
		log.append(' ');

		MessageConsoleStream stream = console.getStream(status.getSeverity());
		stream.print(log.toString());

		stream = console.getStream(IStatus.OK);
		stream.println(status.getMessage());
	}

	public static String severityToString(final int severity) {
		switch (severity) {
		case IStatus.ERROR:
			return Messages.TDSMessageHandler_error;
		case IStatus.WARNING:
			return Messages.TDSMessageHandler_warning;
		case IStatus.INFO:
			return Messages.TDSMessageHandler_info;
		case IStatus.CANCEL:
			return Messages.TDSMessageHandler_cancel;
		default:
			return Messages.TDSMessageHandler_ok;
		}
	}

	public static void showMessage(final IStatus status) {
		final AbstractNotification notification = new TDSNotification("TDS", //$NON-NLS-1$
				status);
		NotificationsUi.getService().notify(Collections.singletonList(notification));
		logMessage(notification);
	}

	public static void logMessage(final IStatus status) {
		final AbstractNotification notification = new TDSNotification("TDS", //$NON-NLS-1$
				status);
		logMessage(notification);
	}

	private static ConsoleWrapper findConsole(final String name) {
		if (consoleWrapper == null) {
			consoleWrapper = MainConsole.getDefault().getConsoleWrapper();
		}

		return consoleWrapper;
	}

	private static Object[] removeElement(final Object[] arr, final int index) {
		final Object[] arrOut = new Object[arr.length - 1];
		final int remainingElements = arr.length - (index + 1);

		System.arraycopy(arr, 0, arrOut, 0, index);
		System.arraycopy(arr, index + 1, arrOut, index, remainingElements);

		return arrOut;
	}

	public static IStatus createStatus(final int level, final String pluginId, String message, Object... args) {
		Throwable exception = null;

		if (args != null) {
			exception = extractElement(Throwable.class, args);
			if (exception != null) {
				args = removeElement(args, exception);

				Throwable cause = exception.getCause();
				while ((message == null) && (cause != null)) {
					message = cause.getMessage();
					cause = cause.getCause();
				}

				if (message == null) {
					message = Messages.TDSMessageHandler_EMPTY_STRING;
				}

				exception.printStackTrace();
			}

			message = String.format(message, args);
		}

		return new Status(level, pluginId, 0, message, exception);
	}

	private static Object[] removeElement(Object[] args, final Object element) {

		for (int i = 0; i < args.length; i++) {
			if (args[i].equals(element)) {
				args = removeElement(args, i);
				break;
			}
		}

		return args;
	}

	private static <T> T extractElement(final Class<T> clazz, final Object[] args) {

		for (int i = 0; i < args.length; i++) {
			if (clazz.isInstance(args[i])) {
				return clazz.cast(args[i]);
			}
		}

		return null;
	}

}