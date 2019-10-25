package br.com.totvs.tds.ui.server;

import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.ui.TDSUtil;

/**
 * Classe utilit�ria de ServerUI.
 *
 * @author leo.watanabe
 */
public final class ServerUIUtil {

	private static Listener verifyNumbersOnlyListener = new Listener() {

		@Override
		public void handleEvent(final Event e) {
			String string = e.text;
			for (int i = 0; i < string.length(); i++) {
				if (string.charAt(i) < '0' || string.charAt(i) > '9') {
					e.doit = false;
					return;
				}
			}
		}
	};

	/**
	 * Gets the host and check if it is localhost or 127.0.0.*
	 *
	 * @param serverInfo
	 * @return
	 */
	public static boolean isLocalhost(final IAppServerInfo serverInfo) {
		String host = serverInfo.getAddress().getHost();
		return host.equalsIgnoreCase("localhost") || host.startsWith("127.0.0."); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/**
	 * Opens a Folder Dialog. Must be called whithin the UI Thread.
	 *
	 * @param title      - The dialog's title.
	 * @param filterPath - Sets the path that the dialog will use to filter the
	 *                   directories it shows to the argument, which may be null. If
	 *                   the string is null, then the operating system's default
	 *                   filter path will be used. <br>
	 *                   <br>
	 *                   Note that the path string is platform dependent. For
	 *                   convenience, either '/' or '\' can be used as a path
	 *                   separator.
	 *
	 *
	 * @return a string describing the absolute path of the selected directory, or
	 *         null if the dialog was cancelled or an error occurred
	 */
	public static String openFolderDialog(final Shell shell, final String title, final String filterPath) {
		return TDSUtil.directoryDialog(shell, title, null, filterPath);
	}

	/**
	 * Obt�m uma inst�ncia do listener de validação num�rica.
	 *
	 * @return listener de validação num�rica.
	 */
	public static Listener verifyNumbersOnlyListener() {
		return verifyNumbersOnlyListener;
	}

	private ServerUIUtil() {
	}

}
