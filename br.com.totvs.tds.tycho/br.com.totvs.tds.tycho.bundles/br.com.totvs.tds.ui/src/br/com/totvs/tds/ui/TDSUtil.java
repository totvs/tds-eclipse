package br.com.totvs.tds.ui;

import org.eclipse.core.runtime.Platform;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Shell;

/**
 * Classe utilitária geral do startup.
 */
public final class TDSUtil {

	private TDSUtil() {
	}

	/**
	 * Verifica se a plataforma é Windows.
	 *
	 * @return true caso seja Windows
	 */
	public static boolean isWindows() {
		return Platform.getOS().equals(Platform.OS_WIN32);
	}

	/**
	 * Verifica se a plataforma é Linux.
	 *
	 * @return true caso seja Linux
	 */
	public static boolean isLinux() {
		return Platform.getOS().equals(Platform.OS_LINUX);
	}

	public static String directoryDialog(Shell shell, String message) {
		return directoryDialog(shell, null, message, null);
	}

	public static String directoryDialog(Shell shell, String title, String message, String filterPath) {
		if (isRunningInTestMode()) {
			return System.getProperty("return");
		}

		DirectoryDialog dialog = new DirectoryDialog(shell, SWT.NONE);

		if ((message != null) && !message.isEmpty()) {
			dialog.setMessage(message);
		}
		if ((title != null) && !title.isEmpty()) {
			dialog.setText(title);
		}
		if ((filterPath != null) && !filterPath.isEmpty()) {
			dialog.setFilterPath(filterPath);
		}

		return dialog.open();
	}

	public static boolean isRunningInTestMode() {

		return (Boolean.TRUE.toString().equals(System.getProperty("isTest", Boolean.FALSE.toString())));
	}

}