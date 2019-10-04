package br.com.totvs.tds.ui;

import org.eclipse.core.runtime.Platform;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;

/**
 * Classe utilit�ria geral do startup.
 */
public final class TDSUtil {

	private static final String[] EMPTY_ARRAY = new String[0];

	private TDSUtil() {
	}

	/**
	 * Verifica se a plataforma � Windows.
	 *
	 * @return true caso seja Windows
	 */
	public static boolean isWindows() {
		return Platform.getOS().equals(Platform.OS_WIN32);
	}

	/**
	 * Verifica se a plataforma � Linux.
	 *
	 * @return true caso seja Linux
	 */
	public static boolean isLinux() {
		return Platform.getOS().equals(Platform.OS_LINUX);
	}

	public static String directoryDialog(final Shell shell, final String message) {
		return directoryDialog(shell, null, message, null);
	}

	public static String directoryDialog(final Shell shell, final String title, final String message,
			final String filterPath) {
		if (isRunningInTestMode()) {
			return System.getProperty("return");
		}

		final DirectoryDialog dialog = new DirectoryDialog(shell);

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

	public static String fileDialog(final Shell shell, final String[] filter, final String[] filterNames) {
		if (isRunningInTestMode()) {
			return System.getProperty("return");
		}

		final FileDialog dialog = new FileDialog(shell);
		dialog.setFilterExtensions(filter);
		dialog.setFilterNames(filterNames);

		return dialog.open();
	}

	public static String fileDialog(final Shell shell) {

		return fileDialog(shell, EMPTY_ARRAY, EMPTY_ARRAY);
	}

}