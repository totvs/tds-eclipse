package br.com.totvs.tds.ui.console;

import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.console.MessageConsole;

import br.com.totvs.tds.ui.TDSUIIcons;

/**
 * The activator class controls the plug-in life cycle.
 */
public final class MainConsole {

	public static final String CONSOLE_VIEW_ID = "org.eclipse.ui.console.ConsoleView"; //$NON-NLS-1$
	private static final String TDS = "TOTVS Developer Studio"; //$NON-NLS-1$

	private static MainConsole instance = null;

	/** Console principal (TDS). */
	private ConsoleWrapper mainConsole;

	private ConsoleWrapper consoleWrapper;

	/**
	 * Construtor padr�o privado.
	 *
	 * @return
	 */
	private MainConsole() {

	}

	/**
	 * Inicializa a Visão de console
	 */
	private void initConsoleView() {
		final IWorkbenchWindow activeWorkbenchWindow = PlatformUI.getWorkbench().getActiveWorkbenchWindow();

		if (activeWorkbenchWindow != null) {
			activeWorkbenchWindow.getActivePage().findView(CONSOLE_VIEW_ID);
		}
	}

	/**
	 * The singletone instance.
	 *
	 * @return - The TdsConsole single instance
	 */
	public static MainConsole getDefault() {
		if (instance == null) {
			instance = new MainConsole();
		}

		return instance;
	}

	/**
	 * Console principal do TDS.
	 *
	 * @return inv�lucro do console principal.
	 */
	public ConsoleWrapper getMainConsole() {
		if (mainConsole == null) {
			mainConsole = getConsoleWrapper();
		}

		return mainConsole;
	}

	/**
	 * Inicializa um inv�lucro de um console indicado. Para recuper�-lo, use
	 *
	 * @return inv�lucro.
	 */
	public ConsoleWrapper getConsoleWrapper() {
		if (consoleWrapper == null) {
			final MessageConsole console = new MessageConsole(TDS, TDSUIIcons.getIconTDS());
			consoleWrapper = new ConsoleWrapper(TDS, console);
			initConsoleView();
		}
		//
		return consoleWrapper;
	}
}
