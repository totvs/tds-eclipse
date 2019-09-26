package br.com.totvs.tds.ui.console;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.resource.ColorRegistry;
import org.eclipse.jface.resource.FontRegistry;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleListener;
import org.eclipse.ui.console.IConsoleManager;
import org.eclipse.ui.console.MessageConsole;
import org.eclipse.ui.console.MessageConsoleStream;
import org.eclipse.ui.themes.IThemeManager;

import br.com.totvs.tds.ui.TDSUIIcons;

/**
 * ConsoleWrapper.
 *
 * @author leo.watanabe
 * @author acandido
 *
 */
public final class ConsoleWrapper implements IPropertyChangeListener {

	private String consoleTitle;
	private MessageConsole console;
	private Map<Integer, MessageConsoleStream> streams = new HashMap<Integer, MessageConsoleStream>();

	private static final String CONSOLE_FONT = "br.com.totvs.tds.ui.preferences.fontConsole"; //$NON-NLS-1$

	private static final String CONSOLE_DEFAULT_COLOR = "br.com.totvs.tds.ui.preferences.color.default"; //$NON-NLS-1$
	private static final String CONSOLE_ERROR_COLOR = "br.com.totvs.tds.ui.preferences.color.error"; //$NON-NLS-1$
	private static final String CONSOLE_BAKGROUND_COLOR = "br.com.totvs.tds.ui.preferences.color.backgroud"; //$NON-NLS-1$
	private static final String CONSOLE_INFORMATION_COLOR = "br.com.totvs.tds.ui.preferences.color.information"; //$NON-NLS-1$
	private static final String CONSOLE_WARNING_COLOR = "br.com.totvs.tds.ui.preferences.color.warning"; //$NON-NLS-1$

	// [17:07:22-erro] A conex�o foi recusada pelo servidor.
	// <tab-width>Ambiente ou credenciais inv�lidas
	private static final int TAB_WIDTH = 16;

	/**
	 * Construtor.
	 *
	 * @param console
	 */
	public ConsoleWrapper(final String consoleTitle, final MessageConsole console) {
		this.consoleTitle = consoleTitle;
		initConsoleWrapper(console);
		captureConsole();

		PlatformUI.getWorkbench().getThemeManager().addPropertyChangeListener(this);
	}

	/**
	 * Obtem o Stream do tipo code.
	 *
	 * @param level tipo de mensagem que ser� exibida no canal
	 * @return the console stream
	 */
	public MessageConsoleStream getStream(final int level) {
		if (!streams.containsKey(level)) {
			Display.getDefault().syncExec(() -> {
				final MessageConsoleStream msgConsoleStream = console.newMessageStream();
				msgConsoleStream.setColor(getColor(level));
				streams.put(level, msgConsoleStream);
			});
		}

		return streams.get(level);
	}

	private void captureConsole() {
		final ConsolePlugin plugin = ConsolePlugin.getDefault();
		final IConsoleManager conMan = plugin.getConsoleManager();
		final IConsoleListener lsCaptureLog = LsCaptureLog.getInstance();

		lsCaptureLog.consolesAdded(conMan.getConsoles());
		conMan.addConsoleListener(lsCaptureLog);
	}

	private void initConsoleWrapper(final MessageConsole useThisConsole) {
		final ConsolePlugin plugin = ConsolePlugin.getDefault();
		final IConsoleManager conMan = plugin.getConsoleManager();
		final IConsole[] consoleList = conMan.getConsoles();

		for (final IConsole iconsole : consoleList) {
			if (iconsole.getName().contains(consoleTitle) && (iconsole instanceof MessageConsole)) {
				console = (MessageConsole) iconsole;
				break;
			}
		}

		if (console == null) {
			if (useThisConsole == null) {
				final ImageDescriptor image = TDSUIIcons.getIconTDS();
				console = new MessageConsole(consoleTitle, image);
			} else {
				console = useThisConsole;
			}

			updateBackgroundColor();
			updateFont();

			console.setTabWidth(TAB_WIDTH);
			console.addPatternMatchListener(new SourceFileMatchListener());
			// console.addPatternMatchListener(new UrlMatchListener());
			console.addPropertyChangeListener(this);

			conMan.addConsoles(new IConsole[] { console });

			try {
				getStream(IStatus.OK).write(Banners.getTdsIntro());
				console.activate();
			} catch (final IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

	}

	private Color getColor(final int level) {
		Color color = null;
		final IThemeManager themeManager = PlatformUI.getWorkbench().getThemeManager();
		final ColorRegistry colorRegistry = themeManager.getCurrentTheme().getColorRegistry();
		//
		switch (level) {
		case IStatus.INFO:
			color = colorRegistry.get(CONSOLE_INFORMATION_COLOR);
			break;
		case IStatus.WARNING:
			color = colorRegistry.get(CONSOLE_WARNING_COLOR);
			break;
		case IStatus.ERROR:
			color = colorRegistry.get(CONSOLE_ERROR_COLOR);
			break;
		default:
			color = colorRegistry.get(CONSOLE_DEFAULT_COLOR);
		}
		//
		return color;
	}

	@Override
	public void propertyChange(final PropertyChangeEvent event) {
//		final String property = event.getProperty();

//		if (property.equals(ShowStandardErrAction.ID)) {
//			updateOnWrite(IStatus.ERROR, (boolean) event.getNewValue());
//			return;
//		}
//
//		if (property.equals(ShowStandardOutAction.ID)) {
//			updateOnWrite(IStatus.INFO, (boolean) event.getNewValue());
//			updateOnWrite(IStatus.WARNING, (boolean) event.getNewValue());
//			updateOnWrite(IStatus.CANCEL, (boolean) event.getNewValue());
//			return;
//		}

		updateBackgroundColor();
		updateFont();
	}

//	private void updateOnWrite(final int level, final boolean activateOnWrite) {
//
//		Display.getDefault().asyncExec(() -> {
//			if (ConsoleWrapper.this.streams.containsKey(level)) {
//				ConsoleWrapper.this.streams.get(level).setActivateOnWrite(activateOnWrite);
//			}
//		});
//	}

	private void updateBackgroundColor() {
		Display.getDefault().asyncExec(() -> {
			final IThemeManager themeManager = PlatformUI.getWorkbench().getThemeManager();
			final ColorRegistry colorRegistry = themeManager.getCurrentTheme().getColorRegistry();
			final Color color = colorRegistry.get(CONSOLE_BAKGROUND_COLOR);

			console.setBackground(color);
		});
	}

	private void updateFont() {
		Display.getDefault().asyncExec(() -> {
			final IThemeManager themeManager = PlatformUI.getWorkbench().getThemeManager();
			final FontRegistry fontRegistry = themeManager.getCurrentTheme().getFontRegistry();
			final Font font = fontRegistry.get(CONSOLE_FONT);

			console.setFont(font);
		});
	}

	/**
	 * @return the console
	 */
	public MessageConsole getConsole() {
		return console;
	}

	/**
	 * fecha a página de console
	 */
	public void close() {
		final ConsolePlugin plugin = ConsolePlugin.getDefault();
		final IConsoleManager conMan = plugin.getConsoleManager();

		conMan.removeConsoles(new IConsole[] { console });
	}

}
