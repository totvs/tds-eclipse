package br.com.totvs.tds.ui.console;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.preference.IPreferenceStore;
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
import org.eclipse.ui.console.IConsoleManager;
import org.eclipse.ui.console.MessageConsole;
import org.eclipse.ui.console.MessageConsoleStream;
import org.eclipse.ui.themes.IThemeManager;
import org.osgi.framework.Bundle;
import org.osgi.framework.Version;

import br.com.totvs.tds.ui.ITDSPreferenceKeys;
import br.com.totvs.tds.ui.TDSUIActivator;
import br.com.totvs.tds.ui.TDSUIIcons;
import br.com.totvs.tds.ui.nl.Messages;

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
				getStream(IStatus.OK).write(getTdsIntro());
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
	 * fecha a p�gina de console
	 */
	public void close() {
		final ConsolePlugin plugin = ConsolePlugin.getDefault();
		final IConsoleManager conMan = plugin.getConsoleManager();
		conMan.removeConsoles(new IConsole[] { console });
	}

	/**
	 * @return the consoleTitle
	 */
	public String getConsoleTitle() {
		return consoleTitle;
	}

	/**
	 * Monta "splash" de texto.
	 *
	 * @return banner text
	 */
	public StringBuffer getTdsIntro() {
		final Bundle bundle = TDSUIActivator.getDefault().getBundle();
		final StringBuffer sb = new StringBuffer();
		final Version version = bundle.getVersion();
		final String versionText = String.format("%d.%d.%d", version.getMajor(), version.getMinor(), //$NON-NLS-1$
				version.getMicro());

		final String[] tdsBanner = tdsBanner(versionText, version.toString(), bundle.getLastModified());
		final String[] christmasBanner = christmasBanner();
		final String[] birthdayBanner = birthdayBanner();

		for (int i = 0; i < tdsBanner.length; i++) {
			sb.append(tdsBanner[i]);
			sb.append(christmasBanner[i]);
			sb.append(birthdayBanner[i]);
			sb.append('\n');
		}
		sb.append('\n');

		return sb;
	}

	private String[] tdsBanner(final Object versionText, final Object bundleVersion, final Object bupdateDate) {
		final String[] banner = new String[8];
		final SimpleDateFormat sdf = new SimpleDateFormat();

		banner[0] = "----------------------------v---------------------------------------------------"; //$NON-NLS-1$
		banner[1] = "  //////  ////    //////    | TOTVS Developer Studio 11.4                       "; //$NON-NLS-1$
		banner[2] = "   //    //  //  //         |                                                   "; //$NON-NLS-1$
		banner[3] = "  //    //  //  //////      | " //$NON-NLS-1$
				+ String.format("%-14.14s %-35.35s", Messages.ConsoleWrapper_version, versionText); //$NON-NLS-1$
		banner[4] = " //    //  //      //       | " //$NON-NLS-1$
				+ String.format("%-14.14s %-35.35s", Messages.ConsoleWrapper_build, bundleVersion); // "; //$NON-NLS-1$
		banner[5] = "//    ////    //////  11.4  | " //$NON-NLS-1$
				+ String.format("%-14.14s %-35.35s", Messages.ConsoleWrapper_update_at, sdf.format(bupdateDate)); //$NON-NLS-1$
		banner[6] = "----------------------------^---------------------------------------------------"; //$NON-NLS-1$
		banner[7] = String.format("%-80.80s", Messages.ConsoleWrapper_totvs_copyright); //$NON-NLS-1$ ;

		return banner;
	}

	private String[] christmasBanner() {
		final String[] banner = new String[8];
		Arrays.fill(banner, ""); //$NON-NLS-1$

		final Calendar current = Calendar.getInstance();
		final Calendar christmasBegin = new Calendar.Builder().setDate(current.get(Calendar.YEAR), 10, 29).build(); // 30
																													// nov
		// inicio
		// do
		// per�odo
		// do
		// advento
		final Calendar christmasEnd = new Calendar.Builder().setDate(current.get(Calendar.YEAR) + 1, 0, 7).build(); // 6
																													// jan
		// dia
		// de
		// reis

		if (current.after(christmasBegin) && current.before(christmasEnd)) {
			banner[0] = "| " + Messages.ConsoleWrapper_happy_holidays; //$NON-NLS-1$
			banner[1] = "|    ***     "; //$NON-NLS-1$
			banner[2] = "|   **.**    "; //$NON-NLS-1$
			banner[3] = "|  *.****.   "; //$NON-NLS-1$
			banner[4] = "| *8**x**8*  "; //$NON-NLS-1$
			banner[5] = "|     #      "; //$NON-NLS-1$
			banner[6] = "+  \\\\\\#///   "; //$NON-NLS-1$
			banner[7] = "|   \\\\#//    "; //$NON-NLS-1$
		}

		return banner;
	}

	private String[] birthdayBanner() {
		final String[] banner = new String[8];
		Arrays.fill(banner, ""); //$NON-NLS-1$

		final IPreferenceStore ps = TDSUIActivator.getDefault().getPreferenceStore();
		final int birthDay = ps.getInt(ITDSPreferenceKeys.USER_BIRTH_DAY);
		final int birthMonth = ps.getInt(ITDSPreferenceKeys.USER_BIRTH_MONTH);

		if ((birthMonth != 0) && (birthDay != 0)) {
			final Calendar current = Calendar.getInstance();
			final Calendar birth = new Calendar.Builder().setDate(current.get(Calendar.YEAR), birthMonth - 1, birthDay)
					.build();

			if (birth.get(Calendar.WEEK_OF_YEAR) == current.get(Calendar.WEEK_OF_YEAR)) {
				banner[0] = "|     iiiiiiiiiii     "; //$NON-NLS-1$
				banner[1] = "|    |:h:a:p:p:y:|    "; //$NON-NLS-1$
				banner[2] = "|  __|___________|__  "; //$NON-NLS-1$
				banner[3] = "| |^^^^^^^^^^^^^^^^^| "; //$NON-NLS-1$
				banner[4] = "| |:b:i:r:t:h:d:a:y:| "; //$NON-NLS-1$
				banner[5] = "| |                 | "; //$NON-NLS-1$
				banner[6] = "| ~~~~~~~~~~~~~~~~~~~ "; //$NON-NLS-1$
				banner[7] = "|   \\|/         \\|/   "; //$NON-NLS-1$
			}
		}

		return banner;
	}

}
