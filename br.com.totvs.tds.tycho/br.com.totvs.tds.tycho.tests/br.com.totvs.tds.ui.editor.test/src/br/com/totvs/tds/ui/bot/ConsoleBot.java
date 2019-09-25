package br.com.totvs.tds.ui.bot;

import java.util.List;

import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotView;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotStyledText;

import br.com.totvs.tds.ui.AbstractTest;

public class ConsoleBot extends AbstractTest {

	public static boolean isTest(final String content) {
		final SWTBotView console = bot.viewById("org.eclipse.ui.console.ConsoleView");
		final SWTBotStyledText st = console.bot().styledText();
		final List<String> lines = st.getLines();

		for (final String line : lines) {
			if (line.contains(content)) {
				return true;
			}
		}

		return false;
	}

	public static void clear() {
		final SWTBotView console = bot.viewById("org.eclipse.ui.console.ConsoleView");
		final SWTBotStyledText st = console.bot().styledText();

		st.contextMenu("Clear");
		console.show();
	}

}
