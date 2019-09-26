package br.com.totvs.tds.ui.bot;

import java.util.List;

import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotView;
import org.eclipse.swtbot.swt.finder.waits.Conditions;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotStyledText;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;

import br.com.totvs.tds.ui.AbstractTest;

public class ConsoleBot extends AbstractTest {

	public static boolean test(final String content) {
		final SWTBotView console = openTDSConsole();
		;
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
		final SWTBotView console = openTDSConsole();
		final SWTBotStyledText st = console.bot().styledText();

		st.contextMenu("Clear");
		console.show();
	}

	private static SWTBotView openTDSConsole() {
		bot.sleep(1000);
		bot.menu("Window").menu("Show View").menu("Other...").click();
		final SWTBotShell shell = bot.shell("Show View").activate();
		final SWTBotTree tree = bot.tree();
		final SWTBotTreeItem expandNode = tree.expandNode("General").click();
		expandNode.getNode("Console").click();
		bot.button("Open").click();
		bot.waitUntil(Conditions.shellCloses(shell));
		pause();

		return bot.viewById("org.eclipse.ui.console.ConsoleView");
	}
}
