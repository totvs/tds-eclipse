package br.com.totvs.tds.ui.scenario;

import org.eclipse.swtbot.swt.finder.widgets.SWTBotMenu;

import br.com.totvs.tds.ui.TdsBot;

public class PlatformScenario {

	public static void select(final TdsBot bot, final String title) {
		final SWTBotMenu menu = bot.menu("Window").menu("Perspective").menu("Open Perspective");
		menu.menu("Other...").click();
		bot.table().select("Plataforma (TOTVS)");
		bot.button("Open").click();
	}

}
