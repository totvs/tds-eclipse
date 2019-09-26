package br.com.totvs.tds.ui.bot;

import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotPerspective;
import org.eclipse.swtbot.swt.finder.waits.DefaultCondition;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotMenu;

import br.com.totvs.tds.ui.AbstractTest;

public class PerspectiveBot extends AbstractTest {

	private static final String TOTVS_PLATFORM = "Plataforma (TOTVS)";

	public static SWTBotPerspective openTotvsPlatform() {
		return openPerspective(TOTVS_PLATFORM);
	}

	public static SWTBotPerspective openPerspective(final String title) {
		if (!bot.activePerspective().getLabel().equals(title)) {
			final SWTBotMenu menu = bot.menu("Window").menu("Perspective").menu("Open Perspective");

			menu.menu("Other...").click();
			bot.table().select(title);
			bot.button("Open").click();

			bot.waitUntil(new DefaultCondition() {

				@Override
				public String getFailureMessage() {
					return "The platform [" + title + "] was not enabled.";
				}

				@Override
				public boolean test() throws Exception {
					final SWTBotPerspective perspective = AbstractTest.bot.activePerspective();
					pause();
					return perspective.getLabel().equals(title) && perspective.isActive();
				}
			});
		}

		bot.resetActivePerspective();
		return bot.activePerspective();
	}
}
