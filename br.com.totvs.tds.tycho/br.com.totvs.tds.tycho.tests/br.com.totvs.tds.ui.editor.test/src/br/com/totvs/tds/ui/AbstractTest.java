package br.com.totvs.tds.ui;

import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.eclipse.finder.waits.Conditions;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotPerspective;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotView;
import org.eclipse.swtbot.swt.finder.waits.DefaultCondition;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotButton;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotMenu;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTable;

public class AbstractTest {

	public static SWTWorkbenchBot bot = new SWTWorkbenchBot();

	protected static void beginTest() {
		System.setProperty("isTest", Boolean.TRUE.toString());

		closeWelcomePage();

		bot.toolbarButtonWithTooltip("&Restore").click();
	}

	private static void closeWelcomePage() {
		for (final SWTBotView view : bot.views()) {
			if (view.getTitle().equals("Welcome")) {
				view.close();
			}
		}
	}

	protected static void endTest() {
		System.setProperty("isTest", Boolean.FALSE.toString());

		bot.sleep(1000);
	}

	public static void openPlatform(final String title) {
		final SWTBotMenu menu = bot.menu("Window").menu("Perspective").menu("Open Perspective");

		menu.menu("Other...").click();
		bot.table().select("Plataforma (TOTVS)");
		bot.button("Open").click();

		bot.waitUntil(new DefaultCondition() {

			@Override
			public String getFailureMessage() {
				return "The platform [" + title + "] was not enabled.";
			}

			@Override
			public boolean test() throws Exception {
				final SWTBotPerspective perspective = AbstractTest.bot.activePerspective();

				return perspective.getLabel().equals(title) && perspective.isActive();
			}
		});
	}

	public static SWTBotShell openShell(final String title) {
		bot.waitUntil(Conditions.shellIsActive(title), 30000);

		return bot.activeShell();
	}

	protected SWTBotButton waitFinish() {
		final SWTBotButton btnFinish = bot.button("Finish");

		bot.waitUntil(new DefaultCondition() {

			@Override
			public String getFailureMessage() {
				return "The widget " + btnFinish + " was not enabled.";
			}

			@Override
			public boolean test() throws Exception {
				return btnFinish.isEnabled();
			}
		}, ITestProperties.SERVER_TIME_OUT, ITestProperties.INTERVAL);

		return btnFinish;
	}

	protected SWTBotTable waitTableEnable(final String label) {
		final SWTBotTable table = bot.tableWithLabel(label);

		bot.waitUntil(new DefaultCondition() {

			@Override
			public String getFailureMessage() {
				return "The widget " + table + " was not enabled.";
			}

			@Override
			public boolean test() throws Exception {
				return table.isEnabled();
			}
		});

		return table;
	}

	/**
	 * Pausa para que eventos sejam processados
	 */
	protected void pause() {
		bot.sleep(2000);
	}

	protected void setReturnDialog(final String value) {
		System.setProperty("return", value);
	}

}
