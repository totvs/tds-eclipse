package br.com.totvs.tds.ui;

import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.eclipse.finder.waits.Conditions;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotView;
import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.waits.DefaultCondition;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotButton;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTable;
import org.junit.AfterClass;
import org.junit.BeforeClass;

public class AbstractTest {

	private static final long JOB_WAIT = 60000;
	private static final long JOB_INTERVAL = 2000;

	public static SWTWorkbenchBot bot = new SWTWorkbenchBot();

	@BeforeClass
	public static void _beginTest() {
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

	@AfterClass
	public static void _endTest() {
		System.setProperty("isTest", Boolean.FALSE.toString());

		bot.sleep(1000);
	}

	public static SWTBotShell waitShell(final String title) {
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

	protected SWTBotTable waitTableEnable(final SWTBot bot, final String label) {
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

	protected SWTBotTable waitTableEnable(final String label) {

		return waitTableEnable(bot, label);
	}

	protected void waitJob(final String jobID) {
		pause();

		bot.waitUntil(Conditions.waitForJobs(jobID, jobID), JOB_WAIT, JOB_INTERVAL);
	}

	/**
	 * Pausa para que eventos sejam processados
	 */
	protected static void pause() {
		bot.sleep(2000);
	}

	protected void setReturnDialog(final String value) {
		System.setProperty("return", value);
	}

}
