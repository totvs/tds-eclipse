/**
 *
 */
package br.com.totvs.tds.ui;

import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.eclipse.finder.waits.Conditions;
import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.waits.DefaultCondition;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotButton;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTable;

/**
 * @author acandido
 *
 */
public class TdsBot extends SWTWorkbenchBot {

	private static final long JOB_WAIT = 60000;
	private static final long JOB_INTERVAL = 2000;

	public void beginTest() {
		System.setProperty("isTest", Boolean.TRUE.toString());

		try {
			viewByTitle("Welcome").close();
		} catch (final Exception e) {
		}

	}

	public void endTest() {
		System.setProperty("isTest", Boolean.FALSE.toString());
		resetWorkbench();

		sleep(1000);
	}

	public void setReturnDialog(final String value) {
		System.setProperty("return", value);
	}

	public SWTBotButton waitFinish() {
		final SWTBotButton btnFinish = button("Finish");

		waitUntil(new DefaultCondition() {

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

	public SWTBotTable waitTableEnable(final SWTBot bot, final String label) {
		final SWTBotTable table = bot.tableWithLabel(label);

		waitUntil(new DefaultCondition() {

			@Override
			public String getFailureMessage() {
				return "The widget " + table + " was not enabled.";
			}

			@Override
			public boolean test() throws Exception {
				return table.isEnabled();
			}
		}, ITestProperties.SERVER_TIME_OUT, ITestProperties.INTERVAL);

		return table;
	}

	public void waitJob(final String jobID) {
		pause();
		waitUntil(Conditions.waitForJobs(jobID, jobID), JOB_WAIT, JOB_INTERVAL);
	}

	/**
	 * Pausa para que eventos sejam processados
	 */
	public void pause() {
		sleep(2000);
	}

	public SWTBotButton waitOk() {
		final SWTBotButton btnOk = button("OK");

		waitUntil(new DefaultCondition() {

			@Override
			public String getFailureMessage() {
				return "The widget " + btnOk + " was not enabled.";
			}

			@Override
			public boolean test() throws Exception {
				return btnOk.isEnabled();
			}
		}, ITestProperties.SERVER_TIME_OUT, ITestProperties.INTERVAL);

		return btnOk;
	}
}
