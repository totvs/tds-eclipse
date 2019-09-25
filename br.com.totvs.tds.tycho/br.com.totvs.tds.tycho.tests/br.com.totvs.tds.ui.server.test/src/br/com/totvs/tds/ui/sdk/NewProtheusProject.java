package br.com.totvs.tds.ui.sdk;

import org.eclipse.swtbot.swt.finder.junit.SWTBotJunit4ClassRunner;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

import br.com.totvs.tds.ui.ITestProperties;
import br.com.totvs.tds.ui.TdsBot;

@RunWith(SWTBotJunit4ClassRunner.class)
public class NewProtheusProject {

	private static TdsBot bot;

	@AfterClass
	public static void afterClass() {
		bot.endTest();
	}

	@BeforeClass
	public static void beforeClass() throws Exception {
		bot = new TdsBot();
		bot.beginTest();
	}

	@Test
	public void canCreateTotvsProject() throws Exception {
		bot.menu("File").menu("New").menu("Project...").click();
		SWTBotShell shell = bot.shell("New Project");
		shell.activate();

		bot.tree().getTreeItem("TOTVS").select();
		bot.tree().getTreeItem("TOTVS").expand();
		bot.tree().getTreeItem("TOTVS").getNode("Projeto Protheus").select();
		bot.button("Next >").click();
		bot.textWithLabel("Nome").setText("MyFirstProject");

		bot.setReturnDialog(ITestProperties.INCLUDE_FOLDERS);
		bot.button("Novo").click();
		bot.waitFinish().click();

		bot.viewByTitle("Project Explorer").setFocus();
		bot.tree().getTreeItem("MyFirstProject").select();
	}

	@After
	public void resetBot() throws Exception {
		//bot.resetWorkbench();
	}

}