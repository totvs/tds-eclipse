package br.com.totvs.tds.ui.server.wizards.path;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;

import org.eclipse.swtbot.swt.finder.junit.SWTBotJunit4ClassRunner;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;

import br.com.totvs.tds.ui.TdsBot;
import br.com.totvs.tds.ui.scenario.PlatformScenario;
import br.com.totvs.tds.ui.scenario.ServerScenario;

@RunWith(SWTBotJunit4ClassRunner.class)
@Ignore
public class BuildPatchByComparison {

	private static TdsBot bot;

	@AfterClass
	public static void afterClass() {
		bot.endTest();
		ServerScenario.stopLocalServer();
	}

	@BeforeClass
	public static void beforeClass() throws Exception {
		ServerScenario.startLocalServer();

		bot = new TdsBot();
		bot.beginTest();
	}

	private SWTBotTreeItem connectedNode;

	@Before
	public void before() throws Exception {
		PlatformScenario.select(bot, "Plataforma (TOTVS)");
		bot.viewByTitle("Servidores").show();

		ServerScenario.addLocalServer(bot);
		connectedNode = ServerScenario.connectToLocalServer(bot, "p12", "admin", "");
	}

	@After
	public void after() throws Exception {
		ServerScenario.disconnectLocalServer(bot);
		ServerScenario.removeLocalServer(bot);

		connectedNode = null;
		// bot.resetWorkbench();
	}

	@Test
	public void buildPatchByComparison() throws Exception {
		connectedNode.setFocus();
		connectedNode.contextMenu("Pacote de Atualização").menu("Gerar").click();

		final SWTBotShell shell = bot.shell("Pacote de Atualização");
		shell.bot().treeWithLabel("Ambientes").getTreeItem("p12").check();
		;
		shell.bot().comboBoxWithLabel("Processo").setSelection("por Comparação (RPO)");
		shell.bot().comboBoxWithLabel("Salvar em").setSelection("Local");

		final Path outputFolder = Files.createTempDirectory("swtbot");
		shell.bot().text().setText(outputFolder.toString());
		shell.bot().button("Next >").click();

		bot.waitTableEnable(shell.bot(), "RPO");
		shell.bot().table().select("ACAXFUN.PRW", "ACDA010.PRW");
		shell.bot().button(">").click();

		bot.waitFinish().click();

		bot.waitJob("p12.patchBuilderJob");

		final File folder = outputFolder.toFile();
		assertEquals(String.format("Pacote em [%s] não foi gerado.", folder.getCanonicalPath()), 1,
				folder.list().length);
	}

	@Test
	public void buildPatchByComparisonWithSpecificName() throws Exception {
		connectedNode.setFocus();
		connectedNode.contextMenu("Pacote de Atualização").menu("Gerar").click();

		final SWTBotShell shell = bot.shell("Pacote de Atualização");
		shell.bot().treeWithLabel("Ambientes").getTreeItem("p12").check();
		;
		shell.bot().comboBoxWithLabel("Salvar em").setSelection("Local");
		final Path outputFolder = Files.createTempDirectory("swtbot");
		shell.bot().text().setText(outputFolder.toString());
		shell.bot().textWithLabel("Arquivo (sem extensão)").setText(System.getenv("USERNAME"));
		shell.bot().button("Next >").click();

		bot.waitTableEnable(shell.bot(), "RPO");
		shell.bot().table().select("ACAXFUN.PRW", "ACDA010.PRW");
		shell.bot().button(">").click();

		bot.waitFinish().click();

		bot.waitJob("p12.patchBuilderJob");

		final File file = new File(outputFolder.toFile(), System.getenv("USERNAME"));
		assertTrue(String.format("Pacote [%s] não foi gerado.", file.getCanonicalPath()), file.exists());
	}

}