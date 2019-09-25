package br.com.totvs.tds.ui.server;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.eclipse.swtbot.swt.finder.junit.SWTBotJunit4ClassRunner;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

import br.com.totvs.tds.ui.ITestProperties;
import br.com.totvs.tds.ui.TdsBot;
import br.com.totvs.tds.ui.scenario.PlatformScenario;
import br.com.totvs.tds.ui.scenario.ServerScenario;

@RunWith(SWTBotJunit4ClassRunner.class)
public class GroupMaintenance {

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
	public void canAddGroup() throws Exception {
		PlatformScenario.select(bot, "Plataforma (TOTVS)");

		bot.viewByTitle("Servidores").show();
		bot.viewByTitle("Servidores").setFocus();

		bot.tree().getTreeItem("Servidores").select().contextMenu("Novo Grupo").click();

		// assistente 'Novo Grupo'
		final SWTBotShell shell = bot.shell("Novo Grupo");
		shell.bot().textWithLabel("Nome").setText("Novo_Grupo");

		bot.waitFinish().click();

		bot.viewByTitle("Servidores").setFocus();
		final List<String> nodes = bot.tree().getTreeItem("Servidores").getNodes();
		assertTrue(nodes.contains("Novo_Grupo"));
	}

	@Test
	public void canAddSubGroups() throws Exception {
		PlatformScenario.select(bot, "Plataforma (TOTVS)");

		final SWTBotTreeItem node_g1 = ServerScenario.addGroup(bot, "G1");
		node_g1.click();

		final SWTBotTreeItem node_g1_1 = ServerScenario.addGroup(bot, node_g1, "G1.1");
		node_g1_1.click();

		ServerScenario.addServer(bot, node_g1_1, "server_g1_1", ITestProperties.APP_SERVER_EXE);

		bot.viewByTitle("Servidores").setFocus();
		final SWTBotTreeItem[] items = bot.tree().getAllItems();
		assertTrue(items.length > 0); // melhorar o assert verificando se o server esta no g1_1_1
	}

	@Test
	public void canEditGroup() throws Exception {
		PlatformScenario.select(bot, "Plataforma (TOTVS)");
		final SWTBotTreeItem node = ServerScenario.addGroup(bot, "editGroup");

		bot.viewByTitle("Servidores").show();
		bot.viewByTitle("Servidores").setFocus();

		node.click();
		node.contextMenu("Editar").click();

		// diálogo 'Edição Servidor'
		final SWTBotShell shell = bot.shell("Edição");
		shell.bot().textWithLabel("Nome").setText("foiEditado");
		shell.bot().button("OK").click();

		bot.viewByTitle("Servidores").setFocus();
		final List<String> nodes = bot.tree().getTreeItem("Servidores").getNodes();
		assertFalse(nodes.contains("foiEditado"));
	}

	@Test
	public void canRemoveGroup() throws Exception {
		PlatformScenario.select(bot, "Plataforma (TOTVS)");
		bot.viewByTitle("Servidores").show();
		bot.viewByTitle("Servidores").setFocus();

		final SWTBotTreeItem node = ServerScenario.addGroup(bot, "removeGroup");
		node.click();

		node.contextMenu("Remover").click();

		// diálogo 'Remoção'
		final SWTBotShell shell = bot.shell("Remoção");
		shell.bot().button("Sim").click();

		final List<String> nodes = bot.tree().getTreeItem("Servidores").getNodes();
		assertFalse(nodes.contains("removeGroup"));
	}

	@Before
	public void initBot() throws Exception {

	}

	@After
	public void resetBot() throws Exception {
		// bot.resetWorkbench();
	}

}