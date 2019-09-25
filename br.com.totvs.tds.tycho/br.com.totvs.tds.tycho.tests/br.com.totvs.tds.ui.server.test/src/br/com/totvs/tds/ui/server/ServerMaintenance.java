package br.com.totvs.tds.ui.server;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.eclipse.swtbot.swt.finder.junit.SWTBotJunit4ClassRunner;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

import br.com.totvs.tds.ui.ITestProperties;
import br.com.totvs.tds.ui.TdsBot;
import br.com.totvs.tds.ui.scenario.PlatformScenario;
import br.com.totvs.tds.ui.scenario.ServerScenario;

@RunWith(SWTBotJunit4ClassRunner.class)
public class ServerMaintenance {

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

	@Test
	public void canAddServer() throws Exception {
		PlatformScenario.select(bot, "Plataforma (TOTVS)");

		bot.viewByTitle("Servidores").show();
		bot.viewByTitle("Servidores").setFocus();
		bot.tree().getTreeItem("Servidores").select().contextMenu("Novo Servidor").click();

		// assistente 'Novo Servidor'
		bot.table().select("Protheus");
		bot.button("Next >").click();

		// diálogo 'Novo Servidor'
		final SWTBotShell shell = bot.shell("Novo Servidor");
		shell.bot().textWithLabel("SmartClient").setText(ITestProperties.SMART_CLIENT_EXE);
		shell.bot().textWithLabel("Nome").setText("localhost");
		shell.bot().button("Validar").click();

		assertEquals(ITestProperties.APP_SERVER_ADDRESS, shell.bot().textWithLabel("Endereço").getText());
		assertEquals(ITestProperties.APP_SERVER_PORT, shell.bot().textWithLabel("Porta").getText());

		bot.waitFinish().click();

		bot.viewByTitle("Servidores").setFocus();
		final List<String> nodes = bot.tree().getTreeItem("Servidores").getNodes();
		assertTrue("canAddServer: servidor não adicionado", nodes.contains("localhost"));
	}

	@Test
	public void canEditServer() throws Exception {
		PlatformScenario.select(bot, "Plataforma (TOTVS)");
		final SWTBotTreeItem node = ServerScenario.addServer(bot, "editServer", ITestProperties.SMART_CLIENT_EXE);

		node.click();
		node.contextMenu("Editar").click();

		// diálogo 'Edição'
		final SWTBotShell shell = bot.shell("Edição");
		shell.bot().textWithLabel("Nome").setText("foiEditado");
		shell.bot().button("OK").click();

		bot.viewByTitle("Servidores").setFocus();
		bot.pause();

		final List<String> nodes = bot.tree().getTreeItem("Servidores").getNodes();

		assertTrue(nodes.contains("foiEditado"));
	}

	@Test
	public void canRemoveServer() throws Exception {
		PlatformScenario.select(bot, "Plataforma (TOTVS)");
		final SWTBotTreeItem node = ServerScenario.addServer(bot, "removeServer", ITestProperties.SMART_CLIENT_EXE);

		node.click();
		node.contextMenu("Remover").click();

		// diálogo 'Remoção'
		final SWTBotShell shell = bot.shell("Remoção");
		shell.bot().button("Sim").click();

		bot.viewByTitle("Servidores").setFocus();
		final List<String> nodes = bot.tree().getTreeItem("Servidores").getNodes();
		assertFalse(nodes.contains("removeServer"));
	}

}