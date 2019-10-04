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

import br.com.totvs.tds.ui.AbstractTest;
import br.com.totvs.tds.ui.ITestProperties;
import br.com.totvs.tds.ui.bot.PerspectiveBot;
import br.com.totvs.tds.ui.bot.ServerBot;

@RunWith(SWTBotJunit4ClassRunner.class)
public class ServerMaintenance extends AbstractTest {

	@AfterClass
	public static void afterClass() {
		ServerBot.stopLocalAppServer();
	}

	@BeforeClass
	public static void beforeClass() throws Exception {
		ServerBot.startLocalAppServer();
		PerspectiveBot.openTotvsPlatform();
	}

	@Test
	public void canAddServer() throws Exception {
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

		waitFinish().click();

		bot.viewByTitle("Servidores").setFocus();
		final List<String> nodes = bot.tree().getTreeItem("Servidores").getNodes();
		assertTrue("canAddServer: servidor não adicionado", nodes.contains("localhost"));
	}

	@Test
	public void canEditServer() throws Exception {
		final SWTBotTreeItem node = ServerBot.addServer("editServer", ITestProperties.SMART_CLIENT_EXE);

		node.click();
		node.contextMenu("Editar").click();

		// diálogo 'Edição'
		final SWTBotShell shell = bot.shell("Edição");
		shell.bot().textWithLabel("Nome").setText("foiEditado");
		shell.bot().button("OK").click();

		bot.viewByTitle("Servidores").setFocus();
		pause();

		final List<String> nodes = bot.tree().getTreeItem("Servidores").getNodes();

		assertTrue(nodes.contains("foiEditado"));
	}

	@Test
	public void canRemoveServer() throws Exception {
		final SWTBotTreeItem node = ServerBot.addServer("removeServer", ITestProperties.SMART_CLIENT_EXE);

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