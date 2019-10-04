package br.com.totvs.tds.ui.scenario;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;

import br.com.totvs.tds.ui.ITestProperties;
import br.com.totvs.tds.ui.TdsBot;

public class ServerScenario {

	private static Process appServer;

	public static SWTBotTreeItem addServer(final TdsBot bot, final String name, final String smartClientExe) {
		bot.viewByTitle("Servidores").setFocus();
		final SWTBotTreeItem parent = bot.tree().getTreeItem("Servidores");

		return addServer(bot, parent, name, smartClientExe);
	}

	public static SWTBotTreeItem addServer(final TdsBot bot, final SWTBotTreeItem parent, final String name,
			final String smartClientExe) {
		parent.click();
		parent.select().contextMenu("Novo Servidor").click();

		// assistente 'Novo Servidor'
		bot.table().select("Protheus");
		bot.button("Next >").click();

		// diálogo 'Novo Servidor'
		final SWTBotShell shell = bot.shell("Novo Servidor");
		shell.bot().textWithLabel("SmartClient").setText(ITestProperties.SMART_CLIENT_EXE);
		shell.bot().textWithLabel("Nome").setText(name);
		shell.bot().button("Validar").click(); // Necess�rio enviar duas vezes (1a levanta o LS)

		bot.waitFinish().click();

		return parent.getNode(name);
	}

	public static void startLocalServer() {
		startLocalServer(ITestProperties.APP_SERVER_EXE, ITestProperties.APP_SERVER_PARAMS);
	}

	public static void startLocalServer(final String appServerExe, final String[] appServerParams) {
		if (appServer == null) {
			final List<String> params = new ArrayList<String>();
			params.add(appServerExe);
			params.addAll(Arrays.asList(appServerParams));

			final ProcessBuilder pb = new ProcessBuilder(params);
			try {
				appServer = pb.start();
			} catch (final IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

		}
	}

	public static void stopLocalServer() {
		if (appServer == null) {
			appServer.destroy();
			appServer = null;
		}
	}

	public static SWTBotTreeItem connectServer(final TdsBot bot, final String serverName, final String environment,
			final String username, final String password) {
		bot.viewByTitle("Servidores").setFocus();
		final SWTBotTreeItem node = bot.tree().getTreeItem("Servidores").getNode(serverName).select();
		node.contextMenu("Conectar").click();

		final SWTBotShell shell = bot.shell("Identifique-se");
		shell.bot().comboBox().setText(environment);
		shell.bot().textWithLabel("Usuário").setText(username);
		shell.bot().textWithLabel("Senha").setText(password);
		shell.bot().button("OK").click();
		bot.pause();

		return node;
	}

	public static SWTBotTreeItem disconnectServer(final TdsBot bot, final String serverName) {
		bot.viewByTitle("Servidores").show();
		final SWTBotTreeItem node = bot.tree().getTreeItem("Servidores").getNode(serverName).select();
		node.contextMenu("Desconectar").click();

		return node;
	}

	public static void removeServer(final TdsBot bot, final String serverName) {
		bot.viewByTitle("Servidores").show();
		final SWTBotTreeItem node = bot.tree().getTreeItem("Servidores").getNode(serverName).select();
		node.contextMenu("Remover").click();

		final SWTBotShell shell = bot.shell("Remoção");
		shell.bot().button("Sim").click();
	}

	public static SWTBotTreeItem addLocalServer(final TdsBot bot) {
		bot.viewByTitle("Servidores").setFocus();

		final SWTBotTreeItem parent = bot.tree().getTreeItem("Servidores");
		final SWTBotTreeItem[] serverNode = parent.getItems();
		final long count = Arrays.asList(serverNode).stream()
				.filter(se -> se.getText().equals(ITestProperties.LOCAL_SERVER)).count();

		if (count == 0) {
			return addServer(bot, ITestProperties.LOCAL_SERVER, ITestProperties.APP_SERVER_EXE);
		}

		return parent.getNode(ITestProperties.LOCAL_SERVER);
	}

	public static SWTBotTreeItem connectToLocalServer(final TdsBot bot, final String environment, final String username,
			final String password) {

		return connectServer(bot, ITestProperties.LOCAL_SERVER, environment, username, password);
	}

	public static void disconnectLocalServer(final TdsBot bot) {

		disconnectServer(bot, ITestProperties.LOCAL_SERVER);
	}

	public static void removeLocalServer(final TdsBot bot) {

		removeServer(bot, ITestProperties.LOCAL_SERVER);
	}

}
