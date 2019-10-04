package br.com.totvs.tds.ui.bot;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.swtbot.swt.finder.waits.Conditions;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;

import br.com.totvs.tds.ui.AbstractTest;
import br.com.totvs.tds.ui.ITestProperties;
import br.com.totvs.tds.ui.screen.CompileKeyScreen;
import br.com.totvs.tds.ui.screen.LoginScreen;
import br.com.totvs.tds.ui.screen.NewGroupScreen;
import br.com.totvs.tds.ui.screen.NewServerScreen;

public class ServerBot extends AbstractTest {

	private static Process appServer;

	public static SWTBotTreeItem addGroup(final String name) {
		final String[] parent = { "Servidores" };

		return addGroup(parent, name);
	}

	public static SWTBotTreeItem addGroup(final String[] parent, final String name) {
		final NewGroupScreen newGroup = new NewGroupScreen();

		return newGroup.setName(name).setParent(parent).finish();
	}

	public static SWTBotTreeItem addServer(final String name, final String smartClientExe) {

		final String[] parent = { "Servidores" };

		return addServer(parent, name, smartClientExe);

	}

	public static SWTBotTreeItem addServer(final String[] parent, final String name, final String smartClientExe) {
		final NewServerScreen newServer = new NewServerScreen();

		return newServer.setName(name).setParent(parent).setSmartClient(smartClientExe).finish();

	}

	public static void startLocalAppServer() {
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

	public static void stopLocalAppServer() {
		if (appServer == null) {
			appServer.destroyForcibly();
			appServer = null;
		}
	}

	public static void connectServer(final String serverName, final String environment, final String username,
			final String password) {

		final LoginScreen loginScreen = new LoginScreen();
		loginScreen.setServerName(serverName).setEnvironment(environment).setUsername(username).setPassword(password)
				.finish();
	}

	public static SWTBotTreeItem disconnectServer(final String serverName) {
		bot.viewByTitle("Servidores").show();
		final SWTBotTreeItem node = bot.tree().getTreeItem("Servidores").getNode(serverName).select();
		node.contextMenu("Desconectar").click();

		return node;
	}

	public static void removeServer(final String serverName) {
		bot.viewByTitle("Servidores").show();
		final SWTBotTreeItem node = bot.tree().getTreeItem("Servidores").getNode(serverName).select();
		node.contextMenu("Remover").click();

		final SWTBotShell shell = bot.shell("Remoção");
		shell.bot().button("Sim").click();
		bot.waitUntil(Conditions.shellCloses(shell));
	}

	public static SWTBotTreeItem addLocalServer() {
		bot.viewByTitle("Servidores").show();
		bot.viewByTitle("Servidores").setFocus();

		final SWTBotTreeItem parent = bot.tree().getTreeItem("Servidores");
		final SWTBotTreeItem[] serverNode = parent.getItems();
		final long count = Arrays.asList(serverNode).stream()
				.filter(se -> se.getText().equals(ITestProperties.LOCAL_SERVER)).count();

		if (count == 0) {
			return addServer(ITestProperties.LOCAL_SERVER, ITestProperties.SMART_CLIENT_EXE);
		}

		return parent.getNode(ITestProperties.LOCAL_SERVER);
	}

	public static void connectLocalServer(final String environment, final String username, final String password) {

		connectServer(ITestProperties.LOCAL_SERVER, environment, username, password);
	}

	public static void disconnectLocalServer() {

		disconnectServer(ITestProperties.LOCAL_SERVER);
	}

	public static void removeLocalServer() {

		removeServer(ITestProperties.LOCAL_SERVER);
	}

	public static void applyComileKey() {
		applyComileKey(ITestProperties.AUT_FILE);
	}

	public static void applyComileKey(final String autFile) {
		bot.menu("Window").menu("Preferences").click();
		final SWTBotShell prefsShell = waitShell("Preferences");

		bot.tree().getTreeItem("TOTVS Developer Studio").click().expand().getNode("Compilação").click().expand()
				.getNode("Chave de Compilação").select();

		final CompileKeyScreen compileKeyScreen = new CompileKeyScreen();
		compileKeyScreen.setAutFile(autFile).finish(prefsShell.bot());

		prefsShell.activate();
		bot.waitUntil(Conditions.shellCloses(prefsShell));
	}

}
