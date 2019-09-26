package br.com.totvs.tds.ui.server;

import static org.junit.Assert.assertTrue;

import org.eclipse.swtbot.swt.finder.junit.SWTBotJunit4ClassRunner;
import org.eclipse.swtbot.swt.finder.waits.Conditions;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotMenu;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;

import br.com.totvs.tds.ui.AbstractTest;
import br.com.totvs.tds.ui.ITestProperties;
import br.com.totvs.tds.ui.bot.ConsoleBot;
import br.com.totvs.tds.ui.bot.PerspectiveBot;
import br.com.totvs.tds.ui.bot.ServerBot;
import br.com.totvs.tds.ui.screen.LoginScreen;

@RunWith(SWTBotJunit4ClassRunner.class)
public class ServerConnectionTest extends AbstractTest {

	@BeforeClass
	public static void beforeClass() throws Exception {
		PerspectiveBot.openTotvsPlatform();
		ServerBot.addLocalServer();
	}

	@AfterClass
	public static void afterClass() {
		ServerBot.removeLocalServer();
	}

	@Test
	public void connectToServer() throws Exception {
		ConsoleBot.clear();

		final LoginScreen loginScreen = new LoginScreen();
		loginScreen.setServerName(ITestProperties.LOCAL_SERVER).setEnvironment(ITestProperties.ENVIRONMENT)
				.setUsername(ITestProperties.USERNAME).setPassword(ITestProperties.PASSWORD).finish();

		final SWTBotTreeItem node = bot.tree().getTreeItem("Servidores").getNode(ITestProperties.LOCAL_SERVER).select();
		final SWTBotMenu disconnect = node.contextMenu("Desconectar");
		final boolean resultLog = ConsoleBot.test("User authenticated succesfully.");
		assertTrue("Conexão não efetuada.", resultLog && disconnect.isEnabled());

		ServerBot.disconnectLocalServer();
	}

	@Test
	@Ignore
	public void connectToServerWithSecureStorage() throws Exception {
		ConsoleBot.clear();
		ServerBot.addLocalServer();

		final LoginScreen loginScreen = new LoginScreen();
		loginScreen.setServerName(ITestProperties.LOCAL_SERVER).setEnvironment(ITestProperties.ENVIRONMENT)
				.setUsername(ITestProperties.USERNAME).setPassword(ITestProperties.PASSWORD).setSecureStorage(true)
				.finish();

		final SWTBotTreeItem node = bot.tree().getTreeItem("Servidores").getNode(ITestProperties.LOCAL_SERVER).select();
		final SWTBotMenu disconnect = node.contextMenu("Desconectar");
		final boolean resultLog = ConsoleBot.test("User authenticated succesfully.");
		assertTrue("Conexão não efetuada.", resultLog && disconnect.isEnabled());

		bot.menu("Window").menu("Preferences").click();
		final SWTBotShell prefsShell = waitShell("Preferences");

		final SWTBotTreeItem generalItem = bot.tree().getTreeItem("General").click().expand();
		final SWTBotTreeItem securityItem = generalItem.getNode("Security").click().expand();
		securityItem.getNode("Secure Storage").click().expand();

		final SWTBotTreeItem defaultSecureStorageItem = prefsShell.bot().tree().getTreeItem("[Default Secure Storage]")
				.click().expand();
		final SWTBotTreeItem secureStorageItem = defaultSecureStorageItem.getNode("org.eclipse.equinox.secure.storage")
				.click().expand();
		final SWTBotTreeItem developerStudioItem = secureStorageItem.getNode("developerStudio").click().expand();
		System.out.println(developerStudioItem);

		prefsShell.activate();

		bot.waitUntil(Conditions.shellCloses(prefsShell));

		ServerBot.disconnectLocalServer();
	}

}