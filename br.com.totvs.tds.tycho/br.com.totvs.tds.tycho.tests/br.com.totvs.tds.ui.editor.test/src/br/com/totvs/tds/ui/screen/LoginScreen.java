package br.com.totvs.tds.ui.screen;

import org.eclipse.swtbot.swt.finder.waits.Conditions;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;

import br.com.totvs.tds.ui.AbstractTest;

public class LoginScreen extends AbstractTest {

	private String serverName;
	private String environment;
	private String username;
	private String password;
	private boolean secureStorage;

	/**
	 * @param serverName the serverName to set
	 * @return
	 */
	public LoginScreen setServerName(final String serverName) {
		this.serverName = serverName;

		return this;
	}

	/**
	 * @param environment the environment to set
	 * @return
	 */
	public LoginScreen setEnvironment(final String environment) {
		this.environment = environment;

		return this;
	}

	/**
	 * @param username the username to set
	 * @return
	 */
	public LoginScreen setUsername(final String username) {
		this.username = username;

		return this;
	}

	/**
	 * @param password the password to set
	 * @return
	 */
	public LoginScreen setPassword(final String password) {
		this.password = password;

		return this;
	}

	public void finish() {
		bot.viewByTitle("Servidores").setFocus();

		bot.viewByTitle("Servidores").setFocus();
		final SWTBotTreeItem node = bot.tree().getTreeItem("Servidores").getNode(serverName).select();
		node.contextMenu("Conectar").click();

		final SWTBotShell shell = bot.shell("Identifique-se");
		shell.bot().comboBox().setText(environment);
		shell.bot().textWithLabel("Usuário").setText(username);
		shell.bot().textWithLabel("Senha").setText(password);
		if (secureStorage) {
			shell.bot().checkBox().select();
		} else {
			shell.bot().checkBox().deselect();
		}
		shell.bot().button("OK").click();

		bot.waitUntil(Conditions.shellCloses(shell));
	}

	public LoginScreen setSecureStorage(final boolean value) {
		this.secureStorage = value;

		return this;
	}

}
