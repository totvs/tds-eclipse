package br.com.totvs.tds.ui.screen;

import org.eclipse.swtbot.swt.finder.waits.Conditions;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;

import br.com.totvs.tds.ui.AbstractTest;

public class NewServerScreen extends AbstractTest {

	private String name;
	private String[] parentName;
	private String smartClient;

	public NewServerScreen setName(final String name) {

		this.name = name;
		return this;
	}

	public NewServerScreen setParent(final String[] parent) {
		this.parentName = parent;

		return this;
	}

	public NewServerScreen setSmartClient(final String smartClient) {
		this.smartClient = smartClient;

		return this;
	}

	public SWTBotTreeItem finish() {
		bot.viewByTitle("Servidores").setFocus();

		final SWTBotTreeItem nodeTree = bot.tree().getTreeItem("Servidores");
		SWTBotTreeItem parentNode = null;

		for (final String parentName : this.parentName) {
			if (nodeTree.getText().equals(parentName)) {
				parentNode = nodeTree;
			}
		}

		parentNode.select().click();
		parentNode.select().contextMenu("Novo Servidor").click();

		// assistente 'Novo Servidor'
		bot.table().select("Protheus");
		bot.button("Next >").click();

		// diálogo 'Novo Servidor'
		final SWTBotShell shell = AbstractTest.waitShell("Novo Servidor");
		shell.bot().textWithLabel("Nome").setText(name);
		shell.bot().textWithLabel("SmartClient").setText(smartClient);
		shell.bot().button("Validar").click();

		waitFinish().click();
		bot.waitUntil(Conditions.shellCloses(shell));

		return parentNode.getNode(name);

	}

}
