package br.com.totvs.tds.ui.screen;

import org.eclipse.swtbot.swt.finder.waits.Conditions;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;

import br.com.totvs.tds.ui.AbstractTest;

public class NewGroupScreen extends AbstractTest {

	private String name;
	private String[] parentName;

	public NewGroupScreen setName(final String name) {

		this.name = name;
		return this;
	}

	public NewGroupScreen setParent(final String[] parent) {
		this.parentName = parent;

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

		parentNode.select();
		parentNode.contextMenu("Novo Grupo").click();

		final SWTBotShell shell = AbstractTest.waitShell("Novo Grupo");
		shell.bot().textWithLabel("Nome").setText(name);

		waitFinish().click();
		bot.waitUntil(Conditions.shellCloses(shell));

		return parentNode.getNode(name);
	}

}
