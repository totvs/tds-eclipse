package br.com.totvs.tds.ui.screen;

import org.eclipse.swtbot.swt.finder.waits.Conditions;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;

import br.com.totvs.tds.ui.AbstractTest;

public class NewProtheusProjectScreen extends AbstractTest {

	private String name;
	private String[] includePaths;

	/**
	 * @param name the name to set
	 * @return
	 */
	public NewProtheusProjectScreen setName(final String name) {
		this.name = name;

		return this;
	}

	/**
	 * @param includePaths the includePaths to set
	 * @return
	 */
	public NewProtheusProjectScreen setIncludePaths(final String[] includePaths) {
		this.includePaths = includePaths;

		return this;
	}

	public void finish() throws Exception {
		bot.menu("File").menu("New").menu("Project...").click();
		final SWTBotShell shell = openShell("New Project");
		shell.activate();

		bot.tree().getTreeItem("TOTVS").select();
		bot.tree().getTreeItem("TOTVS").expand();
		bot.tree().getTreeItem("TOTVS").getNode("Projeto Protheus").select();
		bot.button("Next >").click();
		bot.textWithLabel("Nome").setText(name);

		setReturnDialog(String.join(";", includePaths));

		bot.button("Novo").click();
		waitFinish().click();
		bot.waitUntil(Conditions.shellCloses(shell));

		bot.viewByTitle("Project Explorer").setFocus();
		bot.tree().getTreeItem(name).select();
	}

}