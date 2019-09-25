package br.com.totvs.tds.ui.screen;

import org.eclipse.swtbot.swt.finder.waits.Conditions;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;

import br.com.totvs.tds.ui.AbstractTest;

public class NewFileScreen extends AbstractTest {

	private String name;
	private String content;

	/**
	 * @param name the name to set
	 * @return
	 */
	public NewFileScreen setName(final String name) {
		this.name = name;

		return this;
	}

	/**
	 * @param content file set
	 * @return
	 */
	public NewFileScreen setContent(final String content) {
		this.content = content;

		return this;
	}

	public void finish() throws Exception {
		bot.menu("File").menu("New").menu("Other...").click();
		final SWTBotShell shell = openShell("New");
		shell.activate();

		bot.textWithLabel("Wizards:").setText("File");

		bot.tree().getTreeItem("General").expand().getNode("File").select();
		bot.button("Next >").click();
		bot.textWithLabel("File name:").setText(name);

		waitFinish().click();
		bot.waitUntil(Conditions.shellCloses(shell));

	}

	public String getContent() {
		return content;
	}

}