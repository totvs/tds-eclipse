package br.com.totvs.tds.ui.screen;

import static org.junit.Assert.assertNotEquals;

import java.nio.file.Path;

import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swtbot.swt.finder.waits.Conditions;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTable;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;

import br.com.totvs.tds.server.jobs.BuildPatchProcessType;
import br.com.totvs.tds.ui.AbstractTest;
import br.com.totvs.tds.ui.bot.ServerBot;

public class BuildPatchScreen extends AbstractTest {

	private String serverName;
	private String environment;
	private BuildPatchProcessType processType;
	private Path outputFolder;
	private boolean local;
	private String[] objects;
	private String filename;

	public void finish() throws Exception {
		final SWTBotTreeItem connectedNode = ServerBot.selectServerNode(serverName);
		connectedNode.contextMenu("Pacote de Atualização").menu("Gerar").click();

		final SWTBotShell shell = bot.shell("Pacote de Atualização");
		shell.bot().treeWithLabel("Ambientes").getTreeItem(environment).check();

		shell.bot().comboBoxWithLabel("Processo").setSelection(processType.getLabel());
		shell.bot().comboBoxWithLabel("Salvar em").setSelection(local ? "Local" : "Remoto");
		shell.bot().text().setText(outputFolder.toString());

		if (this.filename != null) {
			shell.bot().textWithLabel("Arquivo (sem extensão)").setText(this.filename);
		}

		shell.bot().button("Next >").click();

		if (BuildPatchProcessType.BY_RPO.equals(processType)) {
			final SWTBotTable table = waitTableEnable(shell.bot(), "RPO");

			if (objects != null) {
				Display.getDefault().syncExec(() -> {
					final TableItem[] items = table.widget.getItems();
					assertNotEquals("Falha na obtenção do mapa do RPO.", items.length, 0);
//
					final int[] rows = new int[objects.length];
					int row = 0;
//
					for (int j = 0; j < items.length; j++) {
						final TableItem item = items[j];
						final String target = item.getText().toLowerCase();

						for (final String object : objects) {
							if (object.equals(target)) {
								rows[row] = j;
								row++;
								break;
							}
						}

						if (row > rows.length) {
							break;
						}
					}
//
					table.select(rows);
					shell.bot().button(">").click();
				});
			}
		} else if (BuildPatchProcessType.BY_COMPARISON.equals(processType)) {

		}

		waitFinish().click();

		waitJob("p12.patchBuilderJob");

		bot.waitUntil(Conditions.shellCloses(shell));
	}

	public BuildPatchScreen setServerName(final String serverName) {
		this.serverName = serverName;

		return this;
	}

	public BuildPatchScreen setEnvironment(final String environment) {
		this.environment = environment;

		return this;
	}

	public BuildPatchScreen setProcessType(final BuildPatchProcessType processType) {
		this.processType = processType;

		return this;
	}

	public BuildPatchScreen setLocalToSave(final Path outputFolder) {
		this.outputFolder = outputFolder;
		this.local = true;

		return this;
	}

	public BuildPatchScreen setRemoteToSave(final Path outputFolder) {
		this.outputFolder = outputFolder;
		this.local = false;

		return this;
	}

	public BuildPatchScreen setObjects(final String... objects) {
		this.objects = objects;

		return this;
	}

	public BuildPatchScreen setOutputFile(final String filename) {
		this.filename = filename;

		return this;
	}

}