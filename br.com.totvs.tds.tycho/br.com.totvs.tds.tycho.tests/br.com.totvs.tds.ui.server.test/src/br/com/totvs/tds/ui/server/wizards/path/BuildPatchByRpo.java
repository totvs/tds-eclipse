package br.com.totvs.tds.ui.server.wizards.path;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;

import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swtbot.swt.finder.junit.SWTBotJunit4ClassRunner;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTable;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

import br.com.totvs.tds.ui.TdsBot;
import br.com.totvs.tds.ui.scenario.PlatformScenario;
import br.com.totvs.tds.ui.scenario.ServerScenario;

@RunWith(SWTBotJunit4ClassRunner.class)
public class BuildPatchByRpo {

	private static TdsBot bot;

	@AfterClass
	public static void afterClass() {
		bot.endTest();

		ServerScenario.stopLocalServer();
	}

	@BeforeClass
	public static void beforeClass() throws Exception {
		ServerScenario.startLocalServer();

		bot = new TdsBot();
		bot.beginTest();
	}

	private SWTBotTreeItem connectedServerNode;

	@Before
	public void before() throws Exception {
		PlatformScenario.select(bot, "Plataforma (TOTVS)");
		bot.viewByTitle("Servidores").show();

		ServerScenario.addLocalServer(bot);
		connectedServerNode = ServerScenario.connectToLocalServer(bot, "p12", "admin", "");
	}

	@After
	public void after() throws Exception {
		ServerScenario.disconnectLocalServer(bot);
		ServerScenario.removeLocalServer(bot);

		connectedServerNode = null;
	}

	@Test
	public void buildPatchFromRPO() throws Exception {
		connectedServerNode.setFocus();
		connectedServerNode.contextMenu("Pacote de Atualização").menu("Gerar").click();

		final SWTBotShell shell = bot.shell("Pacote de Atualização");
		shell.bot().treeWithLabel("Ambientes").getTreeItem("p12").check();

		shell.bot().comboBoxWithLabel("Salvar em").setSelection("Local");

		final Path outputFolder = Files.createTempDirectory("swtbot");
		shell.bot().text().setText(outputFolder.toString());
		shell.bot().button("Next >").click();

		final SWTBotTable table = bot.waitTableEnable(shell.bot(), "RPO");

		Display.getDefault().syncExec(() -> {
			final TableItem[] items = table.widget.getItems();
			assertNotEquals("Falha na obtenção do mapa do RPO.", items.length, 0);

			final int[] rows = new int[10];
			int selSource = 0;
			int row = 0;

			for (final TableItem item : items) {
				if (item.getText().toLowerCase().endsWith("prw")) {
					rows[selSource] = row;
					selSource++;
					if (!(selSource < rows.length)) {
						break;
					}
				}
				row++;
			}

			table.select(rows);
			shell.bot().button(">").click();
		});

		bot.waitFinish().click();

		bot.waitJob("p12.patchBuilderJob");

		final File folder = outputFolder.toFile();
		assertEquals(String.format("Pacote em [%s] não foi gerado.", folder.getCanonicalPath()), 1,
				folder.list().length);
	}

	@Test
	public void buildPatchFromRPOWithSpecificName() throws Exception {
		connectedServerNode.setFocus();
		connectedServerNode.contextMenu("Pacote de Atualização").menu("Gerar").click();
		final Path outputFolder = Files.createTempDirectory("swtbot");

		final SWTBotShell shell = bot.shell("Pacote de Atualização");
		shell.bot().treeWithLabel("Ambientes").getTreeItem("p12").check();

		shell.bot().comboBoxWithLabel("Salvar em").setSelection("Local");
		shell.bot().text().setText(outputFolder.toString());
		shell.bot().textWithLabel("Arquivo (sem extensão)").setText(System.getenv("USERNAME"));
		shell.bot().button("Next >").click();

		final SWTBotTable table = bot.waitTableEnable(shell.bot(), "RPO");

		Display.getDefault().syncExec(() -> {
			final TableItem[] items = table.widget.getItems();
			assertNotEquals("Falha na obtenção do mapa do RPO.", items.length, 0);

			final int[] rows = new int[10];
			int selSource = 0;
			int row = 0;

			for (final TableItem item : items) {
				if (item.getText().toLowerCase().endsWith("prw")) {
					rows[selSource] = row;
					selSource++;
					if (!(selSource < rows.length)) {
						break;
					}
				}
				row++;
			}

			table.select(rows);
			shell.bot().button(">").click();
		});

		bot.waitFinish().click();

		bot.waitJob("p12.patchBuilderJob");

		final List<String> files = Arrays.asList(outputFolder.toFile().list());

		assertEquals(String.format("Pacote [%s] não foi gerado.", outputFolder.toAbsolutePath().toString()), 1,
				files.size());
	}

}