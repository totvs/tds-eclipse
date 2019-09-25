package br.com.totvs.tds.ui.editor;

import static org.junit.Assert.assertTrue;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotEclipseEditor;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotEditor;
import org.eclipse.swtbot.swt.finder.junit.SWTBotJunit4ClassRunner;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.osgi.framework.Bundle;

import br.com.totvs.tds.ui.AbstractTest;
import br.com.totvs.tds.ui.ITestProperties;
import br.com.totvs.tds.ui.TestActivator;
import br.com.totvs.tds.ui.bot.ConsoleBot;
import br.com.totvs.tds.ui.bot.ServerBot;
import br.com.totvs.tds.ui.screen.NewFileScreen;
import br.com.totvs.tds.ui.screen.NewProtheusProjectScreen;

@RunWith(SWTBotJunit4ClassRunner.class)
public class CompilationTest extends AbstractTest {

	private static final String PROG_001_PRW = "prog001.prw";

	@BeforeClass
	public static void beforeClass() throws Exception {
		beginTest();
		ServerBot.startLocalAppServer();

		openPlatform("Plataforma (TOTVS)");
		ServerBot.addLocalServer();
		ServerBot.connectLocalServer(ITestProperties.ENVIRONMENT, ITestProperties.USERNAME, ITestProperties.PASSWORD);
	}

	@AfterClass
	public static void afterClass() {
		ServerBot.disconnectLocalServer();
		ServerBot.removeLocalServer();
		ServerBot.stopLocalAppServer();

		endTest();
	}

	@Test
	public void compileCurrentEditor() throws Exception {
		ConsoleBot.clear();
		final NewProtheusProjectScreen newProtheusProjectScreen = new NewProtheusProjectScreen();
		newProtheusProjectScreen.setName("MyFirstProject").setIncludePaths(ITestProperties.INCLUDE_FOLDERS).finish();

		final NewFileScreen newFileScreen = new NewFileScreen();
		newFileScreen.setName(PROG_001_PRW).setContent(getResourceContent(PROG_001_PRW)).finish();

		final SWTBotEditor botEditor = bot.editorByTitle(PROG_001_PRW);
		botEditor.setFocus();

		final SWTBotEclipseEditor textEditor = botEditor.toTextEditor();
		textEditor.setText(newFileScreen.getContent());
		textEditor.save();
		pause();
		textEditor.contextMenu("Compilar").click();
		pause();
		pause();
		assertTrue(String.format("Compilação de %s falhou", PROG_001_PRW), ConsoleBot.isTest("[SUCCESS]"));
	}

	private String getResourceContent(final String name) {
		final Bundle bundle = TestActivator.getInstance().getBundle();
		final java.net.URL url = bundle.getEntry("resources/" + name);
		final StringBuffer sb = new StringBuffer();

		try {
			final InputStream inputStream = url.openConnection().getInputStream();
			final BufferedReader in = new BufferedReader(new InputStreamReader(inputStream));
			String line;

			while ((line = in.readLine()) != null) {
				sb.append(line);
				sb.append('\n');
			}

			in.close();

		} catch (final IOException e) {
			e.printStackTrace();
			assertTrue(e.getMessage(), true);
		}

		return sb.toString();
	}

}