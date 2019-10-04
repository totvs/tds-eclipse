package br.com.totvs.tds.ui.server;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.eclipse.swtbot.swt.finder.junit.SWTBotJunit4ClassRunner;
import org.eclipse.swtbot.swt.finder.waits.Conditions;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import br.com.totvs.tds.ui.AbstractTest;
import br.com.totvs.tds.ui.ITestProperties;
import br.com.totvs.tds.ui.bot.ConsoleBot;
import br.com.totvs.tds.ui.bot.PerspectiveBot;
import br.com.totvs.tds.ui.screen.CompileKeyScreen;

@RunWith(SWTBotJunit4ClassRunner.class)
public class CompileKeyTest extends AbstractTest {

	@Before
	public static void before() throws Exception {
		PerspectiveBot.openTotvsPlatform();
		ConsoleBot.clear();
	}

	@Test
	public void applyCompileKey() throws Exception {

		bot.menu("Window").menu("Preferences").click();
		final SWTBotShell prefsShell = waitShell("Preferences");

		bot.tree().getTreeItem("TOTVS Developer Studio").click().expand().getNode("Compilação").click().expand()
				.getNode("Chave de Compilação").select();

		final CompileKeyScreen compileKeyScreen = new CompileKeyScreen();
		compileKeyScreen.setAutFile(ITestProperties.AUT_FILE).finish(prefsShell.bot());

		final boolean resultLog = ConsoleBot.test("User successfully authorized.");
		assertEquals("ID´s local/autorização não conferem.", compileKeyScreen.getIdLocal(),
				compileKeyScreen.getIdAuthotization());
		assertTrue("Chave de compilação aplicada não foi aceita.", resultLog);

		prefsShell.activate();

		bot.waitUntil(Conditions.shellCloses(prefsShell));

	}

	@Test
	public void applyInvalidCompileKey() throws Exception {

		bot.menu("Window").menu("Preferences").click();
		final SWTBotShell prefsShell = waitShell("Preferences");

		bot.tree().getTreeItem("TOTVS Developer Studio").click().expand().getNode("Compilação").click().expand()
				.getNode("Chave de Compilação").select();

		final CompileKeyScreen compileKeyScreen = new CompileKeyScreen();
		compileKeyScreen.setAutFile(ITestProperties.INVALID_AUT_FILE).finish(prefsShell.bot());

		final boolean resultLog = ConsoleBot.test("Invalid key (code: 4500.1).");
		assertFalse("Chave de compilação inválida foi aplicada.", resultLog);

		prefsShell.activate();

		bot.waitUntil(Conditions.shellCloses(prefsShell));

	}

}