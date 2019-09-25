package br.com.totvs.tds.ui;

import static org.junit.Assert.assertEquals;

import org.eclipse.swtbot.swt.finder.junit.SWTBotJunit4ClassRunner;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotMenu;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(SWTBotJunit4ClassRunner.class)
public class VerifyPlataforms {

	private static TdsBot bot;

	@AfterClass
	public static void afterClass() {
		bot.endTest();
	}

	@BeforeClass
	public static void beforeClass() throws Exception {
		bot = new TdsBot();
		bot.beginTest();
	}

	@Test
	public void canOpenAdministratorPerpsective() throws Exception {
		SWTBotMenu menu = bot.menu("Window").menu("Perspective").menu("Open Perspective");
		menu.menu("Other...").click();
		bot.table().select("Administrador (TOTVS)");
		bot.button("Open").click();

		assertEquals(bot.activePerspective().getLabel(),"Administrador (TOTVS)");
	}

	@Test
	public void canOpenDeveloperPerpsective() throws Exception {
		SWTBotMenu menu = bot.menu("Window").menu("Perspective").menu("Open Perspective");
		menu.menu("Other...").click();
		bot.table().select("Desenvolvedor (TOTVS)");
		bot.button("Open").click();

		assertEquals(bot.activePerspective().getLabel(),"Desenvolvedor (TOTVS)");
	}

	@Test
	public void canOpenTotvsPlatformPerpsective() throws Exception {
		SWTBotMenu menu = bot.menu("Window").menu("Perspective").menu("Open Perspective");
		menu.menu("Other...").click();
		bot.table().select("Plataforma (TOTVS)");
		bot.button("Open").click();
		
		assertEquals(bot.activePerspective().getLabel(),"Plataforma (TOTVS)");
	}

	@After
	public void resetBot() throws Exception {
		bot.resetWorkbench();
	}

}