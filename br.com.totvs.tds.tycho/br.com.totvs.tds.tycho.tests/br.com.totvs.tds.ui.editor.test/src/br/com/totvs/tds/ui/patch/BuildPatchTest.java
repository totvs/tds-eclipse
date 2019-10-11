package br.com.totvs.tds.ui.patch;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;

import org.eclipse.swtbot.swt.finder.junit.SWTBotJunit4ClassRunner;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

import br.com.totvs.tds.server.jobs.BuildPatchProcessType;
import br.com.totvs.tds.ui.AbstractTest;
import br.com.totvs.tds.ui.ITestProperties;
import br.com.totvs.tds.ui.bot.ConsoleBot;
import br.com.totvs.tds.ui.bot.PerspectiveBot;
import br.com.totvs.tds.ui.bot.ServerBot;
import br.com.totvs.tds.ui.screen.BuildPatchScreen;

@RunWith(SWTBotJunit4ClassRunner.class)
public class BuildPatchTest extends AbstractTest {

	@AfterClass
	public static void afterClass() {
		ServerBot.startLocalAppServer();
	}

	@BeforeClass
	public static void beforeClass() throws Exception {
		ServerBot.startLocalAppServer();
	}

	@Before
	public void before() throws Exception {
		PerspectiveBot.openTotvsPlatform();

		ServerBot.addLocalServer();
		ServerBot.connectLocalServer("p12", "admin", "");
	}

	@After
	public void after() throws Exception {
		ServerBot.disconnectLocalServer();
		ServerBot.removeLocalServer();
	}

	@Before

	@Test
	public void buildPatchFromRPO() throws Exception {
		ConsoleBot.clear();
		final BuildPatchScreen buildPatchScreen = new BuildPatchScreen();
		final Path outputFolder = Files.createTempDirectory("swtbot");

		buildPatchScreen.setServerName(ITestProperties.LOCAL_SERVER).setEnvironment("p12")
				.setProcessType(BuildPatchProcessType.BY_RPO).setLocalToSave(outputFolder)
				.setObjects("ACAXFUN.PRW", "ACDA010.PRW").finish();

		final File folder = outputFolder.toFile();
		assertEquals(String.format("Pacote em [%s] não foi gerado.", folder.getCanonicalPath()), 1,
				folder.list().length);
	}

	@Test
	public void buildPatchFromRPOWithSpecificName() throws Exception {
		ConsoleBot.clear();
		final BuildPatchScreen buildPatchScreen = new BuildPatchScreen();
		final Path outputFolder = Files.createTempDirectory("swtbot");

		buildPatchScreen.setServerName(ITestProperties.LOCAL_SERVER).setEnvironment("p12")
				.setProcessType(BuildPatchProcessType.BY_RPO).setLocalToSave(outputFolder)
				.setObjects("ACAXFUN.PRW", "ACDA010.PRW").setOutputFile(System.getenv("USERNAME")).finish();

		final List<String> files = Arrays.asList(outputFolder.toFile().list());
		assertEquals(String.format("Pacote [%s] não foi gerado.", outputFolder.toAbsolutePath().toString()), 1,
				files.size());
	}

	@Test
	public void buildPatchByComparison() throws Exception {
		ConsoleBot.clear();
		final BuildPatchScreen buildPatchScreen = new BuildPatchScreen();
		final Path outputFolder = Files.createTempDirectory("swtbot");

		buildPatchScreen.setServerName(ITestProperties.LOCAL_SERVER).setEnvironment("p12")
				.setProcessType(BuildPatchProcessType.BY_COMPARISON).setLocalToSave(outputFolder).finish();

		final File folder = outputFolder.toFile();
		assertEquals(String.format("Pacote em [%s] não foi gerado.", folder.getCanonicalPath()), 1,
				folder.list().length);
	}

	@Test
	public void buildPatchByComparisonWithSpecificName() throws Exception {
		ConsoleBot.clear();
		final BuildPatchScreen buildPatchScreen = new BuildPatchScreen();
		final Path outputFolder = Files.createTempDirectory("swtbot");

		buildPatchScreen.setServerName(ITestProperties.LOCAL_SERVER).setEnvironment("p12")
				.setProcessType(BuildPatchProcessType.BY_COMPARISON).setLocalToSave(outputFolder)
				.setOutputFile(System.getenv("USERNAME")).finish();

		final File folder = outputFolder.toFile();
		assertEquals(String.format("Pacote em [%s] não foi gerado.", folder.getCanonicalPath()), 1,
				folder.list().length);

		final File file = new File(outputFolder.toFile(), System.getenv("USERNAME"));
		assertTrue(String.format("Pacote [%s] não foi gerado.", file.getCanonicalPath()), file.exists());
	}

}