package br.com.totvs.tds.lsp.server;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

import br.com.totvs.tds.lsp.server.model.Messages;

public class LspActivator implements BundleActivator {

	public static final String PLUG_IN = "br.com.totvs.tds.lsp";

	private static final boolean DEBUG = Boolean.parseBoolean(Platform.getDebugOption("br.com.totvs.tds.lsp/debug")); //$NON-NLS-1$
	private static final boolean LOG_DA = (DEBUG
			&& Boolean.parseBoolean(Platform.getDebugOption("br.com.totvs.tds.lsp/logDebuggerAdapter"))); //$NON-NLS-1$
	private static final boolean ATTACH_DA = DEBUG
			&& Boolean.parseBoolean(Platform.getDebugOption("br.com.totvs.tds.lsp/attachDebuggerAdapter")); //$NON-NLS-1$

	private static LspActivator plugins;

	@Override
	public void start(final BundleContext context) throws Exception {
		plugins = this;

	}

	@Override
	public void stop(final BundleContext context) throws Exception {
		plugins = null;
	}

	/**
	 *
	 * @return caminho completo do executável DA
	 * @throws IOException
	 */
	public String getDACommand() throws IOException {
		URL daPathUrl = getClass().getResource("/resources/tds-da/debugAdapter.exe"); //$NON-NLS-1$
		if (daPathUrl == null) {
			daPathUrl = getClass().getResource("/resources/tds-da/debugAdapter"); //$NON-NLS-1$
		}
		final File daPath = new File(FileLocator.toFileURL(daPathUrl).getPath());

		if (!daPath.canExecute()) {
			System.err.println(
					String.format(Messages.LSConnectionProvider_LS_insufficient_privileges, daPath.getAbsolutePath()));
			daPath.setExecutable(true);
			if (!daPath.canExecute()) {
				System.err.println(Messages.LSConnectionProvider_Could_not_adjust_privileges);
			}
		}

		return daPath.toString();
	};

	/**
	 *
	 * @return argumentos para execução do DA
	 */
	public List<String> getDAArgs() {
		final List<String> args = new ArrayList<String>();

		// ao iniciar, aguarda 30 segundos para que possa fazer um "attach"
		if (ATTACH_DA) {
			args.add("--wait-for-attach");
			args.add("30000"); // milisegundos
		}
		//////////////////////////////////////////////

		// gera log de execução em arquivo
		if (LOG_DA) {
			args.add("--log-file");
			args.add("r:\\da.log");

			try {
				Files.deleteIfExists(Paths.get("r:", "da.log"));
			} catch (final IOException e) {
				e.printStackTrace();
			}
		}
		//////////////////////////////////////////////

		return args;
	}

	public static LspActivator getInstance() {

		return LspActivator.plugins;
	}

}
