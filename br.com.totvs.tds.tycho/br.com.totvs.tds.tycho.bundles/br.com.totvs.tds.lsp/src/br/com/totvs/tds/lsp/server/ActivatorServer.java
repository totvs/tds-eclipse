package br.com.totvs.tds.lsp.server;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.FileLocator;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

import br.com.totvs.tds.lsp.server.model.Messages;

public class ActivatorServer implements BundleActivator {

	public static final String PLUG_IN = "br.com.totvs.tds.lsp.server";

	private static ActivatorServer plugins;

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
	 * @return caminho compledo do execut�vel DA
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

		return args;
	}

	public static ActivatorServer getInstance() {

		return ActivatorServer.plugins;
	}

}
