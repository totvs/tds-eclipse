package br.com.totvs.tds.lsp.server.model;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.lsp4e.server.ProcessStreamConnectionProvider;

public class LSConnectionProvider extends ProcessStreamConnectionProvider {

	public LSConnectionProvider() {
		super();

		final List<String> commands = new ArrayList<String>();
		commands.add(getServer());
		commands.addAll(getArguments());

		setCommands(commands);
	}

	/**
	 *
	 * @return path to server. Can be null is fragment missing.
	 */
	private String getServer() {
		try {
			URL serverPathUrl = getClass().getResource("/resources/tds-ls/advpls.exe"); //$NON-NLS-1$
			if (serverPathUrl == null) {
				serverPathUrl = getClass().getResource("/resources/tds-ls/advpls"); //$NON-NLS-1$
			}
			final File serverPath = new File(FileLocator.toFileURL(serverPathUrl).getPath());

			if (!serverPath.canExecute()) {
				System.err.println(String.format(Messages.LSConnectionProvider_LS_insufficient_privileges,
						serverPath.getAbsolutePath()));
				serverPath.setExecutable(true);
				if (!serverPath.canExecute()) {
					System.err.println(Messages.LSConnectionProvider_Could_not_adjust_privileges);
				}
			}

			return serverPath.getAbsolutePath();
		} catch (final Exception e) {
			e.printStackTrace();
		}

		return null;
	}

	/**
	 *
	 * @param commandLine
	 * @return the command-line to run the server, or null is expected resources are
	 *         not found
	 * @throws IOException
	 */
	private List<String> getArguments() {

		return Collections.emptyList();
	}

}
