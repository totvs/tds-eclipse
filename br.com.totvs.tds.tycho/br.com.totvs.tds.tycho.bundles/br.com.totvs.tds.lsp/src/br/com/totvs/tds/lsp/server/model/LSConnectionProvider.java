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
			URL serverPathUrl = getClass().getResource("/resources/tds-ls/advpls.exe");
			if (serverPathUrl == null) {
				serverPathUrl = getClass().getResource("/resources/tds-ls/advpls");
			}
			final File serverPath = new File(FileLocator.toFileURL(serverPathUrl).getPath());

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
//		File serverFileUrl = null;
//		if (Platform.OS_WIN32.equals(Platform.getOS())) {
//			serverFileUrl = new File(serverPath, "server/Omnisharp.exe"); //$NON-NLS-1$
//		} else {
//			serverFileUrl = new File(serverPath, "run"); //$NON-NLS-1$
//		}
//
//		if (serverFileUrl == null || !serverFileUrl.exists()) {
//			AcutePlugin.logError(NLS.bind(Messages.omnisharpStreamConnection_serverNotFoundError,serverPath));
//			return null;
//		} else if (!serverFileUrl.canExecute()) {
//			AcutePlugin.logError(NLS.bind(Messages.omnisharpStreamConnection_serverNotExecutableError, serverFileUrl));
//			// return value anyway
//		}
//		return serverFileUrl.getAbsolutePath() + " -lsp"; //$NON-NLS-1$
		return Collections.emptyList();
	}

}
