package br.com.totvs.tds.server.launcher;

public class LocalAppServerLauncher extends AppLauncher {

	private static final String[] DEFAULT_START_OPTIONS = { "-debug" }; //$NON-NLS-1$

	public LocalAppServerLauncher(final String serverName, final String serverFilename) {
		this(serverName, serverFilename, DEFAULT_START_OPTIONS);
	}

	public LocalAppServerLauncher(final String serverName, final String serverFilename, final String[] options) {
		super(serverName, serverFilename, options);
	}

}
