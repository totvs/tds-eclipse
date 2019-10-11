package br.com.totvs.tds.server.launcher;

public class LocalAppServerLauncher extends AppLauncher {

	
	private static final String[] DEFAULT_START_OPTIONS = { "-debug" }; //$NON-NLS-1$

	public LocalAppServerLauncher(String serverName, String serverFilename) {
		this(serverName, serverFilename, DEFAULT_START_OPTIONS);
	}
	
	public LocalAppServerLauncher(String serverName, String serverFilename, String[] options) {
		super(serverName, serverFilename, options);
	}

}
