package br.com.totvs.tds.server.jobs;

import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.services.IServiceLocator;

import br.com.totvs.tds.lsp.server.ILanguageServerService;
import br.com.totvs.tds.lsp.server.model.protocol.CompileOptions;
import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IServerManager;

public class CompileJob extends Job {

	private CompileOptions compileOptions;
	private Map<String, CompileMapData> compileMap;

	public CompileJob(final CompileOptions compileOptions, final Map<String, CompileMapData> compileMap) {
		super(Messages.CompileJob_Compilation);

		this.compileOptions = compileOptions;
		this.compileMap = compileMap;
	}

	@Override
	protected IStatus run(final IProgressMonitor monitor) {
		monitor.beginTask(Messages.CompileJob_Resource_compilation, compileMap.size() + 1);

		final IServiceLocator serviceLocator = PlatformUI.getWorkbench();
		final ILanguageServerService lsService = serviceLocator.getService(ILanguageServerService.class);
		final IServerManager serverManager = ServerActivator.getDefault().getServerManager();
		final IAppServerInfo currentServer = serverManager.getCurrentServer();
		final String authorizationCode = serverManager.getAuthorizationKey().getAuthorizationCode();

		for (final Entry<String, CompileMapData> compileData : compileMap.entrySet()) {
			monitor.subTask(String.format(Messages.CompileJob_Project, compileData.getKey()));

			lsService.buidlFile(currentServer.getToken(), authorizationCode, currentServer.getCurrentEnvironment(),
					compileData.getValue().getFiles(), compileOptions, compileData.getValue().getIncludePaths());

			monitor.worked(1);
		}

		monitor.worked(1);

		return Status.OK_STATUS;
	}

};
