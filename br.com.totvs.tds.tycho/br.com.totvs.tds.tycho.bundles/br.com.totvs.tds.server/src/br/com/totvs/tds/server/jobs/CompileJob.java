package br.com.totvs.tds.server.jobs;

import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.progress.UIJob;
import org.eclipse.ui.services.IServiceLocator;

import br.com.totvs.tds.lsp.server.model.protocol.CompileOptions;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.server.rulers.ServerRules;

public class CompileJob extends UIJob {

	private CompileOptions compileOptions;
	private Map<String, CompileMapData> compileMap;
	private IAppServerInfo server;

	public CompileJob(final CompileOptions compileOptions, final Map<String, CompileMapData> compileMap) {
		super(Messages.CompileJob_Compilation);

		final IServiceLocator serviceLocator = PlatformUI.getWorkbench();
		final IServerManager serverManager = serviceLocator.getService(IServerManager.class);

		initialize(compileOptions, serverManager.getCurrentServer(), compileMap);
	}

	public CompileJob(final CompileOptions compileOptions, final IAppServerInfo server,
			final Map<String, CompileMapData> compileMap) {
		super(Messages.CompileJob_Compilation);

		initialize(compileOptions, server, compileMap);
	}

	private void initialize(final CompileOptions compileOptions, final IAppServerInfo server,
			final Map<String, CompileMapData> compileMap) {

		this.compileOptions = compileOptions;
		this.compileMap = compileMap;
		this.server = server;
		setUser(true);

		setRule(ServerRules.compileRule(server.getId()));
	}

	@Override
	public IStatus runInUIThread(final IProgressMonitor monitor) {
		monitor.beginTask(Messages.CompileJob_Resource_compilation, compileMap.size() + 1);

		for (final Entry<String, CompileMapData> compileData : compileMap.entrySet()) {
			if (monitor.isCanceled()) {
				return Status.CANCEL_STATUS;
			}

			monitor.subTask(String.format(Messages.CompileJob_Project, compileData.getKey()));

			server.buidlFile(compileData.getValue().getFiles(), compileOptions,
					compileData.getValue().getIncludePaths());

			monitor.worked(1);
		}

		return Status.OK_STATUS;
	}

};
