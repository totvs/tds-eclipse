package br.com.totvs.tds.ui.editor.handler;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IStatusLineManager;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.services.IServiceLocator;

import br.com.totvs.tds.lsp.server.ILanguageServerService;
import br.com.totvs.tds.lsp.server.model.protocol.CompileOptions;
import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.ui.editor.EditorActivator;
import br.com.totvs.tds.ui.sdk.wrapper.IProjectWrapper;
import br.com.totvs.tds.ui.sdk.wrapper.IResourceWrapper;
import br.com.totvs.tds.ui.sdk.wrapper.IWrapperManager;
import br.com.totvs.tds.ui.sdk.wrapper.WrapperManager;

public class CompileHandler extends EditorHandler {
	private static final List<String> EMPTY_STRING_LIST = new ArrayList<String>();

	class CompileJob extends Job {

		private Map<String, CompileMapData> compileMap;
		private CompileOptions compileOptions;

		public CompileJob(final CompileOptions compileOptions, final Map<String, CompileMapData> compileMap) {
			super("TDS: Compilação");

			this.compileOptions = compileOptions;
			this.compileMap = compileMap;

		}

		@Override
		protected IStatus run(final IProgressMonitor monitor) {
			monitor.beginTask("Compilação", compileMap.size() + 1);
			final IServiceLocator serviceLocator = PlatformUI.getWorkbench();
			final ILanguageServerService lsService = serviceLocator.getService(ILanguageServerService.class);
			final IServerManager serverManager = ServerActivator.getDefault().getServerManager();
			final IAppServerInfo currentServer = serverManager.getCurrentServer();

			for (final Entry<String, CompileMapData> compileData : compileMap.entrySet()) {
				monitor.subTask(String.format("Projeto %s", compileData.getKey()));

				lsService.buidlFile(currentServer.getToken(), currentServer.getPermimissionToken(),
						currentServer.getCurrentEnvironment(), compileData.getValue().files, compileOptions,
						compileData.getValue().includePaths);

				monitor.worked(1);
			}

			monitor.worked(1);

			return Status.OK_STATUS;
		}
	};

	class CompileMapData {
		List<String> files = new ArrayList<String>();
		List<String> includePaths = EMPTY_STRING_LIST;
	}

	@Override
	public Object execute(final ExecutionEvent event) throws ExecutionException {
		final Map<String, CompileMapData> compileMap = new HashMap<String, CompileMapData>();

		checkServer();
		checkPermission();

		final ISelection selection = HandlerUtil.getCurrentSelection(event);
		final boolean recompile = Boolean.valueOf(event.getParameter("recompile"));
		//
		if (selection instanceof ITextSelection) {
			final IWorkbench wb = PlatformUI.getWorkbench();
			final IWorkbenchWindow window = wb.getActiveWorkbenchWindow();
			final IWorkbenchPage page = window.getActivePage();
			final IEditorPart editor = page.getActiveEditor();
			final IEditorInput input = editor.getEditorInput();
			final IFile file = input.getAdapter(IFile.class);

			if (editor.isDirty()) {
				final IActionBars bars = editor.getEditorSite().getActionBars();
				final IStatusLineManager statusLineManager = bars.getStatusLineManager();
				final IProgressMonitor monitor = statusLineManager.getProgressMonitor();
				editor.doSave(monitor);
				EditorActivator.logStatus(IStatus.INFO, "Compilação", "Arquivo [%s] salvo automaticamente.",
						editor.getTitleToolTip());
			}

			try {
				prepareToCompile(file, compileMap);
			} catch (final CoreException e) {
				EditorActivator.logStatus(IStatus.ERROR, "Compilação", e.getMessage(), e);
			}
		} else if (selection instanceof IStructuredSelection) {
			final IStructuredSelection ss = (IStructuredSelection) selection;

			for (final Iterator<?> it = ss.iterator(); it.hasNext();) {
				final Object element = it.next();
				if (element instanceof IResource) {
					try {
						prepareToCompile((IResource) element, compileMap);
					} catch (final CoreException e) {
						EditorActivator.logStatus(IStatus.ERROR, "Compilação", e.getMessage(), e);
					}
				}
			}
		}

		final CompileOptions compileOptions = getCompileOptions();
		compileOptions.setRecompile(recompile);

		final Job job = new CompileJob(compileOptions, compileMap);
		job.schedule();

		return null;
	}

	private void prepareToCompile(final IResource resource, final Map<String, CompileMapData> compileMap)
			throws CoreException {
		final IWrapperManager wm = WrapperManager.getInstance();
		final IResourceWrapper wrapper = wm.getWrapper(resource);
		List<IFile> files;

		if (wrapper.isContainer()) {
			files = wrapper.getChildFiles(true);
		} else {
			files = new ArrayList<IFile>();
			files.add((IFile) resource);
		}

		for (final IFile file : files) {
			final IProject project = file.getProject();
			final IResourceWrapper wf = wm.getWrapper(file); // testar natureza do projeto

			if (!wf.isIgnoreCompile()) {
				CompileMapData compileMapData = compileMap.get(project.getName());

				if (compileMapData == null) {
					compileMapData = new CompileMapData();
					compileMapData.includePaths = getIncludePaths(project);

					compileMap.put(project.getName(), compileMapData);
				}

				final URI location = file.getLocationURI();
				compileMapData.files.add(String.format("file://%s", location.getSchemeSpecificPart()));
			} else {
				EditorActivator.logStatus(IStatus.INFO, "Compilação",
						"Recurso [%s] configurado para ser ignorado na Compilação.", file.getName());
			}
		}
	}

	private List<String> getIncludePaths(final IProject project) {
		final IWrapperManager wm = WrapperManager.getInstance();

		try {
			final IProjectWrapper wp = wm.getWrapper(project);
			return Arrays.asList(wp.getIncludeSearchList(true));
		} catch (final CoreException e) {
			EditorActivator.logStatus(IStatus.ERROR, "Compilação", e.getMessage(), e);
		}

		return EMPTY_STRING_LIST;
	}

	private CompileOptions getCompileOptions() {
		final CompileOptions compileOptions = new CompileOptions();

		return compileOptions;
	}

}
