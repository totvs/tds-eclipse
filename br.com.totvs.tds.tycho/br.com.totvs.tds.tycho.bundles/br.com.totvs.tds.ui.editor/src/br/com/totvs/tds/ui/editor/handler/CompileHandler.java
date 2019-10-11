package br.com.totvs.tds.ui.editor.handler;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
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

import br.com.totvs.tds.lsp.server.model.protocol.CompileOptions;
import br.com.totvs.tds.server.jobs.CompileJob;
import br.com.totvs.tds.server.jobs.CompileMapData;
import br.com.totvs.tds.ui.editor.EditorActivator;
import br.com.totvs.tds.ui.sdk.SdkUIUtils;

public class CompileHandler extends EditorHandler {

	@Override
	public Object execute(final ExecutionEvent event) throws ExecutionException {
		final Map<String, CompileMapData> compileMap = new HashMap<String, CompileMapData>();

		checkServer();
		checkPermission();

		final ISelection selection = HandlerUtil.getCurrentSelection(event);
		final boolean recompile = Boolean.valueOf(event.getParameter("recompile")); //$NON-NLS-1$
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
				EditorActivator.logStatus(IStatus.INFO, editor.getTitleToolTip());
			}
			try {
				SdkUIUtils.prepareToCompile(file, compileMap);
			} catch (final CoreException e) {
				EditorActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
			}
		} else if (selection instanceof IStructuredSelection) {
			final IStructuredSelection ss = (IStructuredSelection) selection;

			for (final Iterator<?> it = ss.iterator(); it.hasNext();) {
				final Object element = it.next();
				if (element instanceof IResource) {
					try {
						SdkUIUtils.prepareToCompile((IResource) element, compileMap);
					} catch (final CoreException e) {
						EditorActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
					}
				}
			}
		}

		final CompileOptions compileOptions = new CompileOptions();
		compileOptions.setRecompile(recompile);

		final Job job = new CompileJob(compileOptions, compileMap);
		job.schedule();

		return null;
	}

}
