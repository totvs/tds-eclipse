package br.com.totvs.tds.ui.server;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.text.IDocument;
import org.eclipse.lsp4e.LSPEclipseUtils;
import org.eclipse.lsp4e.LanguageServiceAccessor;
import org.eclipse.lsp4j.services.LanguageServer;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IStartup;

import br.com.totvs.tds.ui.server.nl.Messages;

@SuppressWarnings("restriction")
public class Startup implements IStartup {

	@Override
	public void earlyStartup() {
		Display.getDefault().asyncExec(new Runnable() {

			@Override
			public void run() {
				initLanguageServer();
			}
		});
	}

	private IFile findFile(IContainer container) {
		IFile file = null;

		try {
			Pattern source = Pattern.compile("prw|prg|prx", Pattern.CASE_INSENSITIVE); //$NON-NLS-1$

			for (IResource element : container.members()) {
				if (element instanceof IFile) {
					file = (IFile) element;
					String ext = file.getFileExtension();
					Matcher matcher = source.matcher(ext);
					if ((matcher == null) || (!matcher.find())) {
						file = null;
					}
				} else if (element instanceof IContainer) {
					file = findFile((IContainer) element);
				}

				if (file != null) {
					break;
				}
			}
		} catch (CoreException e) {
			ServerUIActivator.logStatus(IStatus.ERROR, Messages.Startup_Initialize, e.getMessage(), e);
		}

		return file;
	}

	private void initLanguageServer() {
		try {
			// força a inicialização do servidor LS para uso em tarefas não associadas a
			// edição
			IProject[] projects = ResourcesPlugin.getWorkspace().getRoot().getProjects();
			IFile file = null;

			for (IProject project : projects) {
				if (project.isOpen()) {
					if (project.isNatureEnabled("br.com.totvs.tds.ui.sdk.protheusNature")) { //$NON-NLS-1$
						IContainer container = project;
						file = findFile(container);
						if (file != null) {
							break;
						}
					}
				}
			}

			IProject project = null;
			boolean removeProject = false;

			if (file == null) {
				removeProject = true;
				project = ResourcesPlugin.getWorkspace().getRoot().getProject("_for_aux_can_remove_"); //$NON-NLS-1$
				if (!project.exists()) {
					project.create(null);
				}
				project.open(null);
				project.setHidden(true);
				IFolder folder = project.getFolder("folder"); //$NON-NLS-1$
				if (!folder.exists()) {
					folder.create(true, true, null);
				}
				file = folder.getFile("p1.prw"); //$NON-NLS-1$
				if (!file.exists()) {
					InputStream source = new ByteArrayInputStream(new byte[0]);
					file.create(source, true, null);
				}
			}
			IDocument document = LSPEclipseUtils.getDocument(file);

			CompletableFuture<List<LanguageServer>> ls = LanguageServiceAccessor.getLanguageServers(document,
					capabilities -> true);

			List<LanguageServer> value = new ArrayList<>();
			ls.complete(value);

			try { // necessário afuardar para a inicialização do LS
				Thread.sleep(3000);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}

			if (removeProject) {
				project.close(null);
				project.delete(true, true, null);
			}

		} catch (CoreException e) {
			ServerUIActivator.logStatus(IStatus.ERROR, Messages.Startup_Initialize, e.getMessage(), e);
		}
	}
}
