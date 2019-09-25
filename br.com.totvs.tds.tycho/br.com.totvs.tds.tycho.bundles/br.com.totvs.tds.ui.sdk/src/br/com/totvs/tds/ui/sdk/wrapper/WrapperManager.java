package br.com.totvs.tds.ui.sdk.wrapper;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.QualifiedName;

import br.com.totvs.tds.ui.sdk.wrapper.internal.FileWrapper;
import br.com.totvs.tds.ui.sdk.wrapper.internal.FolderWrapper;
import br.com.totvs.tds.ui.sdk.wrapper.internal.ProjectWrapper;
import br.com.totvs.tds.ui.sdk.wrapper.internal.WorkspaceWrapper;

/**
 * Implementa IWrapperManager.
 * 
 * @author acandido
 * 
 */
public final class WrapperManager implements IWrapperManager {

	/** Inst�ncia do inv�lucro. */
	private static final IWrapperManager WRAPPER = new WrapperManager();

	/**
	 * Lista de includes da configuração global.
	 */
	private final List<String> globalList = new ArrayList<String>();

	/**
	 * Construtor.
	 */
	private WrapperManager() {
	}

	/**
	 * Inst�ncia.
	 * 
	 * @return a inst�ncia
	 */
	public static IWrapperManager getInstance() {
		return WRAPPER;
	}

	@Override
	public IResourceWrapper getWrapper(final Object element) throws CoreException {
		IResourceWrapper resource = null;

		if (element instanceof IResource) {
			resource = getWrapper((IResource) element);
		}

		return resource;
	}

	/*
	 * (non-Javadoc)
	 */
	@Override
	public IResourceWrapper getWrapper(final IResource resource) throws CoreException {
		IResourceWrapper wrapper = null;

		if (resource != null && resource.exists()) {
			QualifiedName qn = new QualifiedName("wrapper", resource.getFullPath().toString()); //$NON-NLS-1$
			wrapper = (IResourceWrapper) resource.getSessionProperty(qn);

			if (wrapper == null) {
				if (resource instanceof IProject) {
					wrapper = new ProjectWrapper((IProject) resource);
				} else if (resource instanceof IFolder) {
					wrapper = new FolderWrapper((IFolder) resource);
				} else if (resource instanceof IFile) {
					wrapper = new FileWrapper((IFile) resource);
				}
				
				resource.setSessionProperty(qn, wrapper);
			}
		}

		return wrapper;
	}

	/**
	 * Recupera o inv�lucro de IWorkspace.
	 * 
	 * @author Audrin
	 * @param workspace , workspace do qual deseja-se o inv�lucro.
	 * @return inv�lucro de IWorkspace
	 * @throws CoreException , indica falha de acesso a 'SessionProperties'.
	 */
	@Override
	public IWorkspaceWrapper getWorkspaceWrapper(final IWorkspace workspace) throws CoreException {
		QualifiedName qn = new QualifiedName("wrapper", workspace.getRoot().toString()); //$NON-NLS-1$
		IWorkspaceWrapper wrapper = (IWorkspaceWrapper) workspace.getRoot().getSessionProperty(qn);

		if (wrapper == null) {
			wrapper = new WorkspaceWrapper(workspace);
			workspace.getRoot().setSessionProperty(qn, wrapper);
		}

		return wrapper;
	}

	@Override
	public IProjectWrapper getWrapper(final IProject project) throws CoreException {
		return (IProjectWrapper) getWrapper((IResource) project);
	}

	/**
	 * @return lista de includes da configuração global.
	 */
	public String[] getGlobalList() {
		return globalList.toArray(new String[globalList.size()]);
	}

	/**
	 * Ajusta a lista de includes da configuração global.
	 */
	public void setGlobalList(final String[] includeList) {
		globalList.clear();
		globalList.addAll(Arrays.asList(includeList));
	}

}
