package br.com.totvs.tds.ui.sdk.wrapper;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;

/**
 * Classe utilit�ria de wrappers.
 *
 * @author Audrin
 *
 */
public final class WrapperUtil {

	/**
	 * Construtor.
	 */
	private WrapperUtil() {
	}

	/**
	 * Recupera o invólucro de um recurso gen�rico.
	 *
	 * @param resource recurso que ser� utilizado como base para recuperação do
	 *                 invólucro.
	 * @return o invólucro de recurso.
	 * @throws CoreException indica falha de acesso na obtenção do invólucro.
	 */
	public static IResourceWrapper getWrapper(final IResource resource) throws CoreException {
		return WrapperManager.getInstance().getWrapper(resource);
	}

	/**
	 * Recupera o invólucro de WorkspaceWrapper.
	 *
	 * @param workspace
	 * @return
	 * @throws CoreException
	 */
	public static IWorkspaceWrapper getWorkspaceWrapper(final IWorkspace workspace) throws CoreException {
		return WrapperManager.getInstance().getWorkspaceWrapper(workspace);
	}

	/**
	 * Recupera o invólucro de projeto.
	 *
	 * @param resource recurso que ser� utilizado como base para recuperação do
	 *                 invólucro.
	 * @return o invólucro de projeto
	 * @throws CoreException indica falha de acesso na obtenção do invólucro.
	 */
	public static IProjectWrapper getWrapper(final IProject resource) throws CoreException {
		return WrapperManager.getInstance().getWrapper(resource);
	}

	/**
	 * Recupera o invólucro de pasta.
	 *
	 * @param resource recurso que ser� utilizado como base para recuperação do
	 *                 invólucro.
	 * @return o invólucro de pastas.
	 * @throws CoreException indica falha de acesso na obtenção do invólucro.
	 */
	public static IFolderWrapper getWrapper(final IFolder resource) throws CoreException {
		return (IFolderWrapper) WrapperManager.getInstance().getWrapper(resource);
	}

	/**
	 * Recupera o invólucro de arquivo.
	 *
	 * @param resource recurso que ser� utilizado como base para recuperação do
	 *                 invólucro.
	 * @return o invólucro de arquivo.
	 * @throws CoreException indica falha de acesso na obtenção do invólucro.
	 */
	public static IFileWrapper getWrapper(final IFile resource) throws CoreException {
		return (IFileWrapper) WrapperManager.getInstance().getWrapper(resource);
	}

	/**
	 * Recupera um recurso gen�rico pelo caminho dele.
	 *
	 * @param path caminho do recurso
	 * @return IResourceWrapper O recurso selecionado
	 * @throws CoreException indica falha de acesso na obtenção do invólucro.
	 */
	public static IResourceWrapper getResource(final IPath path) throws CoreException {
		final IWorkspace workspace = ResourcesPlugin.getWorkspace();
		final IWorkspaceRoot root = workspace.getRoot();
		final IResource resource = root.findMember(path);

		return getWrapper(resource);
	}

}
