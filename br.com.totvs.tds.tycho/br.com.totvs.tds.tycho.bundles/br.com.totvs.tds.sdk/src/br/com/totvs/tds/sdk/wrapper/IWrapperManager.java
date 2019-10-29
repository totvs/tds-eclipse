package br.com.totvs.tds.sdk.wrapper;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.runtime.CoreException;

/**
 * Interface do gerenciador de inv�lucro.
 *
 * @author acandido
 *
 */
public interface IWrapperManager {

	String INCLUDES_SEPARATOR = ";"; //$NON-NLS-1$

	/**
	 * Recupera o inv�lucro conforme o tipo de recurso (IResource ou
	 * IModelElement).
	 *
	 * @param element , recurso do qual deseja-se o inv�lucro.
	 * @return inv�lucro
	 *
	 * @throws CoreException , indica falha de acesso a 'SessionProperties'. <br/>
	 *
	 */
	IResourceWrapper getWrapper(Object element) throws CoreException;

	/**
	 * Recupera o inv�lucro conforme o tipo de recurso.
	 *
	 * @param resource , recurso do qual deseja-se o inv�lucro.
	 * @return inv�lucro
	 *
	 * @throws CoreException , indica falha de acesso a 'SessionProperties'.
	 */
	IResourceWrapper getWrapper(IResource resource) throws CoreException;

	/**
	 * Recupera o inv�lucro de projetos.
	 *
	 * @param project , recurso do qual deseja-se o inv�lucro.
	 * @return inv�lucro
	 *
	 * @throws CoreException , indica falha de acesso a 'SessionProperties'.
	 */
	IProjectWrapper getWrapper(IProject project) throws CoreException;

	/**
	 * Recupera o inv�lucro de IWorkspace.
	 *
	 * @author Audrin
	 * @param workspace , workspace do qual deseja-se o inv�lucro.
	 * @return inv�lucro de IWorkspace
	 * @throws CoreException , indica falha de acesso a 'SessionProperties'.
	 */
	IWorkspaceWrapper getWorkspaceWrapper(IWorkspace workspace) throws CoreException;

	/**
	 * @return lista de includes da configuração global.
	 */
	String[] getGlobalList();

	/**
	 * Ajusta a lista de includes da configuração global.
	 */
	void setGlobalList(final String[] includeList);

}
