package br.com.totvs.tds.ui.sdk.wrapper;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;

/**
 * Incapsula o IWorkspace.
 * 
 * @author Audrin
 * 
 */
public interface IWorkspaceWrapper {

	/**
	 * Cria um novo projeto.
	 * 
	 * @param voProject
	 * @param monitor
	 * @throws CoreException
	 */
	void createProject(final ProjectVO voProject, final IProgressMonitor monitor) throws CoreException;

	/**
	 * Obtem um IProjectWrapper.
	 * 
	 * @param projectName
	 * @return
	 * @throws CoreException
	 */
	IProjectWrapper getProjectWrapper(final String projectName) throws CoreException;

	/**
	 * Obtem um IProject do workspace.
	 * 
	 * @param projectName
	 * @return
	 * @throws CoreException
	 */
	IProject getProject(final String projectName) throws CoreException;

	/**
	 * Obtem projetos que estão abertos no workspace.
	 * 
	 * @return
	 */
	IProject[] getOpenProjects();

	/**
	 * Obtem um recurso da raiz do workspace atrav�s do nome.
	 * 
	 * @param resourceName
	 * @return
	 */
	IResource findRootMember(final String resourceName) throws Exception;

	/**
	 * Verifica se um recurso possui um item atrav�s do seu nome.
	 * 
	 * @param resource
	 *            , IProject ou IFolder
	 * @param memberName
	 *            , membro a ser localizado no recurso.
	 * @return
	 * @throws CoreException
	 */
	boolean hasMember(IResource resource, String memberName) throws CoreException;

}
