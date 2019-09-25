package br.com.totvs.tds.ui.sdk.wrapper.internal;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubProgressMonitor;

import br.com.totvs.tds.ui.sdk.builder.TotvsNature;
import br.com.totvs.tds.ui.sdk.wrapper.IFolderWrapper;
import br.com.totvs.tds.ui.sdk.wrapper.IProjectWrapper;
import br.com.totvs.tds.ui.sdk.wrapper.IResourceWrapper;
import br.com.totvs.tds.ui.sdk.wrapper.IWorkspaceWrapper;
import br.com.totvs.tds.ui.sdk.wrapper.ProjectVO;
import br.com.totvs.tds.ui.sdk.wrapper.WrapperManager;

/**
 * Encapsula operações da área de trabalho.
 */
public final class WorkspaceWrapper implements IWorkspaceWrapper {

	private IWorkspace workspace;

	public WorkspaceWrapper(final IWorkspace workspace) {
		super();
		this.workspace = workspace;
	}

	/**
	 * Cria um novo projeto.
	 * 
	 * @param voProject
	 * @param monitor
	 * @return
	 * @throws CoreException
	 */
	@Override
	public void createProject(final ProjectVO voProject, final IProgressMonitor monitor) throws CoreException {
		final String[] natures = { TotvsNature.NATURE_ID };
		final String[] includes = voProject.includes.toArray(new String[voProject.includes.size()]);

		final IProjectDescription description = workspace.newProjectDescription(voProject.projectName);
		final IProject project = this.getProject(voProject.projectName);

		final IFolder srcDir = project.getFolder("src"); //$NON-NLS-1$
		final IFolder resourceDir = project.getFolder("resources"); //$NON-NLS-1$
		
		project.create(description, monitor);
		project.open(monitor);
		
		if (!srcDir.exists()) {
			srcDir.create(false, true, new SubProgressMonitor(monitor, 1));
		}
		if (!resourceDir.exists()) {
			resourceDir.create(false, true, new SubProgressMonitor(monitor, 1));
		}

		final IProjectWrapper projectWrapper = WrapperManager.getInstance().getWrapper(project);
		projectWrapper.setIncludeSearchList(includes);
		for (final String nature : natures) {
			projectWrapper.addNature(nature);
		}
	}

	// TODO este m�todo est� replicado no SdkUtil public static IProjectWrapper getProject(final String projectName)
	// Deve ser substitu�do.
	/**
	 * Obtem um IProjectWrapper pelo nome.
	 * 
	 * @param projectName
	 * @return
	 * @throws CoreException
	 */
	@Override
	public IProjectWrapper getProjectWrapper(final String projectName) throws CoreException {
		return WrapperManager.getInstance().getWrapper(this.getProject(projectName));
	}

	/**
	 * Obtem um projeto do workspace.
	 * 
	 * @param projectName
	 * @return
	 */
	@Override
	public IProject getProject(final String projectName) {
		return workspace.getRoot().getProject(projectName);
	}

	/**
	 * Obtem projetos que estão abertos no workspace.
	 * 
	 * @return IProject[]
	 */
	@Override
	public IProject[] getOpenProjects() {
		final List<IProject> openProjects = new ArrayList<IProject>();
		final IWorkspaceRoot root = workspace.getRoot();
		final IProject[] projects = root.getProjects();
		for (final IProject iProject : projects) {
			if (iProject.isOpen()) {
				openProjects.add(iProject);
			}
		}
		return openProjects.toArray(new IProject[0]);
	}

	/**
	 * Obtem um recurso da raiz do workspace atrav�s do nome.
	 * 
	 * @param resourceName
	 * @return IResource
	 */
	@Override
	public IResource findRootMember(final String resourceName) {
		final IResource iResource = workspace.getRoot().findMember(new Path(resourceName));

		return iResource;
	}

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
	@Override
	public boolean hasMember(final IResource resource, final String memberName) throws CoreException {
		if (resource.isAccessible()) {
			final IResourceWrapper wrapper = WrapperManager.getInstance().getWrapper(resource);
			if (wrapper instanceof IProjectWrapper) {
				return ((IProjectWrapper) wrapper).hasMember(memberName);
			} else if (wrapper instanceof IFolderWrapper) {
				return ((IFolderWrapper) wrapper).hasMember(memberName);
			}
		}
		return false;
	}

}
