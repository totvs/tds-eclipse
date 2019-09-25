/**
 * 
 */
package br.com.totvs.tds.ui.sdk.wrapper.internal;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.StringJoiner;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.QualifiedName;

import br.com.totvs.tds.ui.sdk.SdkUIActivator;
import br.com.totvs.tds.ui.sdk.builder.TotvsNature;
import br.com.totvs.tds.ui.sdk.wrapper.IProjectWrapper;
import br.com.totvs.tds.ui.sdk.wrapper.IWrapperManager;
import br.com.totvs.tds.ui.sdk.wrapper.WrapperManager;

/**
 * Inv�lucro de projetos.
 * 
 * @author acandido
 */
public final class ProjectWrapper extends ContainerWrapper implements IProjectWrapper {

	private static final String GLOBAL = "$(GLOBAL)"; //$NON-NLS-1$

	private static final String WORKSPACE = "$(WORKSPACE)"; //$NON-NLS-1$

	/**
	 * Construtor.
	 * 
	 * @param project , projeto associado ao inv�lucro.
	 */
	public ProjectWrapper(final IProject project) {
		super(project);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see br.com.totvs.tds.sdk.wrapper.IResourceWrapper#getResource()
	 */
	@Override
	public IProject getResource() {
		return (IProject) super.getResource();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * br.com.totvs.tds.sdk.wrapper.IProjectWrapper#setIncludeSearchList(java.lang.
	 * String[])
	 */
	@Override
	public void setIncludeSearchList(final String[] folders) {
		StringJoiner sb = new StringJoiner(IWrapperManager.INCLUDES_SEPARATOR);

		for (String folder : folders) {
			sb.add(folder);
		}

		try {
			getResource().setPersistentProperty(QN_PROJECT_INCLUDE, sb.toString());
		} catch (CoreException e) {
			SdkUIActivator.logStatus(IStatus.ERROR, "Interno", e.getMessage(), e);
		}

	}

	@Override
	public List<File> getIncludeFiles() {
		String[] includesDirectories = getIncludeSearchList(true);
		List<File> validFiles = new ArrayList<>();
		for (String includePath : includesDirectories) {
			File directory = new File(includePath);
			if (directory.exists() && directory.isDirectory()) {
				validFiles.addAll(searchIncludeFiles(directory));
			}
		}
		return validFiles;
	}

	@Override
	public List<File> getIncludeFiles(final String prefix) {
		List<File> validFiles = new ArrayList<>();
		List<File> includeFiles = getIncludeFiles();
		for (File file : includeFiles) {
			if (file.getName().contains(prefix)) {
				validFiles.add(file);
			}
		}
		return validFiles;
	}

	/**
	 * Realiza a busca por arquivos v�lidos para includes (*.ch's).
	 * 
	 * @param directory - Diret�rio em que ser� realizado a busca dos arquivos.
	 * @param prefix    - Caso informado o prefixo deve considerar apenas arquivos
	 *                  que comecem com determinado prefixo.
	 * @return Retorna a lista de arquivos v�lidos encontrados.
	 */
	private List<File> searchIncludeFiles(final File directory) {
		List<File> includeFilesTemp = new ArrayList<>();
		for (File file : directory.listFiles()) {
			if (file.getName().endsWith(".ch")) { //$NON-NLS-1$
				includeFilesTemp.add(file);
			}
		}

		Collections.sort(includeFilesTemp, new Comparator<File>() {
			@Override
			public int compare(final File o1, final File o2) {
				return o1.getName().compareTo(o2.getName());
			}
		});

		return includeFilesTemp;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see br.com.totvs.tds.sdk.wrapper.IProjectWrapper#getIncludeSearchList()
	 */
	@Override
	public String[] getIncludeSearchList() {
		return getIncludeSearchList(false);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * br.com.totvs.tds.sdk.wrapper.IProjectWrapper#getIncludeSearchList(boolean)
	 */
	@Override
	public String[] getIncludeSearchList(final boolean physicalPath) {
		if (physicalPath) {
			String[] globalList = WrapperManager.getInstance().getGlobalList();
			String[] includeList = getIncludeSearchList();
			ArrayList<String> result = new ArrayList<String>();

			result.addAll(Arrays.asList(includeList));

			int pos = result.indexOf(GLOBAL);
			if (pos > -1) {
				result.addAll(pos + 1, Arrays.asList(globalList));
				result.remove(GLOBAL);
			}

			for (int i = 0; i < result.size(); i++) {
				String folder = result.get(i);
				if (folder.startsWith(WORKSPACE)) {
					folder = folder.replace(WORKSPACE, ""); //$NON-NLS-1$
					IResource resource = ResourcesPlugin.getWorkspace().getRoot().findMember(folder);

					if (resource != null && resource.exists()) {
						folder = resource.getLocation().toOSString();
						result.set(i, folder);
					} else {
						result.set(i, ""); //$NON-NLS-1$
					}
				} else {
					Path path = new Path(folder);
					if (path.toFile().exists()) {
						result.set(i, path.toFile().getAbsolutePath());
					} else {
						result.set(i, ""); //$NON-NLS-1$
					}
				}
			}

			return result.toArray(new String[result.size()]);
		}

		String[] folders = new String[] {};
		try {
			String includes = getResource().getPersistentProperty(QN_PROJECT_INCLUDE);
			if (includes != null && !includes.isEmpty()) {
				folders = includes.split(IWrapperManager.INCLUDES_SEPARATOR);
			}
		} catch (CoreException e) {
			SdkUIActivator.logStatus(IStatus.ERROR, "Interno", e.getMessage(), e);
		}

		return folders;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * 
	 * @see br.com.totvs.tds.sdk.wrapper.IProjectWrapper#addNature(java.lang.String)
	 */
	@Override
	public boolean addNature(final String nature) throws CoreException {
		boolean result = false;

		if (TotvsNature.NATURE_ID.equals(nature)) {
			justAddNature(TotvsNature.NATURE_ID);
			result = true;
		}

		return result;
	}

	/**
	 * Adiciona a natureza solicitada.
	 * 
	 * @param nature Natureza a ser adicionada.
	 * @throws CoreException Exceção lançada pelo eclipse.
	 */
	private void justAddNature(final String nature) throws CoreException {
		if (!getNatureIds().contains(nature)) {
			IProjectDescription description = getResource().getDescription();
			String[] natures = description.getNatureIds();

			String[] newNatures = new String[natures.length + 1];
			newNatures = new String[natures.length + 1];
			System.arraycopy(natures, 0, newNatures, 0, natures.length);
			newNatures[natures.length] = nature;

			description.setNatureIds(newNatures);
			getResource().setDescription(description, null);
		}
	}

	/**
	 * Remove a natureza do projeto.
	 * 
	 * @param nature
	 * @throws CoreException
	 */
	@Override
	public boolean removeNature(final String nature) throws CoreException {
		boolean result = false;

		if (TotvsNature.NATURE_ID.equals(nature)) {
			justRemoveNature(TotvsNature.NATURE_ID);
			result = true;
		}

		return result;
	}

	/**
	 * Remove a natureza solicitada.
	 * 
	 * @param nature - Nome da natureza que ser� removida.
	 * @throws CoreException - Exceção lançada pelo eclipse.
	 */
	private void justRemoveNature(final String nature) throws CoreException {
		IProjectDescription description = getResource().getDescription();
		String[] natures = description.getNatureIds();

		for (int i = 0; i < natures.length; i++) {
			if (nature.equals(natures[i])) {
				String[] newNatures = new String[natures.length - 1];
				System.arraycopy(natures, 0, newNatures, 0, i);
				System.arraycopy(natures, i + 1, newNatures, i, natures.length - i - 1);
				description.setNatureIds(newNatures);
				getResource().setDescription(description, null);
			}
		}
	}

	@Override
	public List<String> getNatureIds() throws CoreException {
		IProjectDescription description = getResource().getDescription();
		String[] natures = description.getNatureIds();
		return Arrays.asList(natures);
	}

	/**
	 * Obtem membros de um projeto.
	 * 
	 * @return IResource[], conte�do do projeto.
	 * @throws CoreException
	 */
	@Override
	public IResource[] getMembers() throws CoreException {
		return this.getResource().members();
	}

	/**
	 * Verifica pelo nome se um membro existe no projeto.
	 * 
	 * @param memberName
	 * @return boolean
	 * @throws CoreException
	 */
	@Override
	public boolean hasMember(final String memberName) throws CoreException {
		IResource[] members = this.getResource().members();
		for (IResource iResource : members) {
			if (iResource.getName().equalsIgnoreCase(memberName)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Recupera um projeto pelo nome.
	 * 
	 * @param projectName nome do projeto
	 * @return IProjectWrapper projeto selecionado
	 * @throws CoreException indica falha de acesso na obtenção do inv�lucro.
	 */
	public static IProjectWrapper getProjectWrapper(final String projectName) throws CoreException {
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		Path path = new Path(projectName);
		IProject proj = workspace.getRoot().getProject(path.segment(0));
		return new ProjectWrapper(proj);
	}

	/**
	 * Recupera um projeto wrapper pelo IProject.
	 * 
	 * @param project projeto
	 * @return IProjectWrapper projeto selecionado
	 * @throws CoreException indica falha de acesso na obtenção do inv�lucro.
	 */
	public static IProjectWrapper getProjectWrapper(final IProject project) {
		return new ProjectWrapper(project);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see br.com.totvs.tds.sdk.wrapper.IResourceWrapper#isIgnoreCompile()
	 */
	@Override
	public boolean isIgnoreCompile() {

		return false;
	}

	/**
	 * Retorna uma propriedade de QualifiedName.
	 * 
	 * @param qnPropertyKey Chave de propriedade a ser retornada
	 * @return valor da propriedade
	 */
	private String getQnProperty(final QualifiedName qnPropertyKey) {
		try {
			String value = getResource().getPersistentProperty(qnPropertyKey);
			if (value == null) {
				return ""; //$NON-NLS-1$
			}

			return getResource().getPersistentProperty(qnPropertyKey);
		} catch (CoreException e) {
			e.printStackTrace();
		}

		return ""; //$NON-NLS-1$
	}

	/**
	 * Define uma propriedade de QualifiedName.
	 * 
	 * @param qnPropertyKey Chave de propriedade a ser definida
	 * @param value         Valor da propriedade a ser definiada
	 */
	private void setQnProperty(final QualifiedName qnPropertyKey, final String value) {
		try {
			getResource().setPersistentProperty(qnPropertyKey, value);
		} catch (CoreException e) {
			e.printStackTrace();
		}
	}

	@Override
	public String getVersion() {
		String version = getQnProperty(QN_PROJECT_VERSION);
		if (version == null || version.isEmpty()) {
			version = "undefined"; //$NON-NLS-1$
		}
		return version;
	}

	@Override
	public void setVersion(final String version) {
		setQnProperty(QN_PROJECT_VERSION, version);
	}

	@Override
	public void setBeforeApply(final String value) {
		setQnProperty(QN_TPL_BEFORE_APPLY, value);
	}

	@Override
	public String getBeforeApply() {
		return getQnProperty(QN_TPL_BEFORE_APPLY);
	}

	@Override
	public void setAfterApply(final String value) {
		setQnProperty(QN_TPL_AFTER_APPLY, value);
	}

	@Override
	public String getAfterApply() {
		return getQnProperty(QN_TPL_AFTER_APPLY);
	}

	@Override
	public boolean isIncludeSearchOk() {
		String[] includes = getIncludeSearchList(true);

		return (includes.length > 0);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see br.com.totvs.tds.sdk.wrapper.IResourceWrapper#setIgnoreCompile()
	 */
	@Override
	public void setIgnoreCompile(final boolean value) {
		try {
			IProject project = getResource();
			project.setPersistentProperty(QN_RESOURCECOMPILATION, String.valueOf(value));
		} catch (CoreException e) {
			e.printStackTrace();
		}
	}

	@Override
	public void toggleNature(String natureId) throws CoreException {
		if (removeNature(natureId)) {
			addNature(natureId);
		}
	}

}
