package br.com.totvs.tds.ui.sdk.wrapper.internal;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;

import br.com.totvs.tds.ui.sdk.SdkUIActivator;
import br.com.totvs.tds.ui.sdk.wrapper.IResourceWrapper;
import br.com.totvs.tds.ui.sdk.wrapper.WrapperManager;

public class ContainerWrapper {

	static final List<IFile> EMPTY_FILE_LIST = new ArrayList<IFile>();

	final private IContainer resource;

	public ContainerWrapper(IContainer container) {
		this.resource = container;
	}

	/**
	 * Obtém membros de um folder.
	 * 
	 * @return IResource[], conteúdo do folder.
	 * @throws CoreException
	 */
	public IResource[] getMembers() throws CoreException {
		return this.resource.members();
	}

	/**
	 * Verifica pelo nome se um membro existe no folder.
	 * 
	 * @param memberName
	 * @return boolean
	 * @throws CoreException
	 */
	public boolean hasMember(final String memberName) throws CoreException {
		IResource[] members = this.resource.members();
		for (IResource iResource : members) {
			if (iResource.getName().equalsIgnoreCase(memberName)) {
				return true;
			}
		}
		return false;
	}

	public boolean isContainer() {

		return true;
	}

	public List<IFile> getChildFiles(boolean allLevels) {
		try {
			return getChildFiles(this.resource, allLevels);
		} catch (CoreException e) {
			SdkUIActivator.logStatus(IStatus.ERROR, "Interno", e.getMessage(), e);
		}

		return EMPTY_FILE_LIST;
	}

	private List<IFile> getChildFiles(IContainer container, boolean allLevels) throws CoreException {
		IResource[] members = container.members();
		List<IFile> list = new ArrayList<IFile>();

		for (IResource member : members) {
			if (member instanceof IContainer) {
				list.addAll(getChildFiles((IContainer) member, allLevels));
			} else if (member instanceof IFile) {
				IFile file = (IFile) member;
				IResourceWrapper fw = WrapperManager.getInstance().getWrapper(file);

				if (!fw.isIgnoreCompile()) {
					list.add((IFile) member);
				}
			}
		}

		return list;
	}

	/**
	 * @return the resource
	 */
	protected IContainer getResource() {
		return resource;
	}

}
