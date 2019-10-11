package br.com.totvs.tds.sdk.wrapper.internal;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;

import br.com.totvs.tds.sdk.SdkActivator;
import br.com.totvs.tds.sdk.wrapper.IResourceWrapper;
import br.com.totvs.tds.sdk.wrapper.WrapperManager;

public class ContainerWrapper {

	static final List<IFile> EMPTY_FILE_LIST = new ArrayList<IFile>();

	final private IContainer resource;

	public ContainerWrapper(final IContainer container) {
		this.resource = container;
	}

	/**
	 * Obt�m membros de um folder.
	 *
	 * @return IResource[], conte�do do folder.
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
		final IResource[] members = this.resource.members();
		for (final IResource iResource : members) {
			if (iResource.getName().equalsIgnoreCase(memberName)) {
				return true;
			}
		}
		return false;
	}

	public boolean isContainer() {

		return true;
	}

	public List<IFile> getChildFiles(final boolean allLevels) {
		try {
			return getChildFiles(this.resource, allLevels);
		} catch (final CoreException e) {
			SdkActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
		}

		return EMPTY_FILE_LIST;
	}

	private List<IFile> getChildFiles(final IContainer container, final boolean allLevels) throws CoreException {
		final IResource[] members = container.members();
		final List<IFile> list = new ArrayList<IFile>();

		for (final IResource member : members) {
			if (member instanceof IContainer) {
				list.addAll(getChildFiles((IContainer) member, allLevels));
			} else if (member instanceof IFile) {
				final IFile file = (IFile) member;
				final IResourceWrapper fw = WrapperManager.getInstance().getWrapper(file);

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
