/**
 * 
 */
package br.com.totvs.tds.ui.sdk.wrapper.internal;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.QualifiedName;

import br.com.totvs.tds.ui.sdk.wrapper.IFolderWrapper;

/**
 * Inv�lucro de pastas.
 * 
 * @author acandido
 */
public final class FolderWrapper extends ContainerWrapper implements IFolderWrapper {

	/** Identificado de propriedade 'FolderCompilation'. */
	public static final QualifiedName QN_FOLDERCOMPILATION = new QualifiedName("com.totvs.tds.sdk.FolderCompilation", //$NON-NLS-1$
			"com.totvs.tds.sdk.FolderCompilation"); //$NON-NLS-1$

	/**
	 * Construtor.
	 * 
	 * @param folder
	 *            , pasta associada ao inv�lucro.
	 */
	public FolderWrapper(final IFolder folder) {
		super(folder);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see br.com.totvs.tds.sdk.wrapper.IResourceWrapper#getResource()
	 */
	@Override
	public IFolder getResource() {
		return (IFolder) super.getResource();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see br.com.totvs.tds.sdk.wrapper.IFolderWrapper#isIgnoreCompile()
	 */
	@Override
	public boolean isIgnoreCompile() {
		boolean ret = false;

		IFolder folder = getResource();
		ret = hasFatherIgnoreProperty(folder);

		return ret;
	}

	/**
	 * @param resource
	 * @return indica se o recurso possui configuração para ser ignorado
	 */
	private boolean hasFatherIgnoreProperty(final IResource resource) {
		try {
			if (Boolean.TRUE.toString().equals(resource.getPersistentProperty(QN_FOLDERCOMPILATION))) {
				return true;

			} else if (resource.getParent() != null) {
				return hasFatherIgnoreProperty(resource.getParent());
			}

		} catch (CoreException e) {
			e.printStackTrace();
		}

		return false;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see br.com.totvs.tds.sdk.wrapper.IFolderWrapper#setIgnoreCompile()
	 */
	@Override
	public void setIgnoreCompile(final boolean value) {
		try {
			IFolder folder = getResource();
			folder.setPersistentProperty(QN_FOLDERCOMPILATION, String.valueOf(value));
		} catch (CoreException e) {
			e.printStackTrace();
		}
	}

}
