package br.com.totvs.tds.ui.sdk.wrapper.internal;

import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.QualifiedName;

import br.com.totvs.tds.ui.sdk.SdkUIActivator;
import br.com.totvs.tds.ui.sdk.wrapper.IFolderWrapper;
import br.com.totvs.tds.ui.sdk.wrapper.IProjectWrapper;
import br.com.totvs.tds.ui.sdk.wrapper.IResourceWrapper;
import br.com.totvs.tds.ui.sdk.wrapper.IWrapperManager;
import br.com.totvs.tds.ui.sdk.wrapper.WrapperManager;

/**
 * @author acandido
 */
public final class FileWrapper implements IResourceWrapper {

	/** Recurso associado ao inv�lucro. */
	private final IFile resource;

	/**
	 * Construtor.
	 * 
	 * @param resource , arquivo de recurso associada ao inv�lucro.
	 */
	public FileWrapper(final IFile resource) {
		this.resource = resource;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see br.com.totvs.tds.sdk.wrapper.IResourceWrapper#getResource()
	 */
	@Override
	public IFile getResource() {
		return resource;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see br.com.totvs.tds.sdk.wrapper.IResourceWrapper#isIgnoreCompile()
	 */
	@Override
	public boolean isIgnoreCompile() {
		String ext = getResource().getFileExtension();

		if (ext == null) {
			return true;
		}
		
		boolean ignoreCompile = ext.equalsIgnoreCase("trparam") //$NON-NLS-1$
				|| isIgnoreProperty();

		if (ignoreCompile) {
			return true;
		}

		if (resource.getName().startsWith(".")) { //$NON-NLS-1$
			ignoreCompile = true;
		} else {
			try {
				IWrapperManager wrapperManager = WrapperManager.getInstance();
				if (wrapperManager.getWrapper(resource.getParent()) instanceof IFolderWrapper) {
					IFolderWrapper wpFolder = (IFolderWrapper) wrapperManager.getWrapper(resource.getParent());
					ignoreCompile = wpFolder.isIgnoreCompile();
				} else if (wrapperManager.getWrapper(resource.getParent()) instanceof IProjectWrapper) {
					IProjectWrapper wpProject = (IProjectWrapper) wrapperManager.getWrapper(resource.getParent());
					ignoreCompile = wpProject.isIgnoreCompile();
				}
			} catch (CoreException e) {
				SdkUIActivator.logStatus(IStatus.ERROR, Messages.FileWrapper_Internal, e.getMessage(), e);
			}
		}

		return ignoreCompile;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see br.com.totvs.tds.sdk.wrapper.IResourceWrapper#setIgnoreCompile()
	 */
	@Override
	public void setIgnoreCompile(final boolean value) {
		try {
			IFile file = getResource();
			if (value) {
				file.setPersistentProperty(QN_RESOURCECOMPILATION, String.valueOf(value));
			} else {
				Map<QualifiedName, String> pp = file.getPersistentProperties();
				pp.remove(QN_RESOURCECOMPILATION);
			}
		} catch (CoreException e) {
		}
	}

	/**
	 * @param resource
	 * @return indica se o recurso possui configuração para ser ignorado
	 */
	private boolean isIgnoreProperty() {
		try {
			if (getResource().getPersistentProperties().containsKey(QN_RESOURCECOMPILATION)) {
				String resourceCompilation = getResource().getPersistentProperty(QN_RESOURCECOMPILATION);
				return Boolean.valueOf(resourceCompilation);
			}
		} catch (CoreException e) {
		}

		return false;
	}

	@Override
	public boolean isContainer() {

		return false;
	}

	@Override
	public List<IFile> getChildFiles(boolean allLevels) {

		return EMPTY_FILE_LIST;
	}

}
