package br.com.totvs.tds.ui.sdk.widget;

import java.io.File;
import java.io.FilenameFilter;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;

import br.com.totvs.tds.ui.sdk.wrapper.IIncludeDataModel;

/**
 * Modelo de dados para definição da pasta para busca.
 * 
 * @author acandido
 */
public class IncludeDataModel implements IIncludeDataModel {

	private final String folder;
	private String message;
	private boolean warning;

	/**
	 * @param folder Pasta a ser utilizada na busca.
	 */
	public IncludeDataModel(final String folder) {
		this.folder = folder;
		this.message = null;

		validade();
	}

	/**
	 * @return the folder
	 */
	public String getFolder() {
		return folder;
	}

	/**
	 * @return the message
	 */
	public String getMessage() {
		return message;
	}

	/**
	 * Valida a pasta.
	 */
	private void validade() {
		String folder = getFolder();

		message = null;
		warning = false;

		if (!folder.startsWith(GLOBAL)) {
			IPath path = null;
			
			if (folder.startsWith(WORKSPACE)) {
				IWorkspace workspace = ResourcesPlugin.getWorkspace();
				IWorkspaceRoot root = workspace.getRoot();
				IResource resource = root.findMember(folder.substring(WORKSPACE.length()));
				if (resource == null) {
					message = Messages.IncludeDataModel_Not_found;
				}
				path = resource.getLocation();
			} else {
				path = new Path(getFolder());
			}
			
			File file = path.toFile();
			if (!file.exists()) {
				message = Messages.IncludeDataModel_Not_found;
			} else if (!file.isDirectory()) {
				message = Messages.IncludeDataModel_No_folder;
			} else {
				FilenameFilter filter = new FilenameFilter() {

					@Override
					public boolean accept(final File dir, final String name) {
						return (name.endsWith(".ch") || name.endsWith(".per")); //$NON-NLS-1$ //$NON-NLS-2$
					}
				};
				
				if (file.list(filter).length == 0) {
					message = Messages.IncludeDataModel_No_definition_files;
					warning = true;
				}
			}
		}
	}

	/**
	 * @return the warning
	 */
	public boolean isWarning() {
		return warning;
	}

	public boolean isGlobal() {
		return folder.startsWith(GLOBAL);
	}

	public boolean isWorkspace() {
		return folder.startsWith(WORKSPACE);
	}

}
