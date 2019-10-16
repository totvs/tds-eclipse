package br.com.totvs.tds.ui.server.wizards.patch;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IEnvironmentInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.server.jobs.applyPatch.ApplyPatchFileReturn;
import br.com.totvs.tds.server.model.SourceInformation;
import br.com.totvs.tds.ui.server.ServerUIActivator;

/**
 * Classe PatchApplySupport.
 *
 * @author leo.watanabe
 *
 */
public class PatchApplySupport {

	/**
	 * Communicates with the server to return the details of the informed patch.<br>
	 * The where index must be: <br>
	 * 0 = Local patch<br>
	 * 1 = Remote patch.
	 *
	 * @param patchFile
	 * @param server
	 * @param environment
	 * @param whereIndex
	 * @return
	 * @throws Exception
	 */
	public static List<SourceInformation> getPatchDetails(final String patchFile, final IAppServerInfo server,
			final String environment, final int whereIndex) throws Exception {
		List<SourceInformation> patchDetails = null;

		if (server == null) {
			IStatus status = ServerUIActivator.logStatus(IStatus.ERROR, "Valor selecionado para servidor é inválido");
			throw new IllegalArgumentException(status.getMessage());
		}

		try {
			Path serverPatch = null;

			if (whereIndex == 0) {
				// LOCAL
//				serverPatch = Paths.get(server.getTemporaryRemoteDir(environment), FilenameUtils.getName(patchFile));
//				if (!server.fileCopy(environment, patchFile, serverPatch.toString(), false)) {
				IStatus status = ServerUIActivator.logStatus(IStatus.ERROR,
						"Não foi possivel enviar o arquivo para o servidor. O RPO pode estar em uso.");
				throw new RuntimeException(status.getMessage());
//				}
			} else if (whereIndex == 1) {
				// REMOTE
				serverPatch = Paths.get(patchFile);
			} else {
				IStatus status = ServerUIActivator.logStatus(IStatus.ERROR, "Valor selecionado não esperado");
				throw new IllegalArgumentException(status.getMessage());
			}

			patchDetails = server.getPatchInfo(environment, serverPatch);
		} catch (Exception e) {
			ServerUIActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
			throw e;
		}

		return patchDetails;
	}

	private String environment;
	private IAppServerInfo currentServer;

	Set<IFile> patchFilesSet = new HashSet<IFile>();

	private List<IFile> getFiles(final IContainer container) throws CoreException {
		List<IFile> files = new ArrayList<IFile>();
		IResource[] members = container.members();
		for (IResource iResource : members) {
			// if (iResource instanceof IContainer) {
			// files.addAll(getFiles((IContainer) iResource));
			// } else
			if (iResource instanceof IFile) {
				files.add((IFile) iResource);
			}
		}
		return files;
	}

	private List<IFile> getPatchFiles(final Object resource) throws CoreException {
		List<IFile> patchFiles = new ArrayList<IFile>();
		List<IFile> fileList = new ArrayList<IFile>();
		if (resource instanceof IContainer) {
			IContainer iContainer = (IContainer) resource;
			fileList = getFiles(iContainer);
		} else if (resource instanceof IFile) {
			IFile iFile = (IFile) resource;
			fileList.add(iFile);
		}
		for (IFile iFile : fileList) {
			if (isPatchFile(iFile)) {
				patchFiles.add(iFile);
			}
		}
		return patchFiles;
	}

	private boolean isPatchFile(final IFile iFile) {
		boolean isPatchFile = false;
		String fileExtension = iFile.getFileExtension();
		if ("PTM".equalsIgnoreCase(fileExtension) || "UPD".equalsIgnoreCase(fileExtension) //$NON-NLS-1$ //$NON-NLS-2$
				|| "PAK".equalsIgnoreCase(fileExtension)) { //$NON-NLS-1$
			isPatchFile = true;
		}
		return isPatchFile;
	}

	private List<ApplyPatchFileReturn> patchFilesToApplyPatchFileReturn(final Set<IFile> patchFiles) {
		List<ApplyPatchFileReturn> patchFilesToApplyPatchFileReturn = new ArrayList<ApplyPatchFileReturn>();
		ApplyPatchFileReturn applyPatchFileReturn = null;
		for (IFile iFile : patchFiles) {
			applyPatchFileReturn = new ApplyPatchFileReturn(iFile.getLocation().toOSString());
			applyPatchFileReturn.setLocal(true);
			patchFilesToApplyPatchFileReturn.add(applyPatchFileReturn);
		}
		return patchFilesToApplyPatchFileReturn;
	}

	private void setEnvironmentInternal(final IItemInfo itemInfo) throws Exception {
		IAppServerInfo serverInfo = null;

		if (itemInfo != null) {
			if (itemInfo instanceof IEnvironmentInfo) {
				environment = itemInfo.getName();
				IItemInfo parent = itemInfo.getParent();
				if (parent instanceof IAppServerInfo) {
					serverInfo = (IAppServerInfo) parent;
					if (serverInfo != null && serverInfo.isConnected()) {
						currentServer = serverInfo;
					}
				}
			} else if (environment == null && currentServer != null) {
				environment = currentServer.getCurrentEnvironment();
			}
		}
	}

	private void setServerInternal(final IItemInfo itemInfo) throws Exception {
		IAppServerInfo serverInfo = null;

		if (itemInfo != null && itemInfo instanceof IAppServerInfo) {
			serverInfo = (IAppServerInfo) itemInfo;
			if (serverInfo != null && serverInfo.isConnected()) {
				currentServer = serverInfo;

				if ((currentServer != null) && (environment == null)) {
					environment = currentServer.getCurrentEnvironment();
				}
			}
		}
	}

}
