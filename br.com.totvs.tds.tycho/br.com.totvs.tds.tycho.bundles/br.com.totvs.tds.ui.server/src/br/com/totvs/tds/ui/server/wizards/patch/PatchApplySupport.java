package br.com.totvs.tds.ui.server.wizards.patch;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IStatus;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;
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

	Set<IFile> patchFilesSet = new HashSet<IFile>();

}
