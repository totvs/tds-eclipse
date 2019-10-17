package br.com.totvs.tds.server.jobs.applyPatch;

import java.io.File;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;

/**
 * Classe ApplyPatchFileReturn.
 *
 * @author leo.watanabe
 *
 */
public class ApplyPatchFileReturn {

	public static final String RPO_CONTAINS_MORE_RECENT_OBJECTS = "O RPO contém objetos mais recentes que os existentes no pacote.";

	private ApplyPatchState patchState = ApplyPatchState.NEW;
	private ApplyPatchMode applyMode = ApplyPatchMode.NEED_VALIDATE;
	private IPath patchFile;
	private boolean local;
	private String validationMessage;
	private List<String[]> oldPrograms;
	private boolean isTemporary = false;

	public ApplyPatchFileReturn(final IPath patchFile) {
		this.patchFile = patchFile;
	}

	public ApplyPatchFileReturn(final String patchFile) {
		this(Path.fromOSString(patchFile));
	}

	/**
	 * @return the patchFile
	 */
	public IPath getPatchFile() {
		return patchFile;
	}

	/**
	 * Retorna se o arquivo é local ou remoto.
	 *
	 * @return verdadeiro caso sejam arquivos locais
	 */
	public boolean isLocal() {
		return local;
	}

	/**
	 * Define se os arquivos patch s�o locais ou remotos.
	 *
	 * @param local verdadeiro caso sejam arquivos locais
	 */
	public void setLocal(final boolean local) {
		this.local = local;
	}

	/**
	 * Retorna o mode de aplicação de patch.
	 *
	 * @return mode de aplicação do patch
	 * @see ApplyPatchMode
	 */
	public ApplyPatchMode getApplyMode() {
		return applyMode;
	}

	/**
	 * Define o mode de aplicação de patch.
	 *
	 * @param applyMode mode de aplicação do patch
	 * @see ApplyPatchMode
	 */
	public void setApplyMode(final ApplyPatchMode applyMode) {
		this.applyMode = applyMode;
	}

	/**
	 * @return the validationMessage
	 */
	public String getValidationMessage() {
		return validationMessage;
	}

	/**
	 * @param validationMessage the validationMessage to set
	 */
	public void setValidationMessage(final String validationMessage) {
		this.validationMessage = validationMessage;
	}

	/**
	 * @return the oldPrograms
	 */
	public List<String[]> getOldPrograms() {
		return oldPrograms;
	}

	/**
	 * @param list the oldPrograms to set
	 */
	public void setOldPrograms(final List<String[]> list) {
		this.oldPrograms = list;
	}

	public void setTemporary(final boolean isTemporary) {
		this.isTemporary = isTemporary;
	}

	public boolean isTemporary() {
		return isTemporary;
	}

	public long getSize() {
		if (isLocal()) {
			final File file = getPatchFile().toFile();
			if (file.exists()) {
				return file.length();
			}
		}

		return -1;
	}

	/**
	 * @return the patchState
	 */
	public ApplyPatchState getPatchState() {
		return patchState;
	}

	/**
	 * @param patchState the patchState to set
	 */
	public void setPatchState(final ApplyPatchState patchState) {
		this.patchState = patchState;
	}

}
