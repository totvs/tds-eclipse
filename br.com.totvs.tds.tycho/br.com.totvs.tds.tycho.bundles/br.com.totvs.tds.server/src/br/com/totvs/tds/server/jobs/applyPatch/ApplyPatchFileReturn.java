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

	/**
	 * Enum MessageType.
	 *
	 * @author leo.watanabe
	 *
	 */
	public enum MessageType {
		NEW, NEW_ZIP, OK, WARNING, ERROR, DUPLICATE_FILE;

		public String getSituation() {
			switch (MessageType.this) {
			case NEW:
			case NEW_ZIP:
				return "Novo";
			case OK:
				return "OK";
			case WARNING:
				return "Há restrições";
			case ERROR:
				return "Há erros";
			default:
				break;
			}

			return "Duplicado";
		}
	};

	private MessageType messageType = MessageType.NEW;

	private IPath patchFile;

	private boolean local;

	private ApplyPatchMode applyMode = ApplyPatchMode.VALIDATE_PATCH;

	private String validationMessage;

	private List<String[]> oldPrograms;

	private boolean validated = false;

	private boolean isTemporary = false;

	private String documentationURL;

	private String originalFile = null;

	public ApplyPatchFileReturn(final IPath patchFile) { // throws IllegalArgumentException {
		this.patchFile = patchFile;
	}

	public ApplyPatchFileReturn(final String patchFile) { // throws IllegalArgumentException {
		this.patchFile = Path.fromOSString(patchFile);
	}

	/**
	 * @return the patchFile
	 */
	public IPath getPatchFile() {
		return patchFile;
	}

	/**
	 * The patch's full system path if it is local, or the server relative path if
	 * it is remote.
	 *
	 * @return
	 */
	public String getPatchFullPath() {
		return patchFile.makeAbsolute().toOSString();
	}

	/**
	 * Retorna se os arquivos patch s�o locais ou remotos.
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

	/**
	 * Return the type of the message.
	 *
	 * @return
	 */
	public MessageType getMessageType() {
		return messageType;
	}

	/**
	 * Define the type of the return message.
	 *
	 * @param messageType
	 */
	public void setMessageType(final MessageType messageType) {
		this.messageType = messageType;
	}

	/**
	 * Informs that this return message was validated by the user.
	 *
	 * @param isValidated
	 */
	public void setValidated(final boolean isValidated) {
		this.validated = isValidated;
	}

	/**
	 * Informs if this return message was validated by the user.
	 *
	 * @return
	 */
	public boolean isValidated() {
		return this.validated;
	}

	public void setTemporary(final boolean isTemporary) {
		this.isTemporary = isTemporary;
	}

	public boolean isTemporary() {
		return isTemporary;
	}

	public void setDocumentationURL(final String documentationURL) {
		this.documentationURL = documentationURL;
	}

	/**
	 * Returns the documentation URL as String if existent.<br>
	 * Otherwise will return null.
	 *
	 * @return - An external URL.
	 */
	public String getDocumentationURL() {
		return this.documentationURL;
	}

	/**
	 * @return the originalFile
	 */
	public String getOriginalFile() {
		return originalFile == null ? getPatchFile().toOSString() : originalFile;
	}

	/**
	 * @param originalFile the originalFile to set
	 */
	public void setOriginalFile(final String originalFile) {
		this.originalFile = originalFile;
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

}
