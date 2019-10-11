package br.com.totvs.tds.server.jobs;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.IStructuredSelection;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;

/**
 * Atributos de patch.
 */
public class BuildPatchAttributes implements Cloneable {

	private IAppServerInfo server = null;
	private List<String> environmentsList = new ArrayList<String>();
	private BuildPatchProcessType processo = BuildPatchProcessType.UNDEFINED;
	private PatchType patchType = PatchType.PATCH_PTM;
	private String masterPatch = ""; //$NON-NLS-1$
	private String patchFilePath = ""; //$NON-NLS-1$
	private String filename = ""; //$NON-NLS-1$

	private boolean compile;
	private boolean local = false;
	private boolean overwrite;
	private boolean verifyFile = false;
	private String environment;
	private List<String> resources = Collections.emptyList();
	private boolean prefix;
	private Path outputFile;
	private IStructuredSelection selection;
	private List<IFile> files = Collections.emptyList();

	/**
	 * Valida os atributos de patch.
	 *
	 * @return vazio se ok ou a mensagem de erro
	 */
	protected String getErrorMessage() {
		String message = null;
		if ((getEnvironment().length() == 0) || (getServer() == null)) {
			message = Messages.BuildPatchAttributes_Server_environment_not_reported;
		} else if (getPatchFilePath().length() == 0) {
			message = Messages.BuildPatchAttributes_Update_package_name_not_entered;
		} else if (getProcesso() == BuildPatchProcessType.UNDEFINED) {
			message = Messages.BuildPatchAttributes_Process_type_not_reported;
		}

		if (getProcesso() == BuildPatchProcessType.BY_COMPARISON) {
			if (getMasterPatch().length() == 0) {
				message = Messages.BuildPatchAttributes_Uninformed_master_rpo;
			}
		}

		return message;
	}

	/**
	 * Informs whether the resources were set to be compiled.
	 *
	 * @return verdadeiro em caso de compilar
	 */
	public boolean isCompile() {
		return compile;
	}

	/**
	 * Informs if should compile or not by the time the patch is generated.
	 *
	 * @param compile verdadeiro em caso de compilar
	 */
	public void setCompile(final boolean compile) {
		this.compile = compile;
	}

	/**
	 * Se true verifica no RPO se o arquivo existe, se false: não verifica se o
	 * fonte existe.
	 *
	 * @param verify
	 */
	public void setVerifyFilesRPO(final boolean verify) {
		this.verifyFile = verify;
	}

	/**
	 * Indica se os arquivos selecionados existem no RPO.
	 *
	 * @return
	 */
	public boolean getVerifyFilesRPO() {
		return this.verifyFile;
	}

	/**
	 * Returns the patch's process type.
	 *
	 * @return TipoProcesso
	 */
	public BuildPatchProcessType getProcesso() {
		return processo;
	}

	/**
	 * Defines the patch's process type.
	 *
	 * @param processo TipoProcesso
	 */
	public void setProcesso(final BuildPatchProcessType processo) {
		this.processo = processo;
	}

	/**
	 * Recupera o servidor utilizado.
	 *
	 * @return servidor
	 */
	public IAppServerInfo getServer() {
		return server;
	}

	/**
	 * Define o servidor utilizado.
	 *
	 * @param server servidor
	 */
	public void setServer(final IAppServerInfo server) {
		this.server = server;
	}

	/**
	 * Recupera o ambiente utilizado.
	 *
	 * @return ambiente
	 */
	public String getEnvironment() {
		return environment;
	}

	/**
	 * Define o ambiente utilizado.
	 *
	 * @param environment ambiente
	 */
	public void setEnvironment(final String environment) {
		this.environment = environment;
	}

	/**
	 * Recupera o patch master utilizado em comparações.
	 *
	 * @return patch master
	 */
	public String getMasterPatch() {
		return masterPatch;
	}

	/**
	 * Recupera o patch master utilizado em comparações.
	 *
	 * @return patch master
	 */
	public void setMasterPatch(final String masterPath) {
		this.masterPatch = masterPath;
	}

	/**
	 * Recupera o arquivo patch a ser gerado.
	 *
	 * @return arquivo patch
	 */
	public String getPatchFilePath() {
		return patchFilePath;
	}

	/**
	 * Define o arquivo patch a ser gerado.
	 *
	 * @param patchFilePath arquivo patch
	 */
	public void setPatchFilePath(final String patchFilePath) {
		this.patchFilePath = patchFilePath;
	}

	/**
	 * Recupera o tipo de patch a ser gerado.
	 *
	 * @return tipo de patch
	 */
	public PatchType getPatchType() {
		return patchType;
	}

	/**
	 * Define o tipo de patch a ser gerado.
	 *
	 * @param patchType tipo de patch
	 */
	public void setPatchType(final PatchType patchType) {
		this.patchType = patchType;
	}

	/**
	 * Recupera se o patch gerado ser� local ou remoto.
	 *
	 * @return verdadeiro caso seja local
	 */
	public boolean isLocal() {
		return local;
	}

	/**
	 * Define se o patch gerado ser� local ou remoto.
	 *
	 * @param local verdadeiro caso seja local
	 */
	public void setLocal(final boolean local) {
		this.local = local;
	}

	/**
	 * Recupera o nome do patch.
	 *
	 * @return nome
	 */
	public String getFilename() {
		return filename;
	}

	/**
	 * Define o nome do patch.
	 *
	 * @param filename nome do patch
	 */
	public void setFilename(final String filename) {
		this.filename = filename;
	}

	/**
	 * Recupera se � para sobrescrever um patch existente caso o nome do novo patch
	 * gerado seja igual.
	 *
	 * @return verdadeiro caso seja para sobrescrever
	 */
	public boolean isOverwrite() {
		return overwrite;
	}

	/**
	 * Define se � para sobrescrever um patch existente caso o nome do novo patch
	 * gerado seja igual.
	 *
	 * @param overwrite verdadeiro caso seja para sobrescrever
	 */
	public void setOverwrite(final boolean overwrite) {
		this.overwrite = overwrite;
	}

	/**
	 * Auto-explicative method.
	 *
	 * @return
	 */
	public String getServerName() {
		String serverName = ""; //$NON-NLS-1$

		if (server != null) {
			serverName = server.getName();
		}

		return serverName;
	}

	/**
	 * Valida os atributos.
	 */
	public boolean isValid() {
		return getErrorMessage() == null;
	}

	public void addEnvironment(final String currentEnvironment) {
		environmentsList.add(currentEnvironment);
	}

	public void removeEnvironment(final String element) {
		environmentsList.remove(element);
	}

	public List<String> getEnvironmentsList() {

		return environmentsList;
	}

	public void setResources(final List<String> resources) {
		this.resources = resources;
	}

	public List<String> getResources() {
		return this.resources;
	}

	@Override
	public BuildPatchAttributes clone() {

		try {
			return (BuildPatchAttributes) super.clone();
		} catch (final CloneNotSupportedException e) {
			ServerActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
		}

		return null;
	}

	public boolean isPrefix() {
		return prefix;
	}

	public void setPrefix(final boolean prefix) {
		this.prefix = prefix;
	}

	public Path getOutputFile() {

		return this.outputFile;
	}

	public void setOutputFile(final Path path) {
		this.outputFile = path;
	}

	public IStructuredSelection geSelection() {

		return this.selection;
	}

	public void setSelection(final IStructuredSelection selection) {
		this.selection = selection;
	}

	public List<IFile> getResourceFiles() {
		return this.files;
	}

	public void setResourcesFiles(final List<IFile> files) {
		this.files = files;
	}

	public List<IFile> getResourcesFiles() {
		return this.files;
	}
}