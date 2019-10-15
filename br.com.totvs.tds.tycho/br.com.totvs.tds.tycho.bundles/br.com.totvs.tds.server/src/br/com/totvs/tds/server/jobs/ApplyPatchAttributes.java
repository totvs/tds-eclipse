package br.com.totvs.tds.server.jobs;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IServerInfo;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.server.jobs.ApplyPatchFileReturn.MessageType;

/**
 * Atributos para aplicaão de patchs.
 */
public class ApplyPatchAttributes implements Cloneable {

	private List<ApplyPatchFileReturn> applyPatchFilesReturn = new ArrayList<ApplyPatchFileReturn>();
	private String environment;
	private List<IServerInfo> activeServers;
	private IAppServerInfo currentAppServer;
	private List<String> resources;

	public ApplyPatchAttributes() {

	}

	/**
	 * @return the applyPatchFilesReturn
	 */
	public List<ApplyPatchFileReturn> getApplyPatchFilesReturn() {
		return applyPatchFilesReturn;
	}

	/**
	 * @param applyPatchFilesReturn the applyPatchFilesReturn to set
	 */
	public void setApplyPatchFilesReturn(final List<ApplyPatchFileReturn> applyPatchFilesReturn) {
		this.applyPatchFilesReturn = applyPatchFilesReturn;
	}

	/**
	 * @param applyPatchFilesReturn the applyPatchFilesReturn to set
	 */
	public void addApplyPatchFileReturn(final ApplyPatchFileReturn applyPatchFileReturn) {
		final String source = applyPatchFileReturn.getOriginalFile().toLowerCase();

		for (final ApplyPatchFileReturn item : getApplyPatchFilesReturn()) {
			final String target = item.getOriginalFile().toLowerCase();
			if (source.equals(target)) {
				applyPatchFileReturn.setMessageType(MessageType.DUPLICATE_FILE);
				break;
			}
		}

		applyPatchFilesReturn.add(applyPatchFileReturn);
	}

	/**
	 * @return servidor aonde será aplicaodo os patchs
	 */
	public IAppServerInfo getCurrentAppServer() {
		return currentAppServer;
	}

	/**
	 * @param servidor servidor aonde será aplicaodo os patchs
	 */
	public void setCurrentAppServer(final IAppServerInfo servidor) {
		this.currentAppServer = servidor;
	}

	/**
	 * @return ambiente destino.
	 */
	public String getEnvironment() {
		return environment;
	}

	/**
	 * Define o ambiente destino.
	 *
	 * @param environment ambiente destino.
	 */
	public void setEnvironment(final String environment) {
		this.environment = environment;
	}

	public boolean removerApplyPatchFileReturn(final ApplyPatchFileReturn applyPatchFileReturn) {
		return applyPatchFilesReturn.remove(applyPatchFileReturn);
	}

	@Override
	protected Object clone() throws CloneNotSupportedException {
		final ApplyPatchAttributes newInstance = new ApplyPatchAttributes();
		newInstance.setCurrentAppServer(currentAppServer);
		newInstance.setEnvironment(environment);

		return newInstance;
	}

	public List<IServerInfo> getServerList() {
		if (activeServers == null) {
			final IWorkbench serviceLocator = PlatformUI.getWorkbench();
			final IServerManager serverManager = serviceLocator.getService(IServerManager.class);
			activeServers = serverManager.getActiveServers(IAppServerInfo.class);
		}

		return this.activeServers;
	}

	public void setResources(final List<String> resources) {
		this.resources = resources;
	}
}
