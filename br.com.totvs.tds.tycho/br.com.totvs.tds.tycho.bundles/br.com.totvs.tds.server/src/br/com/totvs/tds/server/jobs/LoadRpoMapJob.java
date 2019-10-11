package br.com.totvs.tds.server.jobs;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.services.IServiceLocator;

import br.com.totvs.tds.lsp.server.ILanguageServerService;
import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IRpoElement;
import br.com.totvs.tds.server.model.RpoObject;

/**
 * Job para mapeamento do rpo.
 */
public class LoadRpoMapJob extends Job {

	private IAppServerInfo server;
	private String environment;
	private List<IRpoElement> rpoObjectList;
	private boolean includeTres;

	/**
	 * Job para mapeamento do rpo.
	 * 
	 * @param name             The name of the job.
	 * @param server           The server to load the map from.
	 * @param includeTres
	 * 
	 * 
	 */
	public LoadRpoMapJob(final String name, final IAppServerInfo server, final String environment, boolean includeTres) {
		super(name);
		setSystem(true);
		setUser(true);

		this.server = server;
		this.environment = environment;
		this.includeTres = includeTres;
	}

	@Override
	protected IStatus run(final IProgressMonitor monitor) {
		IStatus ret = Status.CANCEL_STATUS;
		monitor.beginTask(Messages.LoadRpoMapJob_Load_rpo, 2);
		monitor.worked(1);

		rpoObjectList = new ArrayList<IRpoElement>();

		try {
			IServiceLocator serviceLocator = PlatformUI.getWorkbench();
			ILanguageServerService lsService = serviceLocator.getService(ILanguageServerService.class);

			List<String> rpoMap = lsService.getProgramMap(server.getToken(), environment, includeTres);
			for (String object : rpoMap) {
				int pos = object.indexOf("("); //$NON-NLS-1$

				IRpoElement rpoObject = new RpoObject();
				try {
					rpoObject.setName(object.substring(0, pos-1).trim());
					rpoObject.setDate(object.substring(pos + 1, object.length()-1));
					rpoObject.setVisible(true);
					rpoObject.setType(server.getRPOTypeElement(rpoObject.getName()));
				} catch (Exception e) {
					//RPOTypeElement.UNKNOWN
				}
				
				rpoObjectList.add(rpoObject);
			}

			ret = Status.OK_STATUS;
			monitor.worked(2);
		} catch (Exception e) {
			e.printStackTrace();
			ServerActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
		} finally {
			monitor.done();
		}

		return ret;
	}

	/**
	 * Recupera a lista contendo o mapeamento do rpo.
	 * 
	 * @return listagem
	 */
	public List<IRpoElement> getRpoMap() {
		return rpoObjectList;
	}

	public void setMessage(final String message) {
		this.setName(message);
	}

	public void setServer(final IAppServerInfo server) {
		this.server = server;
	}

	public void setEnvironment(final String environment) {
		this.environment = environment;
	}

}
