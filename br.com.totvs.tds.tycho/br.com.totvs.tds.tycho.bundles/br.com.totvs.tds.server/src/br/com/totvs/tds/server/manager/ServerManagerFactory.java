package br.com.totvs.tds.server.manager;

import org.eclipse.ui.services.AbstractServiceFactory;
import org.eclipse.ui.services.IServiceLocator;

import br.com.totvs.tds.server.interfaces.IServerManager;

final public class ServerManagerFactory extends AbstractServiceFactory  {

		@Override
		public Object create(Class serviceInterface, IServiceLocator parentLocator, IServiceLocator locator) {
			
			if (serviceInterface.equals(IServerManager.class)) {
				return new ServerManagerImpl();
			}
			
			return null;
		}

}
