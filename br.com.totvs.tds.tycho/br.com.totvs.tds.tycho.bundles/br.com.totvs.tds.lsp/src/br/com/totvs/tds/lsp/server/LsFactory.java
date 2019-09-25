package br.com.totvs.tds.lsp.server;

import org.eclipse.ui.services.AbstractServiceFactory;
import org.eclipse.ui.services.IServiceLocator;

/**
 * 'Factory' do serviço de notificações.
 *
 * @author acandido
 */
public final class LsFactory extends AbstractServiceFactory {

	@SuppressWarnings("rawtypes")
	@Override
	public Object create(final Class serviceInterface, final IServiceLocator parentLocator,
			final IServiceLocator locator) {

		if (serviceInterface.equals(ILanguageServerService.class)) {
			return LsServiceImpl.getInstance();
		}

		return null;
	}

}
