package br.com.totvs.tds.lsp.server;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.FileLocator;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class ActivatorServer implements BundleActivator {

	private static ActivatorServer plugins;

	@Override
	public void start(BundleContext context) throws Exception {
		plugins = this;
		
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		plugins = null;
	}

	/**
	 * 
	 * @return caminho compledo do execut�vel DA
	 * @throws IOException
	 */
	public String getDACommand() throws IOException {
		URL daPathUrl = getClass().getResource("/resources/tds-da/debugAdapter.exe"); //$NON-NLS-1$
		if (daPathUrl == null) {
			daPathUrl = getClass().getResource("/resources/tds-da/debugAdapter"); //$NON-NLS-1$
		}
		File daPath = new File(FileLocator.toFileURL(daPathUrl).getPath());

		return daPath.toString();
	};

	/**
	 * 
	 * @return argumentos para execução do DA
	 */
	public List<String> getDAArgs() {
		List<String> args = new ArrayList<String>();
				
		return args;
	}

	public static ActivatorServer getInstance() {
		
		return ActivatorServer.plugins;
	}

}
