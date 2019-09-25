package br.com.totvs.tds.ui;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

public class TestActivator extends AbstractUIPlugin {

	private static TestActivator INSTANCE;

	@Override
	public void start(final BundleContext context) throws Exception {
		INSTANCE = this;
	}

	@Override
	public void stop(final BundleContext context) throws Exception {
		// TODO Auto-generated method stub

	}

	public static TestActivator getInstance() {
		return INSTANCE;
	}

}
