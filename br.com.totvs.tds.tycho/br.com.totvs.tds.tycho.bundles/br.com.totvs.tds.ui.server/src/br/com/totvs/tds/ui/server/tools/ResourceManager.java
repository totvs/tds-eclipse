package br.com.totvs.tds.ui.server.tools;

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.Bundle;

import br.com.totvs.tds.ui.server.ServerUIActivator;

public class ResourceManager {

	private static ResourceManager instance = null;

	public static ResourceManager getInstance() {
		if (instance == null) {
			instance = new ResourceManager();
		}
		return instance;
	}

	private ImageRegistry registry;

	public ResourceManager() {
		registry = new ImageRegistry(PlatformUI.getWorkbench().getDisplay());
	}

	public Image getImage(final AbstractUIPlugin activator, final String URL, final String image) {
		Image imageRet = registry.get(URL + image);
		if (imageRet == null) {
			try {
				registry.put(URL + image,
						ImageDescriptor.createFromURL(new URL(activator.getBundle().getEntry(URL), image)));
			} catch (MalformedURLException e) {
				e.printStackTrace();
			}
			imageRet = registry.get(URL + image);
		}
		return imageRet;
	}

	public Image getImage(final String URL, final String image) {
		return getImage(ServerUIActivator.getDefault(), URL, image);
	}

	public ImageDescriptor getImageDescriptor(final AbstractUIPlugin plugin, final String URL, final String image) {
		return org.eclipse.wb.swt.ResourceManager.getPluginImageDescriptor(plugin, URL + image);
	}

	public ImageDescriptor getImageDescriptor(final String URL, final String image) {

		ImageDescriptor imageRet = registry.getDescriptor(URL + image);
		if (imageRet == null) {
			try {
				Bundle bundle = ServerUIActivator.getDefault().getBundle();
				URL entry = bundle.getEntry(URL);
				registry.put(URL + image, ImageDescriptor.createFromURL(new URL(entry, image)));
			} catch (MalformedURLException e) {
				e.printStackTrace();
			}
			imageRet = registry.getDescriptor(URL + image);
		}
		return imageRet;
	}

	public void removeAll() {
		if (registry != null)
			registry.dispose();
	}

	public void removeImage(final String URL, final String image) {
		registry.remove(URL + image);
	}

}
